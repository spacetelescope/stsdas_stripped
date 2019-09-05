# include <stdio.h>
# include <string.h>
# include <ximio.h>
# include <hstio.h>
# include "estreak.h"


/*  G_WRITERESULT:   Writes the computed flat field image and its anciliary
 *                   arrays into the output file(s).
 *
 *   This routine actually calls very specialized sub-routines, one for
 *   each separate instrument, which handle the details pertinent to
 *   each format. A WFPC 4-group GEIS file is handled directly by IRAF's 
 *   IMIO routines, as in the old 'streakflat' task. A FITS-with-5-extensions
 *   NICMOS file is handled by HSTIO singleNicmosGroup routines.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   03 May 96  -  Implementation  (IB)
 *   22 May 96  -  WFPC support  (IB)
 *   07 Oct 96  -  Revised after code review (IB)
 *
 */

int g_writeResult (IOControl *ioc, char *output, int width[], int niter,
                   short badmask, floatArray *flat, floatArray *error, 
                   floatArray *time, floatArray *sample, char time_begin[]) {

/*  Parameters:
 *
 *  IOControl              ioc   i: I/O control structure
 *  char      output[SZ_FNAME]   i: output file name
 *  int        width[MAX_ITER]   i: boxcar filter widths
 *  int                  niter   i: number of iterations
 *  short              badmask   i: flag value for output bad pixels
 *  floatArray            flat   i: flat (main task's result) array
 *  floatArray           error   i: array with error image
 *  floatArray            time   i: array with accum. integration times
 *  floatArray          sample   i: array with accum. # of samples
 *  char          time_begin[]   i: time at which task begun
 *
 */

	int g_outNicmos (IOControl *, char *, int [], int, short, 
            floatArray *, floatArray *, floatArray *, floatArray *, char []);
	int g_outWfpc   (IOControl *, char *, short, floatArray *);

	switch (ioc->instrument) {
	case NICMOS:
	    if (g_outNicmos (ioc, output, width, niter, badmask, flat, 
                             error, time, sample, time_begin)) return (1);
	    break;
	case WFPC:
	    if (g_outWfpc (ioc, output, badmask, flat)) return (1);
	    break;
	}
	return (0);
}




/*  NICMOS routine: writes results and update header at once.  */

int g_outNicmos (IOControl *ioc, char *output, int width[], int niter,
                 short badmask, floatArray *flat, floatArray *error, 
                 floatArray *time, floatArray *sample, char time_begin[]) {

	char                      input[SZ_NAME];
	char              time_end[SZ_TIMESTAMP];
	long                                   l;
	SingleNicmosGroup                     sg;

	void g_timeStamp (char *);
	int g_updateHeader (IOControl *, SingleNicmosGroup *, 
                            IRAFPointer, IRAFPointer, 
                            char *, int *,int, char *, char *);

	/* Read 1st image to get all info to be inherited by output image. */
	initSingleNicmosGroup (&sg);
	strcpy (input, ioc->image[0].filename);
	strcat (input, ioc->inputSuffix);
	strcat (input, ioc->inputExten);
	getSingleNicmosGroup (input, NEXTVER, &sg);

	/* Prepare output arrays. */
	for (l = 0; l < flat->bufsize; l++) {
	    /* SCI and DQ */
	    if (flat->data[l] != BADVAL) {
	        sg.sci.data.data[l] = flat->data[l];
	        sg.dq.data.data[l]  = 0;
	    } else {
	        sg.sci.data.data[l] = 0.0F;
	        sg.dq.data.data[l]  = badmask;
	    }
	    /* ERR, TIME and SMPL */
	    sg.err.data.data[l]  = error->data[l];
	    sg.intg.data.data[l] = time->data[l];
	    sg.smpl.data.data[l] = (short)sample->data[l];
	}

	/* Update header. */
	g_timeStamp (time_end);
	if (g_updateHeader (ioc, &sg, (IRAFPointer)NULL, (IRAFPointer)NULL,
                            output, width, niter, time_begin, time_end)) {
	    g_error ("Cannot update output header.");
	    return (1);
	}

	/* Write and close output. Ensure that it will NOT be appended. */
	if (c_ximaccess (output, IRAF_READ_WRITE))
	    c_imdelete (output);
	putSingleNicmosGroup (output, NEXTVER, &sg, 0);
	freeSingleNicmosGroup (&sg);
	return (0);
}




/*  WFPC routine: writes results but do _not_ update header. This has
 *  to be done only after the 4 groups are finished, to have correct 
 *  ending time posted in output image's header. 
 */

int g_outWfpc (IOControl *ioc, char *output, short badmask, floatArray *flat) {

	char           sci_name[SZ_NAME];
	char           dqf_name[SZ_NAME];
	char           *dot, str[SZ_STR];
	IRAFPointer     sci_imi, dqf_imi;
	IRAFPointer     sci_imo, dqf_imo;
	short                *dqf_buffer;
	float                *sci_buffer;
	float                      angle;
	long                           l;
	int                            i;

	/* Open input image's current group. */
	strcpy (sci_name, ioc->image[0].filename);
	strcpy (dqf_name, ioc->image[0].filename);
	strcat (sci_name, ioc->inputSuffix);
	strcat (dqf_name, ioc->inputSuffix);
	strcat (sci_name, ioc->inputExten);
	strcat (dqf_name, ioc->dqfExten);
	sprintf (str, "[%d]", ioc->group+1);
	strcat (sci_name, str);
	strcat (dqf_name, str);
	sci_imi = c_immap (sci_name, IRAF_READ_ONLY, (IRAFPointer)0);
	if (sci_imi == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Cannot open file %s", sci_name);
	    g_error (ErrText);
	    return (1);
	}
	dqf_imi = c_immap (dqf_name, IRAF_READ_ONLY, (IRAFPointer)0);
	if (dqf_imi == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Cannot open file %s", dqf_name);
	    g_error (ErrText);
	    c_imunmap (sci_imi);
	    return (1);
	}

	/* Open output image's current group. */
	strcpy (sci_name, output);
	strcpy (dqf_name, output);
	dot = strrchr (dqf_name, '.'); /* replace extension */
	*dot = '\0';
	strcat (dqf_name, W_DQOUT);
	if (ioc->group == 0) {
	    sprintf (str, "[1/%d]", WFPC_NGROUPS);
	    strcat (sci_name, str);
	    strcat (dqf_name, str);
	} else {
	    sprintf (str, "[%d]", ioc->group+1);
	    strcat (sci_name, str);
	    strcat (dqf_name, str);
	}
	sci_imo = c_immap (sci_name, IRAF_NEW_COPY, sci_imi);
	if (sci_imo == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Cannot open file %s", sci_name);
	    g_error (ErrText);
	    return (1);
	}
	dqf_imo = c_immap (dqf_name, IRAF_NEW_COPY, dqf_imi);
	if (dqf_imo == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Cannot open file %s", dqf_name);
	    g_error (ErrText);
	    c_imunmap (sci_imo);
	    return (1);
	}

	/* Write output arrays. */
	sci_buffer = c_imps2r (sci_imo, 1, ioc->x_size, 1, ioc->y_size);
	dqf_buffer = c_imps2s (dqf_imo, 1, ioc->x_size, 1, ioc->y_size);
	for (l = 0; l < flat->bufsize; l++) {
	    if (flat->data[l] != BADVAL) {
	        sci_buffer[l] = flat->data[l];
	        dqf_buffer[l] = 0;
	    } else {
	        sci_buffer[l] = 0.0F;
	        dqf_buffer[l]  = badmask;
	    }
	}

	/* Close output. */
	c_imunmap (dqf_imo);
	c_imunmap (sci_imo);
	c_imunmap (dqf_imi);
	c_imunmap (sci_imi);

	return (0);
}
