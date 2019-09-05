# include <ximio.h>
# include <hstio.h>
# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include "readnoise.h"


/*  RN_OUTPUT:   Write the computed readnoise image and its anciliary
 *               (altough empty) arrays into the output file.
 *
 *   This routine actually calls very specialized sub-routines, one for
 *   each separate instrument, which handle the details pertinent to
 *   each format. Current version supports NICMOS and STIS formats.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   07 Nov 96  -  Implementation  (IB)
 *
 */

int rn_outputGroup (IRAFPointer list1, IRAFPointer list2, char *output,
                    Image *img, Algorithm *alg, char *timeStamp, Bool verb) {

	int rn_outNICMOS (IRAFPointer, IRAFPointer, char *, Image *, 
                          Algorithm *, char *);
	int rn_outSTIS   (IRAFPointer, IRAFPointer, char *, Image *, 
                          Algorithm *, char *);

	if (verb)
	    rn_message ("Writing readnoise file...\n");

	switch (img->instrument) {

	case NICMOS:
	    if (rn_outNICMOS (list1, list2, output, img, alg, timeStamp)) 
	        return (1);
	    break;

	case STIS:
	    if (rn_outSTIS (list1, list2, output, img, alg, timeStamp)) 
	        return (1);
	    break;

	}
	return (0);
}




/*  NICMOS output routine.  */

int rn_outNICMOS (IRAFPointer list1, IRAFPointer list2, char *output,
                  Image *img, Algorithm *alg, char *time_begin) {

	char                   filename[SZ_NAME];
	char              time_end[SZ_TIMESTAMP];
	long                                   i;
	SingleNicmosGroup                     sg;

	void rn_timeStamp (char *);
	int rn_putKeyS (Hdr *, char *, char *, char *);
	int rn_putKeyI (Hdr *, char *, int,    char *);
	int rn_updateHistory (Hdr *, IRAFPointer, IRAFPointer, char *, char *);

	/* Read 1st input image to get all info 
         * to be inherited by output image.
         */
	initSingleNicmosGroup (&sg);
	c_imtrew (list1);
	c_imtgetim (list1, filename, SZ_NAME);
	getSingleNicmosGroup (filename, 1, &sg);

	/* Load output arrays. */
	for (i = 0; i < (img->xsize*img->ysize); i++) {
	    /* SCI */
	    sg.sci.data.data[i]  = (alg->rnoise)[i];
	    /* DQ, ERR, TIME and SAMP */
	    sg.dq.data.data[i]   = (short)0;
	    sg.err.data.data[i]  = 0.0F;
	    sg.intg.data.data[i] = 0.0F;
	    sg.smpl.data.data[i] = (short)0;
	}

	/* Update NEXTEND keyword in primary header. */
	if (rn_putKeyI (sg.globalhdr, "NEXTEND", 5, "")) return (1);

	/* Update FILENAME keyword in all FITS HDUs. */
	if (rn_putKeyS (sg.globalhdr, "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.sci.hdr,  "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.err.hdr,  "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.dq.hdr,   "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.smpl.hdr, "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.intg.hdr, "FILENAME", output, "")) return (1);

	/* Update history. */
	rn_timeStamp (time_end);
	if (rn_updateHistory (sg.globalhdr, list1,list2,time_begin,time_end)) {
	    rn_error ("Cannot update output header.");
	    return (1);
	}

	/* Write and close output. Ensure that it will NOT be appended. */
	if (c_ximaccess (output, IRAF_READ_WRITE))
	    c_imdelete (output);
	putSingleNicmosGroup (output, 1, &sg, 0);
	freeSingleNicmosGroup (&sg);
	return (0);
}



/*  STIS output routine.  */

int rn_outSTIS (IRAFPointer list1, IRAFPointer list2, char *output,
                Image *img, Algorithm *alg, char *time_begin) {

	char                   filename[SZ_NAME];
	char              time_end[SZ_TIMESTAMP];
	long                                   i;
	float                           rn, hold;
	SingleGroup                           sg;

	void rn_timeStamp (char *);
	int rn_putKeyS (Hdr *, char *, char *, char *);
	int rn_putKeyF (Hdr *, char *, float,  char *);
	int rn_putKeyI (Hdr *, char *, int,    char *);
	int rn_updateHistory (Hdr *, IRAFPointer, IRAFPointer, char *, char *);
	void rn_stat (float *, int, float, int, float, float *, float *);

	/* Read 1st input image to get all info 
         * to be inherited by output image.
         */
	initSingleGroup (&sg);
	c_imtrew (list1);
	c_imtgetim (list1, filename, SZ_NAME);
	getSingleGroup (filename, 1, &sg);

	/* Load output arrays. */
	for (i = 0; i < (img->xsize*img->ysize); i++) {
	    /* SCI */
	    sg.sci.data.data[i]  = (alg->rnoise)[i];
	    /* DQ and ERR */
	    sg.dq.data.data[i]   = (short)0;
	    sg.err.data.data[i]  = 0.0F;
	}

	/* Compute average readnoise and write into header. */
	rn_stat (alg->rnoise, img->xsize*img->ysize, 0.0F, 0, 0.0F, &rn, &hold);
	if (rn_putKeyF (sg.globalhdr, "READNSE", rn, "")) return (1);

	/* Update NEXTEND keyword in primary header. */
	if (rn_putKeyI (sg.globalhdr, "NEXTEND", 3, "")) return (1);

	/* Update FILENAME keyword in all FITS HDUs. */
	if (rn_putKeyS (sg.globalhdr, "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.sci.hdr,  "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.err.hdr,  "FILENAME", output, "")) return (1);
	if (rn_putKeyS (&sg.dq.hdr,   "FILENAME", output, "")) return (1);

	/* Update history. */
	rn_timeStamp (time_end);
	if (rn_updateHistory (sg.globalhdr, list1,list2,time_begin,time_end)) {
	    rn_error ("Cannot update output header.");
	    return (1);
	}

	/* Write and close output. Ensure that it will NOT be appended. */
	if (c_ximaccess (output, IRAF_READ_WRITE))
	    c_imdelete (output);
	putSingleGroup (output, 1, &sg, 0);
	freeSingleGroup (&sg);
	return (0);
}



int rn_updateHistory (Hdr *hdr, IRAFPointer list1, IRAFPointer list2,
                    char *time_begin, char *time_end) {

	FitsKw                   kw;  /* keyword pointer   */
	char   HistText[SZ_HISTORY];  /* HISTORY record    */
	char                str[10];  /* Work string       */
	char         file1[SZ_NAME];  /* Current filenames */
	char         file2[SZ_NAME];
	int                       i;

	sprintf (HistText, "READNOISE v%s started at %s", VERSION, 
                 time_begin);
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);
	sprintf (HistText, "Input files:");
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);

	/* Add names of input files to header. */
	c_imtrew (list1);
	c_imtrew (list2);
	for (i = 0; i < c_imtlen(list1); i++) {
	    c_imtgetim (list1, file1, SZ_NAME);
	    c_imtgetim (list2, file2, SZ_NAME);
	    sprintf (HistText, "%s   %s", file1, file2);
	    addHistoryKw (hdr, HistText);
	    if (hstio_err())
	        return (1);
	}
	sprintf (HistText, "READNOISE finished at %s", time_end);
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);

	return (0);
}



/* Routines to write a keyword. This is taken straigth from CALNICA.
 *
 *
 * Arguments:
 *	hdr	i: pointer to header to be updated
 *	keyword	i: name of keyword
 *	value	i: value of keyword
 *	comment	i: comment to add with keyword if keyword doesn't exist
 */

int rn_putKeyS (Hdr *hdr, char *keyword, char *value, char *comment) {

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addStringKw (hdr, keyword, value, comment);
	else
	    putStringKw (kw, value);
	if (hstio_err()) return (1);
	return (0);
}

int rn_putKeyI (Hdr *hdr, char *keyword, int value, char *comment) {

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addIntKw (hdr, keyword, value, comment);
	else
	    putIntKw (kw, value);
	if (hstio_err()) return (1);
	return (0);
}

int rn_putKeyF (Hdr *hdr, char *keyword, float value, char *comment) {

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addFloatKw (hdr, keyword, value, comment);
	else
	    putFloatKw (kw, value);
	if (hstio_err()) return (1);
	return (0);
}



