# include <ximio.h>
# include <hstio.h>
# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include "nbadpix.h"


/*  B_OUTPUT:   Write the computed static mask image and its anciliary
 *              (altough empty) arrays into the output file.
 *
 *   This routine actually calls very specialized sub-routines, one for
 *   each separate instrument, which handle the details pertinent to
 *   each format. .
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   07 Aug 96  -  Implementation  (IB)
 *   11 Nov 96  -  Added STIS support  (IB)
 *   30 May 97  -  Task renamed "msbadpix" (IB)
 *
 */

int b_outputGroup (IRAFPointer list, char *mask, Counter *cou, Image *img,
                   char *timeStamp, Bool verbose) {

	int b_outNICMOS (IRAFPointer, char *, Counter *, Image *, char *);
	int b_outSTIS   (IRAFPointer, char *, Counter *, Image *, char *);

	if (verbose)
	    b_message ("Writing output mask file...\n");

	switch (img->instrument) {

	case NICMOS:
	    if (b_outNICMOS (list, mask, cou, img, timeStamp)) 
	        return (1);
	    break;

	case STIS:
	    if (b_outSTIS (list, mask, cou, img, timeStamp)) 
	        return (1);
	    break;

	}
	return (0);
}




/*  NICMOS output routine. */

int b_outNICMOS (IRAFPointer list, char *mask, Counter *cou, Image *img,
                 char *time_begin) {

	char                   filename[SZ_NAME];
	char              time_end[SZ_TIMESTAMP];
	long                                   i;
	SingleNicmosGroup                     sg;

	void b_timeStamp (char *);
	int b_putKeyS (Hdr *, char *, char *, char *);
	int b_updateHistory (Hdr *, IRAFPointer, char *, char *, char *);

	/* Read 1st input image to get all info 
         * to be inherited by output image.
         */
	initSingleNicmosGroup (&sg);
	c_imtrew (list);
	c_imtgetim (list, filename, SZ_NAME);
	getSingleNicmosGroup (filename, 1, &sg);

	/* Load output arrays. */
	for (i = 0; i < (img->xsize*img->ysize); i++) {
	    /* DQ */
	    sg.dq.data.data[i]   = (short)cou->counter[i];
	    /* SCI, ERR, TIME and SAMP */
	    sg.sci.data.data[i]  = 0.0F;
	    sg.err.data.data[i]  = 0.0F;
	    sg.intg.data.data[i] = 0.0F;
	    sg.smpl.data.data[i] = (short)0;
	}

	/* Update FILENAME keyword in all FITS HDUs. */
	if (b_putKeyS (sg.globalhdr, "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.sci.hdr,  "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.err.hdr,  "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.dq.hdr,   "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.smpl.hdr, "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.intg.hdr, "FILENAME", mask, "")) return (1);

	/* Update history. */
	b_timeStamp (time_end);
	if (b_updateHistory (sg.globalhdr, list, mask, time_begin, time_end)) {
	    b_error ("Cannot update output header.");
	    return (1);
	}

	/* Write and close output. Ensure that it will NOT be appended. */
	if (c_ximaccess (mask, IRAF_READ_WRITE))
	    c_imdelete (mask);
	putSingleNicmosGroup (mask, 1, &sg, 0);
	freeSingleNicmosGroup (&sg);
	return (0);
}





/*  NICMOS output routine. */

int b_outSTIS (IRAFPointer list, char *mask, Counter *cou, Image *img,
                 char *time_begin) {

	char                   filename[SZ_NAME];
	char              time_end[SZ_TIMESTAMP];
	long                                   i;
	SingleGroup                          sg;

	void b_timeStamp (char *);
	int b_putKeyS (Hdr *, char *, char *, char *);
	int b_updateHistory (Hdr *, IRAFPointer, char *, char *, char *);

	/* Read 1st input image to get all info 
         * to be inherited by output image.
         */
	initSingleGroup (&sg);
	c_imtrew (list);
	c_imtgetim (list, filename, SZ_NAME);
	getSingleGroup (filename, 1, &sg);

	/* Load output arrays. */
	for (i = 0; i < (img->xsize*img->ysize); i++) {
	    /* DQ */
	    sg.dq.data.data[i]   = (short)cou->counter[i];
	    /* SCI and ERR*/
	    sg.sci.data.data[i]  = 0.0F;
	    sg.err.data.data[i]  = 0.0F;
	}

	/* Update FILENAME keyword in all FITS HDUs. */
	if (b_putKeyS (sg.globalhdr, "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.sci.hdr,  "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.err.hdr,  "FILENAME", mask, "")) return (1);
	if (b_putKeyS (&sg.dq.hdr,   "FILENAME", mask, "")) return (1);

	/* Update history. */
	b_timeStamp (time_end);
	if (b_updateHistory (sg.globalhdr, list, mask, time_begin, time_end)) {
	    b_error ("Cannot update output header.");
	    return (1);
	}

	/* Write and close output. Ensure that it will NOT be appended. */
	if (c_ximaccess (mask, IRAF_READ_WRITE))
	    c_imdelete (mask);
	putSingleGroup (mask, 1, &sg, 0);
	freeSingleGroup (&sg);
	return (0);
}





int b_updateHistory (Hdr *hdr, IRAFPointer list, char *mask,
                   char *time_begin, char *time_end) {


	FitsKw                   kw;  /* keyword pointer  */
	char   HistText[SZ_HISTORY];  /* HISTORY record   */
	char                str[10];  /* Work string      */
	char      filename[SZ_NAME];  /* Current filename */
	int                       i;

	sprintf (HistText, "MSBADPIX v%s started at %s", VERSION, 
                 time_begin);
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);
	sprintf (HistText, "Input files:");
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);

	c_imtrew (list);
	for (i = 0; i < c_imtlen(list); i++) {
	    c_imtgetim (list, filename, SZ_NAME);
	    addHistoryKw (hdr, filename);
	    if (hstio_err())
	        return (1);
	}
	sprintf (HistText, "MSBADPIX finished at %s", time_end);
	addHistoryKw (hdr, HistText);
	if (hstio_err())
	    return (1);

	return (0);
}



/* Write string-valued keyword. This is taken straigth from CALNICA. */

int b_putKeyS (Hdr *hdr, char *keyword, char *value, char *comment) {

/* Arguments:
 *	hdr	i: pointer to header to be updated
 *	keyword	i: name of keyword
 *	value	i: value of keyword
 *	comment	i: comment to add with keyword if keyword doesn't exist
 */

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addStringKw (hdr, keyword, value, comment);
	else
	    putStringKw (kw, value);

	if (hstio_err()) return (1);

	return (0);
}




