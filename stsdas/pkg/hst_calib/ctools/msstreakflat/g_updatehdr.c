/* 	Copyright restrictions apply - see stsdas$copyright.stsdas 
*/ 
# include <stdio.h>
# include <string.h>
# include <hstio.h>
# include "estreak.h"


/*  G_UPDATEHEADER  --  Updates output image header with HISTORY records.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   09 May 96  -  Implementation  (IB)
 *   22 May 96  -  WFPC support  (IB)
 *
 */

int g_updateHeader (IOControl *ioc, SingleNicmosGroup *sg, IRAFPointer sci,
                    IRAFPointer dqf, char output[], int width[], int niter, 
                    char time_begin[], char time_end[]) {

/*  Parameters:
 *
 *  IOControl                  ioc   io: I/O control structure
 *  SingleNicmosGroup           sg   io: NICMOS output image
 *  IRAFPointer                sci   i:  IMIO pointer to WFPC output image
 *  IRAFPointer                dqf   i:  IMIO pointer to WFPC output DQ image
 *  char           output[SZ_NAME]   i:  output file name
 *  int            width[MAX_ITER]   i:  boxcar filter widths
 *  int                      niter   i:  number of iterations
 *  char        time_begin[SZ_STR]   i:  time at which task begun
 *  char          time_end[SZ_STR]   i:  time at which task ended
 *
 */
	int                     group,i;
	FitsKw                       kw;  /* keyword pointer  */
	char       HistText[SZ_HISTORY];  /* HISTORY record   */
	char                str[SZ_STR];  /* Work string      */
	char          filename[SZ_NAME];  /* Current filename */
	float                     angle;

	int g_putKeyS (Hdr *, char *, char *, char *);
	int g_writeHistory (IOControl *, SingleNicmosGroup *, 
                            IRAFPointer, IRAFPointer, char *record);

	/* Update FILENAME keyword in all FITS HDUs. */
	if ((ioc->instrument == NICMOS) ||
	    (ioc->instrument == STIS))  {
	    if (g_putKeyS (sg->globalhdr, "FILENAME", output, "")) return (1);
	    if (g_putKeyS (&sg->sci.hdr,  "FILENAME", output, "")) return (1);
	    if (g_putKeyS (&sg->err.hdr,  "FILENAME", output, "")) return (1);
	    if (g_putKeyS (&sg->dq.hdr,   "FILENAME", output, "")) return (1);
	    if (g_putKeyS (&sg->smpl.hdr, "FILENAME", output, "")) return (1);
	    if (g_putKeyS (&sg->intg.hdr, "FILENAME", output, "")) return (1);
	}

	/* Write HISTORY with starting time. */
	sprintf (HistText, "MSSTREAKFLAT version %s starts at %s", VERSION, 
                 time_begin);
	if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);

	/* Write HISTORY with boxcar filter widths. */
	sprintf (HistText, "Boxcar filter half widths (in pixels) are: ");
	if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);

	/* This takes care of any number of filter widths (iterations). */
	strcpy (HistText, "");
	for (i = 0; i < niter; i++) {
	    sprintf (str, "%6.6d ", width[i]);
	    strcat (HistText, str);
	    if ((unsigned int)strlen (HistText) > SZ_HISTORY-7) {
	        if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);
	        strcpy (HistText, "");
	    }
	}
	if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);

	/* Write HISTORY of streak angles. */
	for (group = 0; group < ioc->ngroups; group++) {
	    switch (ioc->instrument) {
	        case NICMOS: 
	            sprintf (HistText, "Streak angles:"); 
	            break;
	        case WFPC:
	            sprintf (HistText, "Group %d streak angles:", group+1); 
	            break;
	    }
	    if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);

	    /* Write each input image's angle. */
	    for (i = 0; i < ioc->nimage; i++) {
	        angle = ioc->image[i].streakAngle - ioc->angleOffset[group];
	        while (angle >   90.0F) angle -= 180.0F;
	        while (angle <= -90.0F) angle += 180.0F;
	        strcpy (filename, ioc->image[i].filename);
	        strcat (filename, ioc->inputSuffix);
	        strcat (filename, ioc->inputExten);
	        sprintf (HistText, "  %s   %8.3f", filename, angle);
	        if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);
	    }
	}

	/* Write HISTORY with ending time. */
	sprintf (HistText, "MSSTREAKFLAT stops at %s", time_end);
	if (g_writeHistory (ioc, sg, sci, dqf, HistText)) return (1);

	return (0);
}


/*  Write one single HISTORY record. */

int g_writeHistory (IOControl *ioc, SingleNicmosGroup *sg, IRAFPointer sci, 
                    IRAFPointer dqf, char *record) {

	switch (ioc->instrument) {
	case NICMOS:
	    addHistoryKw (sg->globalhdr, record);
	    if (hstio_err()) return (1);
	    break;
	case WFPC:
	    c_imputh (sci, "HISTORY", record);
	    c_imputh (dqf, "HISTORY", record);
	    break;
	}
	return (0);
}



/* Write string-valued keyword. This is taken straigth from CALNICA. */

int g_putKeyS (Hdr *hdr, char *keyword, char *value, char *comment) {

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




