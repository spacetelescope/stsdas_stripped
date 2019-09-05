# include <stdio.h>
# include <stdlib.h>
# include <float.h>
# include "nbadpix.h"


/*   B_NBADPIX  -  NICMOS bad pixel detection tool.
 *
 *   This is the actual main routine, it is called from another module
 *   just to avoid exit() calls.
 *
 *   This routine reads and parses CL parameters, initializes data 
 *   structures and scans eventual group structure in input files
 *
 *   This group scan is in place to accomodate future instruments
 *   that might use GEIS-type groups structure, that is, each group
 *   belongs to a separate detector. NICMOS files have *no* group 
 *   structure in the present context.
 *
 *
 *
 *                                                          Author: I. Busko
 *
 *   Revision history:
 *   ----------------
 *   02 Aug 96  -  Implementation.
 *   21 Oct 96  -  Revised after code review - ver. 1.1 (IB)
 *   11 Nov 96  -  Add STIS support - ver. 1.2 (IB)
 *   07 Feb 97  -  New get_numeric (IB).
 *   07 Feb 97  -  Check/add extension to output name - ver. 1.3 (IB).
 *   30 May 97  -  Task renamed "msbadpix" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

int b_nbadpix (int argc, char **argv) {

	Counter                   cou;    /* Main control structure */
	Image                     img; 
	char        listName[SZ_NAME];   /* Input list name */
	char        maskName[SZ_NAME+6]; /* Output mask name + ext */
	IRAFPointer              list;   /* Input list/template pointer */
	Bool                  verbose;
	char  timeStamp[SZ_TIMESTAMP];
	int                         i;

	void b_timeStamp (char *);
	int b_checkOutName (char *);
	int b_inputParameters (int, char **, char *, char *, Counter *,
                               Bool *);
	int b_checkFiles (IRAFPointer, Image *, Bool);
	int b_singleGroup (IRAFPointer, char *, Counter *, Image *, char *,
                           Bool);

	/* Read parameters. */
	if (b_inputParameters(argc, argv, listName, maskName, &cou,
                              &verbose)) {
	    b_error ("Invalid parameter(s). Correct syntax is:");
	    b_message ("msbadpix @list outmask [window] [thresh] [badfrac] [cleanfrac] [kclip] [nclip]\n");
	    return (1);
	}

	/* Check and/or append extension to output name. */
	if (b_checkOutName (maskName))
	    return (1);

	/* Echo parameters at stdout. */
	if (verbose) {
	    b_timeStamp (timeStamp);
	    sprintf (MsgText, "=== MSBADPIX v%s starting at %s ===\n", 
                     VERSION, timeStamp);
	    b_message (MsgText);
	    sprintf (MsgText, "  Input list:       %s\n", listName);
	    b_message (MsgText);
	    sprintf (MsgText, "  Output MASK file: %s\n", maskName);
	    b_message (MsgText);
	    sprintf (MsgText, "  Window size:      %d\n", cou.window);
	    b_message (MsgText);
	    sprintf (MsgText, "  Threshold:        %g\n", cou.threshold);
	    b_message (MsgText);
	    sprintf (MsgText, "  Badpix fraction:  %g\n", cou.badfrac);
	    b_message (MsgText);
	    sprintf (MsgText, "  Clean histogr.:   %g\n", cou.cleanfrac);
	    b_message (MsgText);
	    sprintf (MsgText, "  Sigma-clip:       %g\n", cou.kclip);
	    b_message (MsgText);
	    sprintf (MsgText, "  Sigma-clip iter:  %d\n", cou.nclip);
	    b_message (MsgText);
	}

	/* Open input list. */
	if ((list = c_imtopen (listName)) == (IRAFPointer)NULL) {
	    b_error ("Error in input list.");
	    return (1);
	}

	/* Check input files and initialize controls to
         * instrument-specific values. 
         */
	if (b_checkFiles (list, &img, verbose))
	    return (1);

	/* Alloc counter and work memory. */
	cou.counter = (int *)   malloc (img.xsize * img.ysize * sizeof(int));
	cou.buffer  = (float *) malloc (cou.window*cou.window * sizeof(float));

	/* Process each group. This assumes that input files have
         * an eventual multi-group structure where each group will
         * give rise to a *separate* bad pixel mask, most likely
         * because each group belongs to a separate detector/camera 
         * as in e.g. the WFPC. For NICMOS the number of groups is
         * always set to one. 
         */ 
	if (verbose)
	    b_message ("Begin processing...\n");
	for (img.group = 1; img.group <= img.ngroups; img.group++) {
	    c_imtrew (list);
	    if (b_singleGroup (list, maskName, &cou, &img, timeStamp, verbose))
	        return (1);
	}

	/* Write 'bye' message. */
	if (verbose) {
	    b_timeStamp (timeStamp);
	    sprintf (MsgText, "=== MSBADPIX finished  at %s ===\n", timeStamp);
	    b_message (MsgText);
	}

	/* Free memory and close input file list. */
	free (cou.buffer);
	free (cou.counter);
	c_imtclose (list);

	return (0);
}
