# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <float.h>
# include "readnoise.h"


/*   RN_READNOISE  -  NICMOS/STIS readnoise tool.
 *
 *   This is the actual main routine, it is called from another module
 *   just to avoid exit() calls.
 *
 *   This routine reads and parses CL parameters, initializes data 
 *   structures and scans eventual group structure in input files
 *
 *   This group scan is in place to accomodate future instruments
 *   that might use GEIS-type groups structure, that is, each group
 *   belongs to a separate detector. NICMOS and STIS files have *no* 
 *   group structure in this context.
 *
 *
 *
 *                                                       Author: I. Busko
 *
 *   Revision history:
 *   ----------------
 *   29 Oct 96  -  Implementation.
 *   07 Feb 97  -  If not supplied, append extension to output name (IB)
 *   07 Feb 97  -  Reject samples with less than 3 dark pairs (IB)
 *   04 Mar 97  -  Fixed blkSize computation - v 1.2 (IB)
 *   30 May 97  -  Task renamed "msreadnoise" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

int rn_readnoise (int argc, char **argv) {

	Algorithm                 alg;  /* Algorithm control structure  */
	Image                     img;  /* File description structure   */ 
	char       listName1[SZ_NAME];  /* 1st input list               */
	char       listName2[SZ_NAME];  /* 2nd input list               */
	char          output[SZ_NAME+6];/* Output noise file name + ext */
	IRAFPointer             list1;  /* 1st list pointer             */
	IRAFPointer             list2;  /* 2nd list pointer             */
	Bool         process, verbose;
	char  timeStamp[SZ_TIMESTAMP];
	int                 memory, i;

	char  *errmsg1 = "Error in input lists.";

	/* Functions. */
	void rn_timeStamp (char *);
	int rn_inputParameters (int, char **, char *, char *, char *, 
                                Image *, Algorithm *, int *, Bool *, Bool *);
	int rn_checkFiles (IRAFPointer, IRAFPointer, Image *, Bool);
	void rn_compBlkSize (Image *, int);
	int rn_allocMemory (Image *, Algorithm *);
	void rn_freeMemory (Image *, Algorithm *);
	int rn_singleGroup (IRAFPointer, IRAFPointer, char *, Image *, 
                            Algorithm *, char *, Bool);
	int rn_checkOutName (char *);

	/* Read parameters. */
	if (rn_inputParameters(argc, argv, listName1, listName2, output,
                              &img, &alg, &memory, &process, &verbose)) {
	    rn_error ("Invalid parameter(s). Correct syntax is:");
	    rn_message ("readnoise @list1 @list2 output [kclip] [nclip] [cleanfrac] [memory] [extver]\n");
	    return (1);
	}

	/* Check and eventually add output file name extension. */
	if (rn_checkOutName (output))
	    return (1);

	/* Echo parameters at stdout. */
	if (verbose & process) {
	    rn_timeStamp (timeStamp);
	    sprintf (MsgText, "=== READNOISE v%s starting at %s ===\n", 
                     VERSION, timeStamp);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Input list 1:      %s\n", listName1);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Input list 2:      %s\n", listName2);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Output NOISE file: %s\n", output);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Sigma-clip:        %g\n", alg.kclip);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Sigma-clip iter:   %d\n", alg.nclip);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Clean histogr.:    %g\n", alg.cleanfrac);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Memory:            %d\n", memory);
	    rn_message (MsgText);
	    sprintf (MsgText, "  Extver:            %d\n", img.extver);
	    rn_message (MsgText);
	} 

	/* Open input lists. */
	if (((list1 = c_imtopen (listName1)) == (IRAFPointer)NULL) ||
	    ((list2 = c_imtopen (listName2)) == (IRAFPointer)NULL)) {
	    rn_error (errmsg1);
	    return (1);
	}
	if (c_imtlen (list1) != c_imtlen (list2)) {
	    rn_error (errmsg1);
	    return (1);
	}

	/* Check input files and initialize controls to
         * instrument-specific values. Exit if in the 
         * checking-only case.
         */
	if (rn_checkFiles (list1, list2, &img, verbose)) 
	    return (1);
	if (!process) {
	    c_imtclose (list2);
	    c_imtclose (list1);
	    return (0);
	}

	if (img.nimages < 3) {
	    rn_error ("Cannot work with less than 3 pairs of dark images.");
	    return (1);
	}

	/* Compute block size. */
	rn_compBlkSize (&img, memory);
	if (verbose) {
	    sprintf (MsgText, "Block size is %d lines long.\n", 
                    img.blkSize / img.xsize);
	    rn_message (MsgText);
	}

	/* Alloc memory. */
	if (rn_allocMemory (&img, &alg)) return (1);

	/* Process each group. This assumes that input files have
         * an eventual multi-group structure where each group will
         * give rise to a *separate* bad pixel mask, most likely
         * because each group belongs to a separate detector/camera 
         * as in e.g. the WFPC. For NICMOS the number of groups is
         * always set to one. 
         */ 
	if (verbose)
	    rn_message ("Begin processing...\n");
	for (img.group = 1; img.group <= img.ngroups; img.group++) {
	    c_imtrew (list1);
	    c_imtrew (list2);
	    if (rn_singleGroup (list1, list2, output, &img, &alg, timeStamp, 
                verbose))
	        return (1);
	}

	/* Write 'bye' message. */
	if (verbose) {
	    rn_timeStamp (timeStamp);
	    sprintf (MsgText, "=== READNOISE finished successfully at %s ===\n",
                    timeStamp);
	    rn_message (MsgText);
	}

	/* Free memory and close input file list. */
	rn_freeMemory (&img, &alg);
	c_imtclose (list2);
	c_imtclose (list1);

	return (0);
}
