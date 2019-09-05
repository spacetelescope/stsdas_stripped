# include <time.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>
# include <ximio.h>
# include <xtables.h>	/* For IRAF INDEF macros */
# include "pedsub.h"

/*   N_PEDSUB -  NICMOS automatic pedestal subtraction.
**
**   This is the actual main routine, it is called from another module
**   just to avoid exit() calls.
**
**   This routine reads and parses CL parameters and initializes data 
**   structures
**
**
**   Author: H. Bushouse
**
**   Algorithms based on the routine "unpedestal" by
**   Roeland van der Marel and Dave Zurek (STScI), with subsequent
**   modifications. 
**				
**   Revision history:
**   ----------------
**   17-Jun-1999  -  Implementation.
**   27-Sep-2005  -  Added CVOS error handling to prevent crashes due to
**                   missing input files (H.Bushouse)
**
*/

int n_pedsub (int argc, char **argv) {

	/* Local variables */
	PedInfo                     info;   /* Task info structure */
	char           listName[SZ_NAME];   /* Input list name */
	char            outName[SZ_NAME];   /* Output list name */
	char         inFileName[SZ_NAME];   /* Input file name */
	char        outFileName[SZ_NAME];   /* Output file name */
	IRAFPointer               inlist;   /* Input list/template pointer */
	IRAFPointer              outlist;   /* Output list/template pointer */
	Bool                     verbose;   /* Verbose output flag */
	char     timeStamp[SZ_TIMESTAMP];   /* Time stamp string */
	int                        ifile;   /* file loop index */
	FILE                         *fp;   /* Log file pointer */

	/* Function declarations */
	void n_timeStamp (char *);
	int n_inputParameters (int, char **, char *, char *, PedInfo *, Bool *);
	int n_singleFile (PedInfo *, char *, char *, FILE *, Bool);
	int n_checkOutName (char *);
	int c_imtlen (IRAFPointer);

	/* Post HSTIO and CVOS error handlers */
	push_hstioerr (errchk);
	c_pusherr (cv_errchk);

	/* Read input task parameters. */
	if (n_inputParameters(argc, argv, listName, outName, &info, &verbose)) {
	    n_error ("Invalid input parameter(s)");
	    return (1);
	}

	/* Open the log file, if necessary */
	if (info.KeepLog) {
	    if ( (fp = fopen (info.LogName, "w")) == NULL) {
		sprintf (MsgText, "Can't open log file %s", info.LogName);
		n_error (MsgText);
		return (1);
	    }
	}

	/* Echo parameters at stdout. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== PEDSUB v%s starting at %s ===\n", 
                     VERSION, timeStamp);
	    n_message (MsgText);
	    sprintf (MsgText, "Input list:  %s\n", listName);
	    n_message (MsgText);
	    sprintf (MsgText, "Output list: %s\n", outName);
	    n_message (MsgText);
	    if (info.KeepLog) {
		sprintf (MsgText, "Log file:    %s\n", info.LogName);
		n_message (MsgText);
	    }
	    sprintf (MsgText, "DQ Mask:     %d\n", info.BitMask);
	    n_message (MsgText);
	    if (info.Filter == NONE)
	        sprintf (MsgText, "Filter:      none\n");
	    else if (info.Filter == MEDIAN)
		sprintf (MsgText, "Filter:      median\n");
	    else
		sprintf (MsgText, "Filter:      mask\n");
	    n_message (MsgText);
	    if (info.Filter != NONE) {
	        sprintf (MsgText, "Box min,max: %d, %d\n", info.MMin,
			 info.MMax);
	        n_message (MsgText);
	    }
	    if (info.DoRefine) {
	        sprintf (MsgText, "Do Refine:   yes\n");
		n_message (MsgText);
	        sprintf (MsgText, "Num Refine:  %d\n", info.Nrefine);
	        n_message (MsgText);
		if (info.PedStep == (float)IRAF_INDEFR)
		    sprintf (MsgText, "Ref Step:    INDEF\n");
		else
		    sprintf (MsgText, "Ref Step:    %7f\n", info.PedStep);
		n_message (MsgText);
	        sprintf (MsgText, "Ref Order:   %d\n", info.Morder);
	        n_message (MsgText);
	    } else {
		sprintf (MsgText, "Do Refine:   no\n");
		n_message (MsgText);
	    }
	    if (info.EqQuads) {
		sprintf (MsgText, "Eq Quads:    yes\n");
		n_message (MsgText);
		sprintf (MsgText, "Eq Order:    %d\n", info.EqOrder);
		n_message (MsgText);
		sprintf (MsgText, "Eq Pix 1,2:  %d %d\n", info.EqPix1,
			 info.EqPix2);
		n_message (MsgText);
		if (info.EqFlat)
		    sprintf (MsgText, "Eq Flat:     yes\n");
		else
		    sprintf (MsgText, "Eq Flat:     no\n");
		n_message (MsgText);
	    } else {
		sprintf (MsgText, "Eq Quads:    no\n");
		n_message (MsgText);
	    }
	}

	/* Open input and output lists. */
	inlist = c_imtopen (listName);
	if (c_iraferr()) {
	    n_error ("Error in input list.");
	    return (1);
	}
	outlist = c_imtopen (outName);
	if (c_iraferr()) {
	    n_error ("Error in output list.");
	    c_imtclose (inlist);
	    return (1);
	}
	if (c_imtlen (inlist) != c_imtlen (outlist)) {
	    n_error ("Input and output lists have different sizes.");
	    c_imtclose (inlist);
	    c_imtclose (outlist);
	    return (1);
	}

	/* Loop over input files. */
	for (ifile = 1; ifile <= c_imtlen (inlist); ifile++) {

	    /* Get the next input/output file names from lists */
	    c_imtgetim (inlist,  inFileName,  SZ_NAME);
	    c_imtgetim (outlist, outFileName, SZ_NAME);

	    /* Check and/or append extension to output name. */
	    if (n_checkOutName (outFileName))
	        return (1);
	    sprintf (MsgText, "%s -> %s\n", inFileName, outFileName);
	    n_message (MsgText);
	    if (info.KeepLog)
		fprintf (fp, "#%s", MsgText);

	    /* Process the file. */
	    if (n_singleFile (&info, inFileName, outFileName, fp, verbose))
	        return (1);
	}

	/* Close file lists. */
	c_imtclose (outlist);
	c_imtclose (inlist);

	/* Write 'bye' message. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== PEDSUB finished  at %s ===\n", timeStamp);
	    n_message (MsgText);
	}

	/* Close the log file */
	if (info.KeepLog)
	    fclose (fp);

	return (0);
}

