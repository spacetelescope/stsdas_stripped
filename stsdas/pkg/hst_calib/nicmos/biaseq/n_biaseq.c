# include <stdio.h>
# include <stdlib.h>
# include <float.h>
# include <ximio.h>
# include <xtables.h>	/* For IRAF INDEF macros */
# include <xxtools.h>
# include "biaseq.h"

/*   N_BIASEQ - NICMOS bias equalization task.
**
**   This is the actual main routine, it is called from another module
**   just to avoid exit() calls.
**
**   This routine reads and parses CL parameters, initializes data 
**   structures and scans eventual IMSET structure in input files
**
**
**
**   Author: H. Bushouse
**
**   Basic algorithms based on IRAF CL script "nbiasfix" by Mark Dickinson
**   (STScI).
**				
**   Revision history:
**   ----------------
**   H. Bushouse	25-Mar-1999	Implementation
**   H. Bushouse	02-May-2000	Renamed to n_biaseq in preparation for
**					public release of v1.0.
**   H. Bushouse	27-Sep-2005	Added CVOS error handling to prevent
**					crashes due to missing input files.
**
*/

int n_biaseq (int argc, char **argv) {

	/* Local variables */
	BiasInfo                    info;   /* Task data structure */
	char           listName[SZ_NAME];   /* Input list name */
	char            outName[SZ_NAME];   /* Output list name */
	char         inFileName[SZ_NAME];   /* Input file name */
	char        outFileName[SZ_NAME];   /* Output file name */
	IRAFPointer               inlist;   /* Input list/template pointer */
	IRAFPointer              outlist;   /* Output list/template pointer */
	Bool                     verbose;   /* Verbose output switch */
	char     timeStamp[SZ_TIMESTAMP];   /* Date/time string */
	int                        ifile;   /* File counter */
	int       skyrange[3*MAX_RANGES];   /* Sky samples range */
	int                         i, j;   /* loop indexes */

	/* Function definitions */
	void n_timeStamp (char *);
	int n_inputParameters (int, char **, char *, char *, BiasInfo *,
			       Bool *);
	int n_singleFile (char *, char *, BiasInfo *, Bool);
	int n_checkOutName (char *);
	int c_imtlen (IRAFPointer);

	/* Post HSTIO and CVOS error handlers */
	push_hstioerr (errchk);
	c_pusherr (cv_errchk);

	/* Read the input parameters */
	if (n_inputParameters(argc, argv, listName, outName, &info, &verbose)) {
	    n_error ("Invalid parameter(s). Correct syntax is:");
	    n_message ("           biaseq @inlist @outlist skysamps\n");
	    return (1);
	}

	/* Decode the sky samp ranges into a list of samps */
	if (c_decode_ranges (info.skylist, skyrange, MAX_RANGES, &info.nskys)) {
	    n_error ("Invalid skysamps range specified.");
	    return (1);
	}
	j = 0;
	c_get_next_number (skyrange, &j);
	info.skysamps[0] = j;
	for (i = 1; i < info.nskys; i++) {
	     c_get_next_number (skyrange, &j);
	     info.skysamps[i] = j;
	}

	/* Set low/high rejection values */
	if (info.nlow  == (int)IRAF_INDEFI)
	    info.nlow  = (int)((float)info.nskys/4. + 0.5);
	if (info.nhigh == (int)IRAF_INDEFI)
	    info.nhigh = (int)((float)info.nskys/4. + 0.5);

	/* Check for valid reject parameters */
	if (info.nlow + info.nhigh >= info.nskys) {
	    n_error ("Bad sky image minmax rejection params");
	    return (1);
	}

	/* Echo parameters at stdout. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== BIASEQ v%s starting at %s ===\n", 
                     VERSION, timeStamp);
	    n_message (MsgText);
	    sprintf (MsgText, "  Input list:  %s\n", listName);
	    n_message (MsgText);
	    sprintf (MsgText, "  Output list: %s\n", outName);
	    n_message (MsgText);
	    for (i = 0; i < info.nskys; i++) {
		 if (i == 0)
		     sprintf (MsgText, "  SkySamps:    %d", info.skysamps[i]);
		 else if (i == info.nskys-1)
		     sprintf (MsgText, " %d\n", info.skysamps[i]);
		 else
		     sprintf (MsgText, " %d", info.skysamps[i]);
		 n_message (MsgText);
	    }
	    sprintf (MsgText, "  Nlow:        %d\n", info.nlow);
	    n_message (MsgText);
	    sprintf (MsgText, "  Nhigh:       %d\n", info.nhigh);
	    n_message (MsgText);
	    sprintf (MsgText, "  DQ Mask:     %d\n", info.bitmask);
	    n_message (MsgText);
	    if (info.fitJumps) {
		sprintf (MsgText, "  FitJumps:    yes\n");
		n_message (MsgText);
		sprintf (MsgText, "  Filtsz:      %d\n", info.jmp_filt);
		n_message (MsgText);
		sprintf (MsgText, "  Thresh:      %-5.1f\n", info.jmp_thresh);
		n_message (MsgText);
	    } else {
		sprintf (MsgText, "  FitJumps:    no\n");
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
	    return (1);
	}
	if (c_imtlen (inlist) != c_imtlen (outlist)) {
	    n_error ("Input and output lists have different sizes.");
	    return (1);
	}

	/* Process each file. */ 
	for (ifile = 1; ifile <= c_imtlen (inlist); ifile++) {
	    c_imtgetim (inlist,  inFileName,  SZ_NAME);
	    c_imtgetim (outlist, outFileName, SZ_NAME);

	    /* Check and/or append extension to output name. */
	    if (n_checkOutName (outFileName))
	        return (1);

	    sprintf (MsgText, "  %s -> %s\n", inFileName, outFileName);
	    n_message (MsgText);

	    if (n_singleFile (inFileName, outFileName, &info, verbose))
	        return (1);
	}

	/* Close file lists. */
	c_imtclose (outlist);
	c_imtclose (inlist);

	/* Write 'bye' message. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== BIASEQ finished  at %s ===\n", timeStamp);
	    n_message (MsgText);
	}

	return (0);
}
