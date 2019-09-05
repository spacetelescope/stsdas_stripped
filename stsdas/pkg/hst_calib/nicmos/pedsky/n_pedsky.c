# include <stdio.h>
# include <stdlib.h>
# include <float.h>
# include <ximio.h>
# include <xxtools.h>
# include "pedsky.h"

/*   N_PEDSKY -  NICMOS automatic pedestal subtraction.
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
**   Algorithms based entirely on IRAF CL script "azped2" by Mark Dickinson
**   and Andrew Zirm (STScI).
**				
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation.
**   H. Bushouse	27-Sep-2005	Added CVOS error handling to prevent
**					crashes due to missing input files.
**
*/

int n_pedsky (int argc, char **argv) {

	/* Local variables */
	TaskInfo                    info;   /* Task info structure */
	char           listName[SZ_NAME];   /* Input list name */
	char            outName[SZ_NAME];   /* Output list name */
	char         inFileName[SZ_NAME];   /* Input file name */
	char        outFileName[SZ_NAME];   /* Output file name */
	IRAFPointer               inlist;   /* Input list/template pointer */
	IRAFPointer              outlist;   /* Output list/template pointer */
	Bool                     verbose;   /* Verbose output flag */
	char     timeStamp[SZ_TIMESTAMP];   /* Time stamp string */
	int                        ifile;   /* file loop index */

	/* Function declarations */
	void n_timeStamp (char *);
	int n_inputParameters (int, char **, char *, char *, TaskInfo *,
			       Bool *);
	int n_singleFile (TaskInfo *, char *, char *, Bool);
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

	/* Echo parameters at stdout. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== PEDSKY v%s starting at %s ===\n", 
                     VERSION, timeStamp);
	    n_message (MsgText);
	    sprintf (MsgText, "  Input list:  %s\n", listName);
	    n_message (MsgText);
	    sprintf (MsgText, "  Output list: %s\n", outName);
	    n_message (MsgText);
	    sprintf (MsgText, "  Sky mode:    %s\n", info.SkyModeName);
	    n_message (MsgText);
	    if (info.SkyMode == CONSTANT) {
		sprintf (MsgText, "  Sky value:   %9.7g\n", info.SkyValue);
		n_message (MsgText);
	    }
	    sprintf (MsgText, "  DQ Mask:     %d\n", info.BitMask);
	    n_message (MsgText);
	    if (info.keepFlags) {
		sprintf (MsgText, "  Keep flags:  yes\n");
		n_message (MsgText);
	    } else {
		sprintf (MsgText, "  Keep flags:  no\n");
		n_message (MsgText);
	    }
	    if (info.doRingMedian) {
		sprintf (MsgText, "  Ring median: yes\n");
		n_message (MsgText);
		sprintf (MsgText, "  Ring inner:  %g\n", info.RingInner);
		n_message (MsgText);
		sprintf (MsgText, "  Ring outer:  %g\n", info.RingOuter);
		n_message (MsgText);
	    } else {
		sprintf (MsgText, "  Ring median: no\n");
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

	/* Loop over input files. */
	for (ifile = 1; ifile <= c_imtlen (inlist); ifile++) {

	    /* Get the next input/output file names from lists */
	    c_imtgetim (inlist,  inFileName,  SZ_NAME);
	    c_imtgetim (outlist, outFileName, SZ_NAME);

	    /* Check and/or append extension to output name. */
	    if (n_checkOutName (outFileName))
	        return (1);

	    sprintf (MsgText, "  %s -> %s\n", inFileName, outFileName);
	    n_message (MsgText);

	    /* Process the file. */
	    if (n_singleFile (&info, inFileName, outFileName, verbose))
	        return (1);
	}

	/* Close file lists. */
	c_imtclose (outlist);
	c_imtclose (inlist);

	/* Write 'bye' message. */
	if (verbose) {
	    n_timeStamp (timeStamp);
	    sprintf (MsgText, "=== PEDSKY finished  at %s ===\n", timeStamp);
	    n_message (MsgText);
	}

	return (0);
}
