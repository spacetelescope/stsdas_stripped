# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <c_iraf.h>
# include <xclio.h>
# include <ximio.h>

# include "pweight.h"

static int OpenTblLists (char *, char *, IRAFPointer *, IRAFPointer *);
static int GetNextTbl (IRAFPointer, IRAFPointer,
		char *, char *, int *, int);

/* This is an IRAF task to compute weights and include them as a column
   in spectral data, for use with the splice task.  This version computes
   the weights based on the phottab, which contains the system throughput.

   Phil Hodge, 2000 Jan 31:
	Initial version.
*/

IRAFTASK (pweight) {

	char *inlist;		/* list of input table names (spectra) */
	char *outlist;		/* list of output table names */
	IRAFPointer ilist, olist;

	char *intable;		/* input table of spectra */
	char *outtable;		/* output table */
	char *wgtname;		/* column name for weight */
	int verbose;
	int extn;		/* used if multiple input but one output */
	int status;
	int PhotWgt (char *, char *, char *, int);

	inlist = calloc (IRAF_SZ_LINE+1, sizeof(char));
	outlist = calloc (IRAF_SZ_LINE+1, sizeof(char));
	wgtname = calloc (STIS_FNAME+1, sizeof(char));
	if (inlist == NULL || outlist == NULL || wgtname == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return;
	}

	intable = calloc (IRAF_SZ_LINE+1, sizeof(char));
	outtable = calloc (IRAF_SZ_LINE+1, sizeof(char));
	if (intable == NULL || outtable == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return;
	}

	/* Get cl parameters. */
	c_clgstr ("intable", inlist, IRAF_SZ_LINE);
	c_clgstr ("outtable", outlist, IRAF_SZ_LINE);
	c_clgstr ("wgt_name", wgtname, STIS_FNAME);
	verbose = c_clgetb ("verbose");

	/* Open lists, and compare numbers of file names. */
	if (OpenTblLists (inlist, outlist, &ilist, &olist))
	    return;

	extn = 0;			/* may be incremented by GetNextTbl */

	while (GetNextTbl (ilist, olist, intable, outtable, &extn,
		IRAF_SZ_LINE) != IRAF_EOF) {

	    status = PhotWgt (intable, outtable, wgtname, verbose);
	    if (status) {
		if (c_iraferr()) {
		    fprintf (stderr,
			"IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
		} else if (status == OUT_OF_MEMORY) {
		    fprintf (stderr, "Out of memory\n");
		    return;		/* fatal error */
		} else {
		    fprintf (stderr,
		"Error (%d) computing weights for these tables:\n", status);
		    fprintf (stderr, "  intable = %s\n", intable);
		    if (outtable[0] == '\0')
			fprintf (stderr, "  outtable = \"\"\n");
		    else
			fprintf (stderr, "  outtable = %s\n", outtable);
		    fflush (stderr);
		}
	    }
	}

	free (outtable);
	free (intable);
	free (wgtname);
	free (outlist);
	free (inlist);

	return;
}

/* This routine opens the three lists of table names.  The numbers of
   names in each list are compared, and an error message is printed if
   the numbers are inconsistent (the return value will be one in this
   case).  The numbers of names don't have to be the same.  If the
   number of names in outlist is not the same as the number in inlist,
   there must be either no names at all in outlist (i.e. input spectra
   will be modified in-place), or outlist could contain just one name
   (multiple output tables will be stored as separate extensions in a
   FITS file).
*/

static int OpenTblLists (char *inlist, char *outlist,
	IRAFPointer *ilist, IRAFPointer *olist) {

/* arguments:
char *inlist            i: list of names of tables with spectra
char *outlist           i: list of output tables
IRAFPointer *ilist      o: imt pointer for inlist
IRAFPointer *olist      o: imt pointer for outlist
*/

	int n_in, n_out;	/* number of names in each list */

	*ilist = c_imtopen (inlist);
	*olist = c_imtopen (outlist);

	n_in = c_imtlen (*ilist);
	n_out = c_imtlen (*olist);

	/* It's OK for the output list to be empty, and it's OK to
	   copy multiple input tables into the same output file
	   (if it's a FITS file).
	*/
	if (n_out > 1 && n_in != n_out) {

	    fprintf (stderr,
	"Lengths of input and output lists are inconsistent:\n");
	    fprintf (stderr, "%d in intable list\n", n_in);
	    fprintf (stderr, "%d in outtable list\n", n_out);

	    return (1);
	}

	return (0);
}

/* This routine gets the next name in each of the lists of table names.
   The function value will be zero if we actually got file name(s), and
   it will be IRAF_EOF if there are no more names in the list.
*/

static int GetNextTbl (IRAFPointer ilist, IRAFPointer olist,
	char *intable, char *outtable, int *extn, int maxch) {

/* arguments:
IRAFPointer ilist       i: imt pointer for intable
IRAFPointer olist       i: imt pointer for outtable
char *intable           o: table with spectra
char *outtable          o: output table name
int *extn               io: extension number for output table, or zero
int maxch               i: size of strings for table names
*/

	char *out;	/* for output table name, if extn should be appended */
	int n_in, n_out;	/* number of names in input and output lists */
	int stat;	/* IRAF_EOF if we've reached the end of the list */

	stat = c_imtgetim (ilist, intable, maxch);

	if (stat == IRAF_EOF)
	    return (IRAF_EOF);

	n_in = c_imtlen (ilist);
	n_out = c_imtlen (olist);

	if (n_out > 0) {

	    if (n_out == 1)
		c_imtrew (olist);
	    stat = c_imtgetim (olist, outtable, maxch);
	    if (stat == IRAF_EOF) {
		fprintf (stderr,
		"ERROR:  Unexpected end of list of outtable names.\n");
		return (IRAF_EOF);
	    }

	    /* Append extension number to outtable, if appropriate. */
	    if (n_in > 1 && n_out == 1) {
		*extn += 1;
		if ((out = calloc (IRAF_SZ_LINE+1, sizeof(char))) == NULL) {
		    fprintf (stderr, "GetNextTbl:  out of memory\n");
		    return (IRAF_EOF);
		}
		sprintf (out, "%s[%d]", outtable, *extn);
		strcpy (outtable, out);
		free (out);
	    }

	} else {

	    outtable[0] = '\0';
	    *extn = 0;
	}

	return (0);
}
