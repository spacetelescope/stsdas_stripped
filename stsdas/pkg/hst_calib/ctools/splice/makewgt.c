# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ximio.h>
# include <xtables.h>

# include "fweight.h"

/* This file contains MakeWgt and most of the functions that it calls.

   Phil Hodge, 1999 Aug 30:
	Initial version.
*/

typedef struct {
	char *tname;		/* table name */
	IRAFPointer tp;		/* pointer to table descriptor */
		/* column descriptors */
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_flux;
	IRAFPointer cp_sm_flux;
	int array;	/* true if table contains array entries */
	int nelem;	/* number of elements, but superceded by nelem column */
	int nrows;	/* number of rows in table */
	int nvals;	/* either nelem or nrows, whichever is appropriate */
} Tbl2Info;

typedef struct {
	char *tname;		/* table name */
	IRAFPointer tp;		/* pointer to table descriptor */
		/* column descriptors */
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_net;
	IRAFPointer cp_back;
	IRAFPointer cp_flux;
	IRAFPointer cp_wgt;
	double gain;	/* atodgain from primary header */
	double exptime;	/* exposure time from table header */
	int array;	/* true if table contains array entries */
	int nelem;	/* number of elements, but superceded by nelem column */
	int nrows;	/* number of rows in table */
	int nvals;	/* either nelem or nrows, whichever is appropriate */
} OutTblInfo;

static int InitTblInfo (Tbl2Info *, OutTblInfo *);
static int ModifyInplace (char *, char *);
static int OpenOutputTable (char *, char *, OutTblInfo *);
static int OpenSmFluxes (char *, Tbl2Info *);
static int CopyTable (char *, char *, char *);
static int Tab1Sanity (char *, IRAFPointer);
static int Tab2Sanity (Tbl2Info *);
static int OutTabSanity (OutTblInfo *);
static int GetCP (IRAFPointer, IRAFPointer,
		IRAFPointer **, IRAFPointer **, int *);
static void CloseTables (Tbl2Info *, OutTblInfo *);
static int ProcessSpectra (Tbl2Info *, OutTblInfo *, int);
static int ReadTab2 (Tbl2Info *, double *, double *);
static int ReadCols (OutTblInfo *, int,
	double *, double *, double *, double *, int *);
static int ComputeWeight (double *, double *, double *,
		double *, double *, double *, int, double, double);
static int PlugHoles (double *, int);
static int WriteOutputWgt (OutTblInfo *, int, double *, int);
static int InterpolateFlux (double *, double *, int,
		double *, double *, int, int);

int MakeWgt (char *intable1, char *intable2, char *outtable,
		char *wgtname, int interpolant, int verbose) {

	Tbl2Info intab2;	/* info for intable2 */
	OutTblInfo outtab;	/* info for outtable */
	int inplace;		/* true if modify intable1 in-place */
	int status;

	inplace = ModifyInplace (intable1, outtable);

	if (verbose) {
	    if (inplace)
		printf ("%s (in-place), %s\n", intable1, intable2);
	    else
		printf ("%s, %s --> %s\n", intable1, intable2, outtable);
	    fflush (stdout);
	}

	if (status = InitTblInfo (&intab2, &outtab))
	    return (status);

	/* Open intable2. */
	if (status = OpenSmFluxes (intable2, &intab2))
	    return (status);

	/* Sanity check for intable2. */
	if (status = Tab2Sanity (&intab2))
	    return (status);

	if (inplace) {
	    strcpy (outtable, intable1);
	} else {
	    /* Copy intable1 to outtable. */
	    if (status = CopyTable (intable1, outtable, wgtname))
		return (status);
	}

	/* Open output table (or open intable1 in-place). */
	if (status = OpenOutputTable (outtable, wgtname, &outtab)) {
	    CloseTables (&intab2, &outtab);
	    return (status);
	}

	if (status = ProcessSpectra (&intab2, &outtab, interpolant)) {
	    CloseTables (&intab2, &outtab);
	    return (status);
	}

	CloseTables (&intab2, &outtab);

	return (0);
}

static int InitTblInfo (Tbl2Info *intab2, OutTblInfo *outtab) {

	intab2->tname = calloc (IRAF_SZ_LINE, sizeof (char));
	if (intab2->tname == NULL)
	    return (OUT_OF_MEMORY);

	intab2->tp = 0;
	intab2->cp_nelem = 0;
	intab2->cp_flux = 0;
	intab2->cp_sm_flux = 0;
	intab2->array = 0;
	intab2->nelem = 0;
	intab2->nrows = 0;
	intab2->nvals = 0;

	outtab->tname = calloc (IRAF_SZ_LINE, sizeof (char));
	if (outtab->tname == NULL)
	    return (OUT_OF_MEMORY);

	outtab->tp = 0;
	outtab->cp_nelem = 0;
	outtab->cp_wl = 0;
	outtab->cp_net = 0;
	outtab->cp_back = 0;
	outtab->cp_flux = 0;
	outtab->cp_wgt = 0;
	outtab->gain = 1.;
	outtab->exptime = 1.;
	outtab->array = 0;
	outtab->nelem = 0;
	outtab->nrows = 0;
	outtab->nvals = 0;

	return (0);
}

/* This function returns true or false, to indicate whether the input
   table intable1 should be written to in-place.  The function value will
   be true (write to intable1 in-place) if the output table name outtable
   is null, or if the input and output file names (i.e. after stripping
   off any expressions in brackets) are the same.
*/

static int ModifyInplace (char *intable1, char *outtable) {

	char *infile, *outfile;		/* input and output file names */
	char *extname;			/* returned by c_tbparse and ignored */
	int hdu;			/* returned by c_tbparse and ignored */
	int junk;
	int flag;			/* local variable for return value */

	if (outtable[0] == '\0')
	    return (1);			/* yes, write to intable1 in-place */

	infile = malloc (IRAF_SZ_LINE * sizeof(char));
	outfile = malloc (IRAF_SZ_LINE * sizeof(char));
	extname = malloc (IRAF_SZ_LINE * sizeof(char));
	if (infile == NULL || outfile == NULL || extname == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return (OUT_OF_MEMORY);
	}

	/* Get the file names. */
	junk = c_tbparse (intable1, infile, extname, IRAF_SZ_LINE, &hdu);
	junk = c_tbparse (outtable, outfile, extname, IRAF_SZ_LINE, &hdu);

	if (strcmp (infile, outfile) == 0)
	    flag = 1;
	else
	    flag = 0;

	free (infile);
	free (outfile);
	free (extname);

	return (flag);
}

/* This routine opens intable2 and finds the WAVELENGTH and FLUX columns.
   If columns by those names are not found, and if intable2 is a text
   table, we'll look for columns c1 and c2 instead.
*/

static int OpenSmFluxes (char *tname, Tbl2Info *intab2) {

	int row;
	int nelem;
	int status;

	strcpy (intab2->tname, tname);

	intab2->tp = c_tbtopn (tname, IRAF_READ_ONLY, 0);
	if (status = c_iraferr())
	    return (status);

	intab2->nrows = c_tbpsta (intab2->tp, TBL_NROWS);

	if (intab2->nrows < 1) {
	    fprintf (stderr, "No data in %s\n", tname);
	    return (1);
	}

	/* Look for all the columns we may need. */
	c_tbcfnd1 (intab2->tp, "NELEM", &intab2->cp_nelem);
	c_tbcfnd1 (intab2->tp, "WAVELENGTH", &intab2->cp_wl);
	c_tbcfnd1 (intab2->tp, "FLUX", &intab2->cp_sm_flux);

	if (intab2->cp_wl == 0 && intab2->cp_sm_flux == 0 &&
			intab2->nelem == 0) {
	    /* intable2 could be a simple text table. */
	    if (c_tbpsta (intab2->tp, TBL_WHTYPE) == TBL_TYPE_TEXT) {
		c_tbcfnd1 (intab2->tp, "c1", &intab2->cp_wl);
		c_tbcfnd1 (intab2->tp, "c2", &intab2->cp_sm_flux);
	    }
	}

	status = 0;
	if (intab2->cp_wl == 0) {
	    fprintf (stderr,
			"WAVELENGTH column not found in table %s\n", tname);
	    status = 1;
	}
	if (intab2->cp_sm_flux == 0) {
	    fprintf (stderr, "FLUX column not found in table %s\n", tname);
	    status = 1;
	}
	if (status)
	    return (1);

	/* Allocated size of wavelength array; actual size could be smaller. */
	intab2->nelem = c_tbcigi (intab2->cp_wl, TBL_COL_LENDATA);

	/* Update nelem, if there's an NELEM column, and assign values
	   to the array flag and to nvals.
	*/
	if (intab2->nelem > 1) {
	    intab2->array = 1;
	    if (intab2->cp_nelem > 0) {
		row = 1;
		c_tbegti (intab2->tp, intab2->cp_nelem, row, &nelem);
		if (nelem > intab2->nelem) {
		    fprintf (stderr,
	"Value of NELEM in intable2 is larger than allocated column size;\n");
		    fprintf (stderr, "  intable2 = %s\n", tname);
		    return (1);
		} else if (nelem < intab2->nelem) {
		    intab2->nelem = nelem;		/* update */
		}
	    }
	    intab2->nvals = intab2->nelem;
	} else {
	    intab2->array = 0;
	    intab2->nvals = intab2->nrows;
	}

	return (0);
}

/* This routine does some sanity checks on intable2. */

static int Tab2Sanity (Tbl2Info *intab2) {

	int nelem1, nelem2;	/* array sizes */

	/* Compare array sizes.  Different sizes are only a problem if
	   there's no NELEM column.
	*/
	if (intab2->cp_nelem == 0) {
	    nelem1 = c_tbcigi (intab2->cp_wl, TBL_COL_LENDATA);
	    nelem2 = c_tbcigi (intab2->cp_sm_flux, TBL_COL_LENDATA);
	    if (nelem1 != nelem2) {
		fprintf (stderr,
	"The array sizes of the columns in intable2 are not the same:\n");
		fprintf (stderr,
	"  wavelength column array size = %d\n", nelem1);
		fprintf (stderr,
	"  weight column array size = %d\n", nelem2);
		return (1);
	    }
	}

	if (intab2->array && intab2->nrows > 1) {
	    fprintf (stderr,
		"intable2 contains arrays, and there are %d rows;\n",
				intab2->nrows);
	    fprintf (stderr,
		" you must specify which row, e.g. use [r:row=<number>]\n");
	    return (1);
	}

	return (0);
}

/* This routine copies the input table intable1 to the output table outtable.
   The primary header will be copied, if appropriate.  If the weight column
   does not exist in the input table, it will be added to the output table.
   The output table will then be closed.
*/

static int CopyTable (char *intable1, char *outtable,
		char *wgtname) {

	IRAFPointer itp, otp;	/* for input and output tables */
	IRAFPointer *icp, *ocp;	/* all columns found in intable1 */
	IRAFPointer icp_wl;	/* wavelength in intable1 (to get nelem) */
	IRAFPointer ocp_wgt;	/* weight column in outtable */
	int nelem;		/* number of elements in wavelength column */
	int ncols;		/* number of columns (except possibly weight) */
	int row;		/* loop index for row number */
	int nrows;		/* number of rows in intable1 */
	int copied;		/* returned by c_tbfpri and ignored */
	int status;

	/* Copy the primary header of intable1 to outtable, if appropriate. */
	c_tbfpri (intable1, outtable, &copied);
	if (status = c_iraferr())
	    return (status);

	/* Open the input table. */
	itp = c_tbtopn (intable1, IRAF_READ_ONLY, 0);
	if (status = c_iraferr())
	    return (status);
	nrows = c_tbpsta (itp, TBL_NROWS);
	if (nrows < 1) {
	    fprintf (stderr, "No data in %s\n", intable1);
	    return (1);
	}

	/* Find the required columns in intable1. */
	if (status = Tab1Sanity (intable1, itp))
	    return (status);

	/* Open the output table as a new copy of intable1. */
	otp = c_tbtopn (outtable, IRAF_NEW_COPY, itp);
	if (status = c_iraferr())
	    return (status);

	/* If the weight column doesn't exist, create it.  In that case,
	   the array size should be the same as for the wavelength column.
	*/
	c_tbcfnd1 (otp, wgtname, &ocp_wgt);
	if (ocp_wgt == 0) {
	    c_tbcfnd1 (itp, "WAVELENGTH", &icp_wl);
	    nelem = c_tbcigi (icp_wl, TBL_COL_LENDATA);
	    c_tbcdef1 (otp, &ocp_wgt, wgtname, "", "", IRAF_REAL, nelem);
	}

	c_tbtcre (otp);
	if (status = c_iraferr())
	    return (status);

	/* Copy header parameters. */
	c_tbhcal (itp, otp);
	if (status = c_iraferr())
	    return (status);

	/* Copy each row.  If the weight column exists in the input table,
	   that column will also be copied to outtable, but it will be
	   overwritten later when we write the weights to outtable.
	*/
	if (status = GetCP (itp, otp, &icp, &ocp, &ncols))
	    return (status);
	for (row = 1;  row <= nrows;  row++) {
	    c_tbrcsc (itp, otp, icp, ocp, row, row, ncols);
	    if (status = c_iraferr())
		return (status);
	}

	c_tbtclo (itp);
	c_tbtclo (otp);

	free (icp);
	free (ocp);

	return (0);
}

/* This routine finds the required columns in the input table,
   just as a check that they're actually present.
*/

static int Tab1Sanity (char *intable1, IRAFPointer itp) {

	IRAFPointer cp_nelem, cp_wl, cp_flux, cp_net, cp_back;

	c_tbcfnd1 (itp, "NELEM", &cp_nelem);
	c_tbcfnd1 (itp, "WAVELENGTH", &cp_wl);
	c_tbcfnd1 (itp, "FLUX", &cp_flux);
	c_tbcfnd1 (itp, "NET", &cp_net);
	c_tbcfnd1 (itp, "BACKGROUND", &cp_back);

	if (cp_wl == 0 || cp_flux == 0 || cp_net == 0) {
	    fprintf (stderr,
		"Required column(s) missing from table %s:\n", intable1);
	    if (cp_wl == 0)
		fprintf (stderr, "  WAVELENGTH column not found\n");
	    if (cp_flux == 0)
		fprintf (stderr, "  FLUX column not found\n");
	    if (cp_net == 0)
		fprintf (stderr, "  NET column not found\n");
	    return (1);
	}

	return (0);
}


/* This routine gets pointers to the column descriptors for all columns
   in the input and output tables.  It also gets the number of columns
   in the input table.
   Memory is allocated for icp and ocp, and they should be deallocated by
   the calling routine.
*/

static int GetCP (IRAFPointer itp, IRAFPointer otp,
		IRAFPointer **icp, IRAFPointer **ocp, int *ncols) {

/* arguments:
IRAFPointer itp     i: descriptor for intable1
IRAFPointer otp     i: descriptor for outtable
IRAFPointer **icp   o: array of column descriptors for intable1
IRAFPointer **ocp   o: array of column descriptors for outtable
int *ncols          o: total number of columns in intable1
*/

	int col;		/* loop index for column number */

	*ncols = c_tbpsta (itp, TBL_NCOLS);

	*icp = calloc (*ncols, sizeof (IRAFPointer));
	*ocp = calloc (*ncols, sizeof (IRAFPointer));
	if (*icp == NULL || *ocp == NULL)
	    return (OUT_OF_MEMORY);

	for (col = 1;  col <= *ncols;  col++) {
	    (*icp)[col-1] = c_tbcnum (itp, col);
	    (*ocp)[col-1] = c_tbcnum (otp, col);
	}

	return (0);
}

/* This routine opens the output table read-write.  This table will
   normally be outtable, but it will intable1 if that is to be modified
   in-place.

   The primary header will be opened to get the ATODGAIN keyword, if
   it is present.
*/

static int OpenOutputTable (char *outtable, char *wgtname, OutTblInfo *outtab) {

	char *pri_hdr;		/* output file name, with [0] */
	char *extname;		/* returned by c_tbparse and ignored */
	int hdu;		/* returned by c_tbparse and ignored */
	int junk;
	IRAFPointer im;		/* for primary header */
	int gain_gotten;	/* true if we got ATODGAIN from header */
	double gain;		/* value read from table header */
	int status;

	pri_hdr = malloc (IRAF_SZ_LINE * sizeof(char));
	extname = malloc (IRAF_SZ_LINE * sizeof(char));
	if (pri_hdr == NULL || extname == NULL)
	    return (OUT_OF_MEMORY);

	/* Get the file name, and append [0] to get the primary header.
	   Open it and get the atodgain, if it's present.
	*/
	junk = c_tbparse (outtable, pri_hdr, extname, IRAF_SZ_LINE, &hdu);

	strcat (pri_hdr, "[0]");
	im = c_immap (pri_hdr, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    clear_cvoserr();
	    gain_gotten = 0;
	} else {
	    if (c_imaccf (im, "ATODGAIN") == 1)		/* IRAF YES */
		outtab->gain = c_imgetd (im, "ATODGAIN");
	    else
		outtab->gain = 1.;
	    c_imunmap (im);
	    gain_gotten = 1;
	    if (outtab->gain <= 0.) {
		fprintf (stderr,
"Warning:  ATODGAIN = %.6g in primary header is invalid;\n", outtab->gain);
		fprintf (stderr, "  gain = 1 will be used.\n");
		outtab->gain = 1.;
	    }
	}

	strcpy (outtab->tname, outtable);

	outtab->tp = c_tbtopn (outtab->tname, IRAF_READ_WRITE, 0);
	if (status = c_iraferr())
	    return (status);

	/* If we weren't able to get ATODGAIN from the primary header,
	   try to get it from the table header.
	*/
	if (!gain_gotten) {
	    gain = c_tbhgtd (outtab->tp, "ATODGAIN");
	    if (c_iraferr())
		clear_cvoserr();
	    else
		outtab->gain = gain;
	    if (outtab->gain <= 0.) {
		fprintf (stderr,
"Warning:  ATODGAIN = %.6g in table header is invalid;\n", outtab->gain);
		fprintf (stderr, "  gain = 1 will be used.\n");
		outtab->gain = 1.;
	    }
	}

	outtab->nrows = c_tbpsta (outtab->tp, TBL_NROWS);

	outtab->exptime = c_tbhgtd (outtab->tp, "EXPTIME");
	if (status = c_iraferr()) {
	    fprintf (stderr, "Couldn't read EXPTIME from header.\n");
	    return (status);
	}
	if (outtab->exptime <= 0.) {
	    fprintf (stderr, "EXPTIME = %.6g is invalid.\n", outtab->exptime);
	    return (1);
	}

	/* Look for all the columns we may need.  NELEM and BACKGROUND are
	   not required, and the weight column might not exist yet.
	*/
	c_tbcfnd1 (outtab->tp, "NELEM", &outtab->cp_nelem);
	c_tbcfnd1 (outtab->tp, "WAVELENGTH", &outtab->cp_wl);
	c_tbcfnd1 (outtab->tp, "FLUX", &outtab->cp_flux);
	c_tbcfnd1 (outtab->tp, "NET", &outtab->cp_net);
	c_tbcfnd1 (outtab->tp, "BACKGROUND", &outtab->cp_back);
	c_tbcfnd1 (outtab->tp, wgtname, &outtab->cp_wgt);

	/* Check for required columns. */
	if (status = OutTabSanity (outtab))
	    return (status);

	/* Allocated size of wavelength array; actual size could be smaller. */
	outtab->nelem = c_tbcigi (outtab->cp_wl, TBL_COL_LENDATA);

	/* If we're opening intable1 in-place, the weight column might
	   not exist yet, in which case we need to create it.
	*/
	if (outtab->cp_wgt == 0) {
	    c_tbcdef1 (outtab->tp, &outtab->cp_wgt,
			wgtname, "", "", IRAF_REAL, outtab->nelem);
	}

	if (outtab->nelem > 1) {
	    outtab->array = 1;
	    outtab->nvals = outtab->nelem;
	} else {
	    outtab->array = 0;
	    outtab->nvals = outtab->nrows;
	}

	free (pri_hdr);
	free (extname);
	return (0);
}

/* This routine checks that the required columns are present in the
   output table.  It also may print a warning if BACKGROUND or NELEM
   is not present.
*/

static int OutTabSanity (OutTblInfo *outtab) {

	if (outtab->cp_wl == 0 || outtab->cp_flux == 0 || outtab->cp_net == 0) {
	    fprintf (stderr,
		"Required column(s) missing from table %s:\n", outtab->tname);
	    if (outtab->cp_wl == 0)
		fprintf (stderr, "  WAVELENGTH column not found\n");
	    if (outtab->cp_flux == 0)
		fprintf (stderr, "  FLUX column not found\n");
	    if (outtab->cp_net == 0)
		fprintf (stderr, "  NET column not found\n");
	    return (1);
	}

	if (outtab->cp_back == 0) {
	    printf ("Note:  BACKGROUND column not found in %s;\n",
			outtab->tname);
	    printf ("  this would have been used to convert NET to GROSS");
	    printf (" for the error estimate.\n");
	    fflush (stdout);
	}

	if (outtab->cp_nelem == 0) {
	    /* Only print a warning if the WAVELENGTH column contains arrays. */
	    if (c_tbcigi (outtab->cp_wl, TBL_COL_LENDATA) > 1) {
		printf ("Note:  NELEM column not found in %s;\n",
			outtab->tname);
		printf ("  total array length will be used.\n");
		fflush (stdout);
	    }
	}

	return (0);
}

static void CloseTables (Tbl2Info *intab2, OutTblInfo *outtab) {

	if (intab2->tp > 0)
	    c_tbtclo (intab2->tp);

	if (intab2->tname != NULL)
	    free (intab2->tname);

	if (outtab->tp > 0)
	    c_tbtclo (outtab->tp);

	if (outtab->tname != NULL)
	    free (outtab->tname);
}

static int ProcessSpectra (Tbl2Info *intab2, OutTblInfo *outtab,
		int interpolant) {

	double *wl2;		/* wavelengths read from intable2 */
	double *sm_flux2;	/* smoothed fluxes read from intable2 */

	double *wl;		/* wavelengths read from outtable */
	double *net;		/* net count rate read from outtable */
	double *back_e;	/* background counts (electrons, not count rate) */
	double *flux;		/* flux read from outtable */

	double *sm_flux;	/* smoothed fluxes interpolated at wl */

	double *outwgt;		/* weights for outtable */

	/* in outtable, number of elements in current row, or number of rows */
	int nelem;
	int row;
	int status;

	/* Allocate memory. */
	wl2 = calloc (intab2->nvals, sizeof (double));
	sm_flux2 = calloc (intab2->nvals, sizeof (double));
	wl = calloc (outtab->nvals, sizeof (double));
	net = calloc (outtab->nvals, sizeof (double));
	back_e = calloc (outtab->nvals, sizeof (double));
	flux = calloc (outtab->nvals, sizeof (double));
	sm_flux = calloc (outtab->nvals, sizeof (double));
	outwgt = calloc (outtab->nvals, sizeof (double));

	if (wl2 == NULL || sm_flux2 == NULL ||
		wl == NULL || net == NULL || back_e == NULL || flux == NULL ||
		sm_flux == NULL || outwgt == NULL)
	    return (OUT_OF_MEMORY);

	/* Read the contents of intable2 into memory. */
	if (status = ReadTab2 (intab2, wl2, sm_flux2))
	    return (status);

	if (outtab->array) {

	    /* Process each row. */
	    for (row = 1;  row <= outtab->nrows;  row++) {
		if (status = ReadCols (outtab, row,
			wl, net, back_e, flux, &nelem))
		    return (status);
		if (status = InterpolateFlux (wl2, sm_flux2, intab2->nvals,
			wl, sm_flux, nelem, interpolant))
		    return (status);
		if (ComputeWeight (wl, net, back_e, flux, sm_flux, outwgt,
			nelem, outtab->exptime, outtab->gain)) {
		    fprintf (stderr, "  row %d in table %s\n",
			row, outtab->tname);
		}
		if (status = WriteOutputWgt (outtab, row, outwgt, nelem))
		    return (status);
	    }

	} else {

	    /* Read wavelength column, interpolate, write weight column. */
	    row = 0;	/* ignored */
	    if (status = ReadCols (outtab, row, wl, net, back_e, flux, &nelem))
		return (status);
	    if (status = InterpolateFlux (wl2, sm_flux2, intab2->nvals,
			wl, sm_flux, nelem, interpolant))
		return (status);
	    if (ComputeWeight (wl, net, back_e, flux, sm_flux, outwgt,
			nelem, outtab->exptime, outtab->gain))
		fprintf (stderr, "  table %s\n", outtab->tname);
	    if (status = WriteOutputWgt (outtab, row, outwgt, nelem))
		return (status);
	}

	free (outwgt);
	free (sm_flux);
	free (flux);
	free (back_e);
	free (net);
	free (wl);
	free (sm_flux2);
	free (wl2);

	return (0);
}

/* This routine reads the wavelengths and fluxes from intable2.
   The wavelengths are checked to be sure they're increasing.
*/

static int ReadTab2 (Tbl2Info *intab2, double wl2[], double sm_flux2[]) {

/* arguments:
Tbl2Info *intab2   i: info for intable2
double wl2[]       o: array of wavelengths read from table
int *sm_flux2      o: array of smoothed fluxes read from table
*/

	int row;	/* row number */
	int i;
	int status;

	if (intab2->array) {

	    row = 1;
	    i = c_tbagtd (intab2->tp, intab2->cp_wl, row, wl2,
			1, intab2->nelem);
	    if (status = c_iraferr())
		return (status);
	    i = c_tbagtd (intab2->tp, intab2->cp_sm_flux, row, sm_flux2,
			1, intab2->nelem);
	    if (status = c_iraferr())
		return (status);

	} else {

	    for (row = 1;  row <= intab2->nrows;  row++) {
		c_tbegtd (intab2->tp, intab2->cp_wl, row, &wl2[row-1]);
		if (status = c_iraferr())
		    return (status);
		c_tbegtd (intab2->tp, intab2->cp_sm_flux, row,
			&sm_flux2[row-1]);
		if (status = c_iraferr())
		    return (status);
	    }
	}

	/* Check wavelengths. */
	for (i = 0;  i < intab2->nvals - 1;  i++) {
	    if (wl2[i+1] <= wl2[i]) {
		fprintf (stderr,
	"Wavelengths in intable2 are not monotonically increasing.\n");
		return (1);
	    }
	}

	return (0);
}

/* This routine reads data from several columns in outtable
   (which is either a copy of intable1 or intable1 itself).
   This also checks that the wavelengths are increasing.
*/

static int ReadCols (OutTblInfo *outtab, int row,
	double wl[], double net[], double back_e[], double flux[], int *nelem) {

/* arguments:
OutTblInfo *outtab    i: info for outtable, after copying from intable1
int row               i: row number; ignored if columns are scalar
double wl[]           o: array of wavelengths
double net[]          o: net count rate at each pixel (dn / s)
double back_e[]       o: background, converted to electrons
double flux[]         o: flux at each pixel
int *nelem            o: number of elements read, or number of rows
*/

	int n;
	int i;
	int gotdata;
	int status;

	if (outtab->array) {

	    if (outtab->cp_nelem > 0) {
		c_tbegti (outtab->tp, outtab->cp_nelem, row, nelem);
		if (status = c_iraferr())
		    return (status);
	    } else {
		*nelem = outtab->nvals;
	    }

	    /* Read data from the current row. */
	    gotdata = 1;		/* optimistic initial value */

	    n = c_tbagtd (outtab->tp, outtab->cp_wl, row, wl, 1, *nelem);
	    if (status = c_iraferr())
		return (status);
	    if (n != *nelem)
		gotdata = 0;

	    n = c_tbagtd (outtab->tp, outtab->cp_net, row, net, 1, *nelem);
	    if (status = c_iraferr())
		return (status);
	    if (n != *nelem)
		gotdata = 0;

	    if (outtab->cp_back > 0) {
		n = c_tbagtd (outtab->tp, outtab->cp_back, row,
			back_e, 1, *nelem);
		if (status = c_iraferr())
		    return (status);
		if (n != *nelem)
		    gotdata = 0;
	    }

	    n = c_tbagtd (outtab->tp, outtab->cp_flux, row, flux, 1, *nelem);
	    if (status = c_iraferr())
		return (status);
	    if (n != *nelem)
		gotdata = 0;

	    if (!gotdata) {
		fprintf (stderr, "Not all elements read from intable1.\n");
		return (1);
	    }

	} else {

	    *nelem = outtab->nrows;

	    /* Read data from each row. */

	    for (row = 1;  row <= outtab->nrows;  row++) {

		c_tbegtd (outtab->tp, outtab->cp_wl, row, &wl[row-1]);
		if (status = c_iraferr())
		    return (status);

		c_tbegtd (outtab->tp, outtab->cp_net, row, &net[row-1]);
		if (status = c_iraferr())
		    return (status);

		if (outtab->cp_back > 0) {
		    c_tbegtd (outtab->tp, outtab->cp_back, row, &back_e[row-1]);
		    if (status = c_iraferr())
			return (status);
		}

		c_tbegtd (outtab->tp, outtab->cp_flux, row, &flux[row-1]);
		if (status = c_iraferr())
		    return (status);
	    }
	}

	/* If background was gotten, convert from count rate to electrons. */
	if (outtab->cp_back > 0) {
	    for (i = 0;  i < *nelem;  i++)
		back_e[i] *= (outtab->exptime * outtab->gain);
	} else {
	    for (i = 0;  i < *nelem;  i++)
		back_e[i] = 0.;
	}

	/* Check wavelengths. */
	for (i = 0;  i < *nelem - 1;  i++) {
	    if (wl[i+1] <= wl[i]) {
		fprintf (stderr,
	"Wavelengths in intable1 are not monotonically increasing.\n");
		return (1);
	    }
	}

	return (0);
}

/* This routine computes the weights.  At each array element, the smoothed
   flux (interpolated from the data in intable2) is converted to electrons
   by multiplying by the instrumental sensitivity (net / flux), the gain
   and the exposure time.  A term computed from the background is then
   included to correct from NET to GROSS for the error estimate.
   The weight is then set equal to that number of electrons divided by
   the dispersion in Angstroms.
*/

static int ComputeWeight (double wl[], double net[], double back_e[],
		double flux[],
		double sm_flux[], double outwgt[],
		int nelem, double exptime, double gain) {

/* arguments:
double wl[]           i: array of wavelengths
double net[]          i: net count rate at each pixel (dn / s)
double back_e[]       i: background, converted to electrons
double flux[]         i: flux at each pixel
double sm_flux[]      i: interpolated, smoothed flux
double outwgt[]       o: weight at each pixel
int nelem             i: number of elements in each array
double exptime        i: exposure time in seconds
double gain           i: CCD gain, or 1
*/

	int i;
	double sensitivity;	/* sensitivity = net counts / flux */
	double *counts;		/* counts, scaled from sm_flux */
	double sum;		/* for taking the average of (back_e/counts) */
	int nsum;		/* number of values in sum */
	double bck_factor;	/* 1 + average of back_e / counts */

	counts = calloc (nelem, sizeof (double));
	if (counts == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return (OUT_OF_MEMORY);
	}

	sum = 0.;
	nsum = 0;
	for (i = 0;  i < nelem;  i++) {
	    if (flux[i] == 0.) {
		counts[i] = 0.;
	    } else {
		sensitivity = net[i] * gain / flux[i];
		counts[i] = sm_flux[i] * sensitivity * exptime;
		if (counts[i] > 0.) {
		    sum += (back_e[i] / counts[i]);
		    nsum++;
		}
	    }
	}

	if (nsum > 0 && sum > 0.)
	    bck_factor = 1. + sum / (double)nsum;
	else
	    bck_factor = 1.;

	for (i = 0;  i < nelem;  i++)
	    outwgt[i] = counts[i] / bck_factor;		/* we're not done yet */

	if (nelem > 1) {
	    /* divide by the dispersion */
	    outwgt[0] /= (wl[1] - wl[0]);
	    outwgt[nelem-1] /= (wl[nelem-1] - wl[nelem-2]);
	    for (i = 1;  i < nelem-1;  i++)
		outwgt[i] /= ((wl[i+1] - wl[i-1]) / 2.);
	}

	free (counts);

	/* Interpolate over regions where the weight is zero. */
	if (PlugHoles (outwgt, nelem))
	    return (1);
	else
	    return (0);
}

/* This routine checks for zero or negative values in outwgt.  If any
   such values are found, they will be replaced by linearly interpolated
   values.  Zero or negative weights at either endpoint will be replaced
   by the nearest positive value.

   If all values are zero or negative, a warning will be printed, any
   negative values will be replaced with zero, and the function will
   return one.
*/

static int PlugHoles (double outwgt[], int nelem) {

	int i, j;
	int nzero;		/* the number of zero or negative weights */
	int prev_good, next_good;	/* indexes at which outwgt is > 0 */
	double prev, next;	/* same as prev_good and next_good */
	double p, q;		/* for interpolating */

	if (nelem < 1)
	    return (0);

	nzero = 0;
	for (i = 0;  i < nelem;  i++) {
	    if (outwgt[i] <= 0.) {
		nzero++;
		if (outwgt[i] < 0.)
		    outwgt[i] = 0.;
	    }
	}

	if (nzero == 0)				/* no gaps to plug */
	    return (0);

	if (nzero == nelem) {
	    fprintf (stderr, "Warning:  All weights are zero;\n");
	    fprintf (stderr,
	"this could be due to the fluxes in intable1 being all zero,\n");
	    fprintf (stderr,
	"or to the fluxes in intable2 being negative or zero.\n");
	    return (1);
	}

	/* Fill in the gap(s) by linear interpolation. */

	prev_good = -1;
	for (i = 0;  i < nelem;  i++) {

	    if (outwgt[i] > 0.) {

		prev_good = i;

	    } else {

		if (prev_good > 0) {
		    /* find the next element such that outwgt > 0 */
		    next_good = -1;
		    for (j = i+1;  j < nelem;  j++) {
			if (outwgt[j] > 0.) {
			    next_good = j;
			    break;
			}
		    }
		    if (next_good > 0) {
			prev = (double)prev_good;
			next = (double)next_good;
			/* interpolate over the current gap */
			for (j = i;  j < next_good;  j++) {
			    q = ((double)j - prev) / (next - prev);
			    p = 1. - q;
			    outwgt[j] = p * outwgt[prev_good] +
					q * outwgt[next_good];
			}
			prev_good = next_good;
			i = next_good;		/* jump ahead */
		    } else {
			i = nelem;		/* end of loop on i */
		    }
		}
	    }
	}

	/* If the endpoints still have zero weight, extrapolate with
	   the first or last non-zero value.
	*/
	if (outwgt[0] <= 0.) {
	    for (i = 1;  i < nelem;  i++) {
		if (outwgt[i] > 0.) {
		    for (j = 0;  j < i;  j++)
			outwgt[j] = outwgt[i];
		    break;
		}
	    }
	}
	if (outwgt[nelem-1] <= 0.) {
	    for (i = nelem-2;  i >= 0;  i--) {
		if (outwgt[i] > 0.) {
		    for (j = i+1;  j < nelem;  j++)
			outwgt[j] = outwgt[i];
		    break;
		}
	    }
	}

	return (0);
}

/* This routine writes the weights to the output table. */

static int WriteOutputWgt (OutTblInfo *outtab, int row,
			double outwgt[], int nelem) {

	int status;

	if (outtab->array) {

	    if (nelem > outtab->nelem)
		return (ARRAY_SIZE_ERROR);

	    /* Write an array of weights at the current row. */
	    c_tbaptd (outtab->tp, outtab->cp_wgt, row, outwgt, 1, nelem);
	    if (status = c_iraferr())
		return (status);

	} else {

	    if (nelem > outtab->nrows)
		return (ARRAY_SIZE_ERROR);

	    /* Write the weight to each row. */
	    for (row = 1;  row <= nelem;  row++) {
		c_tbeptd (outtab->tp, outtab->cp_wgt, row, outwgt[row-1]);
		if (status = c_iraferr())
		    return (status);
	    }
	}

	return (0);
}

/* This does the interpolation.  The wavelengths (in wl2 and wl) are the
   independent variables, and the smoothed fluxes (in sm_flux2) are the
   dependent variables.

   Cubic spline interpolation and linear interpolation are supported.
*/

static int InterpolateFlux (double wl2[], double sm_flux2[], int n2,
		double wl[], double sm_flux[], int nelem, int interpolant) {

/* arguments:
double wl2[]       i: wavelengths from intable2
double sm_flux2[]  i: (smoothed) fluxes from intable2
int n2             i: number of elements in wl2 and sm_flux2
double wl[]        i: wavelengths from outtable
double sm_flux[]   o: interpolated (smoothed) fluxes
int nelem          i: number of elements in wl and sm_flux
int interpolant    i: specifies type of interpolation (e.g. spline)
*/

	int i;			/* index for wavelengths in wl2 */
	int j;			/* index for wl and sm_flux */
	int start;
	int foundit;
	double wavelen;		/* a wavelength in wl */
	double p, q;		/* for linear interpolation */

	int splint_nr (double *, double *, int, double *, double *, int);

	if (n2 < 1)
	    return (1);

	if (n2 == 1) {
	    for (i = 0;  i < nelem-1;  i++)
		sm_flux[i] = sm_flux2[0];
	    return (0);
	}

	if (n2 < 3)
	    interpolant = LINEAR_INTERPOLATION;

	if (interpolant == SPLINE_INTERPOLATION) {

	    splint_nr (wl2, sm_flux2, n2, wl, sm_flux, nelem);

	} else if (interpolant == LINEAR_INTERPOLATION) {

	    wavelen = wl[0];
	    start = -1;
	    for (i = 0;  i < n2-1;  i++) {	/* find starting point */
		if (wl2[i] <= wavelen && wavelen <= wl2[i+1]) {
		    start = i;
		    break;
		}
	    }
	    if (start < 0)		/* bracketing interval not found */
		start = n2 - 2;

	    /* interpolate at each wavelength in wl */
	    for (j = 0;  j < nelem;  j++) {
		wavelen = wl[j];
		foundit = 0;
		for (i = start;  i < n2-1;  i++) {
		    if (wl2[i] <= wavelen && wavelen <= wl2[i+1]) {
			foundit = 1;
			break;
		    }
		}
		if (foundit) {
		    start = i;
		} else {
		    if (wavelen <= wl2[0])
			i = 0;
		    else
			i = n2 - 2;
		}

		q = (wavelen - wl2[i]) / (wl2[i+1] - wl2[i]);
		p = 1. - q;
		sm_flux[j] = p * sm_flux2[i] + q * sm_flux2[i+1];
	    }
	}

	return (0);
}
