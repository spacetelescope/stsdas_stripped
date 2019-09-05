# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <c_iraf.h>
# include <ximio.h>
# include <xtables.h>
# include "pweight.h"

/* This file contains PhotWgt and some of the functions that it calls.

   Phil Hodge, 2000 Jan 31:
	Initial version.
*/

typedef struct {
	char *tname;		/* table name */
	IRAFPointer tp;		/* pointer to table descriptor */
		/* column descriptors */
	IRAFPointer cp_sporder;
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_wgt;
	int array;	/* true if table contains array entries */
	int nelem;	/* number of elements, but superceded by nelem column */
	int nrows;	/* number of rows in table */
	int nvals;	/* either nelem or nrows, whichever is appropriate */
} OutTblInfo;

static void WgtInit (WgtInfo *, PhotInfo *);
static int InitTblInfo (OutTblInfo *);
static int ModifyInplace (char *, char *);
static int OpenOutputTable (char *, char *, OutTblInfo *);
static int CopyTable (char *, char *, char *);
static int OutTabSanity (OutTblInfo *);
static int GetCP (IRAFPointer, IRAFPointer,
		IRAFPointer **, IRAFPointer **, int *);
static void CloseTable (OutTblInfo *);
static int ProcessSpectra (WgtInfo *, PhotInfo *, OutTblInfo *);
static int ReadCols (OutTblInfo *, int, int *, double *, int *);
static int ComputeWeight (WgtInfo *, PhotInfo *,
		int, double [], double [], int);
static int WriteOutputWgt (OutTblInfo *, int, double *, int);
static void FreePhot (PhotInfo *);

int PhotWgt (char *intable, char *outtable, char *wgtname, int verbose) {

	WgtInfo sts;		/* reference info */
	PhotInfo phot;		/* info from reference tables */
	OutTblInfo outtab;	/* info for outtable */
	int inplace;		/* true if modify intable in-place */
	int status;
	int GetInfo (char *, WgtInfo *, PhotInfo *);

	WgtInit (&sts, &phot);
	sts.verbose = verbose;

	inplace = ModifyInplace (intable, outtable);

	if (verbose) {
	    if (inplace)
		printf ("%s (in-place)\n", intable);
	    else
		printf ("%s --> %s\n", intable, outtable);
	    fflush (stdout);
	}

	if (status = InitTblInfo (&outtab))
	    return (status);

	/* Get keyword values; read the pctab and apertab info into memory. */
	if (status = GetInfo (intable, &sts, &phot))
	    return (status);

	if (inplace) {
	    strcpy (outtable, intable);
	} else {
	    /* Copy intable to outtable. */
	    if (status = CopyTable (intable, outtable, wgtname))
		return (status);
	}

	/* Open output table (or open intable in-place). */
	if (status = OpenOutputTable (outtable, wgtname, &outtab)) {
	    CloseTable (&outtab);
	    return (status);
	}

	/* Assign values to the weight column for each row in outtable.
	   The phottab is read here, in the loop over spectral order.
	*/
	if (status = ProcessSpectra (&sts, &phot, &outtab)) {
	    CloseTable (&outtab);
	    return (status);
	}

	CloseTable (&outtab);

	FreePhot (&phot);

	return (0);
}

static void WgtInit (WgtInfo *sts, PhotInfo *phot) {

	sts->verbose = 0;

	sts->aperture[0] = '\0';
	sts->opt_elem[0] = '\0';
	sts->helcorr = OMIT;
	sts->cenwave = 0;
	sts->ra_targ = 0.;
	sts->dec_targ = 0.;
	sts->time_of_exp = -1.;
	sts->hfactor = 1.;
	sts->sporder = 1;
	sts->extrsize = UNKNOWN;

	sts->nrows = 0;

	sts->phottab[0] = '\0';
	sts->pctab[0] = '\0';
	sts->apertab[0] = '\0';

	phot->p_nelem = 0;
	phot->p_wl = NULL;
	phot->p_thru = NULL;

	phot->pct_nelem = 0;
	phot->pct_wl = NULL;
	phot->pct_ratio = NULL;

	phot->f_nelem = 0;
	phot->f_wl = NULL;
	phot->f_thru = NULL;
}

static int InitTblInfo (OutTblInfo *outtab) {

	outtab->tname = calloc (IRAF_SZ_LINE, sizeof (char));
	if (outtab->tname == NULL)
	    return (OUT_OF_MEMORY);

	outtab->tp = 0;
	outtab->cp_nelem = 0;
	outtab->cp_wl = 0;
	outtab->cp_wgt = 0;
	outtab->array = 0;
	outtab->nelem = 0;
	outtab->nrows = 0;
	outtab->nvals = 0;

	return (0);
}

/* This function returns true or false, to indicate whether the input
   table intable should be written to in-place.  The function value will
   be true (write to intable in-place) if the output table name outtable
   is null, or if the input and output file names (i.e. after stripping
   off any expressions in brackets) are the same.
*/

static int ModifyInplace (char *intable, char *outtable) {

	char *infile, *outfile;		/* input and output file names */
	char *extname;			/* returned by c_tbparse and ignored */
	int hdu;			/* returned by c_tbparse and ignored */
	int junk;
	int flag;			/* local variable for return value */

	if (outtable[0] == '\0')
	    return (1);			/* yes, write to intable in-place */

	infile = malloc (IRAF_SZ_LINE * sizeof(char));
	outfile = malloc (IRAF_SZ_LINE * sizeof(char));
	extname = malloc (IRAF_SZ_LINE * sizeof(char));
	if (infile == NULL || outfile == NULL || extname == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return (OUT_OF_MEMORY);
	}

	/* Get the file names. */
	junk = c_tbparse (intable, infile, extname, IRAF_SZ_LINE, &hdu);
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

/* This routine copies intable to outtable.
   The primary header will be copied, if appropriate.  If the weight column
   does not exist in the input table, it will be added to the output table.
   The output table will then be closed.
*/

static int CopyTable (char *intable, char *outtable,
		char *wgtname) {

	IRAFPointer itp, otp;	/* for input and output tables */
	IRAFPointer *icp, *ocp;	/* all columns found in intable */
	IRAFPointer icp_wl;	/* wavelength in intable (to get nelem) */
	IRAFPointer ocp_wgt;	/* weight column in outtable */
	int nelem;		/* number of elements in wavelength column */
	int ncols;		/* number of columns (except possibly weight) */
	int row;		/* loop index for row number */
	int nrows;		/* number of rows in intable */
	int copied;		/* returned by c_tbfpri and ignored */
	int status;

	/* Copy the primary header of intable to outtable, if appropriate. */
	c_tbfpri (intable, outtable, &copied);
	if (status = c_iraferr())
	    return (status);

	/* Open the input table. */
	itp = c_tbtopn (intable, IRAF_READ_ONLY, 0);
	if (status = c_iraferr())
	    return (status);
	nrows = c_tbpsta (itp, TBL_NROWS);
	if (nrows < 1) {
	    fprintf (stderr, "No data in %s\n", intable);
	    return (1);
	}

	/* Open the output table as a new copy of intable. */
	otp = c_tbtopn (outtable, IRAF_NEW_COPY, itp);
	if (status = c_iraferr())
	    return (status);

	/* If the weight column doesn't exist, create it.  In this case,
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


/* This routine gets pointers to the column descriptors for all columns
   in the input and output tables.  It also gets the number of columns
   in the input table.
   Memory is allocated for icp and ocp, and they should be deallocated by
   the calling routine.
*/

static int GetCP (IRAFPointer itp, IRAFPointer otp,
		IRAFPointer **icp, IRAFPointer **ocp, int *ncols) {

/* arguments:
IRAFPointer itp     i: descriptor for intable
IRAFPointer otp     i: descriptor for outtable
IRAFPointer **icp   o: array of column descriptors for intable
IRAFPointer **ocp   o: array of column descriptors for outtable
int *ncols          o: total number of columns in intable
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
   normally be outtable, but it will intable if that is to be modified
   in-place.
*/

static int OpenOutputTable (char *outtable, char *wgtname, OutTblInfo *outtab) {

	int status;

	strcpy (outtab->tname, outtable);

	outtab->tp = c_tbtopn (outtab->tname, IRAF_READ_WRITE, 0);
	if (status = c_iraferr())
	    return (status);

	outtab->nrows = c_tbpsta (outtab->tp, TBL_NROWS);

	/* Look for the columns we may need.  NELEM is not required, SPORDER
	   may not be required, and the weight column might not exist yet.
	*/
	c_tbcfnd1 (outtab->tp, "NELEM", &outtab->cp_nelem);
	c_tbcfnd1 (outtab->tp, "SPORDER", &outtab->cp_sporder);
	c_tbcfnd1 (outtab->tp, "WAVELENGTH", &outtab->cp_wl);
	c_tbcfnd1 (outtab->tp, wgtname, &outtab->cp_wgt);

	if (status = OutTabSanity (outtab))
	    return (status);

	/* Allocated size of wavelength array; actual size could be smaller. */
	outtab->nelem = c_tbcigi (outtab->cp_wl, TBL_COL_LENDATA);

	/* If we're opening intable in-place, the weight column might
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

	return (0);
}

/* This routine prints a warning if NELEM is not present and the
   wavelength column contains arrays.
*/

static int OutTabSanity (OutTblInfo *outtab) {

	if (outtab->cp_nelem == 0) {
	    if (c_tbcigi (outtab->cp_wl, TBL_COL_LENDATA) > 1) {
		printf ("Note:  NELEM column not found in %s;\n",
			outtab->tname);
		printf ("  total array length will be used.\n");
		fflush (stdout);
	    }
	}

	return (0);
}

static void CloseTable (OutTblInfo *outtab) {

	if (outtab->tp > 0)
	    c_tbtclo (outtab->tp);

	if (outtab->tname != NULL)
	    free (outtab->tname);
}

static int ProcessSpectra (WgtInfo *sts, PhotInfo *phot, OutTblInfo *outtab) {

	int sporder;		/* can be either from column or header */
	double *wl;		/* wavelengths read from outtable */
	double *outwgt;		/* weights for outtable */

	/* in outtable, number of elements in current row, or number of rows */
	int nelem;
	int row;
	int status;

	/* Allocate memory. */
	wl = calloc (outtab->nvals, sizeof (double));
	outwgt = calloc (outtab->nvals, sizeof (double));

	if (wl == NULL || outwgt == NULL)
	    return (OUT_OF_MEMORY);

	/* If sporder is a column and the wavelength column contains arrays,
	   we'll get the value of sporder in ReadCols, overwriting this value;
	   otherwise, assign the value here (gotten earlier by GetInfo).
	*/
	sporder = sts->sporder;

	if (outtab->array) {

	    /* Process each row. */
	    for (row = 1;  row <= outtab->nrows;  row++) {
		if (status = ReadCols (outtab, row, &sporder, wl, &nelem))
		    return (status);
		if (status = ComputeWeight (sts, phot,
			sporder, wl, outwgt, nelem))
		    return (status);
		if (status = WriteOutputWgt (outtab, row, outwgt, nelem))
		    return (status);
	    }

	} else {

	    /* Read wavelength column, write weight column. */
	    row = 0;	/* ignored */
	    if (status = ReadCols (outtab, row, &sporder, wl, &nelem))
		return (status);
	    if (status = ComputeWeight (sts, phot, sporder, wl, outwgt, nelem))
		return (status);
	    if (status = WriteOutputWgt (outtab, row, outwgt, nelem))
		return (status);
	}

	free (outwgt);
	free (wl);

	return (0);
}

/* This routine reads the sporder and wavelengths from outtable
   (which is either a copy of intable or intable itself).
   This also checks that the wavelengths are increasing.

   If there is no sporder column, the value of sporder in the
   calling sequence will not be modified.
*/

static int ReadCols (OutTblInfo *outtab, int row,
		int *sporder, double wl[], int *nelem) {

/* arguments:
OutTblInfo *outtab    i: info for outtable, after copying from intable
int row               i: row number; ignored if columns are scalar
int *sporder         io: spectral order number, or not changed
double wl[]           o: array of wavelengths
int *nelem            o: number of elements read, or number of rows
*/

	int n;
	int i;
	int status;

	if (outtab->array) {

	    if (outtab->cp_sporder > 0) {
		c_tbegti (outtab->tp, outtab->cp_sporder, row, sporder);
		if (status = c_iraferr())
		    return (status);
	    }	/* else no change to current value */

	    if (outtab->cp_nelem > 0) {
		c_tbegti (outtab->tp, outtab->cp_nelem, row, nelem);
		if (status = c_iraferr())
		    return (status);
	    } else {
		*nelem = outtab->nvals;
	    }

	    /* Read data from the current row. */

	    n = c_tbagtd (outtab->tp, outtab->cp_wl, row, wl, 1, *nelem);
	    if (status = c_iraferr())
		return (status);
	    if (n != *nelem) {
		fprintf (stderr, "Not all elements read from intable.\n");
		return (1);
	    }

	} else {

	    *nelem = outtab->nrows;

	    /* Read data from each row. */

	    for (row = 1;  row <= outtab->nrows;  row++) {

		c_tbegtd (outtab->tp, outtab->cp_wl, row, &wl[row-1]);
		if (status = c_iraferr())
		    return (status);
	    }
	}

	/* Check wavelengths. */
	for (i = 0;  i < *nelem - 1;  i++) {
	    if (wl[i+1] <= wl[i]) {
		fprintf (stderr,
	"Wavelengths in intable are not monotonically increasing.\n");
		return (1);
	    }
	}

	return (0);
}

/* This routine gets the info from the phottab for the current spectral
   order, and then it computes the weights.
*/

static int ComputeWeight (WgtInfo *sts, PhotInfo *phot,
		int sporder, double wl[], double outwgt[], int nelem) {

/* arguments:
WgtInfo *sts          i: reference info
PhotInfo *phot        i: throughput arrays
int sporder           i: spectral order number
double wl[]           i: array of wavelengths
double outwgt[]       o: weight at each pixel
int nelem             i: number of elements in each array
*/

	double wavelength;	/* observed wavelength */
	double throughput;	/* from phottab and pctab */
	double slit_thru;	/* from apertab */
	int phot_starti;	/* for interpolating phottab values */
	int slit_starti;	/* for interpolating apertab values */
	int i;
	int status;
	int GetPhot (WgtInfo *, PhotInfo *, int);
	double interp1d (double, double *, double *, int, int *);

	/* Get the system throughput (multiplied by the PCT correction). */
	if (status = GetPhot (sts, phot, sporder))
	    return (status);

	phot_starti = 1;
	slit_starti = 1;
	slit_thru = 1.;		/* in case there is no APERTAB */

	for (i = 0;  i < nelem;  i++) {

	    /* Convert from heliocentric wavelength to observed wavelength. */
	    wavelength = wl[i] / sts->hfactor;

	    throughput = interp1d (wavelength,
		phot->p_wl, phot->p_thru, phot->p_nelem, &phot_starti);

	    if (phot->f_nelem > 0) {
		slit_thru = interp1d (wavelength,
			phot->f_wl, phot->f_thru, phot->f_nelem, &slit_starti);
	    }

	    /* not finished yet; divide by dispersion later */
	    outwgt[i] = throughput * slit_thru * AREA_HST * sts->exptime;
	}

	if (nelem > 1) {
	    /* divide by the dispersion */
	    outwgt[0] /= ((wl[1] - wl[0]) / sts->hfactor);
	    outwgt[nelem-1] /= ((wl[nelem-1] - wl[nelem-2]) / sts->hfactor);
	    for (i = 1;  i < nelem-1;  i++)
		outwgt[i] /= (((wl[i+1] - wl[i-1]) / 2.) / sts->hfactor);
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

static void FreePhot (PhotInfo *phot) {

	if (phot->p_wl != NULL)
	    free (phot->p_wl);

	if (phot->p_thru != NULL)
	    free (phot->p_thru);

	if (phot->pct_wl != NULL)
	    free (phot->pct_wl);

	if (phot->pct_ratio != NULL)
	    free (phot->pct_ratio);

	if (phot->f_wl != NULL)
	    free (phot->f_wl);

	if (phot->f_thru != NULL)
	    free (phot->f_thru);

	phot->p_nelem = 0;
	phot->pct_nelem = 0;
	phot->f_nelem = 0;
}
