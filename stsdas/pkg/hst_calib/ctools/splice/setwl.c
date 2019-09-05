# include <stdio.h>
# include <stdlib.h>
# include <xtables.h>
# include <c_iraf.h>

# include "splice.h"

typedef struct {
	IRAFPointer tp;		/* pointer to table descriptor */
	IRAFPointer cp;		/* column descriptor */
	int nrows;		/* number of rows in table */
	int nelem;		/* number of elements in array */
	int npix;		/* nrows or nelem, whichever is appropriate */
} TblInfo;

static int OpenTable (char *, char *, TblInfo *);
static int ReadArray (TblInfo *, int, double *);
static int ReadElem (TblInfo *, int, double *);
static int CloseTable (TblInfo *);
static int CheckWl (TblInfo *, Spectrum *);

/* This routine reads the wavelengths for the output spectrum from
   an input table.

   Phil Hodge, 1999 Oct 25:
	Function created.
*/

int SetWl (char *wavetab, char *colname, Spectrum *outspec) {

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	int row;
	int status;
	int AllocSpec (Spectrum *, int);
	void ShiftWl (double *, int);

	/* Open the table. */
	if ((status = OpenTable (wavetab, colname, &tabinfo)))
	    return (status);

	/* Allocate memory for the output spectrum. */
	if ((status = AllocSpec (outspec, tabinfo.npix)))
	    return (status);

	if (tabinfo.nelem > 1) {

	    /* The column contains arrays. */

	    row = 1;
	    if ((status = ReadArray (&tabinfo, row, outspec->wl)))
		return (status);

	} else {

	    /* We have a table of scalars. */

	    for (row = 1;  row <= tabinfo.nrows;  row++) {

		if ((status = ReadElem (&tabinfo, row, outspec->wl)))
		    return (status);
	    }
	}

	if (CheckWl (&tabinfo, outspec))
	    return (1);

	ShiftWl (outspec->wl, tabinfo.npix);

	if ((status = CloseTable (&tabinfo)))
	    return (status);

	return (0);
}

static int OpenTable (char *wavetab, char *colname, TblInfo *tabinfo) {

	int ncols;		/* number of columns in table */

	tabinfo->tp = c_tbtopn (wavetab, IRAF_READ_ONLY, 0);
	if (c_iraferr())
	    return (c_iraferr());

	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);
	ncols = c_tbpsta (tabinfo->tp, TBL_NCOLS);

	/* Find the wavelength column. */
	if (tabinfo->nrows < 1 || ncols < 1) {
	    printf ("No data in wavelength table %s\n", wavetab);
	    return (1);
	} else if (ncols == 1) {
	    tabinfo->cp = c_tbcnum (tabinfo->tp, 1);
	} else {
	    c_tbcfnd1 (tabinfo->tp, colname, &tabinfo->cp);
	}

	if (tabinfo->cp == 0) {
	    printf (
		"Wavelength column `%s' not found in %s.\n", colname, wavetab);
	    return (1);
	}

	/* If the wavelength column contains an array, get its length. */
	tabinfo->nelem = c_tbcigi (tabinfo->cp, TBL_COL_LENDATA);

	if (tabinfo->nelem > 1 && tabinfo->nrows > 1) {
	    printf (
"Wavelength column in %s contains arrays, and there's more than one row;\n",
		wavetab);
	    printf (
"you must specify which row to use for the wavelengths,\n");
	    printf (
"(e.g. 'abc.fits[1][r:row=3]'.\n");
	    return (1);
	}

	/* Assign a value for the number of elements that we expect
	   in the wavelength array, depending on whether the column
	   contains arrays or scalars.
	*/
	if (tabinfo->nelem > 1)
	    tabinfo->npix = tabinfo->nelem;
	else
	    tabinfo->npix = tabinfo->nrows;

	return (0);
}

static int ReadArray (TblInfo *tabinfo, int row, double wl[]) {

	int nelem;		/* number of elements in array */
	/* number of elements actually read */
	int nwl;

	nelem = tabinfo->nelem;

	nwl = c_tbagtd (tabinfo->tp, tabinfo->cp, row, wl, 1, nelem);
	if (c_iraferr())
	    return (c_iraferr());

	if (nwl < tabinfo->nelem) {
	    printf ("Not all elements were read from wavelength table.\n");
	    return (1);
	}

	return (0);
}

static int ReadElem (TblInfo *tabinfo, int row, double wl[]) {

	c_tbegtd (tabinfo->tp, tabinfo->cp, row, &wl[row-1]);
	if (c_iraferr())
	    return (c_iraferr());

	return (0);
}

/* This routine closes a table. */

static int CloseTable (TblInfo *tabinfo) {

	int status;

	c_tbtclo (tabinfo->tp);
	if ((status = c_iraferr()))
	    return (status);

	return (0);
}

/* If the wavelength array contains any zero or INDEF value, the
   wavelengths will be truncated at that point by reducing the values
   of tabinfo->npix and outspec->nelem.

   There must be at least two wavelengths in the array (after truncation),
   and the wavelengths must be strictly increasing.
*/

static int CheckWl (TblInfo *tabinfo, Spectrum *outspec) {

	int i;
	int npix;
	double last_wl;

	npix = tabinfo->npix;

	/* It may be that the array is not fully populated.  If it's
	   padded with zeros or INDEFD, truncate it.
	*/
	if (outspec->wl[0] == 0. || outspec->wl[0] == IRAF_INDEFD) {
	    printf ("Error:  first wavelength in wavetab is bad.\n");
	    return (1);
	}
	for (i = 1;  i < npix;  i++) {
	    if (outspec->wl[i] == 0. || outspec->wl[i] == IRAF_INDEFD) {
		printf (
"Warning:  wavelengths in wavetab are being truncated after element %d\n", i);
		npix = i;
		break;
	    }
	}

	if (npix <= 0) {
	    printf ("The wavetab is empty.");
	    return (1);
	}
	if (npix == 1) {
	    printf ("There must be at least two wavelengths in wavetab.");
	    return (1);
	}

	/* Update the values. */
	if (npix < tabinfo->npix) {
	    tabinfo->npix = npix;
	    outspec->nelem = npix;
	}

	/* Make sure the wavelengths are increasing. */
	last_wl = outspec->wl[0];
	for (i = 1;  i < npix;  i++) {
	    if (outspec->wl[i] <= last_wl) {
		printf (
		"The wavelengths in wavetab must be strictly increasing.\n");
		return (1);
	    }
	    last_wl = outspec->wl[i];
	}

	return (0);
}
