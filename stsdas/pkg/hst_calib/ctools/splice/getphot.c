# include <stdio.h>
# include <stdlib.h>

# include <c_iraf.h>
# include <xtables.h>
# include "pweight.h"

typedef struct {
	IRAFPointer tp;			/* pointer to table descriptor */
					/* column descriptors */
	IRAFPointer cp_opt_elem;	/* opt_elem (grating) */
	IRAFPointer cp_cenwave;
	IRAFPointer cp_sporder;
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_thru;
	int nrows;			/* number of rows in table */
} TblInfo;

typedef struct {
	char opt_elem[STIS_CBUF+1];
	int cenwave;
	int sporder;
} TblRow;

static int OpenPhotTab (char *, TblInfo *);
static int ReadPhotTab (TblInfo *, int, TblRow *);
static int ReadPhotArray (TblInfo *, int, PhotInfo *);
static int PCMultiply (PhotInfo *);
static int ClosePhotTab (TblInfo *);

/* This routine gets the absolute flux conversion from PHOTTAB,
   multiplies it by the ratio of PCT correction factors, and saves
   the info in the photometry information structure.

   The absolute-flux table should contain the following:
	header parameters:
		none used
	columns:
		OPT_ELEM:  grating name (string)
		CENWAVE:  central wavelength (int)
		NELEM:  actual number of elements in arrays (int)
		WAVELENGTH:  array of wavelengths (read as double)
		THROUGHPUT:  array of throughputs (double)

   The table is read to find the row for which the value of OPT_ELEM,
   CENWAVE, and SPORDER are the same as in the input header.  For that
   row, the number of elements NELEM is read, and the arrays of wavelength
   and throughput are read.  Memory is allocated for p_wl and p_phot; these
   will be deallocated by FreePhot.

   Phil Hodge, 2000 Feb 18:
	Created, based on cs7/getabsphot.c.
*/

int GetPhot (WgtInfo *sts, PhotInfo *phot, int sporder) {

/* arguments:
WgtInfo *sts     i: header keywords
PhotInfo *phot   o: QE throughput values are allocated and assigned
int sporder      i: spectral order number
*/

	int status;

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	TblRow tabrow;		/* values read from a table row */

	int row;		/* loop index */
	int foundit;		/* true if parameters found in table */

	int SameInt (int, int);
	int SameString (char *, char *);

	/* Open the photometry table. */
	if (status = OpenPhotTab (sts->phottab, &tabinfo))
	    return (status);

	/* Check each row for a match with keyword values, then read
	   the arrays of wavelength and throughput if there's a match.
	*/

	foundit = 0;
	for (row = 1;  row <= tabinfo.nrows;  row++) {

	    if (status = ReadPhotTab (&tabinfo, row, &tabrow))
		return (status);

	    if (SameString (tabrow.opt_elem, sts->opt_elem) &&
		SameInt (tabrow.cenwave, sts->cenwave) &&
		SameInt (tabrow.sporder, sporder)) {

		foundit = 1;

		/* Read wavelengths and throughputs into phot. */
		if (status = ReadPhotArray (&tabinfo, row, phot))
		    return (status);

		break;
	    }
	}

	if (status = ClosePhotTab (&tabinfo))
	    return (status);

	if (!foundit) {
	    printf (
	"OPT_ELEM %s, CENWAVE %d, SPORDER %d not found in PHOTTAB %s\n",
			sts->opt_elem, sts->cenwave, sporder, sts->phottab);
	    return (GENERIC_ERROR_CODE);
	}

	/* Now multiply the throughput by the PCT correction. */
	if (status = PCMultiply (phot))
	    return (status);

	return (0);
}

/* This routine opens the throughput table, finds the columns that we
   need, and gets the total number of rows in the table.
*/

static int OpenPhotTab (char *tname, TblInfo *tabinfo) {

	tabinfo->tp = c_tbtopn (tname, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    printf ("ERROR    PHOTTAB `%s' not found.\n", tname);
	    return (OPEN_FAILED);
	}

	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);

	/* Find the columns. */
	c_tbcfnd1 (tabinfo->tp, "OPT_ELEM", &tabinfo->cp_opt_elem);
	c_tbcfnd1 (tabinfo->tp, "CENWAVE", &tabinfo->cp_cenwave);
	c_tbcfnd1 (tabinfo->tp, "SPORDER", &tabinfo->cp_sporder);
	c_tbcfnd1 (tabinfo->tp, "NELEM", &tabinfo->cp_nelem);
	c_tbcfnd1 (tabinfo->tp, "WAVELENGTH", &tabinfo->cp_wl);
	c_tbcfnd1 (tabinfo->tp, "THROUGHPUT", &tabinfo->cp_thru);
	if (tabinfo->cp_opt_elem == 0 ||
	    tabinfo->cp_cenwave == 0 ||
	    tabinfo->cp_sporder == 0 ||
	    tabinfo->cp_nelem == 0 ||
	    tabinfo->cp_wl == 0 ||
	    tabinfo->cp_thru == 0) {
	    printf ("ERROR    Column not found in PHOTTAB.\n");
	    c_tbtclo (tabinfo->tp);
	    return (COLUMN_NOT_FOUND);
	}

	return (0);
}

/* This routine reads the grating name, central wavelength, and spectral
   order number, which are used to select the correct row.
*/

static int ReadPhotTab (TblInfo *tabinfo, int row, TblRow *tabrow) {

	c_tbegtt (tabinfo->tp, tabinfo->cp_opt_elem, row,
			tabrow->opt_elem, STIS_CBUF);
	if (c_iraferr())
	    return (TABLE_ERROR);

	c_tbegti (tabinfo->tp, tabinfo->cp_cenwave, row, &tabrow->cenwave);
	if (c_iraferr())
	    return (TABLE_ERROR);

	c_tbegti (tabinfo->tp, tabinfo->cp_sporder, row, &tabrow->sporder);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}

/* This routine reads the array data from one row.  The number of elements
   in the arrays is gotten, the arrays are allocated, and the wavelengths
   and throughputs are read into the arrays.
*/

static int ReadPhotArray (TblInfo *tabinfo, int row, PhotInfo *phot) {

	int nwl, nthru;		/* number of elements actually read */

	/* Find out how many elements there are in the throughput arrays,
	   and allocate space for the arrays to be read from the table.
	*/
	c_tbegti (tabinfo->tp, tabinfo->cp_nelem, row, &phot->p_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	phot->p_wl = calloc (phot->p_nelem, sizeof(double));
	phot->p_thru = calloc (phot->p_nelem, sizeof(double));
	if (phot->p_wl == NULL || phot->p_thru == NULL)
	    return (OUT_OF_MEMORY);

	nwl = c_tbagtd (tabinfo->tp, tabinfo->cp_wl, row,
			phot->p_wl, 1, phot->p_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	nthru = c_tbagtd (tabinfo->tp, tabinfo->cp_thru, row,
			phot->p_thru, 1, phot->p_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	if (nwl < phot->p_nelem || nthru < phot->p_nelem) {
	    c_tbtclo (tabinfo->tp);
	    printf ("ERROR    Not all coefficients were read from PHOTTAB.\n");
	    return (TABLE_ERROR);
	}

	return (0);
}

/* This routine multiplies the throughput (in-place) by the PCT correction.
   The PCT array is first resampled onto the same wavelength grid as the
   throughput array.
*/

static int PCMultiply (PhotInfo *phot) {

	double *pct;
	int i;
	int status;
	int splint_nr (double *, double *, int, double *, double *, int);

	if (phot->pct_nelem < 1)	/* no PCT correction */
	    return (0);

	/* Allocate space for the interpolated correction factors. */
	pct = malloc (phot->p_nelem * sizeof(double));
	if (pct == NULL)
	    return (OUT_OF_MEMORY);

	/* Interpolate. */
	if (status = splint_nr (phot->pct_wl, phot->pct_ratio, phot->pct_nelem,
			phot->p_wl, pct, phot->p_nelem))
	    return (status);

	/* Multiply the throughput by the PCT correction. */
	for (i = 0;  i < phot->p_nelem;  i++)
	    phot->p_thru[i] *= pct[i];

	free (pct);

	return (0);
}

/* This routine closes the phottab table. */

static int ClosePhotTab (TblInfo *tabinfo) {

	c_tbtclo (tabinfo->tp);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}
