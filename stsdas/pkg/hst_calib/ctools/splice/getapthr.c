# include <stdio.h>
# include <stdlib.h>

# include <c_iraf.h>
# include <xtables.h>
# include "pweight.h"

typedef struct {
	IRAFPointer tp;			/* pointer to table descriptor */
	IRAFPointer cp_aperture;	/* column descriptors */
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_thr;
	int nrows;			/* number of rows in table */
} TblInfo;

typedef struct {
	char aperture[STIS_CBUF+1];
} TblRow;

static int OpenThruTab (char *, TblInfo *);
static int ReadThruTab (TblInfo *, int, TblRow *);
static int ReadThruArray (TblInfo *, int, PhotInfo *);
static int CloseThruTab (TblInfo *);
static int ApDummy (PhotInfo *);

/* This routine gets aperture throughput from APERTAB (_apt) and saves
   the info in the phot information structure.

   The aperture info table should contain the following:
	header parameters:
		none needed
	columns:
		APERTURE:  aperture name (string)
		NELEM:  actual number of elements in arrays (int)
		WAVELENGTH:  wavelengths in Angstroms (double)
		THROUGHPUT:  the fraction of light passed by the slit
			(read as double), corresponding to WAVELENGTH

   Rows are selected on APERTURE.  If a matching row is found, the sizes
   of the wavelength and throughput arrays are gotten from column NELEM,
   memory is allocated, and WAVELENGTH and THROUGHPUT are read.

   It is not a fatal error if the table name is blank or the row can't
   be found.  In this case, the f_wl and f_thru arrays will be allocated
   and set to 1.

   Phil Hodge, 2000 Jan 31:
	Created, based on cs1/getapthr1.c.
*/

int GetApThr (WgtInfo *sts, PhotInfo *phot) {

/* arguments:
WgtInfo *sts     i: calibration switches and info
PhotInfo *phot   o: throughput values are allocated and assigned
*/

	int status;

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	TblRow tabrow;		/* values read from a table row */

	int row;		/* loop index */
	int foundit;		/* true if parameters found in table */
	int SameString (char *, char *);

	if (sts->apertab[0] == '\0') {
	    /* Allocate memory and assign dummy values. */
	    if (status = ApDummy (phot))
		return (status);
	    else
		return (0);
	}

	/* Open the aperture throughput table. */
	if (status = OpenThruTab (sts->apertab, &tabinfo))
	    return (status);

	foundit = 0;
	for (row = 1;  row <= tabinfo.nrows;  row++) {

	    if (status = ReadThruTab (&tabinfo, row, &tabrow))
		return (status);

	    if (SameString (tabrow.aperture, sts->aperture)) {

		foundit = 1;

		/* Read wavelengths and throughputs into phot structure. */
		if (status = ReadThruArray (&tabinfo, row, phot))
		    return (status);
	    }
	}

	if (status = CloseThruTab (&tabinfo))
	    return (status);

	if (!foundit) {
	    printf ("Warning:  Matching row not found in APERTAB %s;\n",
			sts->apertab);
	    printf ("  APERTURE %s.\n", sts->aperture);
	    if (status = ApDummy (phot))
		return (status);
	}

	return (0);
}

/* This routine opens the aperture throughput table, finds the columns
   that we need, and gets the total number of rows in the table.
*/

static int OpenThruTab (char *tname, TblInfo *tabinfo) {

	tabinfo->tp = c_tbtopn (tname, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    printf ("ERROR:  APERTAB `%s' not found.\n", tname);
	    return (OPEN_FAILED);
	}

	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);

	/* Find the columns. */
	c_tbcfnd1 (tabinfo->tp, "APERTURE", &tabinfo->cp_aperture);
	c_tbcfnd1 (tabinfo->tp, "NELEM", &tabinfo->cp_nelem);
	c_tbcfnd1 (tabinfo->tp, "WAVELENGTH", &tabinfo->cp_wl);
	c_tbcfnd1 (tabinfo->tp, "THROUGHPUT", &tabinfo->cp_thr);
	if (tabinfo->cp_aperture == 0 ||
	    tabinfo->cp_nelem == 0 ||
	    tabinfo->cp_wl == 0 ||
	    tabinfo->cp_thr == 0) {
	    printf ("ERROR:  Column not found in APERTAB.\n");
	    c_tbtclo (tabinfo->tp);
	    return (COLUMN_NOT_FOUND);
	}

	return (0);
}

/* This routine reads the column (aperture name) used to select the
   correct row.
*/

static int ReadThruTab (TblInfo *tabinfo, int row, TblRow *tabrow) {

	c_tbegtt (tabinfo->tp, tabinfo->cp_aperture, row,
			tabrow->aperture, STIS_CBUF);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}

/* This routine reads the array data from one row.  The number of elements
   in the arrays is gotten, the arrays are allocated, and the wavelengths
   and throughputs are read into the arrays.
*/

static int ReadThruArray (TblInfo *tabinfo, int row, PhotInfo *phot) {

	int nwl, nthru;		/* number of elements actually read */

	/* Find out how many elements there are in the throughput arrays,
	   and allocate space for the arrays to be read from the table.
	*/
	c_tbegti (tabinfo->tp, tabinfo->cp_nelem, row, &phot->f_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	/* Allocate memory. */
	phot->f_wl = calloc (phot->f_nelem, sizeof(double));
	phot->f_thru = calloc (phot->f_nelem, sizeof(double));
	if (phot->f_wl == NULL || phot->f_thru == NULL) {
	    CloseThruTab (tabinfo);
	    return (OUT_OF_MEMORY);
	}

	nwl = c_tbagtd (tabinfo->tp, tabinfo->cp_wl, row,
			phot->f_wl, 1, phot->f_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	nthru = c_tbagtd (tabinfo->tp, tabinfo->cp_thr, row,
			phot->f_thru, 1, phot->f_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	if (nwl < phot->f_nelem || nthru < phot->f_nelem) {
	    c_tbtclo (tabinfo->tp);
	    printf ("ERROR:  Not all coefficients were read from APERTAB.\n");
	    return (TABLE_ERROR);
	}

	return (0);
}

/* This routine closes the APERTAB table. */

static int CloseThruTab (TblInfo *tabinfo) {

	c_tbtclo (tabinfo->tp);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}

/* This routine allocates the f_wl and f_thru arrays (just one element)
   and sets the values to one.
*/

static int ApDummy (PhotInfo *phot) {

	phot->f_nelem = 1;
	phot->f_wl = malloc (sizeof(double));
	phot->f_thru = malloc (sizeof(double));
	if (phot->f_wl == NULL || phot->f_thru == NULL)
	    return (OUT_OF_MEMORY);

	phot->f_wl[0] = 1.;
	phot->f_thru[0] = 1.;

	return (0);
}
