# include <stdio.h>
# include <stdlib.h>	/* malloc */
# include <string.h>	/* strcpy */

# include <c_iraf.h>
# include <xtables.h>
# include "pweight.h"

typedef struct {
	IRAFPointer tp;			/* pointer to table descriptor */
					/* column descriptors */
	IRAFPointer cp_aperture;
	IRAFPointer cp_cenwave;
	IRAFPointer cp_extrheight;
	IRAFPointer cp_nelem;
	IRAFPointer cp_wl;
	IRAFPointer cp_pc;
	int maxhght;			/* extrheight to use for "infinity" */
	int nrows;			/* number of rows in table */
} TblInfo;

typedef struct {
	char aperture[STIS_CBUF+1];
	int cenwave;
	int extrheight;
} TblRow;

static int OpenPCTab (char *, TblInfo *);
static int ReadPCTab (TblInfo *, int, TblRow *);
static int ReadPCArray (TblInfo *, int,
		double *[], double *[], int *);
static int PCRatio (PhotInfo *, int, double [], double [], int, double [], int);
static int ClosePCTab (TblInfo *);

/* This routine gets the PCT correction to the instrumental throughput
   for two rows, the one for the actual extraction height and the one for
   maximum ("infinite") extraction height.  The ratio of these factors
   will be taken and saved in the phot->pct_ratio array.

   The PCT table should contain the following:
	header parameters:
		MAXHGHT:  value of EXTRHEIGHT for "infinite" height
	columns:
		APERTURE:  aperture name (string)
		CENWAVE:  central wavelength (int)
		EXTRHEIGHT:  height of spectrum extraction box (int)
		NELEM:  actual number of elements in arrays (int)
		WAVELENGTH:  array of wavelengths (double)
		THROUGHPUT:  array of factors (float)

   The table is read to find the rows for which the value of CENWAVE and
   APERTURE are the same as in the input image header, and then the rows
   are selected that have EXTRHEIGHT equal to the table header keyword
   MAXHGHT (PCT_infinity) and EXTRHEIGHT equal to the value of EXTRSIZE
   from the x1d table column (PCT_h).  The arrays of wavelength and
   correction factor are read from those rows, the PCT_h / PCT_infinity
   ratio is computed and assigned to pct_ratio.

   It is not a fatal error for the PCTAB table to not exist, as long as
   this is flagged by the name of the PCTAB name being null.  In this case,
   the number of elements will be set to zero.

   This routine allocates memory for pct_wl and pct_corr, which will be
   deallocated when FreePhot is called.

   Phil Hodge, 2000 Jan 31:
	Created, based on cs7/getpct.c.
*/

int GetPCT (WgtInfo *sts, PhotInfo *phot) {

/* arguments:
WgtInfo *sts    i: calibration switches and info
PhotInfo *phot  o: phot->p_thru will be multiplied by ratio of PCT factors
*/

	int status;

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	TblRow tabrow;		/* values read from a table row */

	double *pct_inf_wl, *pct_inf;	/* from row for MAXHGHT */
	int pct_inf_nelem;
	double *pct_h_wl, *pct_h;	/* from row for EXTRSIZE */
	int pct_h_nelem;
	int row;		/* loop index */
	int found_inf;		/* found row for MAXHGHT? */
	int found_h;		/* found row for EXTRSIZE? */
	int SameInt (int, int);
	int SameString (char *, char *);

	if (sts->pctab[0] == '\0') {
	    phot->pct_nelem = 0;
	    return (0);
	}

	pct_inf_wl = NULL;
	pct_inf = NULL;
	pct_inf_nelem = 0;
	pct_h_wl = NULL;
	pct_h = NULL;
	pct_h_nelem = 0;

	/* Open the PCTAB table. */
	if (status = OpenPCTab (sts->pctab, &tabinfo))
	    return (status);

	/* Check each row for a match with keyword values, then read
	   the arrays of wavelength and throughput if there's a match.
	*/
	found_inf = 0;
	if (sts->extrsize == UNKNOWN)
	    found_h = 1;	/* don't look for the row, use default */
	else
	    found_h = 0;

	for (row = 1;  row <= tabinfo.nrows;  row++) {

	    if (status = ReadPCTab (&tabinfo, row, &tabrow))
		return (status);

	    if (SameString (tabrow.aperture, sts->aperture) &&
		SameInt (tabrow.cenwave, sts->cenwave)) {

		if (SameInt (tabrow.extrheight, tabinfo.maxhght)) {

		    found_inf = 1;
		    if (status = ReadPCArray (&tabinfo, row,
				&pct_inf_wl, &pct_inf, &pct_inf_nelem))
			return (status);
		}

		if (SameInt (tabrow.extrheight, sts->extrsize)) {

		    found_h = 1;
		    if (status = ReadPCArray (&tabinfo, row,
				&pct_h_wl, &pct_h, &pct_h_nelem))
			return (status);
		}

		if (found_inf && found_h)
		    break;
	    }
	}

	if (status = ClosePCTab (&tabinfo))
	    return (status);

	if (found_inf && found_h) {

	    if (status = PCRatio (phot, sts->extrsize,
			pct_inf_wl, pct_inf, pct_inf_nelem,
			pct_h, pct_h_nelem))
		return (status);

	} else {

	    printf ("Warning:  Matching row(s) not found in PCTAB %s;\n",
			sts->pctab);
	    printf ("  APERTURE %s, CENWAVE %d", sts->aperture, sts->cenwave);
	    if (!found_inf)
		printf (", EXTRHEIGHT %d", tabinfo.maxhght);
	    if (!found_h)
		printf (", EXTRHEIGHT %d", sts->extrsize);
	    printf (";\n");
	    printf ("  no correction for extraction height will be applied.\n");
	    phot->pct_nelem = 0;
	}

	if (pct_inf_wl != NULL)
	    free (pct_inf_wl);
	if (pct_inf != NULL)
	    free (pct_inf);
	if (pct_h_wl != NULL)
	    free (pct_h_wl);
	if (pct_h != NULL)
	    free (pct_h);

	return (0);
}

/* This routine opens the PC table, finds the columns that we need,
   and gets the total number of rows in the table.
*/

static int OpenPCTab (char *tname, TblInfo *tabinfo) {

	tabinfo->tp = c_tbtopn (tname, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    printf ("ERROR    PCTAB `%s' not found.\n", tname);
	    return (OPEN_FAILED);
	}

	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);

	tabinfo->maxhght = c_tbhgti (tabinfo->tp, "MAXHGHT");

	/* Find the columns. */
	c_tbcfnd1 (tabinfo->tp, "APERTURE", &tabinfo->cp_aperture);
	c_tbcfnd1 (tabinfo->tp, "CENWAVE", &tabinfo->cp_cenwave);
	c_tbcfnd1 (tabinfo->tp, "EXTRHEIGHT", &tabinfo->cp_extrheight);
	c_tbcfnd1 (tabinfo->tp, "NELEM", &tabinfo->cp_nelem);
	c_tbcfnd1 (tabinfo->tp, "WAVELENGTH", &tabinfo->cp_wl);
	c_tbcfnd1 (tabinfo->tp, "THROUGHPUT", &tabinfo->cp_pc);
	if (tabinfo->cp_aperture == 0 ||
	    tabinfo->cp_cenwave == 0 ||
	    tabinfo->cp_extrheight == 0 ||
	    tabinfo->cp_nelem == 0 ||
	    tabinfo->cp_wl == 0 ||
	    tabinfo->cp_pc == 0) {
	    printf ("ERROR    Column not found in PCTAB.\n");
	    c_tbtclo (tabinfo->tp);
	    return (COLUMN_NOT_FOUND);
	}

	return (0);
}

/* This routine reads the columns used to select the correct row.
   The aperture name, central wavelength, and extraction box height
   are gotten.
*/

static int ReadPCTab (TblInfo *tabinfo, int row, TblRow *tabrow) {

	c_tbegtt (tabinfo->tp, tabinfo->cp_aperture, row,
			tabrow->aperture, STIS_CBUF);
	if (c_iraferr())
	    return (TABLE_ERROR);

	c_tbegti (tabinfo->tp, tabinfo->cp_cenwave, row, &tabrow->cenwave);
	if (c_iraferr())
	    return (TABLE_ERROR);
	c_tbegti (tabinfo->tp, tabinfo->cp_extrheight, row,
			&tabrow->extrheight);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}

/* This routine reads the array data from one row.  The wavelengths and
   photometric corrections are read into pct_n_wl and pct_n respectively;
   "n" can refer to either h (the acual extraction height) or inf (for
   an "infinite" extraction height).
*/

static int ReadPCArray (TblInfo *tabinfo, int row,
		double *pct_n_wl[], double *pct_n[], int *pct_n_nelem) {

	int nret_wl, nret_pc;	/* number of elements actually read */

	/* Find out how many elements there are in the table row.
	   nelem will likely be much smaller than phot->p_nelem.
	*/
	c_tbegti (tabinfo->tp, tabinfo->cp_nelem, row, pct_n_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	/* Allocate space for the arrays to be read from the table. */
	*pct_n_wl = malloc (*pct_n_nelem * sizeof(double));
	*pct_n = malloc (*pct_n_nelem * sizeof(double));
	if (*pct_n_wl == NULL || *pct_n == NULL)
	    return (OUT_OF_MEMORY);

	/* Read the wavelength and correction factors from the table. */

	nret_wl = c_tbagtd (tabinfo->tp, tabinfo->cp_wl, row,
			*pct_n_wl, 1, *pct_n_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	nret_pc = c_tbagtd (tabinfo->tp, tabinfo->cp_pc, row,
			*pct_n, 1, *pct_n_nelem);
	if (c_iraferr())
	    return (TABLE_ERROR);

	if (nret_wl < *pct_n_nelem || nret_pc < *pct_n_nelem) {
	    c_tbtclo (tabinfo->tp);
	    printf ("ERROR    Not all coefficients were read from PCTAB.\n");
	    return (TABLE_ERROR);
	}

	return (0);
}

/* This routine divides pct_h by pct_inf and assigns the result to pct_ratio.

   If the extraction height was not specified, the default PCT_h of 1 will
   be assumed, so pct_ratio will just be the reciprocal of pc_inf.

   If any element of the pct_inf array is zero, the number of elements
   phot->pct_nelem will be set to zero to disable this correction.
*/

static int PCRatio (PhotInfo *phot, int extrsize,
		double pct_inf_wl[], double pct_inf[], int pct_inf_nelem,
		double pct_h[], int pct_h_nelem) {

	int i;

	phot->pct_nelem = pct_inf_nelem;
	phot->pct_wl = malloc (pct_inf_nelem * sizeof (double));
	phot->pct_ratio = malloc (pct_inf_nelem * sizeof (double));
	if (phot->pct_wl == NULL || phot->pct_ratio == NULL)
	    return (OUT_OF_MEMORY);

	for (i = 0;  i < phot->pct_nelem;  i++) {
	    phot->pct_wl[i] = pct_inf_wl[i];
	    if (pct_inf[i] <= 0.) {
		printf (
"Warning:  Correction = %.6g in PCTAB; no PCT correction will be applied.\n",
			pct_inf[i]);
		phot->pct_nelem = 0;
		return (0);
	    }
	    phot->pct_ratio[i] = 1. / pct_inf[i];	/* initial value */
	}

	if (extrsize != UNKNOWN) {

	    /* sanity check */
	    if (pct_h_nelem != pct_inf_nelem) {
		printf ("Warning:  Inconsistent array sizes in PCTAB;\n");
		printf ("  default extraction height will be assumed.\n");
		return (0);
	    }

	    for (i = 0;  i < phot->pct_nelem;  i++)
		phot->pct_ratio[i] *= pct_h[i];
	}

	return (0);
}

/* This routine closes the PCTAB table. */

static int ClosePCTab (TblInfo *tabinfo) {

	c_tbtclo (tabinfo->tp);
	if (c_iraferr())
	    return (TABLE_ERROR);

	return (0);
}
