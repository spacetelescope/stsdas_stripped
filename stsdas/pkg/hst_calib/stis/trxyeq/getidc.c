# include <stdio.h>
# include <stdlib.h>		/* calloc */
# include <string.h>
# include <math.h>		/* fabs, sqrt */
# include <ctype.h>

# include <c_iraf.h>
# include <xtables.h>

# include "trxyeq.h"

typedef struct {
	IRAFPointer tp;			/* pointer to table descriptor */
	IRAFPointer cp_direction;	/* column descriptors */
	IRAFPointer cp_select;

	IRAFPointer cp_scale;
	IRAFPointer cp_xref;
	IRAFPointer cp_yref;

	int nrows;			/* number of rows in table */
} TblInfo;

typedef struct {
	char direction[SZ_CBUF+1];	/* forward or inverse mapping */
	char tsel_val[SZ_CBUF+1];	/* value of select column */
} TblRow;

static int openDistTab (char *, char *, TblInfo *);
static int readDistTab (TblInfo *, int, TblRow *);
static int readCoefficients (char *, TblInfo *, int, DistInfo *);
static int identityCoefficients (DistInfo *);
static int SameString (char *rowvalue, char *value);
static int streq_ic (char *, char *);

/* This routine reads the distortion information from the imaging
   distortion table IDCTAB.  This is only used for obstype=IMAGING.

   The distortion information table should contain the following:
	header parameters:
		NORDER:  polynomial order (int)
	columns:
		DIRECTION:  direction of mapping (string)
		select:  selection column (string) (required if specified)
		XREF, YREF:  zero point in input image (double)
		SCALE:  arcseconds / pixel (double)
		CX00, CY00, etc:  arrays of coefficients (double)

   The table is read to find the first row for which the value of
   DIRECTION agrees with the variable in the calling sequence.
   "FORWARD" means from distorted to undistorted pixel coordinates,
   and this is the value used by trxyeq; "INVERSE" is the value used
   by treqxy.  The 'select' argument can specify the name of another
   character-string column (e.g. DETCHIP or FILTER) that will then be
   used to select the row, without regard for case.  If this column was
   specified (is not ""), it must be present in the table.

   Note that for all columns that include pixel in the units, the size is
   that of a reference pixel, not necessarily an image pixel.

   Note:
	Memory is allocated for the distortion coefficients, dist->xcoeff
	and dist->ycoeff.  The memory should be freed by calling freeDist.

   Phil Hodge, 2002 Mar 19:
	Copy from calstis7 and rewrite.

   Phil Hodge, 2003 Aug 4:
	Initialize dist->offset[0] and dist->offset[1] to 0.

   Phil Hodge, 2004 Sept 17:
	Add direction to the calling sequence, and get coefficients for
	either forward or inverse mapping.
	Get NORDER from the primary header if it is not found in the
	table header.
*/

int getIDC (char *idctab, char *select, char *direction, char *ksel_val,
		DistInfo *dist) {

/* arguments:
char *idctab        i: name of IDC table
char *select        i: keyword and column name to use for row selection
char *ksel_val      i: value of select keyword from image header
DistInfo *dist      o: distortion coefficients
*/

	int status;

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	TblRow tabrow;		/* values read from a table row */

	int row;		/* loop index */
	int foundit = 0;	/* true if parameters found in table */

	if (strcmp (idctab, "none") == 0) {
	    /* Assign coefficients for an identity transformation. */
	    status = identityCoefficients (dist);
	    return (status);
	}

	if (SameString (direction, "FORWARD"))
	    dist->forward = 1;
	else
	    dist->forward = 0;

	/* Open the distortion information table. */
	if (status = openDistTab (idctab, select, &tabinfo))
	    return (status);

	/* Find the row for the forward mapping. */

	for (row = 1;  row <= tabinfo.nrows;  row++) {

	    if (status = readDistTab (&tabinfo, row, &tabrow))
		return (status);

	    if (SameString (tabrow.direction, direction) &&
		SameString (tabrow.tsel_val, ksel_val)) {

		/* This is the row; read the info. */
		foundit = 1;

		if (status = readCoefficients (idctab, &tabinfo, row, dist))
		    return (status);

		break;
	    }
	}

	if (!foundit) {
	    printf ("Matching row not found in %s.\n", idctab);
	    return (2);
	}

	c_tbtclo (tabinfo.tp);
	if (c_iraferr())
	    return (2);

	return (0);
}

/* This routine opens the IDC table, finds the columns
   that we need, and gets the total number of rows in the table.
*/

static int openDistTab (char *tname, char *select, TblInfo *tabinfo) {

	IRAFPointer cp[4];	/* for checking whether columns exist */

	tabinfo->tp = c_tbtopn (tname, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    printf ("ERROR:  IDCTAB `%s' not found.\n", tname);
	    return (2);
	}

	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);

	/* Find the columns. */

	c_tbcfnd1 (tabinfo->tp, "DIRECTION", &tabinfo->cp_direction);
	if (select[0] == '\0')
	    tabinfo->cp_select = 0;
	else
	    c_tbcfnd1 (tabinfo->tp, select, &tabinfo->cp_select);

	c_tbcfnd1 (tabinfo->tp, "SCALE", &tabinfo->cp_scale);
	c_tbcfnd1 (tabinfo->tp, "XREF", &tabinfo->cp_xref);
	c_tbcfnd1 (tabinfo->tp, "YREF", &tabinfo->cp_yref);

	if (tabinfo->cp_direction == 0 ||
	    tabinfo->cp_scale == 0 ||
	    tabinfo->cp_xref == 0 || tabinfo->cp_yref == 0) {

	    c_tbtclo (tabinfo->tp);
	    printf ("ERROR:  Column not found in IDCTAB.\n");
	    return (2);
	}
	if (select[0] != '\0' && tabinfo->cp_select == 0) {
	    c_tbtclo (tabinfo->tp);
	    printf ("ERROR:  Column `%s' not found in IDCTAB.\n", select);
	    return (2);
	}

	/* Look for these columns just to make sure they're present. */
	c_tbcfnd1 (tabinfo->tp, "CX10", &cp[0]);
	c_tbcfnd1 (tabinfo->tp, "CX11", &cp[1]);
	c_tbcfnd1 (tabinfo->tp, "CY10", &cp[2]);
	c_tbcfnd1 (tabinfo->tp, "CY11", &cp[3]);
	if (cp[0] == 0 || cp[1] == 0 || cp[2] == 0 || cp[3] == 0) {
	    c_tbtclo (tabinfo->tp);
	    printf ("Required columns CX10, CX11, CY10, CY11 not found.\n");
	    return (2);
	}

	return (0);
}

/* This routine reads the columns (DIRECTION and optionally another column,
   given by the 'select' argument) used to select the correct row.
*/

static int readDistTab (TblInfo *tabinfo, int row, TblRow *tabrow) {

	c_tbegtt (tabinfo->tp, tabinfo->cp_direction, row,
			tabrow->direction, SZ_CBUF);
	if (c_iraferr())
	    return (2);

	if (tabinfo->cp_select == 0) {

	    /* default value, if column was not present in IDC table */
	    strcpy (tabrow->tsel_val, "ANY");

	} else {

	    c_tbegtt (tabinfo->tp, tabinfo->cp_select, row,
			tabrow->tsel_val, SZ_CBUF);
	    if (c_iraferr())
		return (2);
	}

	return (0);
}

/* This routine reads the data from one row into the dist structure.
   The number of elements in the arrays of distortion coefficients is
   gotten into dist, memory is allocated, and the array values are read.
   The order of the polynomial coefficients is gotten from the NORDER
   header keyword.
*/

static int readCoefficients (char *idctab,
	TblInfo *tabinfo, int row, DistInfo *dist) {

	char colname[SZ_COLNAME];	/* a column name */
	IRAFPointer cp;		/* for getting coefficients */
	int i, j, k;
	int kx, ky;		/* indexes for coefficients of x and y */

	/* Allocate memory for the coefficients. */
	dist->xcoeff = calloc (MAX_NCOEFF, sizeof(double));
	dist->ycoeff = calloc (MAX_NCOEFF, sizeof(double));
	if (dist->xcoeff == NULL || dist->ycoeff == NULL) {
	    c_tbtclo (tabinfo->tp);
	    return (2);
	}
	dist->allocated = 1;			/* set flag */

	/* Initialize.  It is not required that all columns be present;
	   missing columns result in default values for coefficients.
	*/

	for (k = 0;  k < MAX_NCOEFF;  k++) {
	    dist->xcoeff[k] = 0.;
	    dist->ycoeff[k] = 0.;
	}
	kx = WHICH_COEFF (1, 1);	/* CX11, coefficient of x */
	ky = WHICH_COEFF (1, 0);	/* CY10, coefficient of y */
	dist->xcoeff[kx] = 1.;		/* note:  just default values */
	dist->ycoeff[ky] = 1.;

	/* The polynomial order is a header keyword.  If it's not found
	   in the table header, look for it in the primary header.
	*/
	dist->norder = c_tbhgti (tabinfo->tp, "NORDER");
	if (c_iraferr()) {

	    IRAFPointer tp_phdr;
	    char phdr_name[IRAF_SZ_FNAME];

	    clear_cvoserr();
	    strcpy (phdr_name, idctab);
	    strcat (phdr_name, "[0]");
	    tp_phdr = c_tbtopn (phdr_name, IRAF_READ_ONLY, 0);
	    if (c_iraferr()) {
		printf (
		"ERROR:  Can't open IDCTAB `%s' primary header.\n", phdr_name);
		return (2);
	    }
	    dist->norder = c_tbhgti (tp_phdr, "NORDER");
	    if (c_iraferr()) {
		clear_cvoserr();
		printf (
	"Warning:  Can't read NORDER from IDCTAB table or primary header.\n");
		dist->norder = MAX_ORDER;
	    }
	    c_tbtclo (tp_phdr);
	}
	if (dist->norder > MAX_ORDER) {
	    printf (
	"ERROR:  Polynomial order = %d in IDCTAB exceeds maximum of %d\n",
		dist->norder, MAX_ORDER);
	    return (2);
	}

	/* Get the reference point for the input image (the zero point for
	   the mapping), and convert to zero indexing.
	*/

	/* input, distorted image */
	c_tbegtd (tabinfo->tp, tabinfo->cp_xref, row, &dist->xref);
	if (c_iraferr())
	    return (2);
	dist->xref--;
	c_tbegtd (tabinfo->tp, tabinfo->cp_yref, row, &dist->yref);
	if (c_iraferr())
	    return (2);
	dist->yref--;

	/* initial value, should be updated later by geoOffset() */
	dist->offset[0] = 0.;
	dist->offset[1] = 0.;

	/* Get the scale, because the CXij and CYij coefficients map from
	   arcseconds to pixels, and we need pixels to pixels.
	*/
	c_tbegtd (tabinfo->tp, tabinfo->cp_scale, row, &dist->scale);
	if (c_iraferr())
	    return (2);

	/* Get the coefficients. */

	for (i = 0;  i <= dist->norder;  i++) {

	    for (j = 0;  j <= i;  j++) {

		k = WHICH_COEFF (i, j);

		/* CX coefficient for x coordinate. */

		sprintf (colname, "CX%d%d", i, j);

		c_tbcfnd1 (tabinfo->tp, colname, &cp);
		if (cp != 0) {
		    c_tbegtd (tabinfo->tp, cp, row, &dist->xcoeff[k]);
		    if (c_iraferr())
			return (2);
		}

		/* CY coefficient for y coordinate. */

		sprintf (colname, "CY%d%d", i, j);

		c_tbcfnd1 (tabinfo->tp, colname, &cp);
		if (cp != 0) {
		    c_tbegtd (tabinfo->tp, cp, row, &dist->ycoeff[k]);
		    if (c_iraferr())
			return (2);
		}
	    }
	}

	return (0);
}

static int identityCoefficients (DistInfo *dist) {

	int k;
	int kx, ky;		/* indexes for coefficients of x and y */

	/* Allocate memory for the coefficients. */
	dist->xcoeff = calloc (MAX_NCOEFF, sizeof(double));
	dist->ycoeff = calloc (MAX_NCOEFF, sizeof(double));
	if (dist->xcoeff == NULL || dist->ycoeff == NULL) {
	    return (2);
	}
	dist->allocated = 1;			/* set flag */

	/* Assign values for the identity transformation. */
	dist->norder = 1;
	dist->scale = 1.;
	for (k = 0;  k < MAX_NCOEFF;  k++) {
	    dist->xcoeff[k] = 0.;
	    dist->ycoeff[k] = 0.;
	}
	kx = WHICH_COEFF (1, 1);	/* CX11, coefficient of x */
	ky = WHICH_COEFF (1, 0);	/* CY10, coefficient of y */
	dist->xcoeff[kx] = 1.;
	dist->ycoeff[ky] = 1.;
	/* these are arbitrary */
	dist->xref = 512.;
	dist->yref = 512.;

	dist->offset[0] = 0.;
	dist->offset[1] = 0.;

	return (0);
}

static int SameString (char *rowvalue, char *value) {

	if (streq_ic (rowvalue, "ANY"))
	    return (1);
	else if (streq_ic (value, "ANY"))
	    return (1);
	else if (streq_ic (rowvalue, value))
	    return (1);
	else
	    return (0);
}

static int streq_ic (char *s1, char *s2) {

	int c1, c2;
	int i;

	c1 = 1;
	for (i = 0;  c1 != 0;  i++) {

	    c1 = s1[i];
	    c2 = s2[i];
	    if (isupper(c1))
		c1 = tolower (c1);
	    if (isupper(c2))
		c2 = tolower (c2);
	    if (c1 != c2)
		return (0);
	}
	return (1);
}
