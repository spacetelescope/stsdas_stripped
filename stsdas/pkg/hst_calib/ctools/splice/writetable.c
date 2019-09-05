# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ximio.h>
# include <xtables.h>
# include <c_iraf.h>

# include "splice.h"

typedef struct {
	IRAFPointer tp;		/* pointer to table descriptor */
	IRAFPointer cp[NCOLS];	/* column descriptors */
	int nrows;		/* number of rows in table */
	int nelem;		/* number of elements in array */
} TblInfo;

static int CreateTable (char *, IRAFPointer, char **, int, TblInfo *, int *);
static int OpenTable0 (char *, char **, TblInfo *);
static int WriteArray (TblInfo *, int, Spectrum *, double []);
static int WriteElem (TblInfo *, int, Spectrum *, double []);
static int History (IRAFPointer, char *);
static void UnShiftWl (Spectrum *, double []);
static int CloseTable (TblInfo *);

/* This routine creates the output table and writes the output spectrum to it.

   Phil Hodge, 1998 Oct 27

   Phil Hodge, 1998 Nov 13:
	Free table0.

   Phil Hodge, 1999 Oct 25:
	Rename functions;
	rewrite UnShiftWl to agree with the modified ShiftWl.
*/

int WriteTable (char *outtable, IRAFPointer tnt, char *colname[],
		Spectrum *outspec) {

/* arguments:
char *outtable      i: name of output table
IRAFPointer tnt     i: table name template (rewound, then read again)
char *colname[]     i: array of column names
Spectrum *outspec   i: pointer to output spectrum
*/

	TblInfo outinfo;	/* info about output table */
	double *wl;		/* scratch for "unshifted" wavelengths */
	int scalar_columns;	/* true if output columns should be scalars */
	int row;		/* loop index for row number */
	int status;

	/* Create the output table. */
	if ((status = CreateTable (outtable, tnt, colname,
			outspec->nelem, &outinfo, &scalar_columns)))
	    return (status);

	/* Copy the wavelengths from outspec->wl to wl, shifting so that
	   the values are the wavelengths at the center of each interval,
	   rather than at the beginning.
	*/
	wl = calloc (outspec->nelem, sizeof(double));
	if (wl == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}
	UnShiftWl (outspec, wl);

	/* Copy the fluxes and errors from flux to flux density, i.e.
	   divide by the wavelength interval.
	*/

	if (scalar_columns) {

	    /* Write a table of scalars. */
	    for (row = 1;  row <= outspec->nelem;  row++) {
		if ((status = WriteElem (&outinfo, row, outspec, wl)))
		    return (status);
	    }

	} else {

	    /* Columns in the output table should contain arrays. */
	    row = 1;
	    if ((status = WriteArray (&outinfo, row, outspec, wl)))
		return (status);
	}

	free (wl);

	if ((status = CloseTable (&outinfo)))
	    return (status);

	return (0);
}

/* This routine creates the output table.  Several columns are defined,
   based on the definitions in the first input table.  Header parameters
   are copied from the first input table to the output table.
*/

static int CreateTable (char *outtable, IRAFPointer tnt, char *colname[],
		int nelem, TblInfo *outinfo, int *scalar_columns) {

	char *table0;		/* name of first input table (the template) */
	TblInfo ininfo;		/* info about template table */
	char *cname, *cunits, *cfmt;	/* column info */
	int dtype;
	int done;		/* set by c_tbfpri and ignored */
	int status;

	table0 = calloc (STIS_LINE+1, sizeof(char));
	cname = calloc (STIS_CNAME+1, sizeof(char));
	cunits = calloc (STIS_CNAME+1, sizeof(char));
	cfmt = calloc (STIS_CNAME+1, sizeof(char));
	if (table0 == NULL ||
	    cname == NULL || cunits == NULL || cfmt == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}

	/* Get the name of the first input table, as a template. */
	c_imtrew (tnt);
	c_imtgetim (tnt, table0, STIS_LINE);

	/* Copy the primary header, if the tables are FITS. */
	c_tbfpri (table0, outtable, &done);
	if ((status = c_iraferr())) {
	    printf ("Warning, c_tbfpri:  status = %d\n", status);
	    clear_cvoserr();		/* not fatal */
	}
	/* If we were successful in copying the primary header,
	   write history records to the primary header.
	*/
	if (done) {
	    if ((status = History (tnt, outtable)))
		return (status);
	}

	/* Open the first input table. */
	if ((status = OpenTable0 (table0, colname, &ininfo)))
	    return (status);

	outinfo->tp = c_tbtopn (outtable, IRAF_NEW_FILE, 0);
	if ((status = c_iraferr()))
	    return (status);

	outinfo->nrows = 0;		/* dummy value */

	/* If the first input table contains scalar columns, so should the
	   output table.
	*/
	if (ininfo.nelem <= 1) {
	    *scalar_columns = 1;
	    outinfo->nelem = 1;		/* scalar columns */
	} else {
	    *scalar_columns = 0;
	    outinfo->nelem = nelem;	/* ultimate value, not current value */
	}

	/* Create columns. */

	if (ininfo.nelem > 1) {
	    /* create NELEM column */
	    if (ininfo.cp[NELEM_INDEX] == 0) {
		strcpy (cname, colname[NELEM_INDEX]);
		cunits[0] = '\0';
		cfmt[0] = '\0';
	    } else {
		c_tbcigt (ininfo.cp[NELEM_INDEX], TBL_COL_NAME,
			cname, STIS_CNAME);
		c_tbcigt (ininfo.cp[NELEM_INDEX], TBL_COL_UNITS,
			cunits, STIS_CNAME);
		c_tbcigt (ininfo.cp[NELEM_INDEX], TBL_COL_FMT,
			cfmt, STIS_CNAME);
	    }
	    dtype = IRAF_INT;

	    c_tbcdef1 (outinfo->tp, &outinfo->cp[NELEM_INDEX],
			cname, cunits, cfmt, dtype, 1);
	    if ((status = c_iraferr()))
		return (status);
	}

	/* create WAVELENGTH column */
	c_tbcigt (ininfo.cp[WL_INDEX], TBL_COL_NAME, cname, STIS_CNAME);
	c_tbcigt (ininfo.cp[WL_INDEX], TBL_COL_UNITS, cunits, STIS_CNAME);
	c_tbcigt (ininfo.cp[WL_INDEX], TBL_COL_FMT, cfmt, STIS_CNAME);
	dtype = c_tbcigi (ininfo.cp[WL_INDEX], TBL_COL_DATATYPE);

	c_tbcdef1 (outinfo->tp, &outinfo->cp[WL_INDEX],
			cname, cunits, cfmt, dtype, outinfo->nelem);
	if ((status = c_iraferr()))
	    return (status);

	/* create FLUX column */
	c_tbcigt (ininfo.cp[FLUX_INDEX], TBL_COL_NAME, cname, STIS_CNAME);
	c_tbcigt (ininfo.cp[FLUX_INDEX], TBL_COL_UNITS, cunits, STIS_CNAME);
	c_tbcigt (ininfo.cp[FLUX_INDEX], TBL_COL_FMT, cfmt, STIS_CNAME);
	dtype = c_tbcigi (ininfo.cp[FLUX_INDEX], TBL_COL_DATATYPE);

	c_tbcdef1 (outinfo->tp, &outinfo->cp[FLUX_INDEX],
			cname, cunits, cfmt, dtype, outinfo->nelem);
	if ((status = c_iraferr()))
	    return (status);

	/* create ERROR column */
	c_tbcigt (ininfo.cp[ERR_INDEX], TBL_COL_NAME, cname, STIS_CNAME);
	c_tbcigt (ininfo.cp[ERR_INDEX], TBL_COL_UNITS, cunits, STIS_CNAME);
	c_tbcigt (ininfo.cp[ERR_INDEX], TBL_COL_FMT, cfmt, STIS_CNAME);
	dtype = c_tbcigi (ininfo.cp[ERR_INDEX], TBL_COL_DATATYPE);

	c_tbcdef1 (outinfo->tp, &outinfo->cp[ERR_INDEX],
			cname, cunits, cfmt, dtype, outinfo->nelem);
	if ((status = c_iraferr()))
	    return (status);

	/* create DQ column */
	c_tbcigt (ininfo.cp[DQ_INDEX], TBL_COL_NAME, cname, STIS_CNAME);
	c_tbcigt (ininfo.cp[DQ_INDEX], TBL_COL_UNITS, cunits, STIS_CNAME);
	c_tbcigt (ininfo.cp[DQ_INDEX], TBL_COL_FMT, cfmt, STIS_CNAME);
	dtype = c_tbcigi (ininfo.cp[DQ_INDEX], TBL_COL_DATATYPE);

	c_tbcdef1 (outinfo->tp, &outinfo->cp[DQ_INDEX],
			cname, cunits, cfmt, dtype, outinfo->nelem);
	if ((status = c_iraferr()))
	    return (status);

	/* Create the table. */
	c_tbtcre (outinfo->tp);

	/* Copy all header keywords. */
	c_tbhcal (ininfo.tp, outinfo->tp);

	/* Done with template table. */
	if ((status = CloseTable (&ininfo)))
	    return (status);

	free (cname);
	free (cunits);
	free (cfmt);
	free (table0);

	return (0);
}

static int OpenTable0 (char *table, char *colname[], TblInfo *ininfo) {

	int status;

	ininfo->tp = c_tbtopn (table, IRAF_READ_ONLY, 0);
	if ((status = c_iraferr()))
	    return (status);

	/* Find the columns. */
	c_tbcfnd1 (ininfo->tp, colname[NELEM_INDEX],
			&ininfo->cp[NELEM_INDEX]);
	c_tbcfnd1 (ininfo->tp, colname[WL_INDEX], &ininfo->cp[WL_INDEX]);
	c_tbcfnd1 (ininfo->tp, colname[FLUX_INDEX], &ininfo->cp[FLUX_INDEX]);
	c_tbcfnd1 (ininfo->tp, colname[ERR_INDEX], &ininfo->cp[ERR_INDEX]);
	c_tbcfnd1 (ininfo->tp, colname[DQ_INDEX], &ininfo->cp[DQ_INDEX]);

	/* The table could contain either arrays or scalar columns. */
	ininfo->nrows = -1;	/* at this point, we don't care */
	ininfo->nelem = c_tbcigi (ininfo->cp[WL_INDEX], TBL_COL_LENDATA);

	return (0);
}

/* This routine writes the arrays of data at the current row (= 1). */

static int WriteArray (TblInfo *outinfo, int row, Spectrum *outspec,
		double wl[]) {

	int status;

	c_tbepti (outinfo->tp, outinfo->cp[NELEM_INDEX], row, outspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	/* Note:  write wl, not outspec->wl. */
	c_tbaptd (outinfo->tp, outinfo->cp[WL_INDEX], row,
			wl, 1, outspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	c_tbaptd (outinfo->tp, outinfo->cp[FLUX_INDEX], row,
			outspec->flux, 1, outspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	c_tbaptd (outinfo->tp, outinfo->cp[ERR_INDEX], row,
			outspec->error, 1, outspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	c_tbapts (outinfo->tp, outinfo->cp[DQ_INDEX], row,
			outspec->dq, 1, outspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	return (0);
}

/* This routine writes one element to a scalar column at the specified
   row number.
*/

static int WriteElem (TblInfo *outinfo, int row, Spectrum *outspec,
		double wl[]) {

	int status;

	/* Note:  write wl, not outspec->wl. */
	c_tbeptd (outinfo->tp, outinfo->cp[WL_INDEX], row, wl[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbeptd (outinfo->tp, outinfo->cp[FLUX_INDEX], row,
			outspec->flux[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbeptd (outinfo->tp, outinfo->cp[ERR_INDEX], row,
			outspec->error[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbepts (outinfo->tp, outinfo->cp[DQ_INDEX], row,
			outspec->dq[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	return (0);
}

/* This routine writes history records to the output primary header
   with the names of the input tables.
*/

static int History (IRAFPointer tnt, char *outtable) {

	IRAFPointer otp;	/* for output primary header */
	char *table;		/* a table name */
	char *history;		/* a history record */
	int status;

	table = calloc (STIS_LINE+1, sizeof(char));
	history = calloc (STIS_LINE+1, sizeof(char));
	if (table == NULL || history == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}

	/* Construct primary header name. */
	strcpy (table, outtable);
	strcat (table, "[0]");

	otp = c_tbtopn (table, IRAF_READ_WRITE, 0);
	if ((status = c_iraferr()))
	    return (status);

	if (c_imtlen (tnt) == 1) {
	    c_tbhadt (otp, "HISTORY",
		"Rows in the following table were spliced together:");
	} else {
	    c_tbhadt (otp, "HISTORY",
		"The following tables were spliced together:");
	}

	c_imtrew (tnt);
	while (c_imtgetim (tnt, table, STIS_LINE) > 0) {

	    strcpy (history, "   ");
	    strcat (history, table);
	    c_tbhadt (otp, "HISTORY", history);
	    if ((status = c_iraferr()))
		return (status);
	}

	c_tbtclo (otp);

	free (table);
	free (history);
	return (0);
}

/* This routine copies the wavelengths from outspec to wl, shifting
   so that each value in wl is the wavelength in the middle of the
   interval, rather than at the beginning.

   outspec->wl contains the shifted wavelengths, i.e. the wavelengths
   at the bin boundaries;
   outspec->wl has outspec->nelem + 1 elements.

   On output, wl will contain the wavelengths at the bin centers;
   wl has outspec->nelem elements.
*/

static void UnShiftWl (Spectrum *outspec, double wl[]) {

	int i;

	wl[0] = (outspec->wl[0] + outspec->wl[1]) / 2.;

	for (i = 1;  i < outspec->nelem;  i++)
	    wl[i] = 2. * outspec->wl[i] - wl[i-1];
}

/* This routine closes a table. */

static int CloseTable (TblInfo *tabinfo) {

	int status;

	c_tbtclo (tabinfo->tp);
	if ((status = c_iraferr()))
	    return (status);

	return (0);
}
