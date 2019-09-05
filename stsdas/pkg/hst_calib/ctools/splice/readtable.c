# include <stdio.h>
# include <stdlib.h>
# include <xtables.h>
# include <c_iraf.h>

# include "splice.h"

typedef struct {
	IRAFPointer tp;		/* pointer to table descriptor */
	IRAFPointer cp[NCOLS];	/* column descriptors */
	int nrows;		/* number of rows in table */
	int nelem;		/* number of elements in array */
} TblInfo;

/* These parameters and variables are for passing info about the
   weight column and scalar weight to PrintInfo.
*/
# define WGT_COL_FOUND           0
# define WGT_COL_NOT_FOUND       1
# define WGT_NAME_NOT_SPECIFIED  2
# define WGT_KEYWORD_USED        3
# define WGT_KEYWORD_NOT_FOUND   4
static int wgt_flag;		/* info about weight column */
static int sw_flag;		/* info about scalar weight */
static double save_scalar_weight;	/* the scalar weight value */

int positiveWl(Spectrum *, int *, int *);

static int OpenTable (char *, char **, TblInfo *);
static int ReadArray (TblInfo *, int, Spectrum *);
static int ReadElem (TblInfo *, int, Spectrum *);
static int GetSW (TblInfo *, char **, int, double *);
static int CloseTable (TblInfo *);
static void PrintInfo (char *);

/* This function reads an input table into memory.

   Phil Hodge, 1998 Oct 27:
	Initial version.

   Phil Hodge, 1999 Feb 19:
	Add verbose argument, and call PrintInfo.

   Phil Hodge, 1999 Oct 25:
	Move ShiftWl to shiftwl.c; rename functions.

   Phil Hodge, 2013 May 3:
	Add calls to positiveWl to check that all wavelengths are positive.
	Add a check that nelem is at least 2 before calling ShiftWl.

   Phil Hodge, 2014 Mar 28:
	In ReadTable, move the calls to GetSW to a point after calling
	ReadArray or ReadElem, because the Spectrum object is allocated
	by those functions, and allocating a Spectrum initializes the
	scalar weight to 1.
*/

int ReadTable (char *table, char *colname[], Bool verbose,
		SpecArray *spectra) {

/* arguments:
char *table         i: name of current input table
char *colname[]     i: array of column names
SpecArray *spectra  io: array of spectra
*/

	TblInfo tabinfo;	/* pointer to table descriptor, etc */
	double scalar_weight;	/* read after Spectrum has been allocated */
	int row;		/* loop index for row number */
	int n;			/* array index for a spectrum */
	int mod_start, mod_end;	/* flags regarding negative wavelengths */
	int status;
	void ShiftWl (double *, int);

	int NewSpec (SpecArray *);

	/* Open the table. */
	if ((status = OpenTable (table, colname, &tabinfo)))
	    return (status);

	if (tabinfo.nelem > 1) {

	    /* The columns contain arrays. */

	    for (row = 1;  row <= tabinfo.nrows;  row++) {
		if ((status = NewSpec (spectra)))
		    return (status);
		n = spectra->nspec - 1;
		if ((status = ReadArray (&tabinfo, row, spectra->spec[n])))
		    return (status);
		if ((status = GetSW (&tabinfo, colname, row, &scalar_weight)))
		    return (status);
		(spectra->spec[n])->scalar_weight = scalar_weight;
		status = positiveWl(spectra->spec[n], &mod_start, &mod_end);
		if (status > 0) {
		    printf("Warning:  nelem set to 0 because of zero or"
		           " negative wavelengths in %s, row %d.\n",
		           table, row);
		} else if (status < 0) {
		    printf("Warning:  zero or negative wavelengths were"
		           " truncated from end(s) of %s, row %d.\n",
		           table, row);
		}
		/* shift wavelengths */
                if (spectra->spec[n]->nelem >= 2)
                    ShiftWl((spectra->spec[n])->wl, (spectra->spec[n])->nelem);
	    }

	} else {

	    /* We have a table of scalars. */

	    if ((status = NewSpec (spectra)))
		return (status);
	    n = spectra->nspec - 1;

	    for (row = 1;  row <= tabinfo.nrows;  row++) {

		if ((status = ReadElem (&tabinfo, row, spectra->spec[n])))
		    return (status);
		if (row == 1) {
		    if ((status = GetSW (&tabinfo, colname, row,
				&scalar_weight)))
			return (status);
		    (spectra->spec[n])->scalar_weight = scalar_weight;
		}
	    }
	    status = positiveWl(spectra->spec[n], &mod_start, &mod_end);
	    if (status > 0) {
		printf("Warning:  nelem set to 0 because of zero or"
		       " negative wavelengths in %s.\n", table);
	    } else if (status < 0) {
		printf("Warning:  zero or negative wavelengths were"
		       " truncated from end(s) of %s.\n", table);
	    }
            if (spectra->spec[n]->nelem >= 2)
                ShiftWl((spectra->spec[n])->wl, (spectra->spec[n])->nelem);
	}

	if ((status = CloseTable (&tabinfo)))
	    return (status);

	if (verbose)
	    PrintInfo (table);

	return (0);
}

static int OpenTable (char *table, char *colname[], TblInfo *tabinfo) {

	int status;
	int i;			/* loop index */

	tabinfo->tp = c_tbtopn (table, IRAF_READ_ONLY, 0);
	if ((status = c_iraferr()))
	    return (status);

	/* Find the columns. */
	c_tbcfnd1 (tabinfo->tp, colname[NELEM_INDEX],
			&tabinfo->cp[NELEM_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[WL_INDEX], &tabinfo->cp[WL_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[FLUX_INDEX], &tabinfo->cp[FLUX_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[ERR_INDEX], &tabinfo->cp[ERR_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[DQ_INDEX], &tabinfo->cp[DQ_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[WGT_INDEX], &tabinfo->cp[WGT_INDEX]);
	c_tbcfnd1 (tabinfo->tp, colname[SW_INDEX], &tabinfo->cp[SW_INDEX]);

	/* These are required columns. */
	status = 0;
	for (i = WL_INDEX;  i < WGT_INDEX;  i++) {
	    if (tabinfo->cp[i] == 0) {
		printf ("Column `%s' not found.\n", colname[i]);
		status = 1;
	    }
	}
	if (status != 0)
	    return (status);

	/* Set a flag value for use by PrintInfo. */
	if (tabinfo->cp[WGT_INDEX] == 0) {
	    if (colname[WGT_INDEX][0] == '\0')
		wgt_flag = WGT_NAME_NOT_SPECIFIED;
	    else
		wgt_flag = WGT_COL_NOT_FOUND;
	} else {
	    wgt_flag = WGT_COL_FOUND;
	}

	/* The table could contain either arrays or scalar columns. */
	tabinfo->nrows = c_tbpsta (tabinfo->tp, TBL_NROWS);
	tabinfo->nelem = c_tbcigi (tabinfo->cp[WL_INDEX], TBL_COL_LENDATA);

	return (0);
}

static int ReadArray (TblInfo *tabinfo, int row, Spectrum *inspec) {

	int status;
	int nelem;		/* number of elements in arrays */
	/* numbers of elements actually read */
	int nwl, nflux, nerr, ndq, nwgt;
	int i;
	int AllocSpec (Spectrum *, int);

	/* Find out how many elements there are in the arrays, and
	   allocate space for the arrays to be read from the table.
	*/
	if (tabinfo->cp[NELEM_INDEX] == 0) {	/* NELEM column not found */
	    /* Use allocated size of wavelength column. */
	    nelem = tabinfo->nelem;
	} else {
	    /* Get value for current row from NELEM column. */
	    c_tbegti (tabinfo->tp, tabinfo->cp[NELEM_INDEX], row, &nelem);
	    if ((status = c_iraferr()))
		return (status);
	    if (nelem > tabinfo->nelem) {
		printf (
		"Value in NELEM column is larger than column array size.\n");
		return (1);
	    }
	}

	/* Allocate memory for the current input spectrum. */
	if ((status = AllocSpec (inspec, nelem)))
	    return (status);

	nwl = c_tbagtd (tabinfo->tp, tabinfo->cp[WL_INDEX], row,
			inspec->wl, 1, inspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	nflux = c_tbagtd (tabinfo->tp, tabinfo->cp[FLUX_INDEX], row,
			inspec->flux, 1, inspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	nerr = c_tbagtd (tabinfo->tp, tabinfo->cp[ERR_INDEX], row,
			inspec->error, 1, inspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	ndq = c_tbagts (tabinfo->tp, tabinfo->cp[DQ_INDEX], row,
			inspec->dq, 1, inspec->nelem);
	if ((status = c_iraferr()))
	    return (status);

	if (nwl < inspec->nelem || nflux < inspec->nelem ||
	    nerr < inspec->nelem || ndq < inspec->nelem) {
	    printf ("Not all array elements were read from input table.\n");
	    return (1);
	}

	if (tabinfo->cp[WGT_INDEX] == 0) {
	    for (i = 0;  i < inspec->nelem;  i++)
		inspec->weight[i] = 1.;
	} else {
	    nwgt = c_tbagtd (tabinfo->tp, tabinfo->cp[WGT_INDEX], row,
			inspec->weight, 1, inspec->nelem);
	    if ((status = c_iraferr()))
		return (status);
	    if (nwgt < inspec->nelem) {
		printf ("Not all array elements were read from input table.\n");
		return (1);
	    }
	}

	return (0);
}

static int ReadElem (TblInfo *tabinfo, int row, Spectrum *inspec) {

	int status;
	int i;
	int AllocSpec (Spectrum *, int);

	/* If memory hasn't been allocated yet, allocate it. */
	if (inspec->wl == NULL) {
	    if ((status = AllocSpec (inspec, tabinfo->nrows)))
		return (status);
	}

	c_tbegtd (tabinfo->tp, tabinfo->cp[WL_INDEX], row,
			&inspec->wl[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbegtd (tabinfo->tp, tabinfo->cp[FLUX_INDEX], row,
			&inspec->flux[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbegtd (tabinfo->tp, tabinfo->cp[ERR_INDEX], row,
			&inspec->error[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	c_tbegts (tabinfo->tp, tabinfo->cp[DQ_INDEX], row,
			&inspec->dq[row-1]);
	if ((status = c_iraferr()))
	    return (status);

	if (tabinfo->cp[WGT_INDEX] == 0) {
	    for (i = 0;  i < inspec->nelem;  i++)
		inspec->weight[i] = 1.;
	} else {
	    c_tbegtd (tabinfo->tp, tabinfo->cp[WGT_INDEX], row,
			&inspec->weight[row-1]);
	    if ((status = c_iraferr()))
		return (status);
	}

	return (0);
}

/* If the scalar weight column exists, read the value from that column.
   If not, try to get it from the header.  If it's not there either,
   set it to one.
*/

static int GetSW (TblInfo *tabinfo, char *colname[], int row,
		double *scalar_weight) {

	int status;

	if (tabinfo->cp[SW_INDEX] == 0) {		/* column not found */
	    if (colname[SW_INDEX][0] == '\0') {		/* name not specified */
		*scalar_weight = 1.;
		sw_flag = WGT_NAME_NOT_SPECIFIED;	/* for PrintInfo */
	    } else {
		*scalar_weight = c_tbhgtd (tabinfo->tp, colname[SW_INDEX]);
		if ((c_iraferr())) {
		    *scalar_weight = 1.;
		    clear_cvoserr();
		    sw_flag = WGT_KEYWORD_NOT_FOUND;
		} else {
		    sw_flag = WGT_KEYWORD_USED;
		}
	    }

	} else {				/* column exists in table */

	    c_tbegtd (tabinfo->tp, tabinfo->cp[SW_INDEX], row,
			scalar_weight);
	    if ((status = c_iraferr()))
		return (status);
	    sw_flag = WGT_COL_FOUND;
	}

	save_scalar_weight = *scalar_weight;

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

static void PrintInfo (char *table) {

	printf ("Table %s\n", table);

	if (wgt_flag == WGT_NAME_NOT_SPECIFIED) {
	    printf ("  Weight column name is blank; ");
	    printf ("unit weight will be used.\n");
	} else if (wgt_flag == WGT_COL_NOT_FOUND) {
	    printf ("  Weight column not found; ");
	    printf ("unit weight will be used.\n");
	}

	if (sw_flag == WGT_NAME_NOT_SPECIFIED) {
	    printf ("  Scalar weight column name is blank; ");
	    printf ("unit scalar weight will be used.\n");
	} else if (sw_flag == WGT_KEYWORD_USED) {
	    printf ("  Scalar weight taken from header keyword, ");
	    printf ("value is %.6g.\n", save_scalar_weight);
	} else if (sw_flag == WGT_KEYWORD_NOT_FOUND) {
	    printf ("  Neither scalar weight column nor keyword was found;\n");
	    printf ("    unit scalar weight will be used.\n");
	} else if (sw_flag == WGT_COL_FOUND) {
	    printf ("  Value of scalar weight column is %.6g.\n",
		save_scalar_weight);
	}
	fflush (stdout);
}
