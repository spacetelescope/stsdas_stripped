# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <xclio.h>

# include "splice.h"

static void cl_get_string (char *, char *, int);

/* This is an IRAF task to splice STIS spectra.

   Phil Hodge, 1998 Oct 27:
	Initial version.

   Phil Hodge, 1999 Feb 19:
	Add spacing and verbose parameters.

   Phil Hodge, 1999 Oct 25:
	Add wavetab parameter; add cl_get_string function;
	rename oSplice to Splice.

   Phil Hodge, 2014 Aug 5:
	Add dq_fill parameter.
*/

# if defined (NATIVE_IRAF)
IRAFTASK (splice) {
# else
int main (int argc, char **argv) {
# endif

	char *intable;			/* name of input table(s) */
	char *outtable;			/* name of output table */
	char *wavetab;			/* optional table of wavelengths */
	char *spacing;			/* min or max wavelength spacing */
	char *colname[NCOLS];		/* list of column names */
	int sdqflags;			/* "serious" data quality flags */
	int dq_fill;			/* output DQ flag for no good input */
	Bool verbose;
	int wl_spacing;			/* wavelength spacing */
	int i;
	int status;
	int SpliceSpec (char *, char *, char *, char **,
	                int, short, int, Bool);

# if defined (NATIVE_IRAF)

	intable = calloc (STIS_LINE+1, sizeof(char));
	outtable = calloc (STIS_LINE+1, sizeof(char));
	wavetab = calloc (STIS_LINE+1, sizeof(char));
	spacing = calloc (STIS_CNAME+1, sizeof(char));
	if (intable == NULL || outtable == NULL ||
	    wavetab == NULL || spacing == NULL) {
	    printf ("out of memory\n");
	    return;
	}
	for (i = 0;  i < NCOLS;  i++) {
	    if ((colname[i] = calloc (STIS_CNAME+1, sizeof(char))) == NULL) {
		printf ("out of memory\n");
		return;
	    }
	}

	/* Get cl parameters. */
	cl_get_string ("intable", intable, STIS_LINE);
	cl_get_string ("outtable", outtable, STIS_LINE);
	cl_get_string ("wavetab", wavetab, STIS_LINE);
	if (wavetab[0] == '\0')
	    cl_get_string ("spacing", spacing, STIS_CNAME);
	else
	    strcpy (spacing, "explicit");
	sdqflags = c_clgeti ("sdqflags");
	dq_fill = c_clgeti ("dq_fill");
	cl_get_string ("wl_name", colname[WL_INDEX], STIS_CNAME);
	cl_get_string ("flux_name", colname[FLUX_INDEX], STIS_CNAME);
	cl_get_string ("err_name", colname[ERR_INDEX], STIS_CNAME);
	cl_get_string ("dq_name", colname[DQ_INDEX], STIS_CNAME);
	cl_get_string ("wgt_name", colname[WGT_INDEX], STIS_CNAME);
	cl_get_string ("sw_name", colname[SW_INDEX], STIS_CNAME);
	cl_get_string ("n_name", colname[NELEM_INDEX], STIS_CNAME);
	verbose = c_clgetb ("verbose");

# else
	printf ("Host executable not currently supported.\n");
	exit (1);
# endif

	if (spacing[0] == 'e')		/* wavelengths specified explicitly */
	    wl_spacing = EXPLICIT_WL;
	else if (spacing[0] == 'f')	/* fine spacing */
	    wl_spacing = MIN_D_WL;
	else				/* default is coarse spacing */
	    wl_spacing = MAX_D_WL;

	status = SpliceSpec (intable, outtable, wavetab, colname,
		wl_spacing, (short)sdqflags, dq_fill, verbose);
	if (status) {
	    if (c_iraferr())
		printf ("IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
	    else
		printf ("Error (%d) splicing tables.\n", status);
	}

	for (i = 0;  i < NCOLS;  i++)
	    free (colname[i]);
	free (intable);
	free (outtable);
	free (wavetab);
	free (spacing);

# if defined (NATIVE_IRAF)
	return;
# else
	exit (status);
# endif
}

/* This routine gets a cl parameter of type string and then trims
   trailing blanks.
*/

static void cl_get_string (char *param, char *value, int maxch) {

	int i;

	c_clgstr (param, value, maxch);

	for (i = strlen (value) - 1;  i >= 0;  i--) {
	    if (value[i] == ' ' || value[i] == '\t')
		value[i] = '\0';
	    else
		break;
	}
}
