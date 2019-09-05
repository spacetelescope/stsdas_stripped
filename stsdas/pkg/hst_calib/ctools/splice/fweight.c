# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <c_iraf.h>
# include <xclio.h>
# include <ximio.h>

# include "fweight.h"

static int OpenTblLists (char *, char *, char *,
		IRAFPointer *, IRAFPointer *, IRAFPointer *);
static int GetNextTbl (IRAFPointer, IRAFPointer, IRAFPointer,
		char *, char *, char *, int *, int);

/* This is an IRAF task to compute weights and include them as a column
   in spectral data, for use with the splice task.

   There are two input tables.  intable1 contains spectra for which weights
   are to be assigned, e.g. output from x1d.  If the intable1 file contains
   more than one table, each must be specified individually.  intable2 is
   created by the user; it contains wavelengths and fluxes, but the fluxes
   should have been smoothed, e.g. by the continuum task.

   The output table outtable will be a copy of intable1 (or intable1
   can be operated on in-place).  A weight column will be added, if it
   doesn't already exist, and the weights will be assigned.  Note that
   if this weight column is used for splice, no scalar weight should need
   to be specified, as the weight includes the exposure time.

   The weights for outtable will be computed as follows.

   The weight is the signal to noise squared, (S/N)**2, divided by
   the dispersion.  Dividing by the dispersion is appropriate because
   splice adds up fluxes within each output bin.  If an input bin contains
   a lot of counts in a small wavelength interval, it should get higher
   weight than an input bin with the same counts in a wide wavelength
   interval.

   The signal at a pixel is the net number of electrons included in the
   extraction box.  The noise is estimated as the square root of the gross
   number of electrons in the extraction box.  However, the net and gross
   column values are not used directly because they fluctuate, and because
   it is desired that the weight not go up or down significantly due to
   emission or absorption lines.  The signal is therefore gotten by scaling
   the smoothed flux from intable2, interpolated onto the wavelength scale
   of intable1.  The gross counts are gotten by adding the background to
   the signal, instead of to the actual net counts.

	sensitivity = net[i] * gain / flux[i]

	signal = smoothed_flux[i] * sensitivity * exptime
	noise = sqrt (signal + Back_e)
	(S/N)**2 = signal**2 / (signal + Back_e)
	         = signal / (1 + (Back_e / signal))

	weight[i] = signal / (1 + average (Back_e / signal)) / dispersion

   In the above, i is an array index in a spectral order in intable1 (or
   a row number, if intable1 contains scalar columns).  flux and net are
   columns in intable1; note that net is a count rate.  Back_e is from the
   intable1 background column, but it has been converted to electrons by
   multiplying by gain and exposure time.  Furthermore, the average of
   (Back_e / signal) is taken over all pixels, and the factor dividing into
   the signal is formed from that average:  1 + average (Back_e / signal).
   smoothed_flux is from the flux column of intable2, but it has been
   interpolated at the wavelength of array index i.

   In deep absorption lines or regions of low signal to noise, there can
   be points where the net count rate (the true net, not scaled from the
   smoothed flux) will be zero.  At these points, we can't compute the
   sensitivity from the data, so the weight is temporarily set to zero.
   After computing all the weights, zero or negative weights will be
   replaced by linearly interpolated values from adjacent weights that
   are greater than zero.  At the endpoints, zero or negative weights
   will be replaced by the nearest positive weight.

   Phil Hodge, 1999 Aug 30:
	Initial version.
*/

IRAFTASK (fweight) {

	char *inlist1;		/* list of input table names (spectra) */
	char *inlist2;		/* list of input table names (wl & flux) */
	char *outlist;		/* list of output table names */
	IRAFPointer ilist1, ilist2, olist;

	char *intable1;		/* input table of spectra */
	char *intable2;		/* input table of wavelengths and weights */
	char *outtable;		/* output table */
	char *wgtname;		/* column name for weight */
	char *interp_str;	/* interpolant */
	int interpolant;	/* coded value */
	int verbose;
	int extn;		/* used if multiple input but one output */
	int status;
	int MakeWgt (char *, char *, char *, char *, int, int);

	inlist1 = calloc (IRAF_SZ_LINE+1, sizeof(char));
	inlist2 = calloc (IRAF_SZ_LINE+1, sizeof(char));
	outlist = calloc (IRAF_SZ_LINE+1, sizeof(char));
	wgtname = calloc (STIS_CNAME+1, sizeof(char));
	interp_str = calloc (STIS_CNAME+1, sizeof(char));
	if (inlist1 == NULL || inlist2 == NULL || outlist == NULL ||
		wgtname == NULL || interp_str == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return;
	}

	intable1 = calloc (IRAF_SZ_LINE+1, sizeof(char));
	intable2 = calloc (IRAF_SZ_LINE+1, sizeof(char));
	outtable = calloc (IRAF_SZ_LINE+1, sizeof(char));
	if (intable1 == NULL || intable2 == NULL || outtable == NULL) {
	    fprintf (stderr, "out of memory\n");
	    return;
	}

	/* Get cl parameters. */
	c_clgstr ("intable1", inlist1, IRAF_SZ_LINE);
	c_clgstr ("intable2", inlist2, IRAF_SZ_LINE);
	c_clgstr ("outtable", outlist, IRAF_SZ_LINE);
	c_clgstr ("wgt_name", wgtname, STIS_CNAME);
	c_clgstr ("interpolant", interp_str, STIS_CNAME);
	if (interp_str[0] == 'l') {
	    interpolant = LINEAR_INTERPOLATION;
	} else if (interp_str[0] == 's') {
	    interpolant = SPLINE_INTERPOLATION;
	} else {
	    fprintf (stderr, "Interpolant %s not recognized.\n", interp_str);
	    return;
	}
	verbose = c_clgetb ("verbose");

	/* Open lists, and compare numbers of file names. */
	if (OpenTblLists (inlist1, inlist2, outlist,
			&ilist1, &ilist2, &olist))
	    return;

	extn = 0;			/* may be incremented by GetNextTbl */

	while (GetNextTbl (ilist1, ilist2, olist,
		intable1, intable2, outtable, &extn,
		IRAF_SZ_LINE) != IRAF_EOF) {

	    status = MakeWgt (intable1, intable2, outtable,
			wgtname, interpolant, verbose);
	    if (status) {
		if (c_iraferr()) {
		    fprintf (stderr,
			"IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
		} else if (status == OUT_OF_MEMORY) {
		    fprintf (stderr, "Out of memory\n");
		    return;		/* fatal error */
		} else {
		    fprintf (stderr,
		"Error (%d) computing weights for these tables:\n", status);
		    fprintf (stderr, "  intable1 = %s\n", intable1);
		    fprintf (stderr, "  intable2 = %s\n", intable2);
		    if (outtable[0] == '\0')
			fprintf (stderr, "  outtable = \"\"\n");
		    else
			fprintf (stderr, "  outtable = %s\n", outtable);
		    fflush (stderr);
		}
	    }
	}

	free (outtable);
	free (intable2);
	free (intable1);
	free (interp_str);
	free (wgtname);
	free (outlist);
	free (inlist2);
	free (inlist1);

	return;
}

/* This routine opens the three lists of table names.  The numbers of
   names in each list are compared, and an error message is printed if
   the numbers are inconsistent (the return value will be one in this
   case).  The numbers of names don't all have to be the same.  If the
   number of names in inlist1 is not the same as the number in inlist2,
   there must only be one name in inlist2 (i.e. use that table for all
   input spectra).  If the number of names in outlist is not the same as
   the number in inlist1, there must be no names at all in outlist (i.e.
   input spectra will be modified in-place).
*/

static int OpenTblLists (char *inlist1, char *inlist2, char *outlist,
	IRAFPointer *ilist1, IRAFPointer *ilist2, IRAFPointer *olist) {

/* arguments:
char *inlist1            i: list of names of tables with spectra
char *inlist2            i: list of names of tables with wavelength and flux
char *outlist            i: list of output tables
IRAFPointer *ilist1      o: imt pointer for inlist1
IRAFPointer *ilist2      o: imt pointer for inlist2
IRAFPointer *olist       o: imt pointer for outlist
*/

	int n_in1, n_in2, n_out;	/* number of names in each list */
	int agree;			/* true if numbers of names are OK */

	*ilist1 = c_imtopen (inlist1);
	*ilist2 = c_imtopen (inlist2);
	*olist = c_imtopen (outlist);

	n_in1 = c_imtlen (*ilist1);
	n_in2 = c_imtlen (*ilist2);
	n_out = c_imtlen (*olist);

	agree = 1;			/* initial value */

	/* It's OK to use the same intable2 for all input spectra. */
	if (n_in2 > 1 && n_in1 != n_in2)
	    agree = 0;

	/* It's OK for the output list to be empty, and it's OK to
	   copy multiple input tables into the same output file
	   (if it's a FITS file).
	*/
	if (n_out > 1 && n_in1 != n_out)
	    agree = 0;

	if (!agree) {

	    fprintf (stderr,
	"Lengths of input and output lists are inconsistent:\n");
	    fprintf (stderr, "%d in intable1 list\n", n_in1);
	    fprintf (stderr, "%d in intable2 list\n", n_in2);
	    fprintf (stderr, "%d in outtable list\n", n_out);

	    return (1);
	}

	return (0);
}

/* This routine gets the next name in each of the lists of table names.
   The function value will be zero if we actually got file name(s), and
   it will be IRAF_EOF if there are no more names in the list.
*/

static int GetNextTbl (IRAFPointer ilist1, IRAFPointer ilist2,
	IRAFPointer olist,
	char *intable1, char *intable2, char *outtable, int *extn,
	int maxch) {

/* arguments:
IRAFPointer ilist1       i: imt pointer for intable1
IRAFPointer ilist2       i: imt pointer for intable2
IRAFPointer olist        i: imt pointer for outtable
char *intable1           o: table with spectra
char *intable2           o: table with wavelength and flux
char *outtable           o: output table name
int *extn                io: extension number for output table, or zero
int maxch                i: size of strings for table names
*/

	char *out;	/* for output table name, if extn should be appended */
	int n_in1, n_out;	/* number of names in in1 and output lists */
	int stat;	/* IRAF_EOF if we've reached the end of the list */

	stat = c_imtgetim (ilist1, intable1, maxch);

	if (stat == IRAF_EOF)
	    return (IRAF_EOF);

	if (c_imtlen (ilist2) == 1)
	    c_imtrew (ilist2);
	stat = c_imtgetim (ilist2, intable2, maxch);
	if (stat == IRAF_EOF) {
	    fprintf (stderr,
		"ERROR:  Unexpected end of list of intable2 names.\n");
	    return (IRAF_EOF);
	}

	n_in1 = c_imtlen (ilist1);
	n_out = c_imtlen (olist);

	if (n_out > 0) {

	    if (n_out == 1)
		c_imtrew (olist);
	    stat = c_imtgetim (olist, outtable, maxch);
	    if (stat == IRAF_EOF) {
		fprintf (stderr,
		"ERROR:  Unexpected end of list of outtable names.\n");
		return (IRAF_EOF);
	    }

	    /* Append extension number to outtable, if appropriate. */
	    if (n_in1 > 1 && n_out == 1) {
		*extn += 1;
		if ((out = calloc (IRAF_SZ_LINE+1, sizeof(char))) == NULL) {
		    fprintf (stderr, "GetNextTbl:  out of memory\n");
		    return (IRAF_EOF);
		}
		sprintf (out, "%s[%d]", outtable, *extn);
		strcpy (outtable, out);
		free (out);
	    }

	} else {

	    outtable[0] = '\0';
	    *extn = 0;
	}

	return (0);
}
