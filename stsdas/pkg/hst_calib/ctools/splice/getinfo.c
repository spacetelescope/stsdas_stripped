# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ctype.h>

# include <c_iraf.h>
# include <ximio.h>
# include <xtables.h>
# include "pweight.h"

# define GOT_PHOTTAB     0
# define GOT_PCTAB       1
# define GOT_APERTAB     2
# define GOT_APERTURE    3
# define GOT_OPT_ELEM    4
# define GOT_CENWAVE     5
# define GOT_HELCORR     6
# define GOT_RA_TARG     7
# define GOT_DEC_TARG    8
# define GOT_EXPSTART    9
# define GOT_EXPEND     10
# define GOT_EXPTIME    11
# define GOT_WAVELENGTH 12
# define GOT_SPORDER    13
# define GOT_EXTRSIZE   14
# define SZ_GOT         15

/* For a reference file name, this string means that a name was
   intentionally not given.
*/
# define NOT_APPLICABLE   "n/a"

/* This file contains GetInfo and some of the functions that it calls.
   Header keywords are read, and throughput arrays from reference tables
   are read into memory.

   Phil Hodge, 2000 Feb 2:
	Initial version.
*/

static int GetPrimaryInfo (char *, WgtInfo *, int []);
static int GetExtensionInfo (char *, WgtInfo *, int []);
static int DefaultOrError (char *, WgtInfo *, int []);
static int ReadRefTab (WgtInfo *, PhotInfo *, int []);
static int streq_ic (char *, char *);

int GetInfo (char *intable, WgtInfo *sts, PhotInfo *phot) {

/* arguments:
char *intable    i: name of current input table
WgtInfo *sts     o: mostly keyword info
PhotInfo *phot   o: arrays of wavelengths and throughputs from ref tables
*/

	/* array of flags to indicate which keywords were found in the header */
	int ktest[SZ_GOT];
	int i;			/* loop index */
	int status;
	void HelioFactor (WgtInfo *);

	for (i = 0;  i < SZ_GOT;  i++)
	    ktest[i] = 0;

	/* Get info that is normally found in the primary HDU. */
	if (status = GetPrimaryInfo (intable, sts, ktest))
	    return (status);

	/* Get info from the table header.  We also could get some or
	   all of the primary header keywords if they weren't gotten by
	   GetPrimaryInfo (e.g. if there is no primary HDU).
	*/
	if (status = GetExtensionInfo (intable, sts, ktest))
	    return (status);

	/* Check that we have all required info, and set defaults for
	   missing but not required info.
	*/
	if (status = DefaultOrError (intable, sts, ktest))
	    return (status);

	/* Compute the heliocentric correction factor for the wavelengths. */
	if (sts->helcorr == COMPLETE)
	    HelioFactor (sts);

	/* Read info from the reference tables into memory. */
	if (status = ReadRefTab (sts, phot, ktest))
	    return (status);

	return (0);
}

static int GetPrimaryInfo (char *intable, WgtInfo *sts, int ktest[]) {

	char *pri_hdr;		/* input file name, with [0] */
	char *buf;		/* scratch space */
	int hdu;		/* returned by c_tbparse and ignored */
	int junk;
	IRAFPointer im;		/* for primary header */

	pri_hdr = malloc (IRAF_SZ_LINE * sizeof(char));
	buf = malloc (IRAF_SZ_LINE * sizeof(char));
	if (pri_hdr == NULL || buf == NULL)
	    return (OUT_OF_MEMORY);

	/* Get the file name, and append [0] to get the primary header.
	   Open it and get keywords.
	   (buf is ignored here)
	*/
	junk = c_tbparse (intable, pri_hdr, buf, IRAF_SZ_LINE, &hdu);

	strcat (pri_hdr, "[0]");
	im = c_immap (pri_hdr, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    clear_cvoserr();
	} else {
	    if (c_imaccf (im, "PHOTTAB") == 1) {	/* IRAF YES */
		c_imgstr (im, "PHOTTAB", sts->phottab, STIS_FNAME);
		ktest[GOT_PHOTTAB] = 1;
	    }
	    if (c_imaccf (im, "PCTAB") == 1) {
		c_imgstr (im, "PCTAB", sts->pctab, STIS_FNAME);
		ktest[GOT_PCTAB] = 1;
	    }
	    if (c_imaccf (im, "APERTAB") == 1) {
		c_imgstr (im, "APERTAB", sts->apertab, STIS_FNAME);
		ktest[GOT_APERTAB] = 1;
	    }
	    if (c_imaccf (im, "APERTURE") == 1) {
		c_imgstr (im, "APERTURE", sts->aperture, STIS_CBUF);
		ktest[GOT_APERTURE] = 1;
	    }
	    if (c_imaccf (im, "OPT_ELEM") == 1) {
		c_imgstr (im, "OPT_ELEM", sts->opt_elem, STIS_CBUF);
		ktest[GOT_OPT_ELEM] = 1;
	    }
	    if (c_imaccf (im, "CENWAVE") == 1) {
		sts->cenwave = c_imgeti (im, "CENWAVE");
		ktest[GOT_CENWAVE] = 1;
	    }
	    /* helcorr was previously initialized to OMIT. */
	    if (c_imaccf (im, "HELCORR") == 1) {
		c_imgstr (im, "HELCORR", buf, STIS_CBUF);
		ktest[GOT_HELCORR] = 1;
		if (streq_ic (buf, "complete")) {
		    sts->helcorr = COMPLETE;
		}
	    }

	    if (sts->helcorr == COMPLETE) {
		if (c_imaccf (im, "RA_TARG") == 1) {
		    sts->ra_targ = c_imgetd (im, "RA_TARG");
		    ktest[GOT_RA_TARG] = 1;
		}
		if (c_imaccf (im, "DEC_TARG") == 1) {
		    sts->dec_targ = c_imgetd (im, "DEC_TARG");
		    ktest[GOT_DEC_TARG] = 1;
		}
	    }

	    c_imunmap (im);
	}

	free (buf);
	free (pri_hdr);

	return (0);
}

static int GetExtensionInfo (char *intable, WgtInfo *sts, int ktest[]) {

	IRAFPointer tp;		/* for the table */
	IRAFPointer cp_wl;	/* the wavelength column */
	IRAFPointer cp_sporder;	/* column giving the spectral order number */
	IRAFPointer cp_h;	/* the column giving the extraction height */
	double expstart, expend;	/* exposure start & stop times */
	int status;

	tp = c_tbtopn (intable, IRAF_READ_ONLY, 0);
	if (status = c_iraferr())
	    return (status);

	sts->nrows = c_tbpsta (tp, TBL_NROWS);
	if (sts->nrows < 1) {
	    fprintf (stderr, "No rows in table %s\n", intable);
	    return (1);
	}

	c_tbcfnd1 (tp, "WAVELENGTH", &cp_wl);
	if (cp_wl != 0)
	    ktest[GOT_WAVELENGTH] = 1;

	/* If the SPORDER column is defined, get an initial value from the
	   first row.  If there's no such column, get it from the header.
	*/
	c_tbcfnd1 (tp, "SPORDER", &cp_sporder);
	if (cp_sporder != 0) {
	    c_tbegti (tp, cp_sporder, 1, &sts->sporder);
	    if (status = c_iraferr())
		return (status);
	    ktest[GOT_SPORDER] = 1;
	} else {
	    sts->sporder = c_tbhgti (tp, "SPORDER");
	    if (c_iraferr())
		clear_cvoserr();	/* presume keyword was not found */
	    else
		ktest[GOT_SPORDER] = 1;
	}

	/* If the EXTRSIZE column is defined, get the value from the first
	   row.  If there's no such column, get it from the header.
	*/
	c_tbcfnd1 (tp, "EXTRSIZE", &cp_h);
	if (cp_h != 0) {
	    c_tbegti (tp, cp_h, 1, &sts->extrsize);
	    if (status = c_iraferr())
		return (status);
	    ktest[GOT_EXTRSIZE] = 1;
	} else {
	    sts->extrsize = c_tbhgti (tp, "EXTRSIZE");
	    if (c_iraferr())
		clear_cvoserr();	/* presume keyword was not found */
	    else
		ktest[GOT_EXTRSIZE] = 1;
	}

	sts->exptime = c_tbhgtd (tp, "EXPTIME");
	if (c_iraferr())
	    clear_cvoserr();
	else
	    ktest[GOT_EXPTIME] = 1;

	/* Now go through the list of keywords (some required) and see
	   whether we already have a value.  If not, try to get it from
	   the table header.
	*/
	if (!ktest[GOT_PHOTTAB]) {
	    c_tbhgtt (tp, "PHOTTAB", sts->phottab, STIS_FNAME);
	    if (c_iraferr())
		clear_cvoserr();	/* fatal error, but not yet */
	    else
		ktest[GOT_PHOTTAB] = 1;
	}

	if (!ktest[GOT_PCTAB]) {
	    c_tbhgtt (tp, "PCTAB", sts->pctab, STIS_FNAME);
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_PCTAB] = 1;
	}

	if (!ktest[GOT_APERTAB]) {
	    c_tbhgtt (tp, "APERTAB", sts->apertab, STIS_FNAME);
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_APERTAB] = 1;
	}

	if (!ktest[GOT_APERTURE]) {
	    c_tbhgtt (tp, "APERTURE", sts->aperture, STIS_CBUF);
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_APERTURE] = 1;
	}

	if (!ktest[GOT_OPT_ELEM]) {
	    c_tbhgtt (tp, "OPT_ELEM", sts->opt_elem, STIS_CBUF);
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_OPT_ELEM] = 1;
	}

	if (!ktest[GOT_CENWAVE]) {
	    sts->cenwave = c_tbhgti (tp, "CENWAVE");
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_CENWAVE] = 1;
	}

	if (!ktest[GOT_HELCORR]) {
	    char buf[STIS_CBUF+1];
	    c_tbhgtt (tp, "HELCORR", buf, STIS_CBUF);
	    if (c_iraferr()) {
		clear_cvoserr();
	    } else {
		ktest[GOT_HELCORR] = 1;
		if (streq_ic (buf, "complete")) {
		    sts->helcorr = COMPLETE;
		}
	    }
	}

	if (sts->helcorr == COMPLETE) {

	    if (!ktest[GOT_RA_TARG]) {
		sts->ra_targ = c_tbhgtd (tp, "RA_TARG");
		if (c_iraferr())
		    clear_cvoserr();
		else
		    ktest[GOT_RA_TARG] = 1;
	    }

	    if (!ktest[GOT_DEC_TARG]) {
		sts->dec_targ = c_tbhgtd (tp, "DEC_TARG");
		if (c_iraferr())
		    clear_cvoserr();
		else
		    ktest[GOT_DEC_TARG] = 1;
	    }

	    expstart = c_tbhgtd (tp, "EXPSTART");
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_EXPSTART] = 1;
	    expend = c_tbhgtd (tp, "EXPEND");
	    if (c_iraferr())
		clear_cvoserr();
	    else
		ktest[GOT_EXPEND] = 1;
	    if (ktest[GOT_EXPSTART] && ktest[GOT_EXPEND])
		sts->time_of_exp = (expstart + expend) / 2.;
	}

	c_tbtclo (tp);

	return (0);
}

static int DefaultOrError (char *intable, WgtInfo *sts, int ktest[]) {

	int fatal_error;
	int warning_error;
	int helcorr_warning;

	fatal_error = 0;		/* initial values */
	warning_error = 0;
	helcorr_warning = 0;

	/* Check whether we really do have these three table names. */

	if (ktest[GOT_PHOTTAB]) {
	    if (sts->phottab[0] == '\0' ||
		streq_ic (sts->phottab, NOT_APPLICABLE)) {
		ktest[GOT_PHOTTAB] = 0;
	    }
	}
	if (ktest[GOT_PCTAB]) {
	    if (sts->pctab[0] == '\0' ||
		streq_ic (sts->pctab, NOT_APPLICABLE)) {
		ktest[GOT_PCTAB] = 0;
	    }
	}
	if (ktest[GOT_APERTAB]) {
	    if (sts->apertab[0] == '\0' ||
		streq_ic (sts->apertab, NOT_APPLICABLE)) {
		ktest[GOT_APERTAB] = 0;
	    }
	}

	if (!ktest[GOT_WAVELENGTH]) {
	    fprintf (stderr,
		"Required WAVELENGTH column not found ...\n");
	    fatal_error = 1;
	}

	if (!ktest[GOT_SPORDER]) {
	    fprintf (stderr,
		"SPORDER not found, either as a column or a keyword ...\n");
	    fatal_error = 1;
	}

	if (!ktest[GOT_PHOTTAB]) {
	    fprintf (stderr,
		"PHOTTAB keyword not found or not specified ...\n");
	    fatal_error = 1;
	}

	if (!ktest[GOT_PCTAB]) {
	    fprintf (stderr,
		"Warning:  PCTAB keyword not found or not specified ...\n");
	    warning_error = 1;
	}

	if (!ktest[GOT_APERTAB] || !ktest[GOT_APERTURE]) {

	    if (!ktest[GOT_APERTAB]) {
		fprintf (stderr,
		"Warning:  APERTAB keyword not found or not specified ...\n");
		warning_error = 1;
	    }
	    if (!ktest[GOT_APERTURE]) {
		fprintf (stderr, "Warning:  APERTURE keyword not found ...\n");
		warning_error = 1;
	    }
	    sts->apertab[0] = '\0';
	    sts->aperture[0] = '\0';
	}

	if (!ktest[GOT_OPT_ELEM]) {
	    fprintf (stderr, "OPT_ELEM keyword not found ...\n");
	    fatal_error = 1;
	}

	if (!ktest[GOT_CENWAVE]) {
	    fprintf (stderr, "CENWAVE keyword not found ...\n");
	    fatal_error = 1;
	}

	if (!ktest[GOT_HELCORR]) {
	    fprintf (stderr, "Warning:  HELCORR keyword not found;\n");
	    helcorr_warning = 1;
	}

	if (ktest[GOT_EXPTIME]) {
	    if (sts->exptime <= 0.) {
		fprintf (stderr, "EXPTIME = %.6g is invalid ...\n",
		    sts->exptime);
		fatal_error = 1;
	    }
	} else {
	    fprintf (stderr, "EXPTIME keyword not found ...\n");
	    fatal_error = 1;
	}

	if (sts->helcorr == COMPLETE) {

	    if (!ktest[GOT_RA_TARG]) {
		fprintf (stderr, "Warning:  RA_TARG keyword not found;\n");
		helcorr_warning = 1;
	    }
	    if (!ktest[GOT_DEC_TARG]) {
		fprintf (stderr, "Warning:  DEC_TARG keyword not found;\n");
		helcorr_warning = 1;
	    }
	    if (!ktest[GOT_EXPSTART]) {
		fprintf (stderr, "Warning:  EXPSTART keyword not found;\n");
		helcorr_warning = 1;
	    }
	    if (!ktest[GOT_EXPEND]) {
		fprintf (stderr, "Warning:  EXPEND keyword not found;\n");
		helcorr_warning = 1;
	    }
	}

	if (helcorr_warning) {
	    fprintf (stderr,
	"  heliocentric correction will not be applied to wavelengths ...\n");
	    sts->helcorr = OMIT;
	    warning_error = 1;
	}

	if (!ktest[GOT_EXTRSIZE]) {
	    if (ktest[GOT_PCTAB]) {
		fprintf (stderr,
	"Warning:  EXTRSIZE not found, either as a column or a keyword;\n");
		fprintf (stderr,
	"  the default value will be used ...\n");
		warning_error = 1;
	    }
	    sts->extrsize = UNKNOWN;
	}

	if (fatal_error || warning_error)
	    fprintf (stderr, "  ... in table %s\n", intable);

	if (fatal_error)
	    return (1);

	return (0);
}

/* This reads in the info from the PCTAB and the APERTAB.  The PHOTTAB
   will be read later, in the loop over spectral order number.
*/

static int ReadRefTab (WgtInfo *sts, PhotInfo *phot, int ktest[]) {

	int status;
	int GetPCT (WgtInfo *, PhotInfo *);
	int GetApThr (WgtInfo *, PhotInfo *);

	/* Get the ratio of PCT correction factors. */
	if (ktest[GOT_PCTAB]) {
	    if (status = GetPCT (sts, phot))
		return (status);
	} else {
	    phot->pct_nelem = 0;
	}

	/* Get the aperture throughput. */
	if (ktest[GOT_APERTAB]) {
	    if (status = GetApThr (sts, phot))
		return (status);
	} else {
	    phot->f_nelem = 0;
	}

	return (0);
}

/* This function compares two strings without regard to case, returning
   one if the strings are equal.
*/

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
