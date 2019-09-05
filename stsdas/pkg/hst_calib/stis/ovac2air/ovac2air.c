# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ctype.h>	/* for isspace */

# include <c_iraf.h>		/* for IRAF_SZ_FNAME */
# include <c_iraf_priv.h>	/* for clear_cvoserr */
# include <xclio.h>
# include <xtables.h>

# define MIN_WAVELENGTH (2000.)
# define FITS_BUFSIZE  2880	/* size of a FITS block */

static int fileExists (char *);
static int copyFile (char *, char *);
static int processOneTable (char *, int, int, int *);

/* This task converts wavelengths from vacuum to air.
   Only wavelengths greater than 2000 Angstroms will be modified.

Phil Hodge, 1998 Oct 27:
    Task created.

Phil Hodge, 2002 Jun 10:
    Replace the expression for the index of refraction of air.
    The old one was:
	n = 1.0002735182 + 131.4182 / wl**2 + 2.76249e8 / wl**4
    The new one is:
	sigma = 1.e4 / wl
	n = 1 + 643.28e-7 + 294981.e-7 / (146. - sigma**2) +
	                      2554.e-7 / (41. - sigma**2)
    where wl is the wavelength in Angstroms.
    Use IRAF_SZ_FNAME instead of STIS_FNAME.
*/

# if defined (NATIVE_IRAF)
IRAFTASK(ovac2air) {
# else
int main (int argc, char **argv) {
# endif

	char *infile;		/* input file name */
	char *outfile;		/* output file name */
	int count;
	int done;
	int verbose = 1;
	int inplace;
	int i;

	c_irafinit (argc, argv);

	infile = calloc (IRAF_SZ_FNAME+1, sizeof (char));
	outfile = calloc (IRAF_SZ_FNAME+1, sizeof (char));
	if (infile == NULL || outfile == NULL) {
	    printf ("ERROR:  Out of memory.\n");
# if defined (NATIVE_IRAF)
	    return;
# else
	    exit (1);
# endif
	}

# if defined (NATIVE_IRAF)
	c_clgstr ("input", infile, IRAF_SZ_FNAME);
	c_clgstr ("output", outfile, IRAF_SZ_FNAME);
	inplace = 1;
	for (i = 0;  i < strlen (outfile);  i++) {
	    if (!isspace(outfile[i])) {
		inplace = 0;
		break;
	    }
	}
# else
	if (argc == 2) {
	    strcpy (infile, argv[1]);
	    inplace = 1;
	} else if (argc == 3) {
	    strcpy (infile, argv[1]);
	    strcpy (outfile, argv[2]);
	    inplace = 0;
	} else {
	    printf ("syntax:  xx_ovac2air.e input <output>\n");
	    exit (1);
	}
# endif

	if (inplace) {

	    strcpy (outfile, infile);

	    if (verbose)
		printf ("File %s will be processed in-place ...\n", outfile);

	} else {		/* both input and output specified */

	    /* Make sure the output file does not already exist. */
	    if (fileExists (outfile)) {
		printf ("ERROR:  Output file %s already exists.\n", outfile);
# if defined (NATIVE_IRAF)
		return;
# else
		exit (1);
# endif
	    }

	    /* Copy infile to outfile. */
	    if (copyFile (infile, outfile))
# if defined (NATIVE_IRAF)
		return;
# else
		exit (1);
# endif

	    if (verbose)
		printf ("File %s copied to %s ...\n", infile, outfile);
	}

	/* Process each table in outfile. */
	count = 1;
	done = 0;
	while (!done) {

	    if (processOneTable (outfile, count, verbose, &done))
		break;
	    count++;
	}

	free (outfile);
	free (infile);
# if defined (NATIVE_IRAF)
	return;
# else
	exit (0);
# endif
}

/* This function returns 1 if the file exists and 0 if it does not exist. */

static int fileExists (char *outfile) {

/* argument:
char *outfile    i: name of file
*/

	FILE *ifp;

	if ((ifp = fopen (outfile, "r")) == NULL) {
	    return (0);
	} else {
	    fclose (ifp);
	    return (1);
	}
}

/* This routine copies a file. */

static int copyFile (char *infile, char *outfile) {

/* arguments:
char *infile    i: name of input file
char *outfile   i: name of output file
*/

	FILE *ifp, *ofp;	/* for input and output files */
	void *buf;		/* buffer for copying blocks */
	int nin, nout;		/* number read and written */
	int done;

	if ((buf = calloc (FITS_BUFSIZE, sizeof(char))) == NULL)
	    return (1003);

	if ((ofp = fopen (outfile, "wb")) == NULL) {
	    printf ("ERROR    Can't create temporary file %s.\n", outfile);
	    free (buf);
	    return (1021);
	}

	if ((ifp = fopen (infile, "rb")) == NULL) {
	    printf ("ERROR    Can't open %s.\n", infile);
	    fclose (ofp);
	    remove (outfile);
	    free (buf);
	    return (1005);
	}

	done = 0;
	while (!done) {
	    nin = fread (buf, sizeof(char), FITS_BUFSIZE, ifp);
	    if (ferror (ifp)) {
		printf ("ERROR    Can't read from %s (copying to %s).\n",
				infile, outfile);
		fclose (ofp);
		fclose (ifp);
		free (buf);
		return (1023);
	    }
	    if (feof (ifp))
		done = 1;

	    nout = fwrite (buf, sizeof(char), nin, ofp);
	    if (nout < nin) {
		printf ("ERROR    Can't copy %s to %s.\n", infile, outfile);
		fclose (ofp);
		fclose (ifp);
		free (buf);
		return (1025);
	    }
	}

	fclose (ofp);
	fclose (ifp);
	free (buf);

	return (0);
}

static int processOneTable (char *outfile, int count, int verbose, int *done) {

	char *outtable;		/* output table name */
	IRAFPointer tp;
	IRAFPointer cp;
	int nrows, row;		/* number of rows, loop index */
	int nelem;		/* number of elements in wavelength array */
	double *wl;		/* array of wavelengths, in Angstroms */
	double sigma;		/* wavenumber, in micrometers^-1 */
	double n;		/* index of refraction of air at wl */
	int i;			/* loop index */
	int modified;		/* true if any wavelength actually changed */

	if ((outtable = calloc (IRAF_SZ_FNAME+1, sizeof (char))) == NULL) {
	    printf ("ERROR:  Out of memory.\n");
	    return (1);
	}

	/* If count is one, don't append and extension number to the file
	   name.  This lets us work on stsdas tables as well as FITS tables.
	*/
	if (count == 1)
	    strcpy (outtable, outfile);
	else
	    sprintf (outtable, "%s[%d]", outfile, count);

	tp = c_tbtopn (outtable, IRAF_READ_WRITE, 0);
	if (c_iraferr()) {
	    if (count == 1) {
		printf ("ERROR:  Can't open table %s.\n", outtable);
		return (1);
	    }			/* else assume we've reached the end of file */
	    clear_cvoserr();
	    *done = 1;
	    return (0);
	}

	c_tbcfnd1 (tp, "WAVELENGTH", &cp);
	if (cp == 0) {
	    printf ("ERROR:  Column WAVELENGTH not found in %s\n", outtable);
	    return (1);
	}

	nrows = c_tbpsta (tp, TBL_NROWS);
	nelem = c_tbcigi (cp, TBL_COL_LENDATA);
	if ((wl = calloc (nelem, sizeof (double))) == NULL) {
	    printf ("ERROR:  Out of memory (%d elements).\n", nelem);
	    return (1);
	}

	modified = 0;		/* initial value */
	for (row = 1;  row <= nrows;  row++) {

	    if (c_tbagtd (tp, cp, row, wl, 1, nelem) < nelem) {
		printf ("ERROR:  Couldn't get all %d elements from row %d\n",
			nelem, row);
		return (1);
	    }
	    if (c_iraferr()) {
		printf ("ERROR:  Couldn't get wavelengths from row %d\n", row);
		return (1);
	    }

	    for (i = 0;  i < nelem;  i++) {
		if (wl[i] > MIN_WAVELENGTH) {
		    sigma = 1.e4 / wl[i];
		    n = 1. + 643.28e-7 + 294981.e-7 / (146. - sigma*sigma) +
				2554.e-7 / (41. - sigma*sigma);
		    wl[i] /= n;
		    modified = 1;
		}
	    }

	    c_tbaptd (tp, cp, row, wl, 1, nelem);
	    if (c_iraferr()) {
		printf ("ERROR:  Couldn't put wavelengths back into row %d\n",
			row);
		return (1);
	    }
	}

	if (modified) {
	    c_tbhadt (tp,"HISTORY",
		"Wavelengths converted from vacuum to air.");
	    if (verbose) {
		c_tbtnam (tp, outtable, IRAF_SZ_FNAME);
		printf ("  table %s updated.\n", outtable);
	    }
	} else {
	    printf ("Note:  No change was made to %s;\n", outtable);
	    printf ("       all wavelengths were too short.\n");
	}

	if (c_tbpsta (tp, TBL_WHTYPE) != TBL_TYPE_FITS)
	    *done = 1;

	c_tbtclo (tp);

	free (wl);
	free (outtable);
	return (0);
}
