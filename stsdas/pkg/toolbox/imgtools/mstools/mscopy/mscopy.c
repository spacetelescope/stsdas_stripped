# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <xclio.h>
# include <ximio.h>
# include <xxtools.h>
# include <c_iraf_priv.h>	/* for clear_cvoserr() */

# define MAX_DIMEN 7
# define SZ_KEYWORD 8
# define MAX_RANGES 512		/* max number of items in imsets parameter */

# define MISSING_EXTNAME "MISSING EXTNAME Missing Extname missing extname"

/* The component_name array gives the EXTNAME values for the various
   extensions in an imset.  These are assigned from the 'extnames' task
   parameter.
*/
# define MAX_EXT_PER_IMSET  5
static char component_name[MAX_EXT_PER_IMSET][81];

static void interpretExtnames (char []);
static int MsCopyOne (char *, char *, char *);
static int InterpretInput (char *, char *, char *, char *, int);
static int CreateOutput (char *, char *, int, int *, int *, int *, int *);
static int GetLastExtver (char *, int, int *);
static int OneExtension (char *, char *, char *);
static int AdjustKeywords (IRAFPointer, IRAFPointer, char *, int, int *);
static int InterpretImSection (char *, int, int *, int *, int *, int *);
static int UpdatePriHdr (char *, char *, int);
static void ConstructInputName (char *, char *, int, int);
static void ConstructOutputName (char *, char *, int, int);
static void IrafErrorMessage (char *);

/* This task copies image sets (imsets) from one FITS file to another.
   Dummy extensions are copied, and their header keywords will be updated
   in the output if the input name includes an image section.

   Phil Hodge, 1999 Jul 12:
	Task created.

   Phil Hodge, 1999 Oct 26:
	In CreateOutput, add WFPC2 as a possible value for INSTRUME.

   Phil Hodge, 2002 Apr 25:
	Add extnames as a parameter, and use it (unless it's null) to
	assign values to the component_name arrays.

   Howard Bushouse, 2007 Mar 14:
	Added WFC3 support.
*/

IRAFTASK (mscopy) {

	char inlist[IRAF_SZ_LINE];	/* list of input files */
	char outlist[IRAF_SZ_LINE];	/* list of output files, or directory */
	char imsets[IRAF_SZ_LINE];	/* imset numbers */
	char extnames[IRAF_SZ_LINE];	/* EXTNAME values in an imset */
	Bool verbose;			/* print image names? */

	char input[IRAF_SZ_LINE];	/* an input file name */
	char output[IRAF_SZ_LINE];	/* an output file name */
	IRAFPointer in_t;	/* image template pointer for input list */
	IRAFPointer out_t;	/* image template pointer for output list */
	int dir_only;		/* true if output is a directory name */

	int NameInit (char *, char *, IRAFPointer *, IRAFPointer *, int *);
	int NameGio (IRAFPointer, IRAFPointer, int, char *, char *, int);

	c_irafinit (argc, argv);

	c_clgstr ("input", inlist, IRAF_SZ_LINE);
	c_clgstr ("output", outlist, IRAF_SZ_LINE);
	c_clgstr ("imsets", imsets, IRAF_SZ_LINE);
	c_clgstr ("extnames", extnames, IRAF_SZ_LINE);
	verbose = c_clgetb ("verbose");

	/* Assign values to the component_name (EXTNAME) strings. */
	interpretExtnames (extnames);

	/* Open input and output name templates. */
	if (NameInit (inlist, outlist, &in_t, &out_t, &dir_only))
	    return;

	/* For each file in the list ... */
	while (NameGio (in_t, out_t, dir_only,
			input, output, IRAF_SZ_LINE) != IRAF_EOF) {

	    if (verbose) {
		printf ("%s --> %s\n", input, output);
		fflush (stdout);
	    }
	    if (MsCopyOne (input, output, imsets))	/* copy one file */
		return;
	}

	return;
}

static void interpretExtnames (char extnames[]) {

	int i;		/* character in extnames */
	int j;		/* character in an EXTNAME in component_name */
	int word;	/* counter for EXTNAMEs in component_name */
	int ch;		/* a character from extnames */
	int blank;	/* true if the extnames string is blank or null */

	for (i = 0;  i < MAX_EXT_PER_IMSET;  i++)
	    strcpy (component_name[i], MISSING_EXTNAME);

	blank = (extnames[0] == '\0');
	if (!blank) {
	    for (i = 0;  i < strlen (extnames);  i++) {
		if (extnames[i] != ' ') {
		    blank = 0;
		    break;
		}
	    }
	}

	if (blank) {

	    strcpy (component_name[0], "SCI");
	    strcpy (component_name[1], "ERR");
	    strcpy (component_name[2], "DQ");
	    strcpy (component_name[3], "SAMP");
	    strcpy (component_name[4], "TIME");

	} else {

	    word = 0;
	    j = 0;
	    for (i = 0;  i < strlen (extnames);  i++) {
		ch = extnames[i];
		if (ch == ',' || ch == ' ') {
		    component_name[word][j] = '\0';
		    if (j > 0)
			word++;
		    if (word >= MAX_EXT_PER_IMSET) {
			printf (
		"Warning:  There is a limit of %d extensions per imset;",
				MAX_EXT_PER_IMSET);
			printf ("the remaining names will be ignored.\n");
			word--;
			break;
		    }
		    j = 0;
		} else {
		    component_name[word][j] = ch;
		    j++;
		}
	    }
	    component_name[word][j] = '\0';
	}
}

/* This routine copies one file from the user's input list.
   If the output file does not exist, the input primary header will be
   copied to output before copying the data.  The NEXTEND keyword will be
   updated in the output primary header, and a history record will be added.
*/

static int MsCopyOne (char *input, char *output, char *imsets) {

/* arguments:
char *input      i: name of input image
char *output     i: name of output image
char *imsets     i: image sets to copy
*/

	char *inimage;		/* local copy of input name, modified */
	char *outimage;		/* local copy of output name, modified */
	char *infile;		/* input file name */
	char *imsection;	/* image section */
	int *ranges;		/* imset range info */
	int nvalues;		/* returned by c_decode_ranges */
	int stat;		/* returned by c_get_next_number */
	int out_nextend;	/* number of extensions in output file */
	int ext_copied;		/* counter for extensions copied */
	int done;
	int i;
	int iextver, oextver;	/* counters for EXTVER values (imset numbers) */
	int in_total_imsets;	/* total number of imsets in input file */
	int out_num_imsets;	/* initial number of imsets in output file */
	int ext_per_imset;	/* number of extensions per imset (3 or 5) */

	inimage = malloc (IRAF_SZ_LINE * sizeof (char));
	outimage = malloc (IRAF_SZ_LINE * sizeof (char));
	infile = malloc (IRAF_SZ_LINE * sizeof (char));
	imsection = malloc (IRAF_SZ_LINE * sizeof (char));
	ranges = malloc (MAX_RANGES * 3 * sizeof (int));
	if (inimage == NULL || outimage == NULL ||
	    infile == NULL || imsection == NULL ||
	    ranges == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	/* Interpret the imset range info. */
	if (c_decode_ranges (imsets, ranges, MAX_RANGES, &nvalues)) {
	    IrafErrorMessage ("decoding imsets string");
	    return (1);
	}

	if (InterpretInput (input, output, infile, imsection, IRAF_SZ_LINE))
	    return (1);

	if (CreateOutput (infile, output,
		nvalues,
		&ext_per_imset, &in_total_imsets,
		&out_num_imsets, &out_nextend))
	    return (1);

	iextver = 0;			/* initial values */
	oextver = out_num_imsets;
	ext_copied = 0;
	done = 0;

	/* while not done copying imsets ... */
	while (!done) {

	    stat = c_get_next_number (ranges, &iextver);

	    if (stat == IRAF_EOF || iextver > in_total_imsets) {
		done = 1;

	    } else {

		oextver++;
		for (i = 0;  i < ext_per_imset;  i++) {
		    ext_copied++;
		    ConstructInputName (infile, inimage, i, iextver);
		    ConstructOutputName (output, outimage, i, oextver);
		    if (OneExtension (inimage, outimage, imsection))
			return (1);
		}
	    }
	}

	if (UpdatePriHdr (input, output, out_nextend + ext_copied))
	    return (1);

	free (inimage);
	free (outimage);
	free (infile);
	free (imsection);
	free (ranges);

	return (0);
}

/* This routine extracts the file name (including directory) and
   image section from the input name.  The input name is checked
   to be sure that no FITS extension (e.g. [sci,1]) was specified.

   The input file name and the output will be compared; it is an error
   if they are the same.
*/

static int InterpretInput (char *input, char *output,
	char *infile, char *imsection, int maxch) {

/* arguments:
char *input      i: name of input image (directory, file name, image section)
char *output     i: name of output image
char *infile     o: name of input file, including directory
char *imsection  o: image section, if any was specified by the user
int maxch        i: size of infile and imsection
*/

	char *imagename;	/* input with image section stripped off */
	int len_infile;		/* length of infile string */

	if ((imagename = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	/* Break the input name into several pieces, and figure out
	   what the user has specified.
	*/

	c_imgcluster (input, infile, maxch);
	c_imgsection (input, imsection, maxch);

	/* sanity check */
	if (strcmp (infile, output) == 0) {
	    fprintf (stderr,
		"ERROR Input and output files cannot be the same;\n");
	    fprintf (stderr, "  input = %s\n", input);
	    return (1);
	}

	/* Remove the image section.  What's left should just be the
	   file name, i.e. no FITS extension should have been specified.
	*/
	c_imgimage (input, imagename, IRAF_SZ_LINE);

	len_infile = strlen (infile);
	if (strlen (imagename) > len_infile) {
	    fprintf (stderr,
	"ERROR FITS extension was specified:  %s\n", &imagename[len_infile]);
	    fprintf (stderr,
	"  Use the 'imsets' parameter to specify what to copy,\n");
	    fprintf (stderr,
	"  or use the 'imcopy' task for an individual extension.\n");
	    return (1);
	}

	free (imagename);
	return (0);
}

/* This routine opens the primary header of the input to get the keywords
   INSTRUME and NEXTEND.  If the output file does not already exist, it
   will be created using the input primary header as a template.  If the
   output file does already exist, the NEXTEND keyword will be gotten
   (it is required) from the primary header, and image extensions will be
   opened, starting with the last, until an EXTVER keyword with value
   greater than zero has been found.
*/

static int CreateOutput (char *infile, char *output,
		int nvalues,
		int *ext_per_imset, int *in_total_imsets,
		int *out_num_imsets, int *out_nextend) {

/* arguments:
char *infile         i: name of input file
char *output         i: name of output image
int nvalues          i: number of imsets specified (but could be MAX_INT)
int *ext_per_imset   o: number of extensions per imset (e.g. 3)
int *in_total_imsets o: total number of imsets in input file
int *out_num_imsets  o: number of imsets in output file (0 or last EXTVER)
int *out_nextend     o: number of extensions in output file (0 or NEXTEND)
*/

	char *scratch;		/* local copy of input or output */
	IRAFPointer iim, oim;	/* for input and output primary headers */
	int naxis;		/* dimension of input primary header */
	int in_nextend;		/* total number of extensions in input file */
	int i;

	if ((scratch = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}
	strcpy (scratch, infile);
	strcat (scratch, "[0]");

	iim = c_immap (scratch, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    IrafErrorMessage (
		"opening input primary header");
	    return (1);
	}

	naxis = c_imgndim (iim);

	if (naxis != 0) {
	    fprintf (stderr, "ERROR Input primary data unit is not null;\n");
	    fprintf (stderr, "  input = %s\n", infile);
	    fprintf (stderr, "  Use imcopy instead of mscopy.\n");
	    return (1);
	}

	/* Set the number of extensions per imset, based on the instrument. */
	c_imgstr (iim, "INSTRUME", scratch, IRAF_SZ_LINE);
	if (c_iraferr()) {
	    clear_cvoserr();
	    scratch[0] = '\0';
	}
	if (strcmp (scratch, "ACS") == 0) {
	    *ext_per_imset = 3;
	} else if (strcmp (scratch, "NICMOS") == 0) {
	    *ext_per_imset = 5;
	} else if (strcmp (scratch, "STIS") == 0) {
	    *ext_per_imset = 3;
	} else if (strcmp (scratch, "WFPC2") == 0) {
	    *ext_per_imset = 1;
	} else if (strcmp (scratch, "WFC3") == 0) {
	    c_imgstr (iim, "DETECTOR", scratch, IRAF_SZ_LINE);
	    if (strcmp (scratch, "UVIS") == 0)
		*ext_per_imset = 3;
	    else if (strcmp (scratch, "IR") == 0)
		*ext_per_imset = 5;
	} else {
	    fprintf (stderr,
	"Warning INSTRUME = `%s' in %s is not recognized;\n", scratch, infile);
	    fprintf (stderr,
	"  the number of extensions per imset will be assumed to be three.\n");
	    *ext_per_imset = 3;
	}
	for (i = 0;  i < *ext_per_imset;  i++) {
	    if (strcmp (component_name[i], MISSING_EXTNAME) == 0) {
		fprintf (stderr,
		"ERROR Too few EXTNAMEs were listed in 'extnames';\n");
		fprintf (stderr, "%d should have been specified.\n",
			*ext_per_imset);
		return (1);
	    }
	}

	in_nextend = c_imgeti (iim, "NEXTEND");
	if (c_iraferr()) {
	    clear_cvoserr();
	    fprintf (stderr, "Warning NEXTEND not found in %s;\n", infile);
	    fprintf (stderr, "  NEXTEND will be assumed to be %d.\n",
		*ext_per_imset);
	}

	*in_total_imsets = in_nextend / *ext_per_imset;
	if (*in_total_imsets * *ext_per_imset < in_nextend &&
		nvalues >= *in_total_imsets) {
	    fprintf (stderr, "Warning In %s,\n", infile);
	    fprintf (stderr,
	"  it appears that some extensions are not in imsets;\n");
	    fprintf (stderr,
	"  these extensions will not be copied.\n");
	}

	/* Try to open the primary header of the output file,
	   to see if the file exists.
	*/
	c_imgcluster (output, scratch, IRAF_SZ_LINE);
	strcat (scratch, "[0]");
	oim = c_immap (scratch, IRAF_READ_ONLY, 0);

	if (c_iraferr()) {

	    /* The output doesn't already exist; OK, create it. */
	    clear_cvoserr();
	    oim = c_immap (output, IRAF_NEW_COPY, iim);
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to create output");
		return (1);
	    }
	    c_imaddi (oim, "NEXTEND", 0);	/* initial value */
	    *out_num_imsets = 0;
	    *out_nextend = 0;

	} else {

	    /* No error, so the output file already exists. */
	    *out_nextend = c_imgeti (oim, "NEXTEND");
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to get NEXTEND from output");
		return (1);
	    }
	    if (GetLastExtver (output, *out_nextend, out_num_imsets))
		return (1);
	}

	c_imunmap (oim);
	c_imunmap (iim);

	free (scratch);
	return (0);
}

/* It is implicitly assumed that the EXTVER values in the extensions
   are monotonically increasing.  We get the EXTVER from the last image
   extension that has that keyword (as long as the value is greater than
   zero), and out_num_imsets is set to that value of EXTVER.
*/

static int GetLastExtver (char *output, int out_nextend, int *out_num_imsets) {

/* arguments:
char *output         i: name of output image
int out_nextend      i: number of extensions in output file
int *out_num_imsets  o: number of imsets in output file (0 or last EXTVER)
*/

	char *scratch;		/* local copy of input or output */
	IRAFPointer im;		/* for output image extension */
	int n;			/* extension number */
	int extver;		/* value of extver keyword */
	int done;

	if ((scratch = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	n = out_nextend;		/* decremented in loop */
	extver = 0;
	done = (n <= 0);
	while (!done) {

	    sprintf (scratch, "%s[%d]", output, n);

	    im = c_immap (scratch, IRAF_READ_ONLY, 0);

	    if (c_iraferr()) {
		/* Not an image; ignore it. */
		clear_cvoserr();
	    } else {
		/* Image was opened successfully. */
		extver = c_imgeti (im, "EXTVER");
		if (c_iraferr()) {
		    clear_cvoserr();
		    extver = 0;
		}
		c_imunmap (im);
	    }
	    if (extver > 0) {
		done = 1;
	    } else {
		n--;
		if (n <= 0)
		    done = 1;
	    }
	}

	*out_num_imsets = extver;		/* can be zero */

	free (scratch);
	return (0);
}

/* This routine copies one FITS IMAGE extension from input to output.
   If it's a dummy extension (i.e. no data portion), the header will be
   copied, and the coordinate parameter and size (NPIX1 & NPIX2) keywords
   will be updated to reflect any image section that was specified with
   the input file name.  If it's not a dummy extension, it will be
   opened new-copy (and the image kernel takes care of image sections),
   and the data will be copied.
*/

static int OneExtension (char *inimage, char *outimage, char *imsection) {

/* arguments:
char *inimage    i: name of input image (includes explicit extension name)
char *outimage   i: name of output image (includes [append=yes])
char *imsection  i: image section, if any
*/

	char *scratch;		/* local copy of inimage */
	IRAFPointer iim, oim;	/* for input and output primary headers */
	int naxis;		/* dimension of input image */
	int naxis1;		/* length of first axis */
	int datatype;		/* pixel type */
	int ndim;		/* dimension of dummy input image */
	int axlen[MAX_DIMEN];	/* length of each dummy axis */
	int i;
	int iv[MAX_DIMEN], ov[MAX_DIMEN];	/* line pointers */
	/* for copying data of various types: */
	double *ixd, *oxd;
	float *ixr, *oxr;
	int *ixi, *oxi;
	short *ixs, *oxs;

	scratch = malloc (IRAF_SZ_LINE * sizeof (char));
	if (scratch == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	/* Open the input to get keywords. */
	iim = c_immap (inimage, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    IrafErrorMessage ("trying to open an input extension");
	    return (1);
	}

	naxis = c_imgndim (iim);

	if (naxis == 0) {

	    /* Dummy extension; just update header keywords. */
	    oim = c_immap (outimage, IRAF_NEW_COPY, iim);
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to open output header");
		return (1);
	    }
	    ndim = 2;
	    axlen[0] = c_imgeti (iim, "NPIX1");
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to get NPIX1 from dummy extension");
		return (1);
	    }
	    axlen[1] = c_imgeti (iim, "NPIX2");
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to get NPIX2 from dummy extension");
		return (1);
	    }
	    if (AdjustKeywords (iim, oim, imsection, ndim, axlen))
		return (1);

	} else {

	    /* Not a dummy extension; copy the data. */

	    c_imunmap (iim);	/* inimage was opened without image section */

	    /* Append the image section to the input name, and open again. */
	    strcpy (scratch, inimage);
	    strcat (scratch, imsection);
	    iim = c_immap (scratch, IRAF_READ_ONLY, 0);
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to open input extension");
		return (1);
	    }

	    oim = c_immap (outimage, IRAF_NEW_COPY, iim);
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to open output extension");
		return (1);
	    }
	    datatype = c_imgtypepix (iim);
	    naxis1 = c_imglen (iim, 1);

	    /* Copy the data. */
	    for (i = 0;  i < MAX_DIMEN;  i++) {
		iv[i] = 1;
		ov[i] = 1;
	    }
	    if (datatype == IRAF_SHORT) {
		while (c_imgnls (iim, &ixs, iv) != IRAF_EOF) {
		    c_impnls (oim, &oxs, ov);
		    memcpy (oxs, ixs, naxis1*sizeof(short));
		}
	    } else if (datatype == IRAF_INT) {
		while (c_imgnli (iim, &ixi, iv) != IRAF_EOF) {
		    c_impnli (oim, &oxi, ov);
		    memcpy (oxi, ixi, naxis1*sizeof(int));
		}
	    } else if (datatype == IRAF_USHORT) {	/* treat as short */
		while (c_imgnls (iim, &ixs, iv) != IRAF_EOF) {
		    c_impnls (oim, &oxs, ov);
		    memcpy (oxs, ixs, naxis1*sizeof(short));
		}
	    } else if (datatype == IRAF_REAL) {
		while (c_imgnlr (iim, &ixr, iv) != IRAF_EOF) {
		    c_impnlr (oim, &oxr, ov);
		    memcpy (oxr, ixr, naxis1*sizeof(float));
		}
	    } else {			/* use buffers of type double */
		while (c_imgnld (iim, &ixd, iv) != IRAF_EOF) {
		    c_impnld (oim, &oxd, ov);
		    memcpy (oxd, ixd, naxis1*sizeof(double));
		}
	    }
	}

	free (scratch);
	c_imunmap (iim);
	c_imunmap (oim);
	return (0);
}

/* This routine reads keywords from the input image header, modifies them
   depending on the image section, and writes the modified values to the
   output image header.  The keywords that are modified are NPIXi,
   CRPIXi, CDi_j, LTVi, and LTMi_j, where i and j are image axis numbers,
   which run from 1 to 2 in HST data that are currently in multi-extension
   format.
*/

static int AdjustKeywords (IRAFPointer iim, IRAFPointer oim,
	char *imsection, int ndim, int axlen[]) {

/* arguments:
IRAFPointer iim  i: imio pointer for input image
IRAFPointer oim  i: imio pointer for output image
char *imsection  i: image section, if any
int ndim         i: dimension of image
int axlen[]      i: length of each axis
*/

	char keyword[SZ_KEYWORD+1];
	int istart[MAX_DIMEN], istop[MAX_DIMEN], istep[MAX_DIMEN];
	double crpix, cd[MAX_DIMEN], ltv, ltm[MAX_DIMEN];
	int npix;
	int i, j;
	int axis;	/* zero-indexed, for start, stop, step */

	/* Extract istart, istop, istep from imsection. */
	if (InterpretImSection (imsection, ndim, axlen, istart, istop, istep))
	    return (1);

	/* for each (dummy) axis ... */
	for (i = 1;  i <= ndim;  i++) {
	    axis = i - 1;

	    /* Get the current values of keywords. */

	    sprintf (keyword, "CRPIX%d", i);
	    crpix = c_imgetd (iim, keyword);
	    if (c_iraferr()) {
		clear_cvoserr();
		crpix = 0.;
	    }

	    sprintf (keyword, "LTV%d", i);
	    ltv = c_imgetd (iim, keyword);
	    if (c_iraferr()) {
		clear_cvoserr();
		ltv = 0.;
	    }

	    for (j = 1;  j <= ndim;  j++) {

		sprintf (keyword, "CD%d_%d", j, i);
		cd[j] = c_imgetd (iim, keyword);
		if (c_iraferr()) {
		    clear_cvoserr();
		    if (i == j)
			cd[j] = 1.;
		    else
			cd[j] = 0.;
		}

		sprintf (keyword, "LTM%d_%d", j, i);
		ltm[j] = c_imgetd (iim, keyword);
		if (c_iraferr()) {
		    clear_cvoserr();
		    if (i == j)
			ltm[j] = 1.;
		    else
			ltm[j] = 0.;
		}
	    }

	    /* Modify the keyword values. */

	    npix = ((istop[axis] - istart[axis]) / istep[axis] + 1);

	    crpix = (crpix - istart[axis]) / istep[axis] + 1.;
	    ltv = (ltv - istart[axis]) / istep[axis] + 1.;

	    for (j = 1;  j <= ndim;  j++) {
		cd[j]  *= (double)istep[axis];
		ltm[j] /= (double)istep[axis];
	    }

	    /* Put the new values back into the header. */

	    sprintf (keyword, "NPIX%d", i);
	    c_imaddi (oim, keyword, npix);

	    sprintf (keyword, "CRPIX%d", i);
	    c_imaddd (oim, keyword, crpix);

	    sprintf (keyword, "LTV%d", i);
	    c_imaddd (oim, keyword, ltv);

	    for (j = 1;  j <= ndim;  j++) {
		sprintf (keyword, "CD%d_%d", j, i);
		c_imaddd (oim, keyword, cd[j]);
		sprintf (keyword, "LTM%d_%d", j, i);
		c_imaddd (oim, keyword, ltm[j]);
	    }
	    if (c_iraferr()) {
		IrafErrorMessage ("trying to update output extension header");
		return (1);
	    }
	}

	return (0);
}

/* This routine reads the image section string (if any) that was specified
   by the user as part of the input file name.  The string is interpreted
   to get the (one-indexed) starting pixel, ending pixel, and step size
   for each image axis.
*/

static int InterpretImSection (char *imsection, int ndim, int axlen[],
		int istart[], int istop[], int istep[]) {

/* arguments:
char *imsection  i: image section, if any
int ndim         i: dimension of image
int axlen[]      i: length of each axis
int istart[]     o: first pixel (one indexed) of image section, for each axis
int istop[]      o: last pixel of image section, for each axis
int istep[]      o: pixel increment of image section, for each axis
*/

	char *s, *p;		/* pointers into imsection */
	int axis;		/* zero-indexed axis number */
	long i, j, k;		/* start, stop, step */
	int done;

	/* Assign default values. */
	for (axis = 0;  axis < ndim;  axis++) {
	    istart[axis] = 1;
	    istop[axis] = axlen[axis];
	    istep[axis] = 1;
	}

	if (imsection[0] == '\0')
	    return (0);

	s = imsection;

	if (s[0] != '[') {
	    fprintf (stderr, "section must begin with [\n");
	    return (1);
	}

	/* increment past the [ */
	s++;

	axis = -1;
	done = 0;
	while (!done) {

	    if (s[0] == ']' || s[0] == '\0') {

		done = 1;

	    } else {

		axis++;
		if (axis + 1 > ndim) {
		    fprintf (stderr, "too many dimensions in image section\n");
		    return (1);
		}

		if (s[0] == '*') {
		    s++;			/* take the default */
		    if (s[0] == ',') {
			s++;
		    } else if (s[0] == ':') {
			s++;
			k = strtol (s, &p, 10);
			if (k <= 0) {
			    fprintf (stderr, "step size is missing\n");
			    return (1);
			} else if (1 + k > axlen[axis]) {
			    fprintf (stderr, "step size is too large\n");
			    return (1);
			} else {
			    istep[axis] = k;
			}
			s = p;
			if (s[0] != '\0') s++;
		    }
		    continue;
		}

		i = strtol (s, &p, 10);
		if (i <= 0) {
		    fprintf (stderr, "starting index is missing\n");
		    return (1);
		} else if (i > axlen[axis]) {
		    fprintf (stderr, "starting index is too large\n");
		    return (1);
		} else {
		    istart[axis] = i;
		}
		s = p;
		if (s[0] != '\0') s++;

		if (p[0] == ':') {

		    j = strtol (s, &p, 10);
		    if (j <= 0) {
			fprintf (stderr, "stop index is missing\n");
			return (1);
		    } else if (j > axlen[axis]) {
			fprintf (stderr, "stop index is too large\n");
			return (1);
		    } else {
			istop[axis] = j;
		    }
		    s = p;
		    if (s[0] != '\0') s++;

		    if (p[0] == ':') {

			k = strtol (s, &p, 10);
			if (k <= 0) {
			    fprintf (stderr, "step size is missing\n");
			    return (1);
			} else if (istart[axis] + k > istop[axis]) {
			    fprintf (stderr, "step size is too large\n");
			    return (1);
			} else {
			    istep[axis] = k;
			}
			s = p;
			if (s[0] != '\0') s++;

		    } else if (p[0] != ',' && p[0] != ']') {
			fprintf (stderr, "expected to find ',' or ']'\n");
			return (1);
		    }

		} else if (p[0] == ',' || p[0] == ']') {
		    istop[axis] = i;
		} else {
		    fprintf (stderr, "expected to find ',' or ']'\n");
		    return (1);
		}
	    }
	}

	if (axis + 1 != ndim) {
	    fprintf (stderr, "not enough dimensions in image section\n");
	    return (1);
	}

	return (0);
}

/* This routine opens the output primary header read-write, updates the
   NEXTEND keyword to account for the imsets that were written to the
   file, and it adds a HISTORY record giving the name of the input file.
*/

static int UpdatePriHdr (char *input, char *output, int nextend) {

/* arguments:
char *input      i: name of input image
char *output     i: name of output image
int nextend      i: value to which NEXTEND should be set in output
*/

	char *scratch;		/* local copy of output; history record */
	IRAFPointer im;

	if ((scratch = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	/* Extract file name, and append [0]. */
	c_imgcluster (output, scratch, IRAF_SZ_LINE);
	strcat (scratch, "[0]");

	im = c_immap (scratch, IRAF_READ_WRITE, 0);
	if (c_iraferr()) {
	    IrafErrorMessage (
		"opening output primary header to update keywords");
	    return (1);
	}

	c_imaddi (im, "NEXTEND", nextend);
	if (c_iraferr()) {
	    IrafErrorMessage ("trying to update NEXTEND");
	    return (1);
	}

	sprintf (scratch, "Copied from %s", input);
	c_imputh (im, "HISTORY", scratch);
	if (c_iraferr()) {
	    IrafErrorMessage ("adding HISTORY record(s)");
	    return (1);
	}

	c_imunmap (im);

	free (scratch);
	return (0);
}

static void ConstructInputName (char *infile, char *inimage,
		int i, int iextver) {

	sprintf (inimage, "%s[%s,%d,inherit=no]",
			infile, component_name[i], iextver);
}

static void ConstructOutputName (char *outfile, char *outimage,
		int i, int oextver) {

	sprintf (outimage, "%s[%s,%d,append=yes]",
			outfile, component_name[i], oextver);
}

static void IrafErrorMessage (char *message) {

	if (message[0] != '\0')
	    printf ("ERROR %s\n", message);

	fprintf (stderr, "IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
	fflush (stderr);
}
