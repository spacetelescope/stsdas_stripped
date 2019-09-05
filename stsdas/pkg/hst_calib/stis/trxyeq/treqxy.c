# include <stdio.h>
# include <stdlib.h>		/* calloc */
# include <string.h>

# include <c_iraf.h>		/* for c_irafinit */
# include <xclio.h>
# include <ximio.h>
# include <xmwcs.h>

# include "trxyeq.h"

static FILE *initCoords (char *);
static int getCoords (FILE *, char *, char *, char *, double *, double *);
static void printCoords (char *ra_str, char *dec_str,
	double ox, double oy, int decimals);

/* This is the main module for treqxy.

   Phil Hodge, 2004 Oct 4:
	Created.
*/

IRAFTASK (treqxy) {

	char input[SZ_FNAME+1];		/* name of input science file */
	char coords[SZ_FNAME+1];	/* coords, or name of coord file */
	char idctab[SZ_FNAME+1];	/* table of distortion coefficients */
	char select[SZ_FNAME+1];	/* column to select row in idctab */
	int decimals;			/* decimal places to print in sec. */
	Bool verbose;			/* print pixel coordinates? */

	FILE *fd;			/* for reading coordinates */
	char ksel_val[SZ_CBUF+1];	/* value of select keyword */
	char note[SZ_CBUF+1];		/* comment, or null */
	char ra_str[SZ_CBUF+1];		/* right ascension as given by user */
	char dec_str[SZ_CBUF+1];	/* declination as given by user */
	double ra_x, dec_x;		/* output from geocorr and ignored */

	double ix, iy;			/* pixel coordinates */
	double ox, oy;			/* undistorted pixel coordinates */
	double ra, dec;			/* (ox,oy) converted to (ra,dec) */
	int done;			/* loop-termination flag */
	DistInfo dist;			/* distortion */
	double ltm[2], ltv[2];		/* linear transformation */
	IRAFPointer im, mw, ct;

	c_irafinit (argc, argv);

	/* Get parameter values. */
	c_clgstr ("input", input, SZ_FNAME);
	c_clgstr ("coords", coords, SZ_FNAME);
	c_clgstr ("idctab", idctab, SZ_FNAME);
	c_clgstr ("select", select, SZ_FNAME);
	decimals = c_clgeti ("decimals");
	verbose = c_clgetb ("verbose");

	trim_blanks (input);
	trim_blanks (idctab);
	trim_blanks (select);

	dist.allocated = 0;

	fd = initCoords (coords);	/* open the coords file */

	if (getPrimaryInfo (input, idctab, select, ksel_val, note) != 0) {
	    if (checkError())
		return;
	}
	if (note[0] != '\0')
	    printf ("# %s\n", note);

	if (getIDC (idctab, select, "INVERSE", ksel_val, &dist) != 0) {
	    checkError();
	    return;
	}

	im = c_immap (input, IRAF_READ_ONLY, 0);
	if (checkError())
	    return;

	getLTInfo (im, ltm, ltv);

	mw = c_mw_openim (im);
	if (checkError())
	    return;

	ct = c_mw_sctran (mw, "world", "logical", 3);
	if (checkError())
	    return;

	/* Assign the zero-point offsets. */
	geoOffset (im, &dist, ltm, ltv);

	done = 0;
	/* get first point */
	if (getCoords (fd, coords, ra_str, dec_str, &ra, &dec) < 2)
	    done = 1;
	while (!done) {

	    /* map the celestial coordinates to undistorted pixel coordinates */
	    c_mw_c2trand (ct, ra, dec, &ix, &iy);
	    if (checkError())
		return;

	    geocorr (ct, &dist, ltm, ltv, ix, iy, &ox, &oy, &ra_x, &dec_x);

	    if (checkError())
		return;
	    c_clputd ("x", ox);
	    c_clputd ("y", oy);

	    if (verbose)
		printCoords (ra_str, dec_str, ox, oy, decimals);

	    /* get next point */
	    if (getCoords (fd, coords, ra_str, dec_str, &ra, &dec) < 2)
		done = 1;
	}

	if (fd != NULL)
	    fclose (fd);
	freeDist (&dist);
	c_mw_ctfree (&ct);
	c_mw_close (&mw);
	c_imunmap (im);
}

static void printCoords (char *ra_str, char *dec_str,
	double ox, double oy, int decimals) {

/* arguments:
char ra_str, dec_str  i: coordinates as specified by the user
double ox, oy         i: geometrically corrected pixel coordinates
int decimals          i: number of decimal places for pixel coords
*/

	printf("%20s %20s  %*.*f %*.*f\n", ra_str, dec_str,
		decimals+5, decimals, ox, decimals+5, decimals, oy);
}

/* If 'coords' is the name of an existing file, this function opens it
   and returns the file pointer.  If 'coords' is an explicit pair of pixel
   coordinates (or a nonexistent file), the open will fail, and a null
   pointer will be returned.  The null pointer case will be checked in
   getCoords().
*/

static FILE *initCoords (char *coords) {

/* argument:
char *coords        i: a pair of pixel coordinates or the name of a coords file
*/

	FILE *fd;

	fd = fopen (coords, "r");

	return (fd);
}

/* This function reads the next pair of equatorial coordinates.

   If 'fd' is a null pointer, the coordinates are assumed to be given
   explicitly in 'coords'.  The coordinates are read from that string,
   and then 'coords' is blanked out to indicate that there are no more
   coordinates to be read.

   If 'fd' is not null, the next pair of coordinates (if any remain)
   is read from the file.  In this case 'coords' is not used.

   The function value will be 2 if the ra and dec equatorial coordinates
   have successfully been read from the 'coords' string or from the
   specified file.  A smaller function value indicates that the list of
   coordinates has been exhausted.
*/

static int getCoords (FILE *fd, char *coords,
	char *ra_str, char *dec_str, double *ra, double *dec) {

/* arguments:
FILE *fd               i: null or an fd for coords (returned by initCoords)
char *coords          io: a pair of coordinates or the name of a coords file
char *ra_str, *dec_str o: equatorial coordinates from coords string or from fd
double *ra, *dec       o: equatorial coordinates, read from ra_str and dec_str
*/

	/* scr is a copy of coords (or a line from the coords file) */
	char scr[SZ_FNAME+1];
	char *s_ra, *s_dec;
	char separators[] = " ,;\t";
	int len;
	double getRA (char **);
	double getDec (char **);

	if (fd == NULL) {
	    strcpy (scr, coords);
	    coords[0] = '\0';
	} else {
	    if (fgets (scr, SZ_FNAME, fd) == NULL)
		return (0);
	}
	len = strlen (scr);
	if (scr[len-1] == '\n')
	    scr[len-1] = '\0';
	if ((s_ra = strtok (scr, separators)) == NULL)
	    return (0);				/* no more input */
	if ((s_dec = strtok (NULL, separators)) == NULL)
	    return (1);		/* shouldn't happen (user error) */

	strcpy (ra_str, s_ra);
	strcpy (dec_str, s_dec);
	*ra = getRA (&s_ra);
	*dec = getDec (&s_dec);

	return (2);		/* two items were read from input */
}
