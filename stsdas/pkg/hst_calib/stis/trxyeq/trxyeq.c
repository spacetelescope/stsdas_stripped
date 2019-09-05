# include <stdio.h>
# include <stdlib.h>		/* calloc */
# include <string.h>

# include <c_iraf.h>		/* for c_irafinit */
# include <xclio.h>
# include <ximio.h>
# include <xmwcs.h>

# include "trxyeq.h"

static FILE *initCoords (char *);
static int getCoords (FILE *, char *, double *, double *);
static void printCoords (double, double, double, double,
		double, double, int, Bool);
static void round60 (int *, int *, double *, int);

/* This is the main module for trxyeq.

   Phil Hodge, 2003 Jan 3:
	Created.

   Phil Hodge, 2003 Aug 4:
	Call geoOffset to initialize the offsets.

   Phil Hodge, 2004 Sept 17:
	Add direction of mapping to getIDC().
*/

IRAFTASK (trxyeq) {

	char input[SZ_FNAME+1];		/* name of input science file */
	char coords[SZ_FNAME+1];	/* coords, or name of coord file */
	char idctab[SZ_FNAME+1];	/* table of distortion coefficients */
	char select[SZ_FNAME+1];	/* column to select row in idctab */
	int decimals;			/* decimal places to print in sec. */
	Bool hms;			/* output in hms/dms format? */
	Bool verbose;			/* print pixel coordinates? */

	FILE *fd;			/* for reading coordinates */
	char ksel_val[SZ_CBUF+1];	/* value of select keyword */
	char note[SZ_CBUF+1];		/* comment, or null */

	double ix, iy;			/* pixel coordinates */
	double ox, oy;			/* undistorted pixel coordinates */
	double ra, dec;			/* (ix,iy) converted to (ra,dec) */
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
	hms = c_clgetb ("hms");
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

	if (getIDC (idctab, select, "FORWARD", ksel_val, &dist) != 0) {
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

	ct = c_mw_sctran (mw, "logical", "world", 3);
	if (checkError())
	    return;

	/* Assign the zero-point offsets. */
	geoOffset (im, &dist, ltm, ltv);

	done = 0;
	if (getCoords (fd, coords, &ix, &iy) < 2)	/* get first point */
	    done = 1;
	while (!done) {

	    geocorr (ct, &dist, ltm, ltv, ix, iy, &ox, &oy, &ra, &dec);
	    if (checkError())
		return;
	    c_clputd ("ra", ra);
	    c_clputd ("dec", dec);

	    if (verbose)
		printCoords (ix, iy, ox, oy, ra, dec, decimals, hms);

	    if (getCoords (fd, coords, &ix, &iy) < 2)	/* get next point */
		done = 1;
	}

	if (fd != NULL)
	    fclose (fd);
	freeDist (&dist);
	c_mw_ctfree (&ct);
	c_mw_close (&mw);
	c_imunmap (im);
}

static void printCoords (double ix, double iy, double ox, double oy,
	double ra, double dec, int decimals, Bool hms) {

/* arguments:
double ix, iy       i: input pixel coordinates
double ox, oy       i: geometrically corrected pixel coordinates
double ra, dec      i: equatorial coordinates
int decimals        i: decimal places to print in seconds portion of dec
Bool hms            i: output in hms/dms format?
*/

	int ra_decimals;	/* decimals to print for seconds of ra */
	double temp;
	int hours, minutes, degrees, arcmin;
	double seconds, arcsec;
	char sign;

	/* format strings for printing ra & dec */
	char ra_fmt[SZ_CBUF+1], dec_fmt[SZ_CBUF+1];

	/* ra & dec formatted for printing */
	char ra_str[SZ_FNAME+1], dec_str[SZ_FNAME+1];

	if (hms) {

	    ra_decimals = decimals + 1;
	    ra /= 15.;			/* convert degrees to hours */

	    hours = (int)ra;
	    temp = (ra - hours) * 60.;
	    minutes = (int)temp;
	    seconds = (temp - minutes) * 60.;
	    round60 (&hours, &minutes, &seconds, ra_decimals);

	    if (dec < 0.) {
		sign = '-';
		dec = -dec;
	    } else {
		sign = '+';
	    }
	    degrees = (int)dec;
	    temp = (dec - degrees) * 60.;
	    arcmin = (int)temp;
	    arcsec = (temp - arcmin) * 60.;
	    round60 (&degrees, &arcmin, &arcsec, decimals);

	    sprintf (ra_fmt, "%%2d:%%02d:%%0%d.%df",
					ra_decimals+3, ra_decimals);
	    sprintf (dec_fmt, "%%c%%02d:%%02d:%%0%d.%df",
					decimals+3, decimals);

	    sprintf (ra_str, ra_fmt, hours, minutes, seconds);
	    sprintf (dec_str, dec_fmt, sign, degrees, arcmin, arcsec);

	} else {

	    sprintf (ra_fmt,  "%%%d.%df", decimals+8, decimals+4);
	    sprintf (dec_fmt, "%%%d.%df", decimals+8, decimals+4);

	    sprintf (ra_str, ra_fmt, ra);
	    sprintf (dec_str, dec_fmt, dec);
	}

	printf ("%9.3f %9.3f  %10.4f %10.4f  %s %s\n",
			ix, iy, ox, oy, ra_str, dec_str);
}

/* This routine checks for 'seconds' being close enough to 60 that it
   would appear as 60 when printed to a precision of 'decimals' decimal
   places.  In that case, 'seconds' will be set to zero and 'minutes'
   will be incremented so they will print as expected.
*/

static void round60 (int *degrees, int *minutes, double *seconds,
		int decimals) {

/* arguments:
int *degrees       io: either hours of time (ra) or degrees of arc (dec)
int *minutes       io: either minutes of time or minutes of arc
double *seconds    io: either seconds of time or seconds of arc
int decimals        i: decimal places to print for seconds
*/

	double roundoff;
	int i;

	roundoff = 0.5;
	for (i = 0;  i < decimals;  i++)
	    roundoff /= 10.;

	if (*seconds + roundoff > 60.) {
	    *seconds = 0.;
	    (*minutes)++;

	    if (*minutes >= 60) {
		*minutes -= 60;
		(*degrees)++;
	    }
	}
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

/* This function reads the next pair of pixel coordinates.

   If 'fd' is a null pointer, the coordinates are assumed to be given
   explicitly in 'coords'.  The coordinates are read from that string,
   and then 'coords' is blanked out to indicate that there are no more
   coordinates to be read.

   If 'fd' is not null, the next pair of pixel coordinates (if any remain)
   are read from the file.  In this case 'coords' is not used.

   The function value will be 2 if the ix and iy pixel coordinates have
   successfully been read from the 'coords' string or from the specified
   file.  A smaller function value indicates that the list of coordinates
   has been exhausted.
*/

static int getCoords (FILE *fd, char *coords, double *ix, double *iy) {

/* arguments:
FILE *fd            i: null or an fd for coords (returned by initCoords)
char *coords       io: a pair of pixel coordinates or the name of a coords file
double *ix, *iy     o: pixel coordinates read from coords string or from fd
*/

	/* Number of items read from string or file (should be 2). */
	int n;

	if (fd == NULL) {
	    n = sscanf (coords, "%lg %lg", ix, iy);
	    coords[0] = '\0';
	} else {
	    n = fscanf (fd, "%lg %lg", ix, iy);
	}

	return (n);
}
