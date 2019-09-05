# include <stdio.h>
# include <math.h>
# include <string.h>

# include <c_iraf.h>		/* for c_irafinit */
# include <xclio.h>
# include <ximio.h>
# include <xmwcs.h>

# include "trxyeq.h"

static int getRaDec (char *, double *, double *);

# define TINY_PIXEL  (1.e-3)	/* a small fraction of a pixel */
# define MAX_ITER    50

/* ucrpix updates crpix1 & crpix2 based on the pixel and celestial
   coordinates at one point.

   Phil Hodge, 2003 August 5:
	Created.

   Phil Hodge, 2004 Sept 17:
	Add direction of mapping to getIDC().
*/

IRAFTASK (ucrpix) {

	char input[SZ_FNAME+1];		/* name of input image */
	char pixels[SZ_FNAME+1];	/* pixel coordinates */
	char celestial[SZ_FNAME+1];	/* celestial coordinates */
	char idctab[SZ_FNAME+1];	/* table of distortion coefficients */
	char select[SZ_FNAME+1];	/* column to select row in idctab */
	Bool verbose;			/* print pixel coordinates? */
	Bool add_history;		/* add a history record? */

	char ksel_val[SZ_CBUF+1];	/* value of select keyword */
	char note[SZ_CBUF+1];		/* comment, history, or null */

	double ix, iy;			/* pixel coordinates */
	double ra, dec;			/* celestial coordinates */
	double ox, oy;			/* undistorted pixel coordinates */
	double ox0, oy0;		/* undistorted pixel coordinates */
	double ra_x, dec_x;		/* output of geocorr, ignored */
	double crpix1, crpix2;		/* reference pixel */
	double corr1, corr2;		/* adjustments to crpix1 & crpix2 */
	double sumcorr1, sumcorr2;	/* total adjustments to crpix1 & 2 */
	DistInfo dist;			/* distortion */
	double ltm[2], ltv[2];		/* linear transformation */
	IRAFPointer im, mw, ct;
	int done;			/* loop-termination flag */
	int niter;			/* iteration counter */
	int n;

	c_irafinit (argc, argv);

	c_clgstr ("input", input, SZ_FNAME);
	c_clgstr ("pixels", pixels, SZ_FNAME);
	c_clgstr ("celestial", celestial, SZ_FNAME);
	c_clgstr ("idctab", idctab, SZ_FNAME);
	c_clgstr ("select", select, SZ_FNAME);
	verbose = c_clgetb ("verbose");
	add_history = c_clgetb ("history");

	trim_blanks (input);
	trim_blanks (idctab);
	trim_blanks (select);

	/* read pixel coordinates from input string */
	n = sscanf (pixels, "%lg %lg", &ix, &iy);
	if (n < 2) {
	    printf ("ERROR:  can't interpret 'pixels':  `%s'\n", pixels);
	    printf ("A pair of pixel coordinates was expected.\n");
	    return;
	}
	/* read ra & dec from input string */
	n = getRaDec (celestial, &ra, &dec);
	if (n < 2) {
	    printf ("ERROR:  can't interpret 'celestial':  `%s'\n", celestial);
	    return;
	}

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

	/* open image read/write */
	im = c_immap (input, IRAF_READ_WRITE, 0);
	if (checkError())
	    return;

	getLTInfo (im, ltm, ltv);

	done = 0;
	niter = 0;
	sumcorr1 = 0.;
	sumcorr2 = 0.;
	while (!done) {
	    /* set up coordinate transformation */
	    mw = c_mw_openim (im);
	    if (checkError())
		return;
	    ct = c_mw_sctran (mw, "world", "logical", 3);
	    if (checkError())
		return;

	    /* map the celestial coordinates to undistorted pixel coordinates */
	    c_mw_c2trand (ct, ra, dec, &ox0, &oy0);
	    if (checkError())
		return;

	    /* assign the zero-point offsets */
	    geoOffset (im, &dist, ltm, ltv);

	    /* apply distortion correction to pixel coordinates
	       (ignore the output ra and dec)
	    */
	    geocorr (ct, &dist, ltm, ltv, ix, iy, &ox, &oy, &ra_x, &dec_x);

	    /* corrections to crpix1 and crpix2
	       (ox0,oy0) is the undistorted location of the point based on the
	       coordinate parameters, while (ox,oy) is the actual undistorted
	       location.
	    */
	    corr1 = ox - ox0;
	    corr2 = oy - oy0;

	    c_mw_ctfree (&ct);
	    c_mw_close (&mw);

	    /* update the reference pixel */
	    crpix1 = c_imgetd (im, "crpix1");
	    crpix2 = c_imgetd (im, "crpix2");
	    crpix1 += corr1;
	    crpix2 += corr2;
	    sumcorr1 += corr1;
	    sumcorr2 += corr2;
	    c_imputd (im, "crpix1", crpix1);
	    c_imputd (im, "crpix2", crpix2);
	    c_imflush (im);

	    if (fabs (corr1) < TINY_PIXEL && fabs (corr2) < TINY_PIXEL)
		done = 1;

	    niter++;
	    if (niter >= MAX_ITER && !done) {
		printf (
		"Warning:  maximum number of iterations (%d) exceeded\n",
			MAX_ITER);
		done = 1;
	    }
	}

	if (verbose) {
	    printf ("(%.3f,%.3f) added to (crpix1,crpix2)\n",
		sumcorr1, sumcorr2);
	}
	if (add_history) {
	    sprintf (note, "(%.3f,%.3f) added to (crpix1,crpix2)\n",
		sumcorr1, sumcorr2);
	    c_imputh (im, "history", note);
	}

	freeDist (&dist);
	c_imunmap (im);
}

/* Read ra and dec (in degrees) from the string celestial.  The function
   value should be 2; it will be less than 2 if ra or dec was not readable.
*/

static int getRaDec (char *celestial, double *ra, double *dec) {

	char *cp = celestial;
	int n;

	*ra = -9999;
	*dec = -9999;

	*ra = getRA (&cp);
	*dec = getDec (&cp);

	n = 0;
	if (*ra > -9990.)
	    n++;
	if (*dec > -9990.)
	    n++;

	return (n);
}
