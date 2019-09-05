# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <c_iraf.h>
# include <ximio.h>
# include <xmwcs.h>

# include "trxyeq.h"

static void geoEval (DistInfo *, double, double, double *, double *);
static void applyLT (double, double, double [], double [],
			double *, double *);
static void inverseLT (double, double, double [], double [],
			double *, double *);

/* This routine corrects for geometric distortion of an image.
   Note that this is only used for OBSTYPE = IMAGING.

   The mapping from distorted to undistorted coordinates is done as follows:

	convert from distorted image pixels to reference pixels
	subtract the reference point
	    (dist->xref, dist->yref = XREF, YREF from IDC table)
	apply the mapping (DIRECTION="FORWARD", CXij, CYij)
	add the reference point
	convert from reference pixels to undistorted image pixels
	convert from pixel coordinates to equatorial coordinates

   Phil Hodge, 2003 Jan 3:
	Copy from calstis7/geocorr7.c and rewrite.

   Phil Hodge, 2003 Aug 4:
	Add offset to dist, so crpix1 and crpix2 will be unchanged by
	the distortion.  New function geoOffset to find this offset and
	save it in dist.

   Phil Hodge, 2004 Sept 17:
	Include direction in dist, and allow for mapping either from
	distorted to undistorted (dist->direction = 1) or from undistorted
	to distorted (dist->direction = 0).
*/

void geocorr (IRAFPointer ct, DistInfo *dist, double ltm[], double ltv[],
		double ix, double iy,
		double *ox, double *oy, double *ra, double *dec) {

/* arguments:
IRAFPointer ct      i: coordinate transformation
DistInfo *dist      i: distortion info
double ltm, ltv     i: linear transformation
double ix, iy       i: input (distorted) pixel location
double *ox, *oy     o: output (distortion corrected) pixel location
double *ra, *dec    o: right ascension and declination corresponding to (ix,iy)
*/

	double ix_r, iy_r;	/* distorted coords in ref coords */
	double ox_r, oy_r;	/* undistorted coords in ref coords */

	/* Convert to reference coordinates. */
	inverseLT (ix, iy, ltm, ltv, &ix_r, &iy_r);

	/* Apply the distortion correction. */
	geoEval (dist, ix_r, iy_r, &ox_r, &oy_r);

	/* Convert from reference coords to undistorted pixel coords. */
	applyLT (ox_r, oy_r, ltm, ltv, ox, oy);

	/* Convert the undistorted location to ra & dec. */
	c_mw_c2trand (ct, *ox, *oy, ra, dec);
}

/* Before calling geocorr(), this function should be called once to update
   the values of dist->offset[0] and dist->offset[1].  These offsets are
   in image pixels; this is a constant offset added by geoEval so that the
   distortion will be zero at (crpix1,crpix2).
*/

void geoOffset (IRAFPointer im, DistInfo *dist, double ltm[], double ltv[]) {

/* arguments:
IRAFPointer im      i: imio pointer
DistInfo *dist      io: distortion info (offset is modified in-place)
double ltm, ltv     i: linear transformation
double offset[]     o: pixel offset so distortion will be zero at crpix[12]
*/

	double crpix[2];	/* reference pixel */
	double ix_r, iy_r;	/* ref pixel in reference coordinates */
	double ox_r, oy_r;	/* undistorted ref pixel in ref coords */
	double ocrpix[2];	/* ref pixel mapped to undistorted coords */

	dist->offset[0] = 0.;	/* initial values */
	dist->offset[1] = 0.;

	crpix[0] = c_imgetd (im, "crpix1");
	crpix[1] = c_imgetd (im, "crpix2");

	/* Convert to the reference coordinate system. */
	inverseLT (crpix[0], crpix[1], ltm, ltv, &ix_r, &iy_r);

	/* Apply the distortion correction. */
	geoEval (dist, ix_r, iy_r, &ox_r, &oy_r);

	/* Convert from reference coords to undistorted pixel coords. */
	applyLT (ox_r, oy_r, ltm, ltv, &ocrpix[0], &ocrpix[1]);

	dist->offset[0] = crpix[0] - ocrpix[0];
	dist->offset[1] = crpix[1] - ocrpix[1];
	if (!dist->forward) {
	    dist->offset[0] *= -1.;
	    dist->offset[1] *= -1.;
	}
}


/* This routine evaluates the polynomial coefficients at one point in
   the distorted output array to give x and y in the undistorted input
   array (or vice-versa, depending on the direction of the mapping).
   Both input and output are reference pixel coordinates, not necessarily
   image pixels.

   If dist->forward = 1, ix & iy are distorted pixel coordinates and
   ox & oy are undistorted pixel coordinates.  If dist->forward = 0,
   these are reversed.

   Note that an offset is added so that the reference pixel (CRPIX1,CRPIX2)
   gets mapped to the same pixel position.  The reference point, (xref,yref),
   is not necessarily the same as the reference pixel, and the offset
   accounts for the difference in distortion between those two points.

   The same reference point (column names XREF & YREF) is used for both
   forward and inverse mapping, rather than using CXREF & CYREF for the
   reference point in undistorted pixels.  This is because we're not
   actually creating an undistorted image (which would in general be larger
   than the original, distorted image); both the distorted and undistorted
   pixel coordinates refer to the input image.
*/

static void geoEval (DistInfo *dist,
		double ix, double iy, double *ox, double *oy) {

/* arguments:
DistInfo *dist    i: distortion coefficients
double ix, iy     i: pixel coordinates (reference pixels)
double *ox, *oy   o: pixel coordinates (reference pixels)
*/

	double x, y;			/* from ix & iy */
	double xpow[MAX_ORDER+1], ypow[MAX_ORDER+1];
	int i, j, k;

	/* Subtract the reference point. */
	x = ix - dist->xref;
	y = iy - dist->yref;

	if ( ! dist->forward) {
	    /* Shift so the distortion will be zero at the reference pixel,
	       and scale from pixels to arcseconds.
	    */
	    x = (x - dist->offset[0]) * dist->scale;
	    y = (y - dist->offset[1]) * dist->scale;
	}

	/* set up arrays of powers of x and y */
	xpow[0] = 1.;
	ypow[0] = 1.;
	for (i = 1;  i <= dist->norder;  i++) {
	    xpow[i] = xpow[i-1] * x;
	    ypow[i] = ypow[i-1] * y;
	}

	/* initial values;
	   this assumes xcoeff[0] and ycoeff[0] are always zero,
	   but we're going to add those coefficients anyway.
	*/
	*ox = 0.;
	*oy = 0.;

	for (i = 0;  i <= dist->norder;  i++) {

	    for (j = 0;  j <= i;  j++) {

		k = WHICH_COEFF (i, j);

		*ox += dist->xcoeff[k] * xpow[j] * ypow[i-j];
		*oy += dist->ycoeff[k] * xpow[j] * ypow[i-j];
	    }
	}
	if (dist->forward) {
	    /* Scale to pixels, and shift so the distortion will be zero
	       at the reference pixel.
	    */
	    *ox = *ox / dist->scale + dist->offset[0];
	    *oy = *oy / dist->scale + dist->offset[1];
	}

	/* Add the reference point. */
	*ox += dist->xref;
	*oy += dist->yref;
}

/* This routine transforms reference pixel coordinates to image pixel
   coordinates.  The values of ix_r and iy_r input to this routine
   are in the reference coordinate system (unbinned CCD or low-res MAMA,
   and full detector); the values of ix and iy output from this routine
   are pixel coordinates in the current input image.
*/

static void applyLT (double x_r, double y_r,
		double ltm[], double ltv[],
		double *x, double *y) {

/* arguments:
double x_r, y_r    i: pixel coordinates in reference pixel units
double ltm[2]      i: linear transformation, diagonal of matrix part
double ltv[2]      i: linear transformation, vector part
double *x, *y      o: image pixel coordinates
*/

	*x = x_r * ltm[0] + ltv[0];
	*y = y_r * ltm[1] + ltv[1];
}

/* This is the inverse of ApplyLT; it transforms the image coordinates to
   reference pixel coordinates.
*/

static void inverseLT (double x, double y,
		double ltm[], double ltv[],
		double *x_r, double *y_r) {

/* arguments:
double x, y        i: image pixel coordinates
double ltm[2]      i: linear transformation, diagonal of matrix part
double ltv[2]      i: linear transformation, vector part
double *x_r, *y_r  o: pixel coordinates in reference pixel units
*/

	*x_r = (x - ltv[0]) / ltm[0];
	*y_r = (y - ltv[1]) / ltm[1];
}
