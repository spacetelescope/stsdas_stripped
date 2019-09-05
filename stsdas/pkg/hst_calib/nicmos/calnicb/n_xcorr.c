# include <math.h>
# include <float.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define MAX_ITER	4
# define DEF_XCWIN	4

# define DEBUG		0

/* N_XCORR: Cross-correlate two NICMOS images. An input guess of
** the x and y offsets is used and updated with the computed offsets
** at the conclusion of the routine. The second of the two input images
** is used as the reference image, so that the computed offsets refer
** to the offset of the first image relative to the second. The offsets
** are in the sense of the direction that image 1 is shifted on the
** sky relative to image 2. Therefore an object that appears at pixel
** coordinates x, y in image 1, will appear at coordinates x+xoff,
** y+yoff in image 2. The cross-correlation is first computed to the
** nearest integral pixel, and then computed to successively smaller
** fractional pixel shifts.
**
** Revision history:
** H.Bushouse	December 1996	Written for Build 2
** H.Bushouse	27-Feb-1997	Modified n_xcpix to elminate the use of the
**				makearray and freearry functions, which are
**				generating compiler errors (Version 2.0)
** H.Bushouse	03-Jul-1997	Fixed bug in n_getMinError routine that was
**				allowing the min_xpos and min_ypos values to
**				occasionally go uninitialized to valid coords
**				(Version 2.1.1)
** H.Bushouse	18-Jul-1997	Modified n_xcpix to check size of diff values
**				before attempting to square them and put them
**				into the error matrix; modified n_getMinError to
**				normalize the error matrix by its mean value to
**				help avoid overflows when squaring its values to
**				calculate sigma (Version 2.1.2)
** H.Bushouse	07-Jul-1997	Modified n_xcorr to call n_median2d instead of
**				n_imagemean; moved n_imagemean to new n_imstat
**				file (Version 2.2)
** H.Bushouse	01-Dec-1997	Added AsnInfo to n_xcorr argument list; added
**				use of asn.xcwin user input value; changed
**				default	value of nshift from 2 to 3 for
**				subpixel calls to n_xcpix (Version 2.2)
** H.Bushouse	19-Mar-1998	Eliminated use of n_median2d function, as well
**				as use of median,sigma, and THRESHOLD in
**				n_xcpix; n_xcpix now just uses pixels flagged
**				as SOURCE, rather than finding values above a
**				signal threshold. Added use of new n_pixSource
**				function in n_xcpix (Version 2.2)
*/

int n_xcorr (AsnInfo *asn, SingleNicmosGroup *im1, SingleNicmosGroup *im2, 
	      float *xoff, float *yoff) {

/* Arguments:
**	asn	 i: association info structure
**	im1	 i: image 1
**	im2	 i: image 2 (reference image)
**	xoff	io: offset in x-axis
**	yoff	io: offset in y-axis
*/

	/* Local variables */
	int nshift;		/* number of shift locations */
	float szshift;		/* size of each shift */

	/* Function definitions */
	int n_xcpix (SingleNicmosGroup *, SingleNicmosGroup *, int, float,
		     float *, float *);

	/* Xcorr to the nearest whole pixel */
	if (asn->xcwin == 0)
	    nshift = DEF_XCWIN;
	else
	    nshift = asn->xcwin + 1;
	szshift = 1.0;
	if (n_xcpix (im1, im2, nshift, szshift, xoff, yoff))
	    return (status = 0);

	/* Now do Xcorr for fractional pixel shifts */
	nshift = 3;
	szshift = 0.3;
	if (n_xcpix (im1, im2, nshift, szshift, xoff, yoff))
	    return (status = 0);

	nshift = 3;
	szshift = 0.15;
	if (n_xcpix (im1, im2, nshift, szshift, xoff, yoff))
	    return (status = 0);

	/* Successful return */
	return (status = 0);
}

/* N_XCPIX: Cross-correlate two images. This routine uses the method of
** least differences to compute the correlation. */

int n_xcpix (SingleNicmosGroup *im1, SingleNicmosGroup *im2, int nshift,
	     float szshift, float *xoff, float *yoff) {

/* Arguments:
**	im1	 i: image 1
**	im2	 i: image 2 (reference)
**	nshift	 i: number of shift locations
**	szshift	 i: size of shifts
**	xoff	io: offset in x-axis
**	yoff	io: offset in y-axis
*/

	/* Local variables */
	int i, j, ii, jj;		/* loop and pixel indexes */
	int err_sz;			/* size of xcorr error array */
	int npix;			/* number of pix used in xcorr */
	int niter;			/* iteration index */
	float nsigma;			/* sigma threshold */
	float i2, j2;			/* pixel coords */
	float intrp_value;		/* interpolated image value */
	float diff;			/* image1-image2 difference */
	FloatTwoDArray error = IFloatData;	/* xcorr error array */
	ShortTwoDArray nerror = IShortData;	/* num samples in error array */
	float xcen, ycen;		/* x,y correlation positions */
	float limit = sqrt(FLT_MAX);	/* limit for squaring a number */

	/* Function definitions */
	Bool n_pixSource (SingleNicmosGroup *, int, int);
	int  n_bilin_sci (SingleNicmosGroup *, float, float, float *);
	void n_getMinError (FloatTwoDArray *, ShortTwoDArray *, int, float,
			    float *, float *);

	/* Allocate memory for the error matrix */
	err_sz = 2*nshift + 1;
	initFloatData (&error);
	initShortData (&nerror);
	allocFloatData (&error,  err_sz, err_sz);
	allocShortData (&nerror, err_sz, err_sz);

	/* Top of interation loop */
	niter = 0;
	_loop:
	   niter++;

	/* Initialize */
	npix = 0;

	/* Calculate the error matrix for offsets from -NSHIFT to +NSHIFT */
	for (jj = 0; jj < err_sz; jj++) {
	for (ii = 0; ii < err_sz; ii++) {

	     Pix(error,ii,jj)  = 0;
	     Pix(nerror,ii,jj) = 0;

	     /* Loop over image 1 */
	     for (j = 0; j < im1->sci.data.ny; j++) {
	     for (i = 0; i < im1->sci.data.nx; i++) {

		  /* Only use pixels that are flagged as containing a source */
		  if (n_pixSource (im1,i,j)) {

		      /* Compute the pixel coords in image 2 */
		      i2 = i + *xoff + szshift * (nshift - ii);
		      j2 = j + *yoff + szshift * (nshift - jj);

		      /* Make sure the coords lie within image 2 */
		      if (i2 >= 0 && i2 < im2->sci.data.nx-1 &&
			  j2 >= 0 && j2 < im2->sci.data.ny-1) {

			  /* Compute the image 2 value at these
			  ** coordinates using bilinear interpolation */
			  n_bilin_sci (im2, i2, j2, &intrp_value);

			  /* If we got a useable interpolated value,
			  ** compute the difference relative to image 1
			  ** and add it to the error matrix. */
			  if (intrp_value != BADVAL) {
			      diff = Pix(im1->sci.data,i,j) - intrp_value;
			      if (fabs(diff) < limit) {
				  Pix(error,ii,jj)  += diff*diff;
				  Pix(nerror,ii,jj) += 1;
				  npix++;
			      }
			  }
		      }
		  }
	     }
	     }
	}
	}

	if (DEBUG) {
	    printf (" starting xoff = %g, yoff = %g\n", *xoff, *yoff);
	    printf (" npix in xcorr = %d\n", npix/err_sz/err_sz);
	}

	/* Check for a (nearly) empty error matrix */
	if (npix/err_sz/err_sz <= 3) {
	    sprintf (MsgText,
	    "  Too few pixels in xcorr matrix; will use initial offsets");
	    n_message (MsgText);
	    freeFloatData (&error);
	    freeShortData (&nerror);
	    return (status = 1);
	}

	/* Find the location of the minimum in the error array */
	if (szshift >= 1.0)
	    nsigma = 2.5;
	else
	    nsigma = 1.0;
	n_getMinError (&error, &nerror, err_sz, nsigma, &xcen, &ycen);

	/* Update the offsets */
	*xoff = *xoff + szshift * (nshift - xcen);
	*yoff = *yoff + szshift * (nshift - ycen);

	if (DEBUG)
	printf (" xcen=%g, ycen=%g, xoff=%g, yoff=%g\n",
		 xcen, ycen, *xoff, *yoff);

	/* Iterate if the registration has changed
	** by more than half of a shift matrix cell. */
	if (niter < MAX_ITER &&
	    (fabs(xcen-nshift) > 0.5 || fabs(ycen-nshift) > 0.5))
	    goto _loop;

	/* Free the error array */
	freeFloatData (&error);
	freeShortData (&nerror);

	/* Successful return */
	return (status = 0);

}

/* N_GETMINERROR: Find the location of the minimum in the xcorr error array. */

void n_getMinError (FloatTwoDArray *error, ShortTwoDArray *nerror, int npix,
		    float nsigma, float *x, float *y) {

/* Arguments:
**	error	io: xcorr error array
**	nerror	 i: number of samples in error array
**	npix	 i: size of error array
**	nsigma	 i: sigma threshold for accepting correlation min
**	x	 o: x index of min correlation
**	y	 o: y index of min correlation
*/

	/* Local variables */
	int i, j;			/* loop indexes */
	int min_xpos, min_ypos;		/* x,y location of min error */
	float min_val;			/* min val in error array */
	float mean_val;			/* mean val in error array */
	float sigma_val;		/* sigma in error array */

	/* Normalize each error matrix element by number of samples */
	for (j=0; j<npix; j++) {
	     for (i=0; i<npix; i++) {
		  if (PPix(nerror,i,j) > 0)
		      PPix(error,i,j) = PPix(error,i,j) /
					(float)PPix(nerror,i,j);
	     }
	}

	/* Compute the mean and min of the error array */
	mean_val = 0;
	min_val  = PPix(error,0,0);
	for (j = 0; j < npix; j++) {
	     for (i = 0; i < npix; i++) {
		  mean_val += PPix(error,i,j);
		  if (PPix(error,i,j) <= min_val) {
		      min_val = PPix(error,i,j);
		      min_xpos = i;
		      min_ypos = j;
		  }
	     }
	}
	mean_val /= npix*npix;

	/* Normalize the error array to the mean */
	for (j = 0; j < npix; j++) {
	     for (i = 0; i < npix; i++)
		  PPix(error,i,j) /= mean_val;
	}
	min_val /= mean_val;
	mean_val /= mean_val;

	if (DEBUG) {
	for (j=0;j<npix;j++) {
	     printf (" error[*,%d]=",j);
	     for (i=0; i<npix; i++)
		  printf (" %g",PPix(error,i,j));
	     printf ("\n");
	}}

	/* Compute the sigma of the error array */
	sigma_val = 0;
	for (j = 0; j < npix; j++) {
	     for (i = 0; i < npix; i++) {
		  sigma_val += (PPix(error,i,j)-mean_val) *
			       (PPix(error,i,j)-mean_val);
	     }
	}
	sigma_val = sqrt (sigma_val / (float)(npix*npix-1));

	/* If the location of the min value is on an edge,
	** or not statistically significant,
	** then just return the center of the error matrix. */
	if (min_xpos == 0 || min_xpos == npix-1 ||
	    min_ypos == 0 || min_ypos == npix-1 ||
	    min_val >= mean_val-nsigma*sigma_val) {
	    min_xpos = (int)((float)npix/2.);
	    min_ypos = (int)((float)npix/2.);
	}
	*x = min_xpos;
	*y = min_ypos;

	if (DEBUG)
	printf (" mean=%g, sigma=%g, thresh=%g, min=%g\n",mean_val,sigma_val,
		 mean_val-nsigma*sigma_val,min_val);

}

