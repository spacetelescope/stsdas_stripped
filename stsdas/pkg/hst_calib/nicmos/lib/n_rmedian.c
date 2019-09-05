# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <math.h>
# include <xvops.h>
# include "nicmos.h"

# define PI 3.14159265358979323846

/*   N_RMEDIAN  --  Ring median filter a NICMOS image.
**
**      Based on "images$imfilter/rmedian" task in IRAF.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	06-May-1999	Implementation
**
*/

static void rmed_ell_gauss (float, float, float, float *, float *, float *,
			    float *, int *, int *);
static int  rmed_mkring (short *, int, int, float, float, float, float, float,
			 float, float, float);
static int  rmed_medring (SingleNicmosGroup *, SingleNicmosGroup *, short *,
			  int, int, int);
static void rmed_medfilter (float *, int, int, int, float *, short *, int, int,
			    float *);

int n_rmedian (SingleNicmosGroup *im1, SingleNicmosGroup *im2, float rinner,
	       float router, float ratio, float theta) {

/* Arguments:
**	im1	i: input image
**	im2	o: output (filtered) image
**	rinner	i: inner filter radius
**	router	i: outer filter radius
**	ratio	i: filter axis ratio
**	theta	i: filter position angle
*/

	/* Local variables */
	int	nxk, nyk;		/* kernel dimensions */
	int	nring;			/* number of ring pixels */
	short	*kernel;		/* ring filter kernel */
	float	a1, b1, c1, f1;		/* inner ring parameters */
	float	a2, b2, c2, f2;		/* outer ring parameters */

	/* Check for valid input parameters */
	if (rinner < 0.0 || router <= 0.0 || rinner >= router ||
	    ratio < 0.0 || ratio > 1.0 || theta < 0.0 || theta > 180.0) {
	    sprintf (MsgText, "Invalid input parameters in n_rmedian");
	    n_error (MsgText);
	    return (1);
	}

	/* Compute inner and outer ring parameters */
	rmed_ell_gauss (rinner, ratio, theta, &a1, &b1, &c1, &f1, &nxk, &nyk);
	rmed_ell_gauss (router, ratio, theta, &a2, &b2, &c2, &f2, &nxk, &nyk);

	/* Allocate memory for filter kernel */
	kernel = (short *)calloc(nxk * nyk, sizeof(short));
	if (kernel == NULL) {
	    sprintf (MsgText, "Can't allocate memory in n_rmedian");
	    n_error (MsgText);
	    return (1);
	}

	/* Fill in the kernel with the desired ring */
	nring = rmed_mkring (kernel, nxk, nyk, a1, b1, c1, f1, a2, b2, c2, f2);

	/* Perform the ring median filtering on the input image */
	if (rmed_medring (im1, im2, kernel, nxk, nyk, nring))
	    return (1);

	/* Free memory */
	free (kernel);
	
	return (0);

}

static void rmed_ell_gauss (float sigma, float ratio, float theta, float *a,
			    float *b, float *c, float *f, int *nx, int *ny) {

	/* Local variables */
	float sx2, sy2, cost, sint, discrim;

	/* Define some constants. */
	sx2 = sigma * sigma;
	sy2 = (ratio * sigma) * (ratio * sigma);
	cost = cos (2*PI*theta/360.0);
	sint = sin (2*PI*theta/360.0);

	/* Compute the ellipse parameters. */
	*a = cost * cost / sx2 + sint * sint / sy2;
	*b = 2. * (1.0 / sx2 - 1.0 / sy2) * cost * sint;
	*c = sint * sint / sx2 + cost * cost / sy2;
	discrim = (*b) * (*b) - 4. * (*a) * (*c);
	*f = 0.5;
	*nx = 2. * sqrt (-8. * (*c) * (*f) / discrim) + 1.;
	*ny = 2. * sqrt (-8. * (*a) * (*f) / discrim) + 1.;

	/* Force the kernel dimensions to the next nearest odd integer. */
	if (fmod (*nx, 2) == 0)
	    *nx = *nx + 1;
	if (fmod (*ny, 2) == 0)
	    *ny = *ny + 1;

}

static int rmed_mkring (short *kernel, int nx, int ny, float a1, float b1,
			float c1, float f1, float a2, float b2, float c2,
			float f2) {

	/* Local variables */
	int	i, j, x0, y0, x, y, nring;
	float	k1, k2;

	/* Define some constants. */
	x0 = nx / 2 + 1;
	y0 = ny / 2 + 1;

	/* Compute the kernel. */
	nring = 0;
	for (j = 1; j <= ny; j++) {
	    y = j - y0;
	    for (i = 1; i <= nx; i++) {
		x = i - x0;
		k1 = 0.5 * (a1 * x * x + c1 * y * y + b1 * x * y);
		k2 = 0.5 * (a2 * x * x + c2 * y * y + b2 * x * y);
		if (k1 >= f1 && k2 <= f2) {
		    kernel[(j-1)*nx+i-1] = 1;
		    nring = nring + 1;
		} else
		    kernel[(j-1)*nx+i-1] = 0;
	    }
	}
	
	return (nring);

}

static int rmed_medring (SingleNicmosGroup *im1, SingleNicmosGroup *im2,
			 short *kernel, int nxk, int nyk, int nring) {

	/* Local variables */
	int	col, line;
	float	*filter;
	float	*medline;

	/* Allocate space for the points to be medianed. */
	filter = (float *)calloc(nring, sizeof(float));
	medline = (float *)calloc(im1->sci.data.nx, sizeof(float));
	if (filter == NULL || medline == NULL) {
	    sprintf (MsgText, "Memory allocation failed in rmed_medring");
	    n_error (MsgText);
	    return (1);
	}

	/* Generate the output image line by line. */
	for (line = 0; line < im1->sci.data.ny; line++) {

	    /* Median filter the input image line. */
	    rmed_medfilter (im1->sci.data.data, line,
			    im1->sci.data.nx, im1->sci.data.ny, filter, kernel,
			    nxk, nyk, medline);

	    /* Copy the median line into the output image */
	    for (col = 0; col < im1->sci.data.nx; col++)
		 Pix(im2->sci.data,col,line) = medline[col];
	}

	/* Free memory. */
	free (filter);
	free (medline);

	return (0);

}

static void rmed_medfilter (float *data, int line, int nx, int ny,
			    float *filter, short *kernel, int xbox, int ybox,
			    float *medline) {

	/* Local variables */
	int	i, j, k, npts, ksel;
	int	imx, imy;

	/* Loop over the data columns. */
	for (i = 0; i < nx; i++) {

	     /* Load the filter. */
	     npts = 0;
	     for (j = 0; j < ybox; j++) {
		  for (k = 0; k < xbox ; k++) {

		       /* Compute image coords */
		       imx =    i-xbox/2+k;
		       imy = line-ybox/2+j;

		       /* If kernel is zero or image coords are out of
		       ** bounds, skip to the next kernel index */
		       if (kernel[j*xbox+k] == 0 || imx < 0 || imy < 0 ||
			   imx >= nx || imy >= ny)
			   continue;

		       /* Otherwise load this image pixel value into 
		       ** the filter buffer */
		       filter[npts] = data[imy*nx+imx];
		       npts++;
		  }
	     }

	     /* Compute the median of the filter buffer. */
	     if (npts > 0) {
		 ksel = (npts+1)/2;
		 medline[i] = c_asokr (filter, &npts, &ksel);
	     } else
		 medline[i] = 0.0;

	}
}

