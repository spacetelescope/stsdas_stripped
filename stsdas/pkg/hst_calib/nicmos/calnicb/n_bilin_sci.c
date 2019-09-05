#include <math.h>
#include <stdio.h>

#include <hstio.h>	/* defines HST I/O functions */
#include "calnic.h"	/* defines NICMOS data structures */
#include "calnicb.h"	/* defines CALNICB data structures */

#define SQRT2 1.414214

/* N_BILIN_SCI: Perform bilinear interpolation in a NICMOS SCI image.
** Only the SCI array is interpolated. The DQ array is used to discard bad
** input pixels.
**
** Revision history:
** H.Bushouse	18-Feb-1997	Written for Build 2: Adapted from the routine
**				"bilin" in the "regmos" package. 
** H.Bushouse	15-Dec-1997	Cleaned up (Version 2.2)
** H.Bushouse	19-Mar-1998	Added use of new n_pixOK function (Version 2.2)
*/

int n_bilin_sci (SingleNicmosGroup *im, float x, float y, float *pix) {

/* Arguments:
**	im	i: input image
**	x,y	i: pixel coords in input image
**	pix	o: interpolated SCI pixel value
*/

	/* Local variables */
	int x1, x2, y1, y2;			/* pixel indexes */
	int goodpix;				/* good pixel flag */
	float pixel, pix1, pix2, pix3, pix4;	/* pixel values */
	float t, u, v, w;			/* fractional pixel shifts */

	/* Function definitions */
	Bool n_pixOK (SingleNicmosGroup *, int, int);

	/* Check for pixel coords that are outside the image limits */
	if (x > im->sci.data.nx-1 || y > im->sci.data.ny-1) {
	    sprintf (MsgText, "Pixel coords out of bounds in n_bilin_sci");
	    n_error (MsgText);
	    return (status=1);
	}


	/* Determine the pixel indexes */
	x1 = (int)x;
	x2 = x1 + 1;
  
	y1 = (int)y;
	y2 = y1 + 1;

	/* Determine the fractional pixel shifts */
	t = x - (float)x1;
	u = y - (float)y1;
	v = (float) sqrt( (double)( (1.-u)*(1.-u)+ t*t) ) / SQRT2;
	w = (float) sqrt( (double)(u*u+t*t) ) / SQRT2;

	/* At the edge of the array assume that the next row is the same */
	if (x2 > im->sci.data.nx-1)
	    x2 = x1;
	if (y2 > im->sci.data.ny-1)
	    y2 = y1;

	/* Store the input values */
	pix1 = Pix(im->sci.data,x1,y1);
	pix2 = Pix(im->sci.data,x2,y1);
	pix3 = Pix(im->sci.data,x2,y2);
	pix4 = Pix(im->sci.data,x1,y2);

	/* Determine which input pixels are good (unflagged) */
	goodpix = 0;
	if (n_pixOK(im,x1,y1))
	    goodpix += 8;
	if (n_pixOK(im,x2,y1))
	    goodpix += 4;
	if (n_pixOK(im,x2,y2))
	    goodpix += 2;
	if (n_pixOK(im,x1,y2))
	    goodpix += 1;

	/* Compute the interpolated SCI value */
	switch(goodpix){
	case 0:
	     pixel = BADVAL; /* Bad value */
	     break;
	case 1:
	     pixel = pix4;
	     break;
	case 2:
	     pixel = pix3;
	     break;
	case 3:
	     pixel = t*(pix3 - pix4) + pix4;
	     break;
	case 4:
	     pixel = pix2;
	     break;
	case 5:
	     pixel = v*(pix2 - pix4) + pix4;
	     break;
	case 6:
	     pixel = u*(pix3 - pix2) + pix2;
	     break;
	case 7:
	     pixel = t*(pix3 - pix4) + pix4 + (1.-u)*(pix2 - pix3);
	     break;
	case 8:
	     pixel = pix1;        
	     break;
	case 9:
	     pixel = u*(pix4 - pix1) + pix1;
	     break;
	case 10:
	     pixel = w*(pix3 - pix1) + pix1;
	     break;
	case 11:
	     pixel = t*(pix3 - pix4) + pix4 + (1.-u)*(pix1 - pix4);
	     break;
	case 12:
	     pixel = t*(pix2 - pix1) + pix1;
	     break;
	case 13:
	     pixel = t*(pix2 - pix1) + pix1 + u*(pix4 - pix1);
	     break;
	case 14:
	     pixel = t*(pix2 - pix1) + pix1 + u*(pix3 - pix2);
	     break;
	case 15:
	     pixel = (1.-t)*(1.-u)*pix1 + t*(1.-u)*pix2 + t*u*pix3 +
		     (1.-t)*u*pix4;
	     break;
}

	/* Set return value */
	*pix = pixel;

	/* Successful return */
	return (status = 0);

}
