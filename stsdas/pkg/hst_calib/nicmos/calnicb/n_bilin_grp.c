# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define SQRT2 1.414214

/* N_BILIN_GRP: Perform bilinear interpolation in a NICMOS image group.
** The SCI, ERR, and TIME arrays are all interpolated. The DQ array is
** used to discard bad input pixels. The DQ values of input pixels used
** in the interpolation are propagated to the output DQ value.
**
** This version does not use pixels that are flagged as bad to perform
** the interpolation, but rather uses only neighboring good pixels, if any.
** However, in cases where one or more of the necessary input pixels are
** flagged as bad, the output DQ is set to the BADPIX value to indicate
** that the interpolated value may be corrupted.
**
** Revision history:
** H.Bushouse	18-Feb-1997	Written for Build 2: Adapted from the routine
**				"bilin" in the "regmos" package (Version 2.0)
** H.Bushouse	19-Mar-1998	Added use of new n_pixOK function (Version 2.2)
*/

int n_bilin_grp (SingleNicmosGroup *in, float x, float y,
		 float *sci, float *err, int *dq, float *time) {

/* Arguments:
** 	in	i: input image
**	x,y	i: pixel coords in input image
**	sci	o: interpolated SCI value
**	err	o: interpolated ERR value
**	dq	o: interpolated DQ  value
**	time	o: interpolated TIME value
*/

	/* Local variables */
	int x1, x2, y1, y2;			/* pixel indexes */
	int goodpix;				/* good pixel flag */
	float t, u, v, w;			/* fractional pixel shifts */
	float spix1, spix2, spix3, spix4;	/* SCI pixel values */
	float epix1, epix2, epix3, epix4;	/* ERR pixel values */
	short dpix1, dpix2, dpix3, dpix4;	/* DQ  pixel values */
	float tpix1, tpix2, tpix3, tpix4;	/* TIME pixel values */
	float spix, epix, tpix;			/* interpolated values */
	short dpix;				/* interpolated value */
	float wt;				/* error weight */

	/* Function definitions */
	Bool n_pixOK (SingleNicmosGroup *, int, int);

	/* Determine the pixel indexes */
	x1 = (int)x;
	x2 = x1 + 1;
  
	y1 = (int)y;
	y2 = y1 + 1;

	/* Check for pixel coords that are outside the image limits */
	if (x1 < 0 || y1 < 0 || x2 > in->sci.data.nx || y2 > in->sci.data.ny) {
	    sprintf (MsgText, "Pixel coords out of bounds in n_bilin_grp");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* At the edge of the array assume that the next row is the same */
	if (x2 > in->sci.data.nx-1)
	    x2 = x1;
	if (y2 > in->sci.data.ny-1)
	    y2 = y1;

	/* Store the input values */
	spix1 = Pix(in->sci.data,x1,y1);
	spix2 = Pix(in->sci.data,x2,y1);
	spix3 = Pix(in->sci.data,x2,y2);
	spix4 = Pix(in->sci.data,x1,y2);
	epix1 = Pix(in->err.data,x1,y1);
	epix2 = Pix(in->err.data,x2,y1);
	epix3 = Pix(in->err.data,x2,y2);
	epix4 = Pix(in->err.data,x1,y2);
	dpix1 = DQPix(in->dq.data,x1,y1);
	dpix2 = DQPix(in->dq.data,x2,y1);
	dpix3 = DQPix(in->dq.data,x2,y2);
	dpix4 = DQPix(in->dq.data,x1,y2);
	tpix1 = Pix(in->intg.data,x1,y1);
	tpix2 = Pix(in->intg.data,x2,y1);
	tpix3 = Pix(in->intg.data,x2,y2);
	tpix4 = Pix(in->intg.data,x1,y2);

	/* Determine the fractional pixel shifts */
	t = x - (float)x1;
	u = y - (float)y1;
	v = (float) sqrt( (double)( (1.-u)*(1.-u)+ t*t) ) / SQRT2;
	w = (float) sqrt( (double)(u*u+t*t) ) / SQRT2;

	/* Determine which pixels are good (unflagged) */
	goodpix = 0;
	if (n_pixOK (in,x1,y1))
	    goodpix += 8;
	if (n_pixOK (in,x2,y1))
	    goodpix += 4;
	if (n_pixOK (in,x2,y2))
	    goodpix += 2;
	if (n_pixOK (in,x1,y2))
	    goodpix += 1;

	/* Compute the interpolated values */
	switch (goodpix) {
	case 0:
	     spix = 0;
	     epix = 0;
	     tpix = 0;
	     if (t==0 && u==0)
		 dpix = dpix1 | BADPIX;
	     else if (t==0)
		 dpix = dpix1 | dpix3 | BADPIX;
	     else if (u==0)
		 dpix = dpix1 | dpix2 | BADPIX;
	     else
		 dpix = dpix1 | dpix2 | dpix3 | dpix4 | BADPIX;
	     wt = 1;
	     break;
	case 1:
	     spix = spix4;
	     epix = epix4*epix4;
	     tpix = tpix4;
	     dpix = dpix4 | BADPIX;
	     wt = 1;
	     break;
	case 2:
	     spix = spix3;
	     epix = epix3*epix3;
	     tpix = tpix3;
	     dpix = dpix3 | BADPIX;
	     wt = 1;
	     break;
	case 3:
	     spix = t*spix3 + (1.-t)*spix4;
	     epix = t*t*epix3*epix3 + (1.-t)*(1.-t)*epix4*epix4;
	     tpix = t*tpix3 + (1.-t)*tpix4;
	     if (t==0)
		 dpix = dpix4 | BADPIX;
	     else
		 dpix = dpix3 | dpix4 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t);
	     break;
	case 4:
	     spix = spix2;
	     epix = epix2*epix2;
	     tpix = tpix2;
	     dpix = dpix2 | BADPIX;
	     wt = 1;
	     break;
	case 5:
	     spix = v*spix2 + (1.-v)*spix4;
	     epix = v*v*epix2*epix2 + (1.-v)*(1.-v)*epix4*epix4;
	     tpix = v*tpix2 + (1.-v)*tpix4;
	     if (v==0)
		 dpix = dpix4 | BADPIX;
	     else
		 dpix = dpix2 | dpix4 | BADPIX;
	     wt = v*v + (1.-v)*(1.-v);
	     break;
	case 6:
	     spix = u*spix3 + (1.-u)*spix2;
	     epix = u*u*epix3*epix3 + (1.-u)*(1.-u)*epix2*epix2;
	     tpix = u*tpix3 + (1.-u)*tpix2;
	     if (u==0)
		 dpix = dpix2 | BADPIX;
	     else
		 dpix = dpix2 | dpix3 | BADPIX;
	     wt = u*u + (1.-u)*(1.-u);
	     break;
	case 7:
	     spix = t*spix3 + (1.-t)*spix4 + (1.-u)*(spix2 - spix3);
	     epix = t*t*epix3*epix3 + (1.-t)*(1.-t)*epix4*epix4 +
		    (1.-u)*(1.-u)*(epix2 - epix3)*(epix2 - epix3);
	     tpix = t*tpix3 + (1.-t)*tpix4 + (1.-u)*(tpix2 - tpix3);
	     dpix = dpix2 | dpix3 | dpix4 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t) + (1.-u)*(1.-u);
	     break;
	case 8:
	     spix = spix1;        
	     epix = epix1*epix1;        
	     tpix = tpix1;        
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else
		 dpix = dpix1 | BADPIX;
	     wt = 1;
	     break;
	case 9:
	     spix = u*spix4 + (1.-u)*spix1;
	     epix = u*u*epix4*epix4 + (1.-u)*(1.-u)*epix1*epix1;
	     tpix = u*tpix4 + (1.-u)*tpix1;
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix4;
	     else if (u==0)
		 dpix = dpix1 | BADPIX;
	     else
		 dpix = dpix1 | dpix4 | BADPIX;
	     wt = u*u + (1.-u)*(1.-u);
	     break;
	case 10:
	     spix = w*spix3 + (1.-w)*spix1;
	     epix = w*w*epix3*epix3 + (1.-w)*(1.-w)*epix1*epix1;
	     tpix = w*tpix3 + (1.-w)*tpix1;
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix3 | BADPIX;
	     else if (u==0)
		 dpix = dpix1 | dpix3 | BADPIX;
	     else
		 dpix = dpix1 | dpix3 | BADPIX;
	     wt = w*w + (1.-w)*(1.-w);
	     break;
	case 11:
	     spix = t*spix3 + (1.-t)*spix4 + (1.-u)*(spix1 - spix4);
	     epix = t*t*epix3*epix3 + (1.-t)*(1.-t)*epix4*epix4 +
		    (1.-u)*(1.-u)*(epix1 - epix4)*(epix1 - epix4);
	     tpix = t*tpix3 + (1.-t)*tpix4 + (1.-u)*(tpix1 - tpix4);
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix4;
	     else if (u==0)
		 dpix = dpix1 | dpix3 | dpix4 | BADPIX;
	     else
		 dpix = dpix1 | dpix3 | dpix4 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t) + (1.-u)*(1.-u);
	     break;
	case 12:
	     spix = t*spix2 + (1.-t)*spix1;
	     epix = t*t*epix2*epix2 + (1.-t)*(1.-t)*epix1*epix1;
	     tpix = t*tpix2 + (1.-t)*tpix1;
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | BADPIX;
	     else if (u==0)
		 dpix = dpix1 | dpix2;
	     else
		 dpix = dpix1 | dpix2 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t);
	     break;
	case 13:
	     spix = t*spix2 + (1.-t)*spix1 + u*(spix4 - spix1);
	     epix = t*t*epix2*epix2 + (1.-t)*(1.-t)*epix1*epix1 +
		    u*u*(epix4 - epix1)*(epix4 - epix1);
	     tpix = t*tpix2 + (1.-t)*tpix1 + u*(tpix4 - tpix1);
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix4;
	     else if (u==0)
		 dpix = dpix1 | dpix2;
	     else
		 dpix = dpix1 | dpix2 | dpix4 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t) + u*u;
	     break;
	case 14:
	     spix = t*spix2 + (1.-t)*spix1 + u*(spix3 - spix2);
	     epix = t*t*epix2*epix2 + (1.-t)*(1.-t)*epix1*epix1 +
		    u*u*(epix3 - epix2)*(epix3 - epix2);
	     tpix = t*tpix2 + (1.-t)*tpix1 + u*(tpix3 - tpix2);
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix2 | dpix3 | BADPIX;
	     else if (u==0)
		 dpix = dpix1 | dpix2;
	     else
		 dpix = dpix1 | dpix2 | dpix3 | BADPIX;
	     wt = t*t + (1.-t)*(1.-t) + u*u;
	     break;
	case 15:
	     spix = (1.-t)*(1.-u)*spix1 + t*(1.-u)*spix2 + t*u*spix3 +
		    (1.-t)*u*spix4;
	     epix = (1.-t)*(1.-t)*(1.-u)*(1.-u)*epix1*epix1 +
		    t*t*(1.-u)*(1.-u)*epix2*epix2 + t*t*u*u*epix3*epix3 +
		    (1.-t)*(1.-t)*u*u*epix4*epix4;
	     tpix = (1.-t)*(1.-u)*tpix1 + t*(1.-u)*tpix2 + t*u*tpix3 +
		    (1.-t)*u*tpix4;
	     if (t==0 && u==0)
		 dpix = dpix1;
	     else if (t==0)
		 dpix = dpix1 | dpix4;
	     else if (u==0)
		 dpix = dpix1 | dpix2;
	     else
		 dpix = dpix1 | dpix2 | dpix3 | dpix4;
	     wt = (1.-t)*(1.-t)*(1.-u)*(1.-u) + t*t*(1.-u)*(1.-u) + t*t*u*u +
		  (1.-t)*(1.-t)*u*u;
	     break;
	}

	/* Compute final error value */
	if (epix > 0 && wt > 0)
	    epix = sqrt (epix) / wt;
	else
	    epix = 0;

	/* Set return values */
	*sci  = spix;
	*err  = epix;
	*time = tpix;
	*dq   = dpix;

	/* Successful return */
	return (status = 0);
}

