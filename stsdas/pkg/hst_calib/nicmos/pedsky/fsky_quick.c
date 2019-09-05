# include <math.h>
# include <stdio.h>
# include <float.h>
# include <stdlib.h>
# include <xtables.h>	/* For the IRAF_INDEF macros */
# include "pedsky.h"

# define  INDEF		(float)IRAF_INDEFR
# define  NSIG		4.0

static float **comatrix (float **x, int xsize, int ysize, int col, int row);
static float determ (float **x, int size);

/*   FINDSKY_QUICK  --  Find best sky and pedestal values for a NICMOS image.
**
**	Based on matrix solution method used in the Nicred biasfix routine.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	18-May-1999	Implementation
**
*/

int findSky_Quick (TaskInfo *info, SingleNicmosGroup *input,
		   SingleNicmosGroup *Flat, Bool verbose) {

	/* Local variables */
	int	i, j;			/* Loop indexes */
	int	quad;			/* Quadrant index */
	int	pix;			/* Pixel index */
	float	smin, smax;		/* Sky scaling params */
	float	mean, median, mode;	/* Image statistics */
	float	stdv, min, max;		/* Image statistics */
	float	a[4], b;		/* Fit coefficients */
	float	*wts[4];		/* Weight arrays */

	/* Function declarations */
	int  n_iterstat (SingleNicmosGroup *, int, int, int, int, float, float,
			 short, float, int, float *, float *, float *, float *,
			 float *, float *);
	void multiinterceptfit (float *, float *, float **, int, int, float *,
				float *);

	/* Determine sky scaling params, if not entered by the user */
	if (info->SkySmin == INDEF || info->SkySmax == INDEF) {

	    /* Compute the mean and stdv of the image using
	    ** iterative sigma clipping to reject outliers. */
	    if (n_iterstat (input, info->statlim[0], info->statlim[1],
			    info->statlim[2], info->statlim[3], -FLT_MAX,
			    FLT_MAX, info->BitMask, 3.0, 5, &mean, &median,
			    &mode, &stdv, &min, &max))
		return (1);

	    smin = median - NSIG*stdv;
	    smin = (smin < 0.0 ? smin : 0.0);
	    smax = median + NSIG*stdv;

	} else {

	    smin = info->SkySmin;
	    smax = info->SkySmax;

	}

	/* Loop over the input image quadrants, setting up the weight arrays. */
	for (quad=0; quad < 4; quad++) {

	     /* Allocate memory for the weight arrays */
	     wts[quad] = (float *)calloc(input->sci.data.nx*input->sci.data.ny,
					 sizeof (float));

	     /* Load the weight arrays: pixels that are within the sky scaling
	     ** limits are used, those outside are not used. */
	     for (j=info->qy1[quad]; j <= info->qy2[quad]; j++) {
		  for (i=info->qx1[quad]; i <= info->qx2[quad]; i++) {

		       pix = j * input->sci.data.nx + i;

		       if (Pix(input->sci.data,i,j) > smin &&
			   Pix(input->sci.data,i,j) < smax &&
			   !(info->BitMask & DQPix(input->dq.data,i,j)) &&
			   Pix(Flat->sci.data,i,j) != 0)
			   wts[quad][pix] = 1;
		       else
			   wts[quad][pix] = 0;
		  }
	     }
	}
		 
	/* Compute optimal sky and pedestal values */
	multiinterceptfit (Flat->sci.data.data, input->sci.data.data, wts,
			   input->sci.data.nx*input->sci.data.ny, 4, a, &b);

	/* Save the sky and pedestal values */
	info->SkyValue    = b;
	info->PedValue[0] = a[0];
	info->PedValue[1] = a[1];
	info->PedValue[2] = a[2];
	info->PedValue[3] = a[3];

	if (verbose) {
	    sprintf (MsgText, "  Converged on sky value %9.7g\n",
		     info->SkyValue);
	    n_message (MsgText);
	    for (quad=0; quad < 4; quad++) {
		 sprintf (MsgText, "  Quadrant %d pedestal estimate = %9.7g\n",
			  quad+1, info->PedValue[quad]);
		 n_message (MsgText);
	    }
	}

	/* Free memory */
	for (quad=0; quad < 4; quad++)
	     free (wts[quad]);

	/* Successful return */
	return (0);
}

void multiinterceptfit (float *x, float *y, float **weights, int npix,
			int nsets, float *intercepts, float *slope) {
 
  int i,j,k;
  int size = nsets + 2;
  float delta;
  float sign;
 
  float **allocmat (int xsize, int ysize);
  float detcomatrix (float **, int, int, int);
  void freemat (float **ptr);

  float **a = (float **)allocmat(size, size);
 
  for (i=0 ; i < size; i++) {
      for (j=0; j < size; j++) {
          a[i][j] = 0.0;
      }
  }
 
  for (k=0; k < nsets; k++) {
      for ( i = 0; i < npix; i++ ) {
          a[1][0] += weights[k][i] * x[i] * y[i];
          a[1][1] += weights[k][i] * x[i] * x[i];
          a[2+k][0] += weights[k][i] * y[i];
          a[1][2+k] += weights[k][i] * x[i];
          a[2+k][1] = a[1][2+k];
          a[2+k][2+k] += weights[k][i];
      }
  }
 
  delta = detcomatrix(a, size, 0, 0);
  *slope = detcomatrix(a, size, 1, 0)/delta;
  sign = -1.;
  for (k = 0; k < nsets; k++) {
      intercepts[k] = sign * detcomatrix(a, size, k+2, 0) / delta;
      sign *= -1;
  }

  freemat(a);

}

float detcomatrix (float **x, int size, int col, int row) {

  float det;
  float **ptr;

  void freemat (float **ptr);

  ptr = comatrix(x, size, size, col, row);
  det = determ(ptr, size-1);

  freemat(ptr);
  return det;

}
 
static float determ (float **x, int size) {

  int sign = 1;
  float det = 0.0;
  int i;
 
  float detcomatrix (float **, int, int, int);

  if (size == 1)  {
    return **x;
  } else if (size == 2) {
      return (x[0][0]*x[1][1] - x[1][0]*x[0][1]);
  } else if (size == 3) {
      return (x[0][0]*(x[1][1]*x[2][2] - x[1][2]*x[2][1]) -
              x[0][1]*(x[1][0]*x[2][2] - x[1][2]*x[2][0]) +
              x[0][2]*(x[1][0]*x[2][1] - x[2][0]*x[1][1]));
  } else {
    for (i=0;i<size;i++) {
      det += x[0][i] * detcomatrix(x, size, i, 0) * sign;
      sign = -sign;
    }
  }
  return det;
}

static float **comatrix (float **x, int xsize, int ysize, int col, int row) {

  float **allocmat (int xsize, int ysize);

  /*float **y = allocmat(xsize, ysize);*/
  float **y = allocmat(xsize-1, ysize-1);
  int i,ii,j,jj;
 
  for (j=0;j<xsize-1;j++) {
       for (i=0;i<ysize-1;i++)
            y[i][j] = 0.0;
  }

  for (i=ii=0; i<ysize-1; i++,ii++) {
    if (row == i) ii++;
    for (j=jj=0; j<xsize-1; j++,jj++) {
      if (col == j) jj++;
      y[i][j] = x[ii][jj];
    }
  }

  return y;

}
 
float **allocmat (int xsize, int ysize) {

    float **make2d (float *ptr, int xsize, int ysize);

    /*float *ptr1d = (float *) malloc(xsize * ysize * sizeof(float));*/
    float *ptr1d = (float *) calloc(xsize * ysize, sizeof(float));
    if (ptr1d == NULL) {
        sprintf (MsgText, "Memory allocation error in allocmat()");
        n_error (MsgText);
    }
    return make2d(ptr1d, xsize, ysize);
}
 
void freemat (float **ptr) {
    free (*ptr);
    free (ptr);
}

float **make2d (float *ptr, int xsize, int ysize) {

    /*float **ptr2d = (float **)malloc(ysize * sizeof(float *));*/
    float **ptr2d = (float **)calloc(ysize, sizeof(float *));
    if (ptr2d == NULL) {
        sprintf (MsgText, "Memory allocation error in make2d()");
        n_error (MsgText);
    }
    for (; ysize--;) {
        ptr2d[ysize] = ptr + xsize * ysize;
    }
    return ptr2d;
}
 
