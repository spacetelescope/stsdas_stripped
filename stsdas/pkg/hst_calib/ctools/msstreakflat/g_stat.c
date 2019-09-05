# include <stdio.h>
# include <math.h>
# include <float.h>
# include "estreak.h"

/*  G_STAT:  Compute average, stddev  and number of valid data points for 
 *  a region of an image, with optional outlier clipping. With clipping
 *  the input array is modified in place, excluded pixels are set to the
 *  BADVAL value. 
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   24 Apr 96  -  Implementation (IB)
 *   29 Oct 96  -  Include window upper row/col in summation(IB)
 *   30 Dec 97  -  Fixed avg and stddev computations (IB)
 *
 */

void g_stat (floatArray *pic, int x1, int x2, int y1, int y2, Bool sclip, 
                      float high, float low, float  *avg, float *stddev, 
                      int *n, Bool verbose) {

	int          i, j;
	int	     i1, i2, j1, j2;
	float        lupper, llower;
	double       accum, sqaccum, hold;

	/* Bounds protection. */
	i1 = (x1 >= 0) ? x1 : 0;
	j1 = (y1 >= 0) ? y1 : 0; 
	i2 = (x2 <  pic->nx) ? x2 : pic->nx-1; 
	j2 = (y2 <  pic->ny) ? y2 : pic->ny-1; 

	/* Set clipping limits. */
	if (sclip) {
	    lupper = high;
	    llower = low;

	} else {
	    lupper =  FLT_MAX;
	    llower = -FLT_MAX;
	}

	/* Clear accumulators and counter. */
	sqaccum = 0.0;
	accum   = 0.0;
	*n      = 0;

	/* Accumulate. */
	for (j = j1; j <= j2; j++) {
	    for (i = i1; i <= i2; i++) {
	        if ((GPPix(pic,i,j) != BADVAL) &&
	            (GPPix(pic,i,j) < lupper)  &&
	            (GPPix(pic,i,j) > llower)) {
	            accum   += (double)GPPix(pic,i,j);
	            sqaccum += (double)GPPix(pic,i,j) * (double)GPPix(pic,i,j);
	            (*n)++;
	        } else
	            GPPix(pic,i,j) = BADVAL;
	    }
	}

	/* Results. */
	if (*n > 0) {
	    *avg = (float)(accum / (double)*n);
	    hold = (sqaccum / (double)*n) - (*avg * *avg);
	    if (hold > 0.0)
	        *stddev = (float)sqrt(hold);
	    else
	        *stddev = 0.0;
	} else {
	    *avg = 0.0;
	    *stddev = 0.0;
	}

	if (verbose) {
	    sprintf (ErrText, 
            " avg= %0.1f +/- %0.1f, npts= %d\n", *avg, *stddev, *n);
	    g_message (ErrText);
	}
}

