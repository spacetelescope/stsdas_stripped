# include <stdio.h>
# include <math.h>
# include <float.h>
# include "readnoise.h"

# define  REJECTED -100000 /* Flag for rejecting pixel in s-clip loop */


/*   RN_STAT:  Compute average and variance with histogram cleanup
 *             and sigma-clipping.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   04 Nov 96  -  Implementation (IB)
 *
 */

void rn_stat (float *pixel, int npix, float kclip, int nclip, float cleanf,
              float *avg, float *variance) {

	int               i, iclip, n;
	int	       i1, i2, j1, j2;
	int                     clean;
	double         accum, sqaccum;
	double            value, hold;
	double         lupper, llower;

	void rn_shellsort (float *, int);

	lupper =  DBL_MAX;
	llower = -DBL_MAX;

	/* To get rid of outliers, lets first sort the pixel values
         * and discard a fraction of the brightest and the same fraction 
         * of the dimmest (to keep the average undisturbed in case there 
         * are *no* outliers). 
         */
	if (cleanf > 0.001F) {
	    rn_shellsort (pixel, npix);
	    clean = npix / (int)(2.0F / cleanf);
	    for (i = 0;              i < clean; pixel[i++] = REJECTED);
	    for (i = (npix - clean); i < npix;  pixel[i++] = REJECTED);
	}

	/* Sigma-clip loop. */
	for (iclip = 0; iclip <= nclip; iclip++) {

	    /* Clear accumulators and counter. */
	    sqaccum = 0.0;
	    accum   = 0.0;
	    n       = 0;

	    /* Accumulate. */
	    for (i = 0; i < npix; i++) {
	        value = (double)pixel[i];
	        if ((value != REJECTED) &&
	            (value < lupper)    &&
	            (value > llower)) {
	            accum   += value;
	            sqaccum += value * value;
	            n++;
	        } else
	            pixel[i] = REJECTED;
	    }

	    /* Results. */
	    hold = accum / (double)n;
	    *variance = (float)((sqaccum / (double)n) - (hold * hold));
	    *avg = (float) hold;

	    /* Update cutoffs for next s-clip iteration, taking
             * care for not spending an extra sqrt computation.
             */
	    if (iclip < nclip) {
	        hold   = sqrt (*variance);
	        lupper = *avg + hold * kclip;
	        llower = *avg - hold * kclip;
	    }
	}
}





/*
 *   SHELLSORT  -  Shell sort. 
 *
 *   Sorts an array arr[0...n-1] of float type into ascending numerical 
 *   order of its value elements. arr is replaced on output by its sorted 
 *   rearrangement.
 *
 *   Order: n^1.5
 *
 */

void rn_shellsort (float arr[], int n) {

	register int  gap, i, j;
	float              temp;

	gap = 1;
	do (gap = 3*gap +1); 
	while (gap <= n);
	for (gap /= 3; gap > 0; gap /= 3)
	for (i = gap; i < n; i++) {
	    temp = arr[i];
	    for (j = i-gap; (j>=0)&&(arr[j] > temp); j-=gap)
	        arr[j+gap] = arr[j];
	    arr[j+gap] = temp;
	}
}
