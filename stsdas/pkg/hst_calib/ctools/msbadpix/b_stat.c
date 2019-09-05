# include <stdio.h>
# include <math.h>
# include <float.h>
# include "nbadpix.h"

# define  REJECTED -100000 /* Flag for rejecting pixel in s-clip loop */


/*   B_STAT:  Compute average and stddev with histogram cleanup
 *            and sigma-clipping.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   06 Aug 96  -  Implementation (IB)
 *
 */

void b_stat (Counter *cou, int npix, float *avg, float *stddev) {

	int               i, iclip, n;
	int	       i1, i2, j1, j2;
	int                     clean;
	double         accum, sqaccum;
	double            pixel, hold;
	double         lupper, llower;

	void b_shellsort (float *, int);

	lupper =  DBL_MAX;
	llower = -DBL_MAX;

	/* To get rid of outliers, lets first sort the pixel values
         * and discard a fraction of the brightest and the same fraction 
         * of the dimmest (to keep the average undisturbed in case there 
         * are *no* outliers). Experiments showed that certain bad pixel
         * structures such as bad columns can't be detected without this 
         * cleanup step.
         */
	if (cou->cleanfrac > 0.001F) {
	    b_shellsort (cou->buffer, npix);
	    clean = npix / (int)(2.0F / cou->cleanfrac);
	    for (i = 0;              i < clean; cou->buffer[i++] = REJECTED);
	    for (i = (npix - clean); i < npix;  cou->buffer[i++] = REJECTED);
	}

	/* Sigma-clip loop. */
	for (iclip = 0; iclip <= cou->nclip; iclip++) {

	    /* Clear accumulators and counter. */
	    sqaccum = 0.0;
	    accum   = 0.0;
	    n       = 0;

	    /* Accumulate. */
	    for (i = 0; i < npix; i++) {
	        pixel = (double)cou->buffer[i];
	        if ((pixel != REJECTED) &&
	            (pixel < lupper)    &&
	            (pixel > llower)) {
	            accum   += pixel;
	            sqaccum += pixel * pixel;
	            n++;
	        } else
	            cou->buffer[i] = REJECTED;
	    }

	    /* Results. */
	    hold = accum / (double)n;
	    *stddev = (float)sqrt(((sqaccum / (double)n) - (hold * hold)));
	    *avg = (float) hold;

	    /* Update cutoffs for next s-clip iteration. */
	    lupper = (double)*avg + *stddev * cou->threshold;
	    llower = (double)*avg - *stddev * cou->threshold;

	}
}





/*
 *   SHELLSORT  -  Shell sort. 
 *
 *    Sorts an array arr[0...n-1] of float type into ascending numerical 
 *    order of its value elements. arr is replaced on output by its sorted 
 *    rearrangement.
 *
 *    Order: n^1.5
 *
 *    From "C - A Reference Manual", Harbison, S.P., Steele, Jr, G.L., 
 *    4th edition, Prentice Hall, 1995.  
 *
 */

void b_shellsort (float arr[], int n) {

	int         gap, i, j;
	float             temp;

	gap = 1;
	do (gap = 3*gap +1); while (gap <= n);
	for (gap /= 3; gap > 0; gap /= 3)
	    for (i = gap; i < n; i++) {
	        temp = arr[i];
	        for (j = i-gap; (j>=0)&&(arr[j] > temp); j-=gap)
	            arr[j+gap] = arr[j];
	        arr[j+gap] = temp;
	    }
}



