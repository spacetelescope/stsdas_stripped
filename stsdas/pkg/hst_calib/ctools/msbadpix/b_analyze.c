# include <stdio.h>
# include <math.h>
# include "nbadpix.h"


/*   B_ANALYZE  -  Analyze one image from input list and update
 *                 counter array.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   05 Aug 96  -  Implementation (IB)
 *
 */


void b_analyze (float *pix, Counter *cou, Image *img, Bool verbose) {

	int                 npix;  /* # of valid pixels in buffer  */
	int         line, column;
	int              counter;  /* Total in this image */
	float        avg, stddev;
	double  aavg, astddev, n;  /* Local stats */

	void b_extractWindow (float *, int, int, int *, Counter *, Image *);
	void b_stat (Counter *, int, float *, float *);

	/* Clear local accumulators. */
	aavg    = 0.0;
	astddev = 0.0;
	counter = 0;

	/* Loop over pixel array. */
	for (line = 0; line < img->ysize; line++) {
	    for (column = 0; column < img->xsize; column++) {

	        /* Extract data from pixel-centered window. */
	        b_extractWindow (pix, line, column, &npix, cou, img);

	        /* Compute mean and stddev with cleanup and sigma-clip. */
	        b_stat (cou, npix, &avg, &stddev);

	        /* If central pixel value is beyond threshold, update 
                counter array. */
	        if (fabs((double)pix[column+line*img->xsize] - avg) >
                    (double)(cou->threshold * stddev)) {
	            cou->counter[column+line*img->xsize]++;
	            counter++;
	        }

	        /* Update local stats. */
	        aavg    += (double)avg;
	        astddev += (double)stddev;
	    }
	}

	if (verbose) {
	    n = (double)img->xsize * img->ysize;
	    aavg    = aavg    / n;
	    astddev = astddev / n;

	    sprintf (MsgText, "avg = %5.3g  stdev = %5.3g  deviant pixels = %d\n", (float)aavg, (float)astddev, counter);
	    b_message (MsgText);
	}


}
