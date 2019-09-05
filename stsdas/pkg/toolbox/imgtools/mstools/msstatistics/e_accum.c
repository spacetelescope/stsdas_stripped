# include <stdio.h>
# include <math.h>
# include <string.h>
# include <stdlib.h>
# include <ximio.h>
# include "msstat.h"


/*  E_ACCUM  -  Scan pixel, mask, error and DQF arrays, and accumulate
 *              each eligible  pixel into the accumulators indexed by the 
 *              current HDU. 
 *
 *  By eligible pixels we mean the ones that give a zero result from ANDing 
 *  the task's global mask with the pixel's DQF and the image mask. 
 *
 *  The routine looks into the statType vector to see if higher moments are 
 *  present, and compute the corresponding sums (or update the histogram) only 
 *  if needed.
 *
 *  The routine structure was not break up into several vops-like calls to
 *  independent routines that handle each array, because the pixel rejection
 *  criteria introduce "crosstalk" between the arrays: science pixels rejected
 *  by the upper/lower cutoff criterion have their corresponding pixels in the
 *  remaining arrays also rejected, thus all arrays have to be visible at the
 *  same time.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   10 Jun 96  -  Implementation  (IB)
 *   02 Jul 96  -  Support for multiple open HDUs (IB)
 *   09 Jul 96  -  Upper/lower cutoffs (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs (IB)
 *
 */

int e_accum (Control *con, float lower, float upper) {

	Bool              weig, hweig, highm;
	int               sci_hdu, first_hdu;
	int                hdu, szlin, szcol;
	int                             i, j;
	int                             *msk;
	short                           *dqf;
	float           *pix[MAX_HDUS], *err;
	double                     x, x2, e2;

	int  e_imglen (IRAFPointer, int);
	int  e_getLine (Control *, int, int, float *[], short **, float **, 
                        int **);
	Bool e_reject  (Control *, float, int, float, float, int, int,
                        float *[], short *, float *, int *); 
	void e_locateHDU (Control *, int *, int *);

	/* See if high moments are to be computed. */
	highm = False;
	for (i = 0; i < con->nstats; i++) {
	    if ((con->stats[i] == ALL) ||
	        (con->stats[i] == SKEW) ||
	        (con->stats[i] == KURT))
	        highm = True;
	}

	/* See if weighted stats were asked by the user. */
	weig = False;
	for (i = 0; i < con->nstats; i++) {
	    if ((con->stats[i] == ALL) ||
	        (con->stats[i] == WMEAN) ||
	        (con->stats[i] == WVAR))
	        weig = True;
	}

	/* Locate position of sci array and first open array in 
         * hdu vector. This is needed because the loop over image 
         * lines is external to the loop over (open) HDUs. 
         */
	e_locateHDU (con, &sci_hdu, &first_hdu);

	/* Find size of section. Notice that task will crash if
         * the first open array is compressed and there is an 
         * image section. This is because e_imglen will return 
         * the number of image rows given by the NPIX2 keyword,
         * which is larger than the actual number of rows in the
         * section. This will cause the loop in szcol to run out
         * of physical rows. 
         */
	szlin = e_imglen (con->im[first_hdu], 1);
	szcol = e_imglen (con->im[first_hdu], 2);

	/* Alloc image line buffers. */
	for (hdu = 0; hdu < con->nhdu; pix[hdu++] = (float *) malloc 
                                       (szlin * sizeof (float)));

	err = (float *) malloc (szlin * sizeof (float));
	dqf = (short *) malloc (szlin * sizeof (short));
	msk = (int   *) malloc (szlin * sizeof (int));

	/* Scan image line by line. */
	for (j = 1; j <= szcol; j++) {

	    if (e_getLine (con, szlin, j, pix, &dqf, &err, &msk)) {
	        e_IRAFerror();
	        return (1);
	    }

	    /* The following code is not very efficient because all
             * rejection testing is done *inside* the innermost pixel loop. 
             * This is so to satisfy multiple and complex requirements: 
             * (i) the weighted quantities and upper/lower cutoffs apply only 
             * to the science array, and (ii) pixels rejected by upper/lower 
             * cutoff in the science array must also be rejected in the other 
             * arrays. 
             */

	    /* Scan pixels in this line. */
	    for (i = 0; i < szlin; i++) {

	        /* Scan requested HDUs. */
	        for (hdu = 0; hdu < con->nhdu; hdu++) {

	            if (con->im[hdu] != (IRAFPointer)NULL) {

	                /* Get current pixel. */
	                x  = (double)*(pix[hdu]+i);

	                /* See if weighted stats are to be 
                         * computed for this HDU. 
                         */
	                hweig = False;
	                if (weig && (hdu == sci_hdu))
	                    hweig = True;

	                /* Accumulate non-rejected pixels. */
	                if (!e_reject (con, (float)x, i, lower, upper,  
                                       hdu, sci_hdu, pix, dqf, err, msk)) {
	                    x2 = x * x;
	                    if (con->errOK) 
	                        e2 = (double)err[i] * (double)err[i];
	                    if (x < con->accum[hdu].min) 
	                        con->accum[hdu].min = x;
	                    if (x > con->accum[hdu].max) 
	                        con->accum[hdu].max = x;
	    	            con->accum[hdu].sum  += x;
	    	            con->accum[hdu].sum2 += x2;
	                    con->accum[hdu].npix++;
	                    if (highm) {
	    	                con->accum[hdu].sum3 += x2 * x;
	    	                con->accum[hdu].sum4 += x2 * x2;
	                    }
	                    if (hweig && con->errOK && (e2 > 0.0)) {
	    	                con->accum[hdu].sumxw += x   / e2;
	                        con->accum[hdu].sumww += 1.0 / e2;
	                    }
	                }
	            }
	        }
	    }
	}

	/* Free line buffers. */
	for (hdu = 0; hdu < con->nhdu; free (pix[hdu++]));
	free (err);
	free (dqf);
	free (msk);

	return (0);
}
