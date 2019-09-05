# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include "msstat.h"

# if defined(NATIVE_IRAF)
# define max(a,b) ((a) > (b) ? (a) : (b))
# define min(a,b) ((a) < (b) ? (a) : (b))
# endif

/*   E_HIST  -  Scan pixel, mask, error and DQF arrays, and accumulate
 *              histograms. 
 *
 *   This routine checks for the availability of a proper histogram
 *   vector for each HDU, and if no one exists, it allocates the
 *   necessary memory. This memory can be deallocated by e.g. routine
 *   e_clearaccum. The histogram size (# of bins) is a function of the
 *   maximum and minimum data values, thus the histogram handling can
 *   only be done after all data was scanned once.
 *
 *   The code structure is very similar to e_accum, the main difference
 *   being the histogram memory allocation section and the histogram
 *   updating code itself. Thus, this routine is subject to the same
 *   efficiency concerns that apply to e_accum.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   11 Jul 96  -  Implementation  (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   16 Apr 97  -  Fixed support for sections with compressed HDUs (IB)
 *
 */

int e_hist (Control *con, float lower, float upper) {

	int         sci_hdu, first_hdu;
	int             hdu, bin, i, j;
	int               szlin, szcol;
	int                       *msk;
	short                     *dqf;
	float     *pix[MAX_HDUS], *err;
	float                    x, dz;

	int  e_imglen (IRAFPointer, int);
	int  e_getLine (Control *, int, int, float *[], short **, float **, 
                        int **);
	Bool e_reject  (Control *, float, int, float, float, int, int,
                        float *[], short *, float *, int *); 
	void e_locateHDU (Control *, int *, int *);

	/* Alloc histogram memory if needed. If no proper data is available
         * to compute histogram, signal by setting pointer to NULL for 
         * current HDU only. The inverse bin width is used only to avoid
         * a division operation in the bin address computation. 
         */
	for (hdu = 0; hdu < con->nhdu; hdu++) {

	    if (con->accum[hdu].histogram == NULL               &&
	        con->im[hdu]              != (IRAFPointer)NULL) {
	        con->accum[hdu].floor    = (float)con->accum[hdu].min;
	        con->accum[hdu].ceiling  = con->accum[hdu].max;
	        con->accum[hdu].nbins    = min (con->accum[hdu].npix/4, HLIM);
	        con->accum[hdu].nbins    = max (con->accum[hdu].nbins, 2);
	        con->accum[hdu].binWidth = (con->accum[hdu].ceiling -
                                            con->accum[hdu].floor) /
                                            (float)(con->accum[hdu].nbins - 1);
	        if (con->accum[hdu].nbins < 3 || 
                    con->accum[hdu].ceiling <= con->accum[hdu].floor ||
                    con->accum[hdu].binWidth <= 0.0) {
	                con->accum[hdu].histogram = NULL;
	        } else {
	            con->accum[hdu].histogram = (long *) 
                                                calloc(con->accum[hdu].nbins, 
                                                sizeof(long));
	            if (con->accum[hdu].histogram == NULL) {
	                e_error (
                        "Cannot allocate memory for histogram accumulation.");
	                return (1);
	            }
	            con->accum[hdu].invWidth = 1.0 / con->accum[hdu].binWidth;
	        }
	    }
	}

	/* Locate position of sci array and first open array in 
         * hdu vector. This is needed because there might be no
         * science array. 
         */
	e_locateHDU (con, &sci_hdu, &first_hdu);

	/* Find size of section. Notice that task will crash if
         * the first open array is compressed and there is an 
         * image section. This is because e_imglen will return 
         * the number of image lines given by the NPIX2 keyword,
         * which is larger than the actual number of lines in the
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
             * rejection testing is done *inside* the innermost pixel 
             * loop. See remarks in e_accum source file. 
             */

	    /* Scan pixels in this line. */
	    for (i = 0; i < e_imglen (con->im[first_hdu], 1); i++) {

	        /* Scan requested HDUs. */
	        for (hdu = 0; hdu < con->nhdu; hdu++) {

	            /* Do only if histogram and HDU exist. */
	            if (con->im[hdu]              != (IRAFPointer)NULL &&
	                con->accum[hdu].histogram != NULL) {

	                /* Get current pixel. */
	                x  = *(pix[hdu]+i);

	                /* Accumulate non-rejected pixels. */
	                if (!e_reject (con, x, i, lower, upper,  
                                       hdu, sci_hdu, pix, dqf, err, msk)) {
	                    bin = (int)((x - con->accum[hdu].floor) * 
                                  con->accum[hdu].invWidth);
	                    con->accum[hdu].histogram[bin]++;
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
