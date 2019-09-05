# include <stdio.h>
# include <math.h>
# include <stdlib.h>
# include "estreak.h"

/*  G_DOERROR:   Generates the output error array (STIS and NICMOS only).
 *
 *  The input image's error arrays are ignored. The output error array is
 *  built from the individual flatfield estimates generated from each
 *  input image. The pixelwise standard deviation across this flatfield set
 *  equals the _random_ flat error at each pixel. The final error array is
 *  produced as the (pixelwise) square root of the sum of all the residuals
 *  (individual estimate minus final flat) squared divided by the number of 
 *  input images. All individual flats are normalized to the final flat
 *  before the residual computation takes place.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   03 May 96  -  Implementation (IB)
 *   12 May 98  -  Added normalization in error array computation (IB)
 *
 */

int g_doError (IOControl *ioc,  floatArray *flat,  floatArray *error,
              Bool verbose) {

/*  Parameters:
 *
 *  IOControl            ioc  i: I/O control structure
 *  floatArray          flat  i: final flatfield image
 *  floatArray         error  o: error array (allocated by caller)
 *  Bool             verbose  i: verbose ?
 * 
 */

/*  Local variables                                                       */

	int        x_w1, x_w2, y_w1, y_w2;  /* Window                     */
	float          stddev, davg, favg;
	int                      image, n;
	long                            l;
	floatArray                    pix;  /* Current image              */
	double                 norm, hold;
	double                     *sumsq;  /* Accumulators               */
	int                         *npix;

	char *malloc_err = "Error computation: cannot allocate memory.";

	void g_stat (floatArray *, int, int, int, int, Bool, float, float,
                     float *, float *, int *, Bool);

	/* Window where to compute average. */
	x_w1 = (int)(ioc->x_size * WIND1) - 1;
	x_w2 = (int)(ioc->x_size * WIND2) - 1;
	y_w1 = (int)(ioc->y_size * WIND1) - 1;
	y_w2 = (int)(ioc->y_size * WIND2) - 1;

	if (verbose)
	    g_message ("Computing error array...\n");

	/* Calloc accumulators. */
	sumsq = (double *) calloc ((size_t)((long)ioc->y_size *
                                  (long)ioc->x_size), (size_t)sizeof(double));
	if (sumsq == NULL) {
	    g_error (malloc_err); 
	    return (1);
	}
	npix = (int *) calloc ((size_t)((long)ioc->y_size*(long)ioc->x_size), 
                               (size_t)sizeof(int));
	if (npix == NULL) {
	    g_error (malloc_err); 
	    return (1);
	}

	/* Compute average on central region of flat array. */
	g_stat (flat, x_w1, x_w2, y_w1, y_w2, False, 0.0F,0.0F, 
                &favg, &stddev, &n, False);

	/* Loop over image list. */
	for (image = 0; image < ioc->nimage; image++) {

	    /* Read temporary image. */
	    if (g_openImage (ioc, image, True)) return (1);
	    g_getBlock (ioc, 1, ioc->y_size, &pix);

	    /* Compute average on central region of data array. */
	    g_stat (&pix, x_w1, x_w2, y_w1, y_w2, False, 0.0F,0.0F, 
                    &davg, &stddev, &n, False);

	    /* Normalization factor. */
	    norm = favg / davg;

	    /* Compute residuals and accumulate. */
	    for (l = 0; l < pix.bufsize; l++) {
	        if ((pix.data[l]   != BADVAL) && 
                    (flat->data[l] != BADVAL)) {
	            hold      = pix.data[l] * norm - flat->data[l];
	            sumsq[l] += hold * hold;
	            npix[l]++;
	        }
	    }

	    /* Close temporary image. */
	    g_closeImage (ioc);
	}

	/* Compute error image. */
	for (l = 0; l < error->bufsize; l++) {
	    if (npix[l] > 1)
	        error->data[l] = (float) sqrt (sumsq[l] / (double) npix[l]);
	    else
	        error->data[l] = 0.0F;
	}

	/* Free memory. */
	free (npix);
	free (sumsq);
	return (0);
}
