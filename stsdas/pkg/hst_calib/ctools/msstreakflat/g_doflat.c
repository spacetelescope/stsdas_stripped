# include <stdio.h>
# include <stdlib.h>
# include "estreak.h"


/*  G_DOFLAT:   Core streakflat algorithm.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   30 Apr 96  -  Implementation (IB)
 *   29 Oct 96  -  Window size matches SPP version (IB)
 *
 */

int g_doFlat (IOControl *ioc, int niter, int width[], int ngood,
              Bool verbose, floatArray *flat) {

/*  Parameters:
 *
 *  IOControl            ioc  i: I/O control structure
 *  int                niter  i: number of iterations
 *  float    width[MAX_ITER]  i: fwhm of smoothing boxcar at each iter.
 *  int                ngood  i: min. # of pixels for median computation.
 *  Bool             verbose  i: verbose ?
 *  floatArray          flat  o: flatfield result (allocated by caller)
 *
 */

/*  Local variables                                                       */

	int        x_w1, x_w2, y_w1, y_w2;  /* Window                     */
	int                iter, image, n;
	long                            l;
	floatArray                    pix;  /* Current image              */
	floatArray                   work;  /* Work array                 */
	float       median_avg, image_avg;
	float               stddev, angle;

	char *malloc_err = "Main loop: cannot allocate memory.";

/*  Function declarations                                                 */

	int g_median (IOControl *, Bool, int, floatArray *);
	int g_smooth (floatArray *, float, int);
	void g_stat (floatArray *, int, int, int, int, Bool, float, float,
                     float *, float *, int *, Bool);

	/* Window where to compute average. */
	x_w1 = (int)(ioc->x_size * WIND1) - 1;
	x_w2 = (int)(ioc->x_size * WIND2) - 1;
	y_w1 = (int)(ioc->y_size * WIND1) - 1;
	y_w2 = (int)(ioc->y_size * WIND2) - 1;

	/* Compute median to be used as flatfield's first estimate. */ 
	if (g_median (ioc, False, ngood, flat))
	    return (1);

	/* Compute average on central region. */
	if (verbose)
	    g_message ("  median statistics: ");
	g_stat (flat, x_w1, x_w2, y_w1, y_w2, False, 0.0F,0.0F, 
                &median_avg, &stddev, &n, verbose);

	/* Loop over different half widths of boxcar filtering. */
	for (iter = 0; iter < niter; iter++) {
	    if (verbose) {
	        sprintf (ErrText, 
                "  Begin 1-D smoothing, iteration %d\n", iter+1);
	        g_message (ErrText);
	    }

	    /* Alloc work array. */
	    if (allocArray (&work, ioc->x_size, ioc->y_size))
	        g_error (malloc_err); 

	    /* Loop over image list. */
	    for (image = 0; image < ioc->nimage; image++) {

	        /* Read input image. */
	        if (g_openImage (ioc, image, False)) return (1);
	        g_getBlock (ioc, 1, ioc->y_size, &pix);
	        memcpy (work.data, pix.data, pix.bufsize*sizeof(float));

	        /* Compute input / previous flat. */
	        for (l = 0; l < work.bufsize; l++) {
	            if ((work.data[l]  != BADVAL) && 
                        (flat->data[l] != BADVAL) &&
                        (flat->data[l] !=   0.0F))
	                work.data[l] = work.data[l]*median_avg/flat->data[l];
	            else
	                work.data[l] = BADVAL;
	        }

	        /* Boxcar-smooth the result along the streak direction. */
	        angle = ioc->image[ioc->current].streakAngle - 
                        ioc->angleOffset[ioc->group];
	        if (g_smooth (&work, angle, width[iter])) {
	            freeArray (&work);
	            return (1);
	        }

	        /* Compute average on central region. */
	        if (verbose)
	            g_message ("    smoothed streak statistics: ");
	        g_stat (&work, x_w1, x_w2, y_w1, y_w2, False, 0.0F,0.0F, 
                        &image_avg, &stddev, &n, verbose);

	        /* Divide input data by the smoothed streak. */
	        for (l = 0; l < work.bufsize; l++) {
	            if ((work.data[l] != BADVAL) && 
                        (pix.data[l]  != BADVAL) &&
                        (work.data[l] !=   0.0F))
	                work.data[l] = pix.data[l]*image_avg/work.data[l];
	            else
	                work.data[l] = BADVAL;
	        }

	        /* Close input image. */
	        g_closeImage (ioc);

	        /* Write result into temporary image assoc. to current input.*/
	        if (g_openImage (ioc, image, True)) return (1);
	        g_putImage  (ioc, &work);
	        g_closeImage (ioc);
	    }

	    /* Free work array. */
	    freeArray (&work);

	    /* Compute median of temporary images, this is 
             * the new flatfield estimate. 
             */
	    if (g_median (ioc, True, ngood, flat)) {
	        freeArray (&work);
	        return (1);
	    }
	}

	return (0);
}
