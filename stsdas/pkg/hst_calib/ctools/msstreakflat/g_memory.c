# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include "estreak.h"

/*  G_MEMORY  --   Routines to handle memory-resident data.
 *
 *
 *  - Initializes I/O control structure and alloc "virtual file" data arrays.
 *  - Refresh in-memory data arrays with current group's data.
 *  - Free memory.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   01 May 96  -  Implementation  (IB)
 *   30 May 96  -  Virtual file support  (IB)
 *
 */


int g_initMemory (IOControl *ioc, Bool verbose) {

	int         i, count, nimages;
	long   imageSize, need, avail;
	floatArray                pix;

	count = 0;

	/* Compute the available and needed amounts of memory. */
	imageSize = (long)ioc->x_size * (long)ioc->y_size *
                    (long)sizeof(float);
	nimages = (ioc->instrument != NICMOS) ? ioc->nimage+1: ioc->nimage+3;
	need  = imageSize * (long)nimages;
	avail = (long)(ioc->availMemory * MBYTE);
	if (need >= avail) {

	    /* Not enough memory is available: compute block size for 
             * median routine.
             */ 
	    ioc->blkSize = avail / (ioc->x_size * ioc->nimage * sizeof(float));
	    ioc->blkSize = (ioc->blkSize > ioc->y_size) ? 
                           ioc->y_size : ioc->blkSize;
	    ioc->blkSize = (ioc->blkSize < 1) ? 1 : ioc->blkSize;
	} else {

	    /* There is enough memory for keeping all images in memory
             * during median computation. Set block size accordingly. 
             */
	    ioc->blkSize = ioc->y_size;

	    /* Now see if each image's tmp data can also be kept
             * in the remaining memory. If so, alloc permanent space
             * for both input and tmp arrays and mark image accordingly. 
             */
	    avail -= imageSize * (long)ioc->nimage;
	    for (i = 0; i < ioc->nimage; i++) {
	        if (avail > imageSize) {

	            /* Alloc buffers. */
	            if (allocArray (ioc->image[i].inp, ioc->x_size, 
                                    ioc->y_size))
	                return (1);
	            if (allocArray (ioc->image[i].tmp, ioc->x_size, 
                                    ioc->y_size))
	                return (1);

	            /* Change access mode of this image. */
	            ioc->image[i].accessMode = Memory;
	            count++;

	            /* Decrease amount of available memory for next image. 
                     * Notice that only one image must be accounted for,
                     * because this memory will NOT be used for storing 
                     * input & temporary pairs, but only one of them. This 
                     * remaining memory is used only at median computation 
                     * time. 
                     */
	            avail -= imageSize;
	        }
	    }
	}

	if (verbose && (count > 0)) {
	    sprintf (ErrText, "%d images reside in memory.\n", count);
	    g_message (ErrText);
	}

	return (0);
}





/*  Refresh memory. */

int g_refreshMemory (IOControl *ioc) {

	int    i, nimages;
	floatArray  pix;

	for (i = 0; i < ioc->nimage; i++) {
	    if (ioc->image[i].accessMode == Memory) {
	        ioc->image[i].accessMode = Ddisk;
	        if (g_openImage (ioc, i, False)) return (1);
	        g_getBlock (ioc, 1, ioc->y_size, &pix);
	        memcpy (ioc->image[i].inp->data , pix.data, 
                        pix.bufsize*sizeof(float));
	        g_closeImage (ioc);
	        ioc->image[i].accessMode = Memory;
	    }
	}
	return (0);
}





/* Free memory */

int g_freeMemory (IOControl *ioc) {

	int  i;

	for (i = 0; i < ioc->nimage; i++) {
	    if (ioc->image[i].accessMode == Memory) {
	        free (ioc->image[i].inp->data);
	        free (ioc->image[i].tmp->data);
	    }
	    free (ioc->image[i].inp);
	    free (ioc->image[i].tmp);
	}
	return (0);
}
