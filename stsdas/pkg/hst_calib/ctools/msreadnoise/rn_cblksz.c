# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include "readnoise.h"


/*  RN_COMPBLKSIZE  --  Computes block size based on amount of available
 *                      memory, image size and number of images.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   01 Nov 96  -  Implementation  (IB)
 *
 */


void rn_compBlkSize (Image *img, int memory) {

	long   imageSize, need, avail;

	/* Compute the available and needed amounts of memory. */
	imageSize = (long)img->xsize * img->ysize * sizeof(float);
	need  = imageSize * (img->nimages + 3);
	avail = memory * MBYTE;

	if (need >= avail) {

	    /* Not enough memory is available: compute block size. */
	    img->blkSize = (avail - imageSize) / 
                           (img->xsize * (img->nimages + 2) * sizeof(float));
	    img->blkSize = (img->blkSize > img->ysize) ? 
                           img->ysize : img->blkSize;
	    img->blkSize = (img->blkSize < 1) ? 1 : img->blkSize;
	} else {

	    /* There is enough memory for keeping full images in memory. */
	    img->blkSize = img->ysize;
	}

	/* In pixel units ! */
	img->blkSize *= img->xsize;
}
