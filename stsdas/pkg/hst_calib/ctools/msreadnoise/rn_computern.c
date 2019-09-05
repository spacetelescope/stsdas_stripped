# include <stdio.h>
# include <math.h>
# include <stdlib.h>
# include "readnoise.h"


/*   RN_COMPUTERN  --  Compute readout noise for current block.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   04 Nov 96  -  Implementation (IB)
 *
 */

void rn_computeRN (Image *img, Algorithm *alg) {

	int                i;
	long          offset;
	float  avg, variance;
	
	void rn_stat (float *, int, float, int, float, float *, float *);

	/* Offset for translating block address into full array address. */
	offset = img->blkStart * img->xsize;

	/* Scan pixels. */
	for (i = 0; i < img->blkSize; i++) {

	    /* Compute stats for current pixel. */
	    rn_stat ((alg->buffer)[i], img->nimages, alg->kclip, alg->nclip,
                      alg->cleanfrac, &avg, &variance);

	    /* Readout noise depends on variance only. */
	    (alg->rnoise)[offset+i] = sqrt (variance / 2.0);
	}
}
