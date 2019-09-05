# include <stdio.h>
# include "nbadpix.h"


/*   B_COMPUTEFLAGS  -  Scan the counter array and, at each pixel, compare 
 *                      counter value (divided by listsize) with badfrac 
 *                      parameter. If larger, set counter to BAD_FLAG 
 *                      (instrument-dependent value). If smaller, set 
 *                      counter to zero.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   07 Aug 96  -  Implementation (IB)
 *
 */

void b_computeFlags (int nimage, Counter *cou, Image *img, Bool verbose) {

	int   limit;
	long      i;

	limit = (int)(cou->badfrac * (float)nimage);

	for (i = 0; i < (img->xsize * img->ysize); i++) {
	    if (cou->counter[i] > limit)
	        cou->counter[i] = BAD_FLAG;
	    else
	        cou->counter[i] = 0;
	}
}
