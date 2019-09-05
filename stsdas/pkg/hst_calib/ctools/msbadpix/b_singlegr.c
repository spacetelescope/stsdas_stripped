# include <stdio.h>
# include <float.h>
# include "nbadpix.h"


/*   B_SINGLEGROUP  -  Process one group from the input list.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   05 Aug 96  -  Implementation (IB)
 *
 */

int b_singleGroup (IRAFPointer list, char *mask, Counter *cou, Image *img, 
                   char *timeStamp, Bool verbose) {

	int                   i;
	char  filename[SZ_NAME];

	int  b_singleImage (char *, Counter *, Image *, Bool);
	void b_computeFlags (int, Counter *, Image *, Bool);
	int  b_outputGroup (IRAFPointer, char *, Counter *, Image *, 
                            char *, Bool);

	/* Clear counter array. */
	for (i = 0; i < (img->xsize * img->ysize); cou->counter[i++] = 0);

	/* Process each image in input list. */
	c_imtrew (list);
	for (i = 0; i < c_imtlen(list); i++) {

	    /* Get image name. */
	    c_imtgetim (list, filename, SZ_NAME);

	    /* Process it. */
	    if (b_singleImage (filename, cou, img, verbose))
	        return (1);
	}

	/* Compute flags. */
	b_computeFlags (c_imtlen(list), cou, img, verbose);

	/* Output result. */
	if (b_outputGroup (list, mask, cou, img, timeStamp, verbose))
	    return (1);

	return (0);
}
