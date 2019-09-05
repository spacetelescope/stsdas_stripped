# include <stdio.h>
# include "nbadpix.h"


/*   B_SINGLEIMAGE  -  Process one image from input list.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   05 Aug 96  -  Implementation (IB)
 *
 */

int b_singleImage (char *filename, Counter *cou, Image *img, Bool verbose) {

	void b_analyze (float *, Counter *, Image *, Bool);

	float  *pix;  /* science pixel array */

	if (verbose) {
	    sprintf (MsgText, "  %s:  ", filename);
	    b_message (MsgText);
	}

	/* Open science HDU and get pixel array. */
	img->im = b_immap (img, filename, True); 
	if (img->im == (IRAFPointer)NULL) {
	    b_error ("Cannot open image.");
	    return (1);
	}
	pix = (float *) c_imgs2r (img->im, 1, img->xsize, 1, img->ysize);

	/* Analyze pixel array and update counter. */
	b_analyze (pix, cou, img, verbose);

	/* Close science HDU. */
	c_imunmap (img->im);

	return (0);
}
