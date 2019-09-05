# include <stdio.h>
# include <ximio.h>
# include "readnoise.h"


/*   RN_SINGLEIMAGE  --  Process one image pair from input lists.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   04 Nov 96  -  Implementation (IB)
 *
 */

int rn_singleImage (char *file1, char *file2, Image *img, Algorithm *alg) {

	int       i, row1, row2;
	float    *buff1, *buff2;

	IRAFPointer rn_openScience (Image *, char *);

	/* Open the two science HDUs. */
	if ((img->im1 = rn_openScience (img, file1)) == (IRAFPointer)NULL)
	    return (1);
	if ((img->im2 = rn_openScience (img, file2)) == (IRAFPointer)NULL) {
	    c_imunmap (img->im1);
	    return (1);
	}

	/* Read current block's pixels. */
	row1 = img->blkStart + 1;
	row2 = img->blkEnd   + 1;
	buff1 = c_imgs2r (img->im1, 1, img->xsize, row1, row2);
	if (c_iraferr())  {
	    rn_IRAFerror();
	    return (1);
	}
	buff2 = c_imgs2r (img->im2, 1, img->xsize, row1, row2);
	if (c_iraferr())  {
	    rn_IRAFerror();
	    return (1);
	}

	/* Update buffer array. */
	for (i = 0; i < img->blkSize; i++)
	    (alg->buffer)[i][img->image] = buff1[i] - buff2[i];

	/* Close science HDUs. */
	c_imunmap (img->im2);
	c_imunmap (img->im1);

	return (0);
}
