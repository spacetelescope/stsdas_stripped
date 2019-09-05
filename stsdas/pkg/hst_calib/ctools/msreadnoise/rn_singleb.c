# include <stdio.h>
# include <float.h>
# include "readnoise.h"


/*   RN_SINGLEBLOCK  --  Process one block across all image pairs in 
 *                       input lists.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   04 Nov 96  -  Implementation (IB)
 *
 */

int rn_singleBlock (IRAFPointer list1, IRAFPointer list2, 
                   Image *img, Algorithm *alg, Bool verbose) {

	char   filename1[SZ_NAME];
	char   filename2[SZ_NAME];

	int rn_singleImage (char *, char *, Image *, Algorithm *);
	void rn_computeRN (Image *, Algorithm *);

	/* Loop over images in input lists. */
	for (img->image = 0; img->image < c_imtlen (list1); (img->image)++) {

	    /* Retrieve image names. */
	    c_imtgetim (list1, filename1, SZ_NAME);
	    c_imtgetim (list2, filename2, SZ_NAME);

	    /* Read images and process them. */
	    if (rn_singleImage (filename1, filename2, img, alg))
	        return (1);
	}

	/* Compute readout noise for current block. */
	rn_computeRN (img, alg);

	return (0);
}
