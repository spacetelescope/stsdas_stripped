# include <stdio.h>
# include <float.h>
# include "readnoise.h"


/*   RN_SINGLEGROUP  --  Process one group from input lists.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   04 Nov 96  -  Implementation (IB)
 *   04 Mar 97  -  Fixed blkSize computation (IB)
 *
 */

int rn_singleGroup (IRAFPointer list1, IRAFPointer list2, char *output, 
                   Image *img, Algorithm *alg, char *timeStamp, Bool verbose) {

	int block;
	Bool   go;

	int rn_singleBlock (IRAFPointer, IRAFPointer, Image *, 
                            Algorithm *, Bool);
	int rn_outputGroup (IRAFPointer, IRAFPointer, char *, Image *, 
                            Algorithm *, char *, Bool);

	block = img->blkSize / img->xsize;

	/* Initialize to first block. */
	img->blkStart = 0;
	img->blkEnd   = block - 1;

	/* Loop over blocks. This rather complicated logic is
         * needed because the last block may not be blkSize
         * in length.
         */
	go = True;
	while (1) {

	    /* Process current block. */
	    if (rn_singleBlock (list1, list2, img, alg, verbose))
	        return (1);

	    if (!go) break;

	    /* Bump block's starting and ending lines. */
	    img->blkStart += block;
	    img->blkEnd    = img->blkStart + block - 1;
	    if (img->blkEnd >= img->ysize) {
	        img->blkEnd =  img->ysize - 1;
	        img->blkSize = (img->blkEnd - img->blkStart + 1) * img->xsize;
	        go = False;
	    }
	    if (img->blkStart > img->blkEnd)
	        break;
	}

	/* Output result for current group. */
	if (rn_outputGroup (list1,list2,output,img,alg,timeStamp,verbose))
	    return (1);

	return (0);
}
