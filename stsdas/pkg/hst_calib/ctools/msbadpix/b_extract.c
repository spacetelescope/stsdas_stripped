# include <stdio.h>
# include "nbadpix.h"

# if defined(NATIVE_IRAF)
# define max(a,b) ((a) > (b) ? (a) : (b))
# define min(a,b) ((a) < (b) ? (a) : (b))
# endif


/*   B_EXTRACTWINDOW  -  Extract pixels from square window, discarding
 *                       central pixel and any others outside image
 *                       boundaries.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   05 Aug 96  -  Implementation (IB)
 *
 */


void b_extractWindow (float *pix, int line, int column, int *npix, 
                     Counter *cou, Image *img) {

	int   i1, i2, j1, j2;
	int   i, j;

	/* Bounds checking. */
	i1 = max (column - cou->window/2, 0);
	j1 = max (line   - cou->window/2, 0);
	i2 = min (column + cou->window/2, img->xsize-1);
	j2 = min (line   + cou->window/2, img->ysize-1);

	*npix = 0;

	for (j = j1; j <= j2; j++) {
	    for (i = i1; i <= i2; i++) {
	        cou->buffer[(*npix)++] = pix[i+j*img->xsize];
	        if ((i == column) && (j == line))
	            (*npix)--;
	    }
	}
}
