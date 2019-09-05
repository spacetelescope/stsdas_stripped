# include <stdio.h>
# include <ximio.h>
# include "msstat.h"


/*   E_REJECT  -  Set rejection flag based on DQ, mask and cutoffs.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   11 Jul 96  -  Implementation  (IB)
 *
 */

Bool e_reject (Control *con, float x, int column, float lower, 
               float upper,  int hdu, int sci_hdu, float *pix[],
               short *dqf, float *err, int *msk) { 

	Bool  reject;

	reject = False;

	if (con->dqfOK) {
	    if (con->gmask[con->ftype] & dqf[column])
	        reject = True;
	}

	if (con->mskOK) {
	    if (msk[column])
	        reject = True;
	}

	if (hdu == sci_hdu) {
	    if (x > upper || x < lower)
	        reject = True;
	} else {
	    if (sci_hdu >= 0) {
	        if (*(pix[sci_hdu]+column) > upper || 
                    *(pix[sci_hdu]+column) < lower)
	            reject = True;
	    }
	}

	return (reject);
}
