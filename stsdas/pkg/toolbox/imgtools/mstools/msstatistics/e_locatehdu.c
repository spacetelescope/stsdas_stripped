# include <stdio.h>
# include "msstat.h"


/*  E_LOCATEHDU  -  Locate position of sci array and first open array `
 *                  in hdu vector. 
 *
 *
 *   Revision history:
 *   ----------------
 *   12 Jul 96  -  Implementation  (IB)
 *
 */

void e_locateHDU (Control *con, int *sci_hdu, int *first_hdu) {

	int  hdu;

	/* Locate position of sci array in hdu vector. A value of -1
         * signals that there is no science array. 
         */
	*sci_hdu = -1;
	for (hdu = 0; hdu < con->nhdu; hdu++) {
	    if (con->hdu[hdu] == SCIENCE)
	        *sci_hdu = hdu;
	}

	/* Locate position of first open array in hdu vector. */
	*first_hdu = -1;
	for (hdu = con->nhdu-1; hdu >= 0; hdu--) {
	    if (con->im[hdu] != (IRAFPointer)NULL)
	        *first_hdu = hdu;
	}

}
