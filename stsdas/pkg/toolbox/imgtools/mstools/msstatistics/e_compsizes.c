# include <stdio.h>
# include "msstat.h"


/*  E_COMPSIZES  -  Check sizes of image, DQ, mask and error arrays.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   11 Jul 96  -  Implementation (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs (IB)
 *
 */

int e_compSizes (Control *con, int type) {

	int e_imglen (IRAFPointer, int);

	/* Compare this HDU axis sizes with mask's. */
	if (con->mskOK) {
	    if (c_imgndim (con->msk) > 0 && c_imgndim (con->im[type]) > 0) {
	        if ((e_imglen(con->im[type],1) != 
                     e_imglen(con->msk,1))         ||
	            (e_imglen(con->im[type],2) != 
                     e_imglen(con->msk,2))) {
	            return (1);
	        }
	    }
	}

	/* Compare this HDU axis sizes with DQF's. */
	if (con->dqfOK) {
	    if (c_imgndim (con->dqf) > 0 && c_imgndim (con->im[type]) > 0) {
	        if ((e_imglen(con->im[type],1) != 
                     e_imglen(con->dqf,1))        ||
	            (e_imglen(con->im[type],2) != 
                     e_imglen(con->dqf,2))) {
	            return (1);
	        }
	    }
	}

	/* Compare this HDU axis sizes with error's. */
	if (con->errOK) {
	    if (c_imgndim (con->err) > 0 && c_imgndim (con->im[type]) > 0) {
	        if ((e_imglen(con->im[type],1) != 
                     e_imglen(con->err,1))        ||
	            (e_imglen(con->im[type],2) != 
                     e_imglen(con->err,2))) {
	            return (1);
	        }
	    }
	}

	return (0);
}
