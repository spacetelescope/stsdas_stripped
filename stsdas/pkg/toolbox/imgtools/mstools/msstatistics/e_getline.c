# include <stdio.h>
# include <math.h>
# include <string.h>
# include <ximio.h>
# include "msstat.h"


/*  E_GETLINE  -  Get one line from all arrays.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   11 Jul 96  -  Implementation  (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs (IB)
 *
 */


int e_getLine (Control *con, int size, int j, float *pix[], short **dqf, 
               float **err, int **msk) {

	int   hdu;

	int e_imgl2r (IRAFPointer, int, int, float *);
	int e_imgl2s (IRAFPointer, int, int, short *);
	int e_imgl2i (IRAFPointer, int, int, int *);

	/* This reads one line from each HDU in list. */
	for (hdu = 0; hdu < con->nhdu; hdu++) {
	    if (con->im[hdu] != (IRAFPointer)NULL) {
	        if (e_imgl2r (con->im[hdu], j, size, pix[hdu]))
	            return (1);
	    }
	}

	/* This reads one line from mask, DQ and error HDUs. */
	if (con->mskOK) {
	    if (e_imgl2i (con->msk, j, size, *msk))
	        return (1);
	}
	if (con->dqfOK) {
	    if (e_imgl2s (con->dqf, j, size, *dqf))
	        return (1);
	}
	if (con->errOK) {
	    if (e_imgl2r (con->err, j, size, *err))
	        return (1);
	}

	return (0);
}



/*  These three routines conceal the details of handling 
 *  compressed arrays in the HSTIO sense.
 */

int e_imgl2r (IRAFPointer im, int row, int size, float *buffer) {

	int          i, j;
	float      pixval;
	float      *local;

	if ((c_imgndim (im) == 0)       &&
	    (c_imaccf (im, "PIXVALUE"))) {
	    pixval = c_imgetr (im, "PIXVALUE");
	    for (i = 0; i < size; buffer[i++] = pixval);
	} else {
	    local = c_imgl2r (im, row);
	    if (c_iraferr()) {
	        return (1);
	    }
	    for (i = 0, j = 0; i < size; buffer[i++] = local[j++]);
	}

	return (0);
}

int e_imgl2s (IRAFPointer im, int row, int size, short *buffer) {

	int          i, j;
	short      pixval;
	short      *local;

	if ((c_imgndim (im) == 0)        &&
	    (c_imaccf (im, "PIXVALUE"))) {
	    pixval = (short)c_imgetr (im, "PIXVALUE");
	    for (i = 0; i < size; buffer[i++] = pixval);
	} else {
	    local = c_imgl2s (im, row);
	    if (c_iraferr()) {
	        return (1);
	    }
	    for (i = 0, j = 0; i < size; buffer[i++] = local[j++]);
	}

	return (0);
}

int e_imgl2i (IRAFPointer im, int row, int size, int *buffer) {

	int          i, j;
	int        pixval;
	int        *local;

	if ((c_imgndim (im) == 0)        &&
	    (c_imaccf (im, "PIXVALUE"))) {
	    pixval = (int)c_imgetr (im, "PIXVALUE");
	    for (i = 0; i < size; buffer[i++] = pixval);
	} else {
	    local = c_imgl2i (im, row);
	    if (c_iraferr()) {
	        return (1);
	    }
	    for (i = 0, j = 0; i < size; buffer[i++] = local[j++]);
	}

	return (0);
}



