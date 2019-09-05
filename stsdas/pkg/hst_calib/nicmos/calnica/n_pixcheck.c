# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_PIXOK: Function to check whether or not an image pixel is OK, based
** on its DQ flag settings. Returns boolean False if pixel is bad; True
** if pixel is OK.
**
** Revision history:
** H.Bushouse	08-Apr-1998	Copied from CALNICB for Version 3.2
** H.Bushouse	23-Jun-2000	Revised the scheme from checking for all
**				individual bit settings to simply doing a
**				"bitwise and" with the BITMASK value. (V 4.0)
*/

/* The following bitmask value corresponds to these DQ settings:
**  Reed-Solomon error =    1
**  Poor nlinearity    =    2
**  Poor darkcorr      =    4
**  Poor flatfield     =    8
**  GROT               =  *OK*
**  Bad (hot/cold)     =   32
**  Saturated          =   64
**  Missing data       =  128
**  Bad pix in calib   =  256
**  Cosmic-ray hit     =  512
**  Source             =  *OK*
**  Zero-read signal   =  *OK*
**                       ----
**  Total mask value   = 1007
*/
# define BITMASK 1007

Bool n_pixOK (SingleNicmosGroup *im, int i, int j) {

/* Arguments:
** 	im	i: input image
**	i	i: pixel coord
**	j	i: pixel coord
*/


	/* Test the pixel DQ value against the BitMask */
	if (DQPix(im->dq.data,i,j) & BITMASK)
	    return (False);

	/* No bad bits set; return True */
	return (True);
}

