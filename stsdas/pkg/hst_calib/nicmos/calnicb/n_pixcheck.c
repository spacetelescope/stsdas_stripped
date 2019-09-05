# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_PIXOK: Function to check whether or not an image pixel is OK, based
** on its DQ flag settings. Returns boolean False if pixel is bad; True
** if pixel is OK.
**
** Revision history:
** H.Bushouse	19-Mar-1998	Written for Version 2.2
*/

Bool n_pixOK (SingleNicmosGroup *im, int i, int j) {

/* Arguments:
** 	im	i: input image
**	i	i: pixel coord
**	j	i: pixel coord
*/

	/* Check for zero value - pixel is OK */
	if (DQPix(im->dq.data,i,j) == 0)
	    return (True);

	/* Check for Source value only - pixel is OK */
	if (DQPix(im->dq.data,i,j) == SOURCE)
	    return (True);

	/* Check for Zerosig value only - pixel is OK */
	if (DQPix(im->dq.data,i,j) == ZEROSIG)
	    return (True);

	/* Check all bad pixel conditions; if DQ of pixel
	** has any of these bits set, return False. */
	if (DQPix(im->dq.data,i,j) & REED_SOL)
	    return (False);
	if (DQPix(im->dq.data,i,j) & BAD_LIN)
	    return (False);
	if (DQPix(im->dq.data,i,j) & BAD_DARK)
	    return (False);
	if (DQPix(im->dq.data,i,j) & BAD_FLAT)
	    return (False);
	if (DQPix(im->dq.data,i,j) & GROT)
	    return (False);
	if (DQPix(im->dq.data,i,j) & DEFECTIVE)
	    return (False);
	if (DQPix(im->dq.data,i,j) & SATURATED)
	    return (False);
	if (DQPix(im->dq.data,i,j) & MISSING)
	    return (False);
	if (DQPix(im->dq.data,i,j) & BADPIX)
	    return (False);
	if (DQPix(im->dq.data,i,j) & CR_HIT)
	    return (False);

	/* No bad bits set; return True */
	return (True);
}

/* N_PIXSOURCE: Function to check whether or not an image pixel contains
** source signal, based on its DQ flag settings. Returns boolean False if
** the pixel does not have the SOURCE DQ bit set. Returns True if the
** SOURCE bit is set without any bad bits also set.
**
** Revision history:
** H.Bushouse	19-Mar-1998	Written for Version 2.2
*/

Bool n_pixSource (SingleNicmosGroup *im, int i, int j) {

/* Arguments:
** 	im	i: input image
**	i	i: pixel coord
**	j	i: pixel coord
*/

	/* False if SOURCE bit not set */
	if (!(DQPix(im->dq.data,i,j) & SOURCE))
	    return (False);

	/* SOURCE bit is set; check for any bad bits */
	if (n_pixOK(im,i,j)) {

	    /* Pixel is OK */
	    return (True);

	} else {

	    /* Pixel is not OK */
	    return (False);
	}

}

