# include "pedsky.h"

/*   N_STORESKY  --  Store sky value in image header keyword.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**
*/

int storeSky (TaskInfo *info, SingleNicmosGroup *input) {

/* Arguments:
**	info	i: task info structure
**	input  io: input image
*/

	/* Store the sky value in the header keyword SKYVAL */
	if (putKeyF (&(input->sci.hdr), "SKYVAL", info->SkyValue, ""))
	    return (1);

	return (0);

}

