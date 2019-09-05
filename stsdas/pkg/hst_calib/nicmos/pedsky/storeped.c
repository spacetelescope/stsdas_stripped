# include "pedsky.h"

/*   N_STOREPED  --  Store pedestal values in image header keywords.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**
*/

int storePed (TaskInfo *info, SingleNicmosGroup *input) {

/* Arguments:
**	info	i: task info structure
**	input  io: input image
*/

	/* Store the pedestal value for each quadrant in the header
	** keywords PEDQUAD[n] */
	if (putKeyF (&(input->sci.hdr), "PEDQUAD1", info->PedValue[0], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "PEDQUAD2", info->PedValue[1], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "PEDQUAD3", info->PedValue[2], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "PEDQUAD4", info->PedValue[3], ""))
	    return (1);

	return (0);
}

