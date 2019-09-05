# include "pedsub.h"

/*   N_STOREPED  --  Store pedestal values in image header keywords.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**   H. Bushouse	14-Jun-2000	Added DCQUAD[n] keywords.
**
*/

int storePed (PedInfo *info, SingleNicmosGroup *input) {

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

	if (putKeyF (&(input->sci.hdr), "DCQUAD1", info->DCValue[0], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "DCQUAD2", info->DCValue[1], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "DCQUAD3", info->DCValue[2], ""))
	    return (1);
	if (putKeyF (&(input->sci.hdr), "DCQUAD4", info->DCValue[3], ""))
	    return (1);

	return (0);
}

