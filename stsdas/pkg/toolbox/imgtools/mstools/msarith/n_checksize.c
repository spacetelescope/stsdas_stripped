# include "msarith.h"

/*    Checks axis sizes. 



      Revision history:
      ---------------
      01 Mar 96  -  Implementation (IB)
      12 Nov 96  -  STIS support (IB)

*/

int n_checkSize (GenericGroup *g1, GenericGroup *g2) {

	switch (g1->instrument) {
	case NICMOS:
	    if ((g1->sng->sci.data.tot_nx != g2->sng->sci.data.tot_nx) ||
                (g1->sng->sci.data.tot_ny != g2->sng->sci.data.tot_ny))
	        return (1);
	    break;
	case STIS:
	    if ((g1->sg->sci.data.tot_nx != g2->sg->sci.data.tot_nx) ||
                (g1->sg->sci.data.tot_ny != g2->sg->sci.data.tot_ny))
	        return (1);
	    break;
	}

	/* Successful return */
	return (0);
}
