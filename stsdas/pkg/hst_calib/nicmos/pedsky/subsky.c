# include "pedsky.h"

/*   SUBSKY  --  Subtract sky using scaled flatfield image.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**
*/

void subSky (TaskInfo *info, SingleNicmosGroup *input, SingleNicmosGroup *Flat){

/* Arguments:
**	info	i: task info structure
**	input  io: input image
**	Flat	i: flatfield image
*/

	/* Local variables */
	int i, j;		/* pixel indexes */

	/* Subtract the flatfield (scaled by the sky value) from the
	** input image */
	for (j = 0; j < input->sci.data.ny; j++) {
	     for (i = 0; i < input->sci.data.nx; i++) {

		  Pix(input->sci.data,i,j) -= info->SkyValue *
					      Pix(Flat->sci.data,i,j);
	     }
	}

}

