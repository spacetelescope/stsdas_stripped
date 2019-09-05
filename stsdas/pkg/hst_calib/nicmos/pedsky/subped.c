# include "pedsky.h"

/*   SUBPED  --  Subtract pedestal signal from a NICMOS image.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	04-May-1999	Implementation
**
*/

void subPed (TaskInfo *info, SingleNicmosGroup *input) {

/* Arguments:
**	info	i: task info structure
**	input  io: input image
*/

	/* Local variables */
	int q;			/* quadrant index */
	int i, j;		/* pixel indexes */

	/* Loop over quadrants of input image */
	for (q = 0; q < 4; q++) {

	     /* Subtract the pedestal value from the quadrant */
	     for (j = QYI[q]; j <= QYF[q]; j++) {
		  for (i = QXI[q]; i <= QXF[q]; i++) {
		       Pix(input->sci.data,i,j) -= info->PedValue[q];
		  }
	     }
	}
	     
}

