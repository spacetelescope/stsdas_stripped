# include "../lib/nicmos.h"

/*   N_FIRSTDIFFS  --  Compute first differences of input file IMSETs.
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	26-Mar-1999	Implementation.
**
*/

int n_firstDiffs (MultiNicmosGroup *input) {

/* Arguments:
**	input	io: input image
*/

	/* Local variables */
	int k;		/* Imset index */
	int i, j;	/* Pixel indexes */

	/* Loop over the input IMSETs, subtracting preceding IMSET from
	** current IMSET. */
	for (k=0; k < input->ngroups-1; k++) {

	     for (j=0; j < input->group[k].sci.data.ny; j++) {
		  for (i=0; i < input->group[k].sci.data.nx; i++) {

		       Pix(input->group[k].sci.data,i,j) =
			   Pix(input->group[k].sci.data,i,j) -
			   Pix(input->group[k+1].sci.data,i,j);

		  }
	     }
	}

	return (0);
}

