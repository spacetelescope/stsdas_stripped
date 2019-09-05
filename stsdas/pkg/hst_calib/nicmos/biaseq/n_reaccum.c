# include <stdio.h>
# include "../lib/nicmos.h"

/*   N_REACCUM  --  Reaccumulate first differences of NICMOS IMSETs.
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	19-Apr-1999	Implementation.
**
*/

int n_reaccum (MultiNicmosGroup *input) {

/* Arguments:
**	input	io: input image
*/

	/* Local variables */
	int k;		/* Imset index */
	int i, j;	/* Pixel indexes */

	/* Loop over the input IMSETs */
	for (k = input->ngroups; k > 1; k--) {

	     for (j = 0; j < input->group[k-2].sci.data.ny; j++) {
		  for (i = 0; i < input->group[k-2].sci.data.nx; i++) {

		       Pix(input->group[k-2].sci.data,i,j) =
			   Pix(input->group[k-2].sci.data,i,j) +
			   Pix(input->group[k-1].sci.data,i,j);

		  }
	     }
	}

	return (0);
}

