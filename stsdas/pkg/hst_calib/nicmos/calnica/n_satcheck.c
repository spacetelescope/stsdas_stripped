# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_SATCHECK: Flag pixels as saturated in a MultiAccum group if they're
** flagged as such in the preceding group.
**
** Revision history:
** H.Bushouse	24-Jul-1997	Created (Version 3.0)
** H.Bushouse	20-Jun-2000	Removed nic from argument list (Version 4.0)
*/

void n_satcheck (SingleNicmosGroup *group1, SingleNicmosGroup *group2) {

/* Arguments:
**	group1	 i: first image group
**	group2	io: second image group
*/

	/* Local variables */
	int i, j;		/* loop indexes */

	/* Loop through the DQ image of group 1 */
	for (j=0; j<group1->dq.data.ny; j++) {
	     for (i=0; i<group1->dq.data.nx; i++) {

		  /* If a pixel has a saturation flag, make sure the
		  ** flag is also set in the next group */

		  if (DQPix(group1->dq.data,i,j) & SATURATED)
		      DQSetPix(group2->dq.data,i,j,
			 DQPix(group2->dq.data,i,j) | SATURATED);
	     }
	}

}
