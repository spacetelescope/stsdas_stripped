# include <stdio.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_BACKCALC: Calculate predicted background level for NICMOS observations.
** 
** Revision history:
** H. Bushouse	Sept. 1995	Build 1
*/

int n_backcalc (NicInfo *nic, MultiNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
*/

	/* Local variables */

	/* Function definitions */
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	if (nic->BACK.corr == PERFORM) {
	    sprintf (MsgText, "BACKCALC not yet implemented; will be skipped");
	    n_warn (MsgText);

	    nic->BACK.corr = OMIT;
	}

	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {
	     n_calReport (&nic->BACK, nic->group,
			  &input->group[nic->group-1].sci.hdr,
 			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}
