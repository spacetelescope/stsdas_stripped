# include <stdio.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_USERWARN: Generate warnings based on engineering data.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
*/

int n_userwarn (NicInfo *nic, MultiNicmosGroup *input) {

	/* Local variables */

	/* Function definitions */
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	if (nic->WARN.corr == PERFORM) {
	    sprintf (MsgText, "WARNCALC not yet implemented; will be skipped");
	    n_warn (MsgText);

	    nic->WARN.corr = OMIT;
	}

	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {
	     n_calReport (&nic->WARN, nic->group,
			  &input->group[nic->group-1].sci.hdr,
 			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}
