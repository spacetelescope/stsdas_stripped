# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structure */
# include "calnica.h"	/* defines CALNICA data structure */

/* N_DOMASK: Call maskcorr for each readout of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doMask (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *mask) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be masked
**	mask	 i: mask image
*/

	/* Local variables */

	/* Function definitions */
	int n_maskcorr (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Apply the mask to each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->MASK.corr == PERFORM) {
		 if (n_maskcorr (nic, &(input->group[nic->group-1]), mask))
		     return (status);
	     }

	     n_calReport (&nic->MASK, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);

	}

	/* Successful return */
	return (status = 0);
}

/* N_MASKCORR: Combine static bad pixel mask with NICMOS DQ array.
** The input DQ array is logically "OR'd" with the mask DQ array.
** Also set all DQ values to BADPIX if there was a TDF transition.
** The input SCI, ERR, SAMP, and TIME arrays are unchanged.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Nov.  1996	Build 2: Added check for TDF transition
** H.Bushouse	28-Jul-1997	Changed NicInfo.tdftrans from scalar to vector
**				(Version 3.0)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
*/

int n_maskcorr (NicInfo *nic, SingleNicmosGroup *input,
		SingleNicmosGroup *mask) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be masked
**	mask	 i: mask image
*/

	/* Local variables */
	int i, j;			/* loop indexes */

	/* Function definitions */
	void n_aor (SingleNicmosGroup *, SingleNicmosGroup *);

	/* Combine the DQ mask with the input DQ */
	n_aor (input, mask);

	/* Was there a TDF transition during the exposure? */
	if (nic->tdftrans[nic->group-1] > 0) {

	    /* Set the BADPIX value in the entire DQ array */
	    for (j = 0; j < input->dq.data.ny; j++) {
		 for (i = 0; i < input->dq.data.nx; i++) {
		      DQSetPix (input->dq.data,i,j,
				DQPix(input->dq.data,i,j) | BADPIX);
		 }
	    }
	}

	/* Successful return */
	return (status = 0);
}

