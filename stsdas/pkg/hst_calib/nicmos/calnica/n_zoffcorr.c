# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOZOFF: Call the ZOFFCORR step for all MULTIACCUM groups.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for v4.0
*/

int n_doZoff (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *zoff) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: MULTIACCUM science image
**	zoff	 i: zero-read reference image
*/

	/* Local variables */

	/* Function definitions */
	int n_zoffcorr (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the MultiAccum zero-read subtraction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->ZOFF.corr == PERFORM) {
		 if (n_zoffcorr (nic, &(input->group[nic->group-1]), zoff))
		     return (status);
	     }

	     n_calReport (&nic->ZOFF, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}

/* N_ZOFFCORR: Perform zero-read subtraction for MULTIACCUM data
** groups. The science images are subtracted and the DQ arrays 
** are combined. The ERR, SAMP, and TIME arrays are unchanged.
** The exposure time for the group being corrected is reduced
** by an amount equal to the exposure time of the zero-read.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	29-Jul-1997	Modified to use zero-read SAMPTIME value
**				(Version 3.0)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
*/

int n_zoffcorr (NicInfo *nic, SingleNicmosGroup *input,
		SingleNicmosGroup *zoff) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be zero-subtracted
**	zoff	 i: zero-read image
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	double ztime;		/* zero-read exposure time */

	/* Function definitions */
	void n_aor (SingleNicmosGroup *, SingleNicmosGroup *);

	/* Subtract the science arrays from one another */
	for (j=0; j < input->sci.data.ny; j++) {
	     for (i=0; i < input->sci.data.nx; i++) {
		  Pix(input->sci.data,i,j) -= Pix(zoff->sci.data,i,j);
	     }
	}

	/* Combine (i.e. logical "OR") the data quality arrays */
	n_aor (input, zoff);

	/* Subtract the exposure time of the zero-read image
	** from the exposure time of the science image being processed */
	ztime = 0;
	if (getKeyD (&(zoff->sci.hdr), "SAMPTIME", &ztime)) {
	    n_kwerr ("SAMPTIME", nic->ZOFF.ref.name);
	    return (status = 1);
	}
	nic->exptime[nic->group-1] -= ztime;

	/* Subtract the time arrays from one another */
	for (j=0; j < input->intg.data.ny; j++) {
	     for (i=0; i < input->intg.data.nx; i++) {
		  Pix(input->intg.data,i,j) -= Pix(zoff->intg.data,i,j);
	     }
	}

	/* Successful return */
	return (status = 0);
}

