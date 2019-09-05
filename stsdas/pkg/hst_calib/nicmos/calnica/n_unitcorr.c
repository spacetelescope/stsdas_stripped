# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOUNIT: Call UNITCORR for all readouts of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doUnit (NicInfo *nic, MultiNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
*/

	/* Local variables */

	/* Function definitions */
	int n_unitcorr (NicInfo *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the units correction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->UNIT.corr == PERFORM) {
		 if (n_unitcorr (nic, &(input->group[nic->group-1])))
		     return (status);
	     }

	     n_calReport (&nic->UNIT, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}

/* N_UNITCORR: Convert NICMOS data from units of counts to countrates.
** The input SCI and ERR arrays are divided by the exposure time.
** The DQ, SAMP, and TIME arrays are unchanged.
** The BUNIT keyword in the SCI and ERR image headers are updated.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	28-Jul-1997	Changed NicInfo.bunit from scalar to vector
**				(Version 3.0)
** H.Bushouse	15-Feb-1998	Added check to set exptime of MultiAccum
**				zeroth readout to nic->sampzero, instead of
**				zero (Version 3.2)
** H.Bushouse	30-Sep-1998	Modified to use TIME image data to compute
**				countrates, instead of nic->exptime, since
**				individual pixels may have different exposure
**				times (Version 3.3)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
** H.Bushouse	16-Jun-2000	Added check for ZSIG.done=PERFORMED when
**				operating on zeroth-read, in case it was done
**				in a previous run (Version 4.0)
*/

int n_unitcorr (NicInfo *nic, SingleNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	float time;		/* exposure time */

	/* Function definitions */
	void n_amulk (SingleNicmosGroup *, float);

	/* Skip conversion if units are already countrate */
	if (nic->bunit[nic->group-1] == COUNTRATE) {
	    sprintf (MsgText,
	       "Data already in units of countrates; UNITCORR will be skipped");
	    n_warn (MsgText);
	    nic->UNIT.corr = SKIP;
	    return (status = 0);
	}

	/* Compute the inverse exposure time:
	** If we're processing a MultiAccum zeroth read, use the value of
	** nic->sampzero for the exposure time (Vsn 3.2) */
	if (nic->obsmode == MULTIACCUM && nic->group == nic->ngroups &&
	    (nic->ZSIG.corr == PERFORM || nic->ZSIG.done == PERFORMED)) {
	    time = 1.0 / nic->sampzero;
	    n_amulk (input, time);

	/* Otherwise, divide the input SCI and ERR arrays by the TIME array */
	} else {

	    for (j=0; j < input->sci.data.ny; j++) {
		 for (i=0; i < input->sci.data.nx; i++) {
		      time = Pix(input->intg.data,i,j);
		      if (time != 0) {
			  Pix(input->sci.data,i,j) /= time;
			  Pix(input->err.data,i,j) /= time;
		      } else {
			  Pix(input->sci.data,i,j) = 0.0;
			  Pix(input->err.data,i,j) = 0.0;
		      }
		 }
	    }
	}

	/* Update the units keyword in the SCI and ERR headers */
	if (putKeyS (&input->sci.hdr, "BUNIT", "COUNTS/S", ""))
	    return (status = 1);
	if (putKeyS (&input->err.hdr, "BUNIT", "COUNTS/S", ""))
	    return (status = 1);
	nic->bunit[nic->group-1] = COUNTRATE;

	/* Successful return */
	return (status = 0);
}

