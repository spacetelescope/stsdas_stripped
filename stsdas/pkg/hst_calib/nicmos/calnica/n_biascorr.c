# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOBIAS: Apply biascorr to each readout of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doBias (NicInfo *nic, MultiNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be bias corrected
*/

	/* Local variables */

	/* Function definitions */
	int n_biascorr (NicInfo *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the bias correction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->BIAS.corr == PERFORM) {
		 if (n_biascorr (nic, &(input->group[nic->group-1])))
		     return (status);
	     }

	     n_calReport (&nic->BIAS, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);

	}

	/* Successful return */
	return (status = 0);
}

# define WRAP_THRESH	-23500.0

/* N_BIASCORR: Correct for 16-bit wrap-around effect in a
** NICMOS image. SCI image pixel values below the wrap threshold
** are assumed to have wrapped around and are corrected by adding
** +65536 to them to force them positive again. The pixel values
** are modified in-place in the input SCI image. The ERR, DQ, SAMP,
** and TIME images are not modified.
**
** Revision history:
** H.Bushouse	Feb. 1996	Build 1
** H.Bushouse	Aug. 1996	Build 2: Don't do ADCZERO (bias) subtraction,
**				but do wrapped pixel correction instead.
** H.Bushouse	18-Aug-1997	Changed WRAP_THRESH from -26000 to -23500
**				(Version 3.0)
** H.Bushouse	21-Jun-2000	Only report if number of wrapped pixels
**				is > 0 (Version 4.0)
*/

int n_biascorr (NicInfo *nic, SingleNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be bias corrected
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int nwrppix;		/* number of wrapped pixels */

	/* Initialize counter */
	nwrppix = 0;

	/* Do the wrap correction in-place in input image */
	for (j=0; j < input->sci.data.ny; j++) {
	     for (i=0; i < input->sci.data.nx; i++) {
		  if (Pix(input->sci.data,i,j) < WRAP_THRESH) {
		      Pix(input->sci.data,i,j) += 65536.0;
		      nwrppix++;
		  }
	     }
	}

	/* Report number of wrapped pixels */
	if (nwrppix > 0) {
	    sprintf (MsgText,
		     "BIASCORR detected %d wrapped pixels in imset %d;",
		     nwrppix, nic->group);
	    n_message (MsgText);
	}

	/* Successful return */
	return (status = 0);
}

