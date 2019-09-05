# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structure */
# include "calnica.h"	/* defines CALNICA data structure */

/* N_PHOTCALC: Store the photometry parameter values in the global header
** keywords PHOTMODE, PHOTFLAM, PHOTFNU, PHOTZPT, PHOTPLAM, and PHOTBW.
** 
** The input data are NOT modified (only the global header is modified).
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	April 1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 3.2.2)
**
** W. Hack	    added temperature dependent scaling for the photometry
*/

int n_photcalc (NicInfo *nic, PhotData *phot, MultiNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	phot	 i: Photometry parameters
**	input	io: input image
*/

	/* Function definitions */
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	if (nic->PHOT.corr == PERFORM) {

	    /* Write the photometry values to header keywords */
	    if (putKeyS (input->group[0].globalhdr, "PHOTMODE", phot->mode, ""))
		return (status = 1);
	    if (putKeyF (input->group[0].globalhdr, "PHOTFLAM", phot->flam, ""))
		return (status = 1);
	    if (putKeyF (input->group[0].globalhdr, "PHOTFNU",  phot->fnu,  ""))
		return (status = 1);
	    if (putKeyF (input->group[0].globalhdr, "PHOTZPT",  phot->zpt,  ""))
		return (status = 1);
	    if (putKeyF (input->group[0].globalhdr, "PHOTPLAM", phot->plam, ""))
		return (status = 1);
	    if (putKeyF (input->group[0].globalhdr, "PHOTBW",   phot->bw,   ""))
		return (status = 1);

	    /* Do the temperature-dependent zero-point correction for each group */

	    for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

		    if (n_photzpt (nic, &(input->group[nic->group-1]), phot))
				return (status);

	    }

	    if (putKeyF (input->group[0].globalhdr, "ZPSCALE", phot->zpscale, ""))
	    return (status = 1);

	    if (putKeyF (input->group[0].globalhdr, "PHOTFERR", phot->ferr, ""))
	    return (status = 1);
    }

	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     n_calReport (&nic->PHOT, nic->group,
			  &input->group[nic->group-1].sci.hdr,
 			  input->group[nic->group-1].globalhdr);
	}
	/* Successful return */
	return (status = 0);
}

/* N_PHOTZPT: Correct the NICMOS image for the effects of the
** temperature-dependent photometric zero-point changes. The 
** zero-point scaling factor is computed from values read in
** from the PHOTTAB reference table. This scaling factor gets
** applied in-place to the science image. The scaling factor
** image error is combined with the science
** data errors. The input DQ, SAMP and TIME arrays
** are unchanged.
**
** Revision history:
** W.J. Hack	Oct 2008	Build 1
**
*/

int n_photzpt (NicInfo *nic, SingleNicmosGroup *input,
		PhotData  *phot) {

/* Arguments:
**	nic	     i: NICMOS info structure
**	phot	 i: photometry table structure
**	input	io: image to be zero-point corrected
*/
    float zpscale;

	/* Function definitions */
	void n_amulk (SingleNicmosGroup *, float);

	/* If the exposure time is zero, don't bother with subtraction */
	if (nic->exptime[nic->group-1] == 0)
	    return (status = 0);

    /* If tfbtemp is invalid/not computed, the skip this correction */
    if (nic->tfbtemp == -1) { 
        sprintf(MsgText,"Photometric zero-point NOT corrected.");
        n_warn(MsgText);
        sprintf(MsgText,"No valid temperature from bias found.");
        n_message(MsgText);
        return (status=0);
    }
    /* Check to see if PHOTTAB had temperature-dependent zero-point
       coefficients and limits. If not, skip this correction. */
    if (phot->ferr == -1 || phot->tfblow == -1) {
        sprintf(MsgText,"Photometric zero-point NOT corrected.");
        n_warn(MsgText);
        sprintf(MsgText,"No Photometric zero-point coefficients found.");
        n_message(MsgText);
        return (status = 0);
    }    
    /* Perform temperature bounds checking to see whether the image
       temperature is within the range which can be corrected by the 
       relationship found in the PHOTTAB. 
    */    
    if (nic->tfbtemp < phot->tfblow || nic->tfbtemp > phot->tfbhigh) {
        sprintf(MsgText,"Photometric zero-point NOT corrected.");
        n_warn(MsgText);
        sprintf(MsgText,"Temperature from bias of %.3f out of limits: %.3f to %.3f",nic->tfbtemp, phot->tfblow, phot->tfbhigh);
        n_message(MsgText);
        return (status=0);
    } 
    zpscale = (phot->f_c1 * (nic->tfbtemp - phot->reft)) + phot->f_c0;    
    phot->zpscale = zpscale;
    
	/* Do the zero-point correction in-place in input */
	n_amulk (input, zpscale);

    return (status=0);
}
