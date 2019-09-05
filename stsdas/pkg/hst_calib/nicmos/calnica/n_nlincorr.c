# include <math.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DONLIN: Call NLINCORR for all readouts of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
** H.Bushouse	20-Jun-2000	Removed nic from n_satcheck argument list (V4.0)
*/

int n_doNlin (NicInfo *nic, MultiNicmosGroup *input, NlinData *nlin,
	      SingleNicmosGroup *zsig) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image to be corrected
**	nlin	 i: nonlinearity reference data
**	zsig	 i: MULTIACCUM zero-read signal image
*/

	/* Local variables */

	/* Function definitions */
	int n_nlincorr (NicInfo *, SingleNicmosGroup *, NlinData *,
			SingleNicmosGroup *);
	void n_satcheck (SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the non-linearity correction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->NLIN.corr == PERFORM) {
		 if (n_nlincorr (nic, &(input->group[nic->group-1]), nlin,
				 zsig))
		     return (status);

		 /* Flag pixels in the next group as saturated if they're
		 ** flagged as saturated in this group */
		 if (nic->obsmode == MULTIACCUM && nic->group-1 > 0)
		     n_satcheck (&(input->group[nic->group-1]),
				 &(input->group[nic->group-2]));
	     }

	     n_calReport (&nic->NLIN, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}

/* N_NLINCORR: Correct NICMOS science image values for detector nonlinearity.
** This routine assumes that the correction is represented by a 2nd-order
** polynomial for each pixel in the detector. Three arrays of coefficients
** and six arrays of (co)variances define the polynomial function.
** It is further assumed that there are two "node" or transition value arrays
** in the reference data that define the transition values (in units of DNs)
** from the regions below and above the correction regime. Science image values
** below the first node value receive no correction; values between the
** first and second node values receive the correction; values above
** the second node are deemed saturated, are flagged as such, and receive
** no correction. In practice, the node 1 values are set to zero in the
** reference data so that the correction is applied down to the lowest signal
** levels. There is also a DQ flag array in the reference data, which is
** propagated into the science data.
**
** Note that early versions of the nonlinearity correction used a 1st-order
** (linear) polynomial to represent the correction. Reference files for this
** scheme contain only two coefficient arrays and three (co)variance arrays.
** The algorithms used in this routine are designed to be backwards compatible
** with these older reference data by having initialized the unused higher
** order coeffs and errors to zero in the n_getNLinData routine.
**
** A correction is made for estimated signal in the zeroth read of a
** MultiAccum observation. Without the correction, the zero-subtracted pixel
** values can fall into the wrong correction regime. This can lead to
** pixels that are truly saturated going unflagged because their values
** are below the saturation limits in the NLINFILE, or, for those that fall
** within the linear correction (middle) regime it can lead to the wrong
** amount of correction. The correction for signal in the zeroth read is
** accomplished by adding the estimated zero-read signal to the incoming
** SCI image values before applying the linearity correction, and then
** removing it again afterwards.
**
** The SCI and ERR arrays are updated, and the DQ values are propagated.
** The SAMP and TIME arrays are not modified.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2
** H.Bushouse	14-Nov-1996	Changed correction scheme from 3 2nd-order
**				functions to 1 linear function. (Version 2.0)
** H.Bushouse	27-Jan-1997	Fixed bug so that all NLINFILE DQ values get
**				propagated into science data. Changed n1,n2 to
**				type float. (Version 2.1)
** H.Bushouse	25-Apr-1997	Added check for non-negative number before
**				taking sqrt in error computation. (Version 2.2)
** H.Bushouse	28-Jul-1997	Added zero-read signal correction using zsig
**				image and check for pixels that are already
**				flagged as saturated from earlier readouts
**				(Version 3.0)
** H.Bushouse	13-Feb-1998	Added use of new ZSIG step switch and
**				application of ZSIG only to groups other than
**				the MultiAccum zeroth-read (Version 3.2)
** H.Bushouse	05-Aug-1999	Modifications for use of 2nd-order correction
**				scheme: additional local variables for use of
**				up to 3 coeffs and up to 6 error terms, as well
**				as corresponding changes to computation of
**				corrected SCI and ERR values (Version 3.3)
*/

int n_nlincorr (NicInfo *nic, SingleNicmosGroup *input, NlinData *nlin,
		SingleNicmosGroup *zsig) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image to be corrected
**	nlin	 i: nonlinearity reference data
**	zsig	 i: MULTIACCUM zero-read signal image
*/

	/* Local variables */
	int i, j;		/* pixel indexes */
	int nsatpix;		/* number of saturated pixels */
	float sval, sval2;	/* science image values */
	float eval;		/* error image value */
	float c1, c2, c3;	/* coefficient values */
	float e11, e22, e33;	/* coeff variances */
	float e12, e13, e23;	/* coeff covariances */
	float ee;		/* temp error value */
	float n1, n2;		/* node values */

	/* Initialize saturated pixel counter */
	nsatpix = 0;

	/* Loop through science image */
	for (j=0; j < input->sci.data.ny; j++) {
	for (i=0; i < input->sci.data.nx; i++) {

	     /* Get the science and error image values */
	     sval = Pix(input->sci.data,i,j);
	     eval = Pix(input->err.data,i,j);

	     /* Add in the MULTIACCUM zero-read signal (Vsn 3.0), only if
	     ** ZSIG step is turned on, and only for groups other than the
	     ** zeroth-read itself (Vsn 3.2) */
	     if (nic->ZSIG.corr == PERFORM && nic->group != nic->ngroups) {
		 sval += Pix(zsig->sci.data,i,j);
		 if (DQPix(zsig->dq.data,i,j) & ZEROSIG) {
		     DQSetPix(input->dq.data,i,j,
			DQPix(input->dq.data,i,j) | ZEROSIG);
		 }
	     }

	     /* Get the node values for this pixel */
	     n1 = Pix(nlin->nodes[0].data,i,j);
	     n2 = Pix(nlin->nodes[1].data,i,j);

	     /* Propagate the DQ value from the NLIN ref data */
	     DQSetPix(input->dq.data,i,j,
		DQPix(input->dq.data,i,j) | DQPix(nlin->dqual[0].data,i,j));

	     /* If it's already flagged as saturated,
	     ** skip the correction (Vsn 3.0) */
	     if (DQPix(input->dq.data,i,j) & SATURATED) {
		 nsatpix++;

	     /* Apply the correction for the non-linear region */
	     } else if (sval >= n1 && sval <= n2) {

	       c1  = Pix(nlin->coeff[0].data,i,j);
	       c2  = Pix(nlin->coeff[1].data,i,j);
	       c3  = Pix(nlin->coeff[2].data,i,j);
	       e11 = Pix(nlin->error[0].data,i,j);
	       e22 = Pix(nlin->error[1].data,i,j);
	       e12 = Pix(nlin->error[2].data,i,j);
	       e33 = Pix(nlin->error[3].data,i,j);
	       e13 = Pix(nlin->error[4].data,i,j);
	       e23 = Pix(nlin->error[5].data,i,j);

	       /* Compute the new science image pixel value */
	       sval2 = sval * sval;
	       Pix(input->sci.data,i,j) = (c1 + c2*sval + c3*sval2)*sval; 

	       /* Remove the MULTIACCUM zero-read signal (Vsn 3.0), only
	       ** if ZSIG step is turned on, and only for groups other than
	       ** the zeroth-read itself (Vsn 3.2) */
	       if (nic->ZSIG.corr == PERFORM && nic->group != nic->ngroups)
		   Pix(input->sci.data,i,j) -= Pix(zsig->sci.data,i,j);

	       /* Compute the uncertainty on the new value */
	       ee = eval*eval + e11 + sval2*e22 + sval2*sval2*e33 +
		    2 * (fabs(sval)*e12 + sval2*e13 + fabs(sval)*sval2*e23);
	       if (ee >= 0)
		   Pix(input->err.data,i,j) = sqrt (ee);
	       else
		   Pix(input->err.data,i,j) = 0;

	     /* Above the second node, mark the pixel as saturated */
	     } else if (sval > n2) {
	       nsatpix++;
	       DQSetPix(input->dq.data,i,j,
		  DQPix(input->dq.data,i,j) | SATURATED);
	     }
	}}

	/* Report the number of saturated pixels */
	sprintf (MsgText, "NLINCORR detected %d saturated pixels in imset %d",
		 nsatpix, nic->group);
	n_message (MsgText);

	/* Successful return */
	return (status = 0);
}

