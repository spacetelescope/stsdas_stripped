# include <stdio.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOZSIG: Calls ZSIGCORR calibration step for a MultiAccum science image.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
** H.Bushouse	14-Aug-2002	Bug fix: set nic->group to 1st read so that
**				it has a valid value when used in the
**				getDynamicDarkImage routine (v4.0.1)
*/

int n_doZsig (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *mask,
	      SingleNicmosGroup *nois, SingleNicmosGroup *dark, NlinData *nlin,
	      SingleNicmosGroup *zsig) {

/* Arguments:
**	nic	i: NICMOS info structure
**	input	i: input MultiAccum image
**	mask	i: bad pixel mask image
**	nois	i: read noise image
**	dark	i: dark current image
**	nlin	i: non-linearity correction data
**	zsig	o: zero-read signal image
*/

	/* Local variables */

	/* Function definitions */
	int n_getDarkImage (NicInfo *, SingleNicmosGroup *, int);
	int n_zsigcorr (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *,
			SingleNicmosGroup *, SingleNicmosGroup *, NlinData *,
			SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);
	
        /* Calculate signal in zeroth read */
	if (nic->ZSIG.corr == PERFORM) {

	  /* First retrieve dark image for 1st read, which is needed
	  ** for the zsigcorr processing */
          nic->group = nic->ngroups-1;		/* set group num to 1st read */
	  if (n_getDarkImage (nic, dark, nic->ngroups-1)) {
	      printf ("Error in n_getDarkImage, status = %d\n", status);
	      return (status);
	  }

	  if (n_zsigcorr (nic, input, mask, nois, dark, nlin, zsig)) {
	      printf ("Error in n_zsigcorr, status = %d\n", status);
              return (status);
	  }
	}
 
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {
	     n_calReport (&nic->ZSIG, nic->group,
			  &input->group[nic->group-1].sci.hdr,
 			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);

}

# define ZTHRESH 5.0	/* default signal clipping threshold (sigma) */

/* N_ZSIGCALC: Calculate the amount of signal from sources in a MultiAccum
** zeroth-read image. This is done by computing the amount of signal that
** arrived between the zeroth and first reads, and then scaling that to the
** exposure time of the zeroth read in order to estimate how much signal
** probably came in between the reset and the zeroth read. Note that this
** does not work well if the source flux is so high that it's beginning to
** go non-linear already in either the zeroth or first read. The raw data
** values in the zeroth and first readouts are also differenced with the
** super zero read reference image and checked for saturation in those
** readouts.
**
** Revision history:
** H.Bushouse	25-Jul-1997	Created (Version 3.0)
** H.Bushouse	18-Aug-1997	Modified to use super zero read image in
**				NlinData structure (Version 3.0)
** H.Bushouse	11-Sep-1997	Added use of nic.zsthresh to allow user input
**				of threshold value (Version 3.0)
** H.Bushouse	28-Oct-1997	Added checks for dummy ref data before
**				doing computations (Version 3.0)
** H.Bushouse	12-Feb-1998	Added computation of valid zeroth-read value
**				for pixels saturated in first read; removed use
**				of ZTIME macro, replacing it with new
**				nic->sampzero variable; added use of new
**				ZEROSIG DQ value; add zsig value to zeroth-read
**				input image; changed name from n_zsigcalc to
**				n_zsigcorr (Version 3.2)
** H.Bushouse	08-Apr-1998	Added use of new n_pixOK function (Version 3.2)
** H.Bushouse	06-Oct-1998	Removed checks for dummy ref data since it's
**				now handled in n_getRefData; changed n_noiscalc
**				to use dark ref image as argument (Version 3.3)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
*/

int n_zsigcorr (NicInfo *nic, MultiNicmosGroup *input,
		SingleNicmosGroup *mask, SingleNicmosGroup *nois,
		SingleNicmosGroup *dark, NlinData *nlin,
		SingleNicmosGroup *zsig) {

/* Arguments:
**	nic	i: NICMOS info structure
**	input	i: input images
**	mask	i: bad pixel mask image
**	nois	i: read noise image
**	dark	i: dark current image
**	nlin	i: non-linearity correction data
**	zsig	o: zero-read signal image
*/

	/* Local variables */
	int i, j;			/* pixel indexes */
	int nsat0, nsat1;		/* number of saturated pixels */
	float zscale;			/* exposure time scale factor */

	/* Function definitions */
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_asub (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_aor  (SingleNicmosGroup *, SingleNicmosGroup *);
	int n_Nois  (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
		     SingleNicmosGroup *);
	Bool n_pixOK (SingleNicmosGroup *, int, int);

	/* Initialize counters */
	nsat0 = 0; nsat1 = 0;

	/* Set proper threshold value; use default if no user input */
	if (nic->zsthresh == 0)
	    nic->zsthresh = ZTHRESH;

	/* Copy first read to zsig image */
	if (n_copyGroup (zsig, &(input->group[nic->ngroups-2]))) {
	    printf ("Error in n_copyGroup, status = %d\n", status);
	    return (status);
	}
	/* Subtract the zeroth read from the first read */
	n_asub (zsig, &(input->group[nic->ngroups-1]));

	/* Mask bad pixels in the zsig image */
	n_aor (zsig, mask);

	/* Compute noise in the zsig image */
	nic->group = nic->ngroups - 1;
	if (n_Nois (nic, zsig, nois, dark))
	    return (status);

	/* Subtract dark current from the zsig image */
	n_asub (zsig, dark);

	/* Compute the scale factor to match the zeroth-read exptime */
	zscale = nic->sampzero / nic->exptime[nic->ngroups-2];

	/* Loop over the zsig image */
	for (j=0; j < zsig->sci.data.ny; j++) {
	for (i=0; i < zsig->sci.data.nx; i++) {

	     /* Only look at good pixels */
	     if (n_pixOK(zsig,i,j)) {

		 /* Scale pixels with greater than zsthresh*err signal to
		 ** the exposure time of the zeroth read */
		 if (Pix(zsig->sci.data,i,j) >=
		     nic->zsthresh*Pix(zsig->err.data,i,j)) {
		     Pix(zsig->sci.data,i,j) *= zscale;
		     DQSetPix(zsig->dq.data,i,j,
			DQPix(zsig->dq.data,i,j) | ZEROSIG);
		     DQSetPix(input->group[nic->ngroups-1].dq.data,i,j,
			DQPix(input->group[nic->ngroups-1].dq.data,i,j) |
			ZEROSIG);

		 /* Mask out low signal pixels by setting them to zero */
		 } else
		     Pix(zsig->sci.data,i,j) = 0.0;

		 /* Flag pixels that are saturated in the zeroth read */    
		 if (Pix(input->group[nic->ngroups-1].sci.data,i,j) -
		     Pix(nlin->zsci[0].data,i,j) >
		     Pix(nlin->nodes[1].data,i,j)) {
		     DQSetPix(zsig->dq.data,i,j,
			DQPix(zsig->dq.data,i,j) | SATURATED);
		     DQSetPix(input->group[nic->ngroups-1].dq.data,i,j,
			DQPix(input->group[nic->ngroups-1].dq.data,i,j) |
			SATURATED);
		     nsat0++;
		 }

		 /* Flag pixels that are saturated in the first read */
		 if (Pix(input->group[nic->ngroups-2].sci.data,i,j) -
		     Pix(nlin->zsci[0].data,i,j) >
		     Pix(nlin->nodes[1].data,i,j)) {
		     DQSetPix(zsig->dq.data,i,j,
			DQPix(zsig->dq.data,i,j) | SATURATED);
		     DQSetPix(input->group[nic->ngroups-2].dq.data,i,j,
			DQPix(input->group[nic->ngroups-2].dq.data,i,j) |
			SATURATED);
		     nsat1++;

		     /* For these pixels, replace the zsig value by
		     ** differencing with the super-zero read image */
		     Pix(zsig->sci.data,i,j) =
			 Pix(input->group[nic->ngroups-1].sci.data,i,j) -
			 Pix(nlin->zsci[0].data,i,j);
		 }

	     /* Set flagged pixels to zero in the zsig image */
	     } else
		 Pix(zsig->sci.data,i,j) = 0.0;
	}}

	/* Report the number of saturated pixels detected */
	sprintf (MsgText,
		 "ZSIGCORR detected %d saturated pixels in 0th read", nsat0);
	n_message (MsgText);
	sprintf (MsgText,
		 "ZSIGCORR detected %d saturated pixels in 1st read", nsat1);
	n_message (MsgText);

	/* Add zsig image values to zeroth-read image of input data (Vsn 3.2) */
	for (j=0; j < zsig->sci.data.ny; j++) {
	     for (i=0; i < zsig->sci.data.nx; i++) {
		  Pix(input->group[nic->ngroups-1].sci.data,i,j) +=
			Pix(zsig->sci.data,i,j);
	     }
	}
	
	/* Successful return */
	return (status = 0);

}

