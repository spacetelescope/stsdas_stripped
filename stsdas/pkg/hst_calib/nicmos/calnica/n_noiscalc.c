# include <math.h>
# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DONOIS: Call NOISCALC for each readout of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doNois (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *nois,
	      SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
**	nois	 i: detector readnoise image
**	dark	 i: dark current image
*/

	/* Local variables */

	/* Function definitions */
	int n_getDarkImage (NicInfo *, SingleNicmosGroup *, int);
	int n_noiscalc (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
			SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the noise calculation for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->NOIS.corr == PERFORM) {
		 if (n_getDarkImage (nic, dark, nic->group))
		     return (status);
		 if (n_noiscalc (nic, &(input->group[nic->group-1]), nois,
				 dark))
		     return (status);
	     }

	     n_calReport (&nic->NOIS, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);
}

/* N_NOISCALC: Perform noise calculation (populate ERR image) for 
** NICMOS data groups.
**
** For non-RAMP mode images, the ERR image is computed from the combined
** detector readnoise and photon counting errors (Poisson noise). A special
** correction is made for removing the shading signal contribution from the
** science image pixel values before computing the Poisson noise because the
** shading signal is noiseless.
**
** For RAMP mode images, the downlinked variance image is
** remapped into the ERR image.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	06-Oct-1998	Modified n_Nois to use dark ref image to
**				compute and subtract shading signal from
**				science image before computing noise (Vsn 3.3)
*/

int n_noiscalc (NicInfo *nic, SingleNicmosGroup *input, SingleNicmosGroup *nois,
		SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
**	nois	 i: detector readnoise image
**	dark	 i: dark current image
*/

	/* Function definitions */
	int n_RampNois (SingleNicmosGroup *);
	int n_Nois     (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
			SingleNicmosGroup *);

	/* Do the noise calculations */
	if (nic->obsmode == RAMP) {

	    /* Remap the RAMP variance data into a noise image */
	    if (n_RampNois (input))
		return (status);

	} else {

	    /* Compute a noise image */
	    if (n_Nois (nic, input, nois, dark))
		return (status);
	}

	/* Successful return */
	return (status = 0);
}

/* N_RAMPNOIS: Remap RAMP-mode variance data into a noise image.
** This is accomplished by simply taking the square-root of the
** variance data. The input ERR image is modified in-place.
*/

int n_RampNois (SingleNicmosGroup *input) {

/* Arguments:
**	input	io: input image
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	float variance;		/* variance value */

	/* Take the square-root of the variance array */
	for (j=0; j < input->err.data.ny; j++) {
	     for (i=0; i < input->err.data.nx; i++) {

		  variance = Pix(input->err.data,i,j);

		  if (variance >= 0.0) {
		      Pix(input->err.data,i,j) = sqrt(variance);
		  } else {
		      sprintf (MsgText, 
			      "NOISCALC: Negative value in input ERR image");
		      n_error (MsgText);
		      return (status = 1);
		  }
	     }
	}

	/* Successful return */
	return (status = 0);
}

# define  NIC1_DARK	0.03	/* mean dark current rate (in electrons/sec) */
# define  NIC2_DARK	0.05 	/* mean dark current rate (in electrons/sec) */
# define  NIC3_DARK	0.03 	/* mean dark current rate (in electrons/sec) */
# define  BY_COL	1
# define  BY_ROW	2

/* N_NOIS: Compute a noise image for a NICMOS data group by combining
** the detector readnoise and the Poisson noise in the science image.
** The input ERR image is modified in-place. The DQ values from the
** NOISFILE are propagated into the input DQ array. The input SCI, SAMP,
** and TIME arrays are unchanged.
**
** NOTE: Routine assumes that the values in the readnoise reference
** image are in units of ELECTRONS (not DNs). The noise calculation is
** performed in units of electrons and then converted back to DNs.
*/

int n_Nois (NicInfo *nic, SingleNicmosGroup *input, SingleNicmosGroup *nois,
	    SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image
**	nois	 i: detector readnoise image
**	dark	 i: detector dark current image
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	float noise;		/* readnoise value */
	float signal;		/* science image value */
	float dark_val;		/* average dark value (in DN) */
	int shad_dir;		/* constant shading direction (column or row) */
	float *shad_ave;	/* average shading value for a column or row */
	int shad_num;		/* number of pixels in shading average */
	int bad_line;		/* bad column/row index */

	/* Allocate local memory for the shading signal data */
	shad_ave = (float *) calloc(SZ_NICIMG, sizeof(float));

	/* Set the shading direction and convert dark current to DN */
	if (nic->camera == 1) {
	    shad_dir = BY_ROW;
	    dark_val = NIC1_DARK * nic->exptime[nic->group-1] / nic->adcgain;
	    bad_line = 127;
	} else if (nic->camera == 2) {
	    shad_dir = BY_COL;
	    dark_val = NIC2_DARK * nic->exptime[nic->group-1] / nic->adcgain;
	    bad_line = 127;
	} else if (nic->camera == 3) {
	    shad_dir = BY_COL;
	    dark_val = NIC3_DARK * nic->exptime[nic->group-1] / nic->adcgain;
	    bad_line = 128;
	}

	/* Compute the average shading signature for each column or row */
	if (shad_dir == BY_COL) {
	    for (i = 0; i < dark->sci.data.nx; i++) {
		 shad_ave[i] = 0; shad_num = 0;
		 for (j = bad_line-3; j <= bad_line+3; j++) {
		      if (!(DQPix(input->dq.data,i,j) & DEFECTIVE) &&
			  j != bad_line) {
			  shad_ave[i] += Pix(dark->sci.data,i,j) - dark_val;
			  shad_num++;
		      }
		 }
		 if (shad_num > 0)
		     shad_ave[i] /= shad_num;
	    }
	} else if (shad_dir == BY_ROW) {
	    for (j = 0; j < dark->sci.data.ny; j++) {
		 shad_ave[j] = 0; shad_num = 0;
		 for (i = bad_line-3; i <= bad_line+3; i++) {
		      if (!(DQPix(input->dq.data,i,j) & DEFECTIVE) &&
			  i != bad_line) {
			  shad_ave[j] += Pix(dark->sci.data,i,j) - dark_val;
			  shad_num++;
		      }
		 }
		 if (shad_num > 0)
		     shad_ave[j] /= shad_num;
	    }
	}

	/* Combine (in quadrature) the detector readnoise and the
	** photon noise for each pixel. */
	for (j=0; j < input->err.data.ny; j++) {
	     for (i=0; i < input->err.data.nx; i++) {

		  noise  = Pix(nois->sci.data, i,j);  /* read noise (in e)    */
		  signal = Pix(input->sci.data,i,j);  /* photon noise (in DN) */
  
		  if (shad_dir == BY_COL)
		      signal -= shad_ave[i];       /* subtract shading signal */
  		  else
		      signal -= shad_ave[j];

		  noise = sqrt (noise*noise + fabs(signal*nic->adcgain));
		  Pix(input->err.data,i,j) = noise / nic->adcgain;

		  /* Propagate the NOISFILE DQ values */
		  DQSetPix(input->dq.data,i,j,
		     DQPix(input->dq.data,i,j) | DQPix(nois->dq.data,i,j) );
	     }
	}

	/* Free local memory */
	free (shad_ave);

	/* Successful return */
	return (status = 0);
}

