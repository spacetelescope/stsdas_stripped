# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_COMBNITER: Combine the NUMITER images at each pattern position.
** The first image in the association member list for a given pattern
** position is considered the "reference" image for that position. Offsets
** of other images at that position are measured relative to the reference
** image and all images are shifted to the coordinates of the reference
** image before combining. The resulting combined image replaces the
** input data for the reference image. Other images at that position
** have their pattern positions reset to zero to indicate to subsequent
** routines that they are not to be used.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	29-Apr-1997	Pass in NicInfo; call n_compOffset only for
**				external (EXT) images (Version 2.0)
** H.Bushouse	01-Dec-1997	Added AsnInfo to n_xcorr and n_aerrclip
**                              argument lists for use of xcwin and crthresh
**				parameters (Version 2.2)
** H.Bushouse	17-Mar-1998	Added use of asn->readoffs to skip calculation
**				of image offsets (Version 2.2)
** H.Bushouse	03-Jun-1998	Added call to n_idSources to reflag sources in
**				the combined images (Version 2.2)
** H.Bushouse	04-Apr-2000	Only call n_idSources if obs_type is EXTernal
**				(Version 2.4)
*/

int n_combNiter (NicInfo *nic, AsnInfo *asn, AsnImages *input) {

/* Arguments:
**	nic	 i: observation info structure
**	asn	 i: association info structure
**	input	io: input images
*/

	/* Local variables */
	int i, j, k, npos;		/* loop indexes */
	int nref;			/* index of reference image */
	char history[72+1];		/* history card image */

	/* Function definitions */
	int n_compOffset (MemberInfo *, MemberInfo *);
	int n_xcorr (AsnInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
		      float *, float *);
	int n_imcomb (AsnInfo *, AsnImages *, int, int);
	int n_idSources (SingleNicmosGroup *);

	/* Loop through the pattern positions,
	** combining the NUMITER images at each position */

	for (npos = 1; npos <= asn->numpos; npos++) {

	     sprintf (MsgText, "Combining %d images at position %d:",
		      asn->posmems[npos-1], npos);
	     n_message (MsgText);

	     if (asn->readoffs) {
		 sprintf (MsgText, "Using image offsets from ASN table ...");
		 n_message (MsgText);
	     }

	     /* Find the first image at this position */
	     k = 0;
	     while (k < asn->nmembers && asn->member[k].patpos != npos) k++;
	     nref = k;
                  
	     /* Loop over all the assoc members */
	     for (k = 0; k < asn->nmembers; k++) {

		  /* Compute offsets of each image at this pattern position
		  ** relative to the reference image. This is done only for
		  ** external observations, and only if offsets have not been
		  ** read from the input assoc table. */
		  if (nic->obs_type == EXT && !asn->readoffs) {
		      if (asn->member[k].patpos == npos && k != nref) {

			  /* First compute offsets based on WCS information */
			  if (n_compOffset (&(asn->member[k]),
					    &(asn->member[nref])))
			      return (status);

			  /* Now refine the registration using
			  ** cross-correlation */
			  if (n_xcorr (asn, &(input->member[k]),
				&(input->member[nref]), &(asn->member[k].dx),
				&(asn->member[k].dy)))
			      return (status);
		      }
		  }

		  /* Report the image offsets */
		  if (asn->member[k].patpos == npos) {
		      sprintf (MsgText, "  %s  dx=%7.2f  dy=%7.2f",
			       asn->member[k].name, asn->member[k].dx,
			       asn->member[k].dy);
		      n_message (MsgText);
		  }
	     }
        
	     /* Combine the images at this pattern position */
	     if (n_imcomb (asn, input, npos, nref))
		 return (status);

	     /* Reflag sources in the combined image. This is only
	     ** done for external observations. */
	     if (asn->posmems[npos-1] > 0 && nic->obs_type == EXT) {
		 /* First, remove any existing source flags in the DQ image */
		 for (j = 0; j < input->member[nref].sci.data.ny; j++) {
		      for (i = 0; i < input->member[nref].sci.data.nx; i++) {
			   if (DQPix(input->member[nref].dq.data,i,j) & SOURCE)
			       DQSetPix(input->member[nref].dq.data,i,j,
				DQPix(input->member[nref].dq.data,i,j)-SOURCE);
		      }
		 }

		 /* Now do a fresh search for sources */
		 if (n_idSources (&(input->member[nref])))
		     return (status);
	     }
         
	     /* Do some housekeeping */
	     for (k = 0; k < asn->nmembers; k++) {

		  /* Add history records to reference image */
		  if (asn->member[k].patpos == npos && k == nref) {
		      sprintf (history, "Combined images:");
		      if (addHistoryKw (&(input->member[k].sci.hdr),
					history) == -1)
			  return (status = 1);
		      sprintf (history, "  %s", input->member[k].filename);
		      if (addHistoryKw (&(input->member[k].sci.hdr),
					history) == -1)
			  return (status = 1);
		  }

		  /* Reset the patpos and mospos indexes for images that are
		  ** no longer needed and free the memory for those images */
		  if (asn->member[k].patpos == npos && k != nref) {

		      sprintf (history, "  %s", input->member[k].filename);
		      if (addHistoryKw (&(input->member[nref].sci.hdr),
					history) == -1)
			  return (status = 1);

		      asn->member[k].patpos = 0;
		      asn->mosmems[asn->member[k].mospos-1]--;
		      asn->member[k].mospos = 0;
		      freeSingleNicmosGroup(&(input->member[k]));
		  }
	     }

	} /* end of loop over pattern positions */

	/* Successful return */
	return (status = 0);
}

/* N_IMCOMB: Combine images at a given pattern position. */

int n_imcomb (AsnInfo *asn, AsnImages *input, int npos, int nref){

	int i, j, k;			/* loop indexes */
	int nsamp;			/* number of samples */
	float *sci;			/* sci sample vector */
	float *err;			/* err sample vector */
	int   *dq;			/* dq sample vector */
	float *time;			/* time sample vector */
	float mean, stdv, efftime;	/* combine results */
	int badpix, ngood;		/* combine results */
	float x, y;			/* pixel coords */

	/* Function definitions */
	int  n_bilin_grp (SingleNicmosGroup *, float, float, float *, float *,
			  int *, float *);
	void n_aerrclip (AsnInfo *, int, float *, float *, int *, float *,
			 float *, float *, int *, int *, float *,int,int);

	/* Make sure we need to do any combining */
	if (asn->posmems[npos-1] < 2)
	    return (status = 0);

	/* Allocate memory for local arrays */
	sci  = NULL;
	err  = NULL;
	dq   = NULL;
	time = NULL;
	sci  = (float *) calloc (asn->posmems[npos-1], sizeof(float));
	err  = (float *) calloc (asn->posmems[npos-1], sizeof(float));
	dq   =   (int *) calloc (asn->posmems[npos-1], sizeof(int));
	time = (float *) calloc (asn->posmems[npos-1], sizeof(float));

	/* Loop through the 2d image array, accumulating
	** samples at each pixel */

	for (j = 0; j < input->member[0].sci.data.ny; j++) {
	for (i = 0; i < input->member[0].sci.data.nx; i++) {

	     /* Use only the images belonging to this pattern position */
	     nsamp = 0;
	     for (k = 0; k < asn->nmembers; k++) {
		  if (asn->member[k].patpos == npos) {

		      /* Compute the image coordinates */
		      x = i - asn->member[k].dx;
		      y = j - asn->member[k].dy;

		      /* Make sure the coords lie within the image */
		      if (x >= 0 && x <= input->member[k].sci.data.nx-1 &&
			  y >= 0 && y <= input->member[k].sci.data.ny-1) {

			  /* Compute the interpolated image value at
			  ** these coordinates */
			  if (n_bilin_grp (&(input->member[k]), x, y,
			      &(sci[nsamp]), &(err[nsamp]),
			      &(dq[nsamp]), &(time[nsamp])))
			      return (status);
			  nsamp++;
		      }
		  }
	     }

	     /* Combine the samples for this pixel */
	     n_aerrclip (asn, nsamp, sci, err, dq, time, &mean, &stdv, &badpix,
			 &ngood, &efftime,i,j);

	     /* Replace the input data with the combined values */
	     Pix(input->member[nref].sci.data,i,j) = mean;
	     Pix(input->member[nref].err.data,i,j) = stdv;
	     DQSetPix(input->member[nref].dq.data,i,j,badpix);
	     Pix(input->member[nref].smpl.data,i,j) = ngood;
	     Pix(input->member[nref].intg.data,i,j) = efftime;

	} /* end of loop over x */
	} /* end of loop over y */

	/* Free memory for local arrays */
	free (sci);
	free (err);
	free (dq);
	free (time);

	/* Successful return */
	return (status = 0);

}
