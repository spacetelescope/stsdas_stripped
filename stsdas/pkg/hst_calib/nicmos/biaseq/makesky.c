# include <math.h>
# include <float.h>
# include <stdio.h>
# include "biaseq.h"

static void rejhigh (float *, float *, float *, int *, int);
static void rejlow  (float *, float *, float *, int *, int);

/*   MAKESKY  --  Make sky image.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	14-Apr-1999	Implementation.
**
*/

int makeSky (MultiNicmosGroup *fdiff, SingleNicmosGroup *sky, int nskys,
	     int *skysamps, int nlow, int nhigh, short bitmask) {

/* Arguments:
**	fdiff		i: first difference image
**	sky		o: sky image
**	nskys		i: number of sky sample indices
**	skysamps	i: sky sample indices
**	nlow		i: number of low samples to reject
**	nhigh		i: number of high samples to reject
**	bitmask		i: DQ bit mask for pixel rejection
*/

	/* Local variables */
	int i, j, k, q;		/* loop indexes */
	int imset;		/* imset index */
	int npix;		/* number of unrejected pixel samples */
	int nl, nh;		/* number of low/high pixels to reject */
	float flow, fhigh;	/* fraction of low/high pixels to reject */
	float skytime;		/* sky image exposure time */
	float mean, median, mode;	/* image statistics */
	float stdv, min, max;	/* image statistics */
	float sum_sci;		/* sum of sci pixel values */
	float sum_err;		/* sum of err pixel values */
	float sum_tim;		/* sum of tim pixel values */
	float *exptimes;	/* work arrays */
	float *scales;
	float *medians;
	float *zeros;
	float *scivals;
	float *errvals;
	float *timvals;

	/* Function definitions */
	int n_stats (SingleNicmosGroup *, int, int, int, int, float, float,
		     short, float *, float *, float *, float *, float *,
		     float *);
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);

	/* Check for invalid sky sample numbers */
	for (i=0; i < nskys; i++) {
	     if (skysamps[i] < 1 || skysamps[i] >= fdiff->ngroups-1) {
		 sprintf (MsgText, "Sky samples must be within 1 through %d",
			  fdiff->ngroups-1);
		 n_error (MsgText);
		 return (1);
	     }
	}

	/* Allocate memory for local arrays */
	exptimes = NULL;
	scales   = NULL;
	medians  = NULL;
	zeros    = NULL;
	scivals  = NULL;
	errvals  = NULL;
	timvals  = NULL;
	exptimes = (float *) calloc(nskys,sizeof(float));
	scales   = (float *) calloc(nskys,sizeof(float));
	medians  = (float *) calloc(nskys,sizeof(float));
	zeros    = (float *) calloc(nskys,sizeof(float));
	scivals  = (float *) calloc(nskys,sizeof(float));
	errvals  = (float *) calloc(nskys,sizeof(float));
	timvals  = (float *) calloc(nskys,sizeof(float));

	/* Normalize low/high reject params */
	flow  = (float)nlow  / (float)nskys;
	fhigh = (float)nhigh / (float)nskys;

	/* Copy the first input group to the sky image to
	** initialize it. */
	if (n_copyGroup (sky, &(fdiff->group[0])))
	    return (1);

	/* Reset NEXTEND keyword value in sky image header */
	if (putKeyI (sky->globalhdr, "NEXTEND", 5, ""))
	    return (1);

	/* Compile list of exposure times for each sky imset */
	sum_tim = 0.0;
	for (k=0; k < nskys; k++) {
	     imset = skysamps[k] - 1;
	     if (getKeyF(&(fdiff->group[imset].sci.hdr), "DELTATIM",
		 &exptimes[k]))
		 return (1);
	     sum_tim += exptimes[k];
	}

	/* Compute average exposure time of sky image and save it
	** in the sky image header */
	skytime = sum_tim / nskys;
	if (putKeyF (sky->globalhdr, "EXPTIME", skytime, ""))
	    return (1);

	/* Compute normalized imset scale factors */
	for (k=0; k < nskys; k++)
	     scales[k] = skytime / exptimes[k];

	/* Loop over image quadrants */
	for (q=0; q<4; q++) {

	     /* Compute the median of each input image quadrant;
	     ** the medians will be used to set scale factors */
	     for (k=0; k < nskys; k++) {
		  imset = skysamps[k] - 1;
		  if (n_stats (&(fdiff->group[imset]), QXI[q], QXF[q], QYI[q],
			       QYF[q], -FLT_MAX, FLT_MAX, bitmask, &mean,
			       &median, &mode, &stdv, &min, &max))
		      return (1);
		  medians[k] = median;
	     }

	     /* Compute the normalized zeropoints for each image */
	     sum_tim = 0.0;
	     for (k=0; k < nskys; k++)
		  sum_tim += medians[k];
	     sum_tim /= nskys;
	     for (k=0; k < nskys; k++)
		  zeros[k] = scales[k]*medians[k] - sum_tim;

	     /* Loop over quadrant pixels, combining imsets */
	     for (j=QYI[q]; j <= QYF[q]; j++) {
	     for (i=QXI[q]; i <= QXF[q]; i++) {

		  /* Loop over imsets */
		  npix = 0;
		  for (k=0; k < nskys; k++) {
		       imset = skysamps[k] - 1;

		       /* Accumulate unflagged samples into work arrays */
		       if (!(DQPix(fdiff->group[imset].dq.data,i,j)&bitmask) &&
			   !(DQPix(fdiff->group[imset+1].dq.data,i,j)&bitmask)){

			   scivals[npix] =
				Pix(fdiff->group[imset].sci.data,i,j) *
				scales[k] - zeros[k];
			   errvals[npix] =
				Pix(fdiff->group[imset].err.data,i,j) *
				scales[k] - zeros[k];
			   errvals[npix] *= errvals[npix];
			   timvals[npix] =
				(Pix(fdiff->group[imset].intg.data,i,j) -
				 Pix(fdiff->group[imset+1].intg.data,i,j)) *
				scales[k];
			   npix++;
		       }
		  }

		  /* Compute number of high/low samples to reject */
		  nh = (int)(fhigh * npix + 0.001);
		  nl = (int)(flow  * npix + 0.001);

		  /* Reject high samples */
		  rejhigh (scivals, errvals, timvals, &npix, nh);

		  /* Reject low samples */
		  rejlow  (scivals, errvals, timvals, &npix, nl);

		  /* Compute sums of unrejected samples */
		  sum_sci = 0.0;
		  sum_err = 0.0;
		  sum_tim = 0.0;
		  for (k=0; k < npix; k++) {
		       sum_sci += scivals[k];
		       sum_err += errvals[k];
		       sum_tim += timvals[k];
		  }

		  /* Compute averages of unrejected samples and
		  ** and store in output sky image extensions. */
		  if (npix > 0) {
		      Pix(sky->sci.data,i,j) = sum_sci / npix;
		      Pix(sky->err.data,i,j) = sqrt(sum_err) / npix;
		      DQSetPix(sky->dq.data,i,j,0);
		      Pix(sky->smpl.data,i,j) = npix;
		      Pix(sky->intg.data,i,j) = sum_tim / npix;
		  } else {
		      Pix(sky->sci.data,i,j) = 0.0;
		      Pix(sky->err.data,i,j) = 0.0;
		      DQSetPix(sky->dq.data,i,j,256);
		      Pix(sky->smpl.data,i,j) = 0;
		      Pix(sky->intg.data,i,j) = 0.0;
		  }
	     }
	     } /* End of loop over quadrant pixels */
	} /* End of loop over quadrants */

	/* Free local memory */
	free (exptimes);
	free (scales);
	free (medians);
	free (zeros);
	free (scivals);
	free (errvals);
	free (timvals);

	return (0);
}

static void rejhigh (float *sci, float *err, float *tim, int *npix, int nhigh) {

	int k, kk;
	int imax;
	float dmax;

	if (nhigh > 0) {
	    for (k=0; k < nhigh; k++) {
		 if (*npix > 1) {
		     dmax = sci[0]; imax = 0;
		     for (kk=1; kk < *npix; kk++) {
			  if (sci[kk] > dmax) {
			      dmax = sci[kk];
			      imax = kk;
			  }
		     }

		     if (imax < (*npix)-1) {
			 for (kk=imax; kk < *npix-1; kk++) {
			      sci[kk] = sci[kk+1];
			      err[kk] = err[kk+1];
			      tim[kk] = tim[kk+1];
			 }
		     }
		     (*npix)--;
		 }
	    }
	}

}

static void rejlow (float *sci, float *err, float *tim, int *npix, int nlow) {

	int k, kk;
	int imin;
	float dmin;

	if (nlow > 0) {
	    for (k=0; k < nlow; k++) {
		 if (*npix > 1) {
		     dmin = sci[0]; imin = 0;
		     for (kk=1; kk < *npix; kk++) {
			  if (sci[kk] < dmin) {
			      dmin = sci[kk];
			      imin = kk;
			  }
		     }

		     if (imin < (*npix)-1) {
			 for (kk=imin; kk < *npix-1; kk++) {
			      sci[kk] = sci[kk+1];
			      err[kk] = err[kk+1];
			      tim[kk] = tim[kk+1];
			 }
		     }
		     (*npix)--;
		 }
	    }
	}

}

