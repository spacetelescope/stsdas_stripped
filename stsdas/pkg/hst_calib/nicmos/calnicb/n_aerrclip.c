/* This version of n_aerrclip and wtmean compute the clipped mean and
** uses the clipped mean in the rejection test. This version differs
** from the others in that only the largest outlier is rejected before
** reiterating.
** It also computes the simple sum of the integration times of all
** samples, instead of the weighted average.
** Furthermore, this version of wtmean will compute the unweighted mean
** if all the input error values are zero.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	04-Feb-1997	Modified n_wtmean to compute clipped mean;
**				modified rejection test in n_aerrclip to use
**				clipped mean (Version 2.0)
** H.Bushouse	07-Feb-1997	Modified n_wtmean to compute sum of exposure
**				times, not weighted mean (Version 2.0)
** H.Bushouse	24-Feb-1997	Modified n_wtmean to compute unweighted mean if
**				all input ERR values are zero (Version 0.1.5)
** H.Bushouse	25-Feb-1997	Modified n_wtmean to fix bug in logic used to
**				find min/max SCI values (Version 2.0)
** H.Bushouse	30-Apr-1997	Modified to only reject largest outlier before
**				reiterating (Version 2.0)
** H.Bushouse	02-Dec-1997	Pass in AsnInfo for use of crthresh; use pixel
**                              masks in "if" statements, rather than as a
**                              multiplicative mask (Version 2.2)
** H.Bushouse	02-Jun-1998	Added special handling for new ZEROSIG DQ flag
**				value (Version 2.2)
*/

# include <math.h>
# include <stdlib.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define THRESH 5.0     /* default sigma threshold for rejection */

/* N_AERRCLIP: Average a set of pixel values, rejecting outliers based
** on their error values.
*/

void n_aerrclip (AsnInfo *asn, int nsamp, float *sci, float *err, int *dq,
		 float *time, float *mean, float *stdv, int *badpix, int *ngood,
		 float *efftime, int i, int j) {

/* Arguments:
**	asn	i: association info structure
**	nsamp	i: number of samples
**	sci	i: input SCI pixel values
**	err	i: input ERR pixel values
**	dq	i: input DQ  pixel values
**	time	i: input TIME pixel values
**	mean	o: mean value
**	stdv	o: error in the mean
**	badpix	o: output dq value
**	ngood	o: number of good samples used
**	efftime o: total exposure time of samples used
*/

	/* Local variables */
	int k;			/* loop index */
	int nrej;		/* number of rejected samples */
	float diff;		/* difference value */
	float cmean;		/* clipped mean */
	int *mask;		/* mask vector */
	int maxdiffk;		/* sample index of maximum outlier */
	float maxdiff;		/* difference value of maximum outlier */

	/* Function definitions */
	void n_wtmean (float *, float *, float *, int *, int, float *, float *,
		       float *, float *);

	/* Set CR reject threshold; use default if no user input */
	if (asn->crthresh == 0)
	    asn->crthresh = THRESH;

	/* Initialize the returned values */
	*mean    = 0;
	*stdv    = 0;
	*badpix  = 0;
	*ngood   = 0;
	*efftime = 0;

	/* Check for trivial cases */
	if (nsamp == 0)
	    return;
	else if (nsamp == 1) {
	    if (dq[0] == 0 || dq[0] == SOURCE || dq[0] == ZEROSIG ||
		dq[0] == SOURCE + ZEROSIG) {
		*mean = sci[0];
		*stdv = err[0];
		*ngood = 1;
		*efftime = time[0];
		*badpix = dq[0];
	    } else {
		*badpix = dq[0];
	    }
	    return;
	}

	/* Initialize the mask vector from the input DQ values */
	mask = (int *)calloc(nsamp, sizeof(int));
	for (k = 0; k < nsamp; k++) {
	     if (dq[k] == 0 || dq[k] == SOURCE || dq[k] == ZEROSIG ||
		 dq[k] == SOURCE + ZEROSIG)
		 mask[k] = 1;
	     else
		 mask[k] = 0;

	     (*badpix) = (*badpix) | dq[k];
	}

	/* If there are only 2 samples, just compute the mean with
	** exclusion of flagged pixels */
	if (nsamp == 2) {
	    n_wtmean (sci, err, time, mask, nsamp, mean, stdv, efftime, &cmean);

	/* If there are 3-4 samples, compute the mean with exclusion
	** of flagged pixels and one iteration of rejecting outliers */
	} else if (nsamp <= 4) {

	    n_wtmean (sci, err, time, mask, nsamp, mean, stdv, efftime, &cmean);

	    /* Find the largest outlier */
	    maxdiff = 0;
	    for (k=0; k<nsamp; k++) {
		 if (err[k] > 0 && mask[k] > 0)
		     diff = fabs(sci[k]-cmean) / err[k];
		 else
		     diff = 0;

		 if (diff > maxdiff) {
		     maxdiff = diff;
		     maxdiffk = k;
		 }
	    }

	    /* Reject the largest outlier, if it's above the threshold,
	    ** and recompute the mean. */
	    if (maxdiff > asn->crthresh) {
		mask[maxdiffk] = 0;
		n_wtmean (sci, err, time, mask, nsamp, mean, stdv, efftime,
			  &cmean);
	    }

	/* If there are more than 4 samples, compute the mean with
	** exclusion of flagged pixels and iteratively reject outliers */
	} else {
	    nrej = 1;
	    while (nrej) {

		/* Compute the mean and stdv */
		n_wtmean (sci, err, time, mask, nsamp, mean, stdv, efftime,
			  &cmean);

		/* Find the largest outlier */
		nrej = 0;
		maxdiff = 0;
		for (k=0; k<nsamp; k++) {
		     if (err[k] > 0 && mask[k] > 0)
		         diff = fabs(sci[k]-cmean) / err[k];
		     else
			 diff = 0;

		     if (diff > maxdiff) {
			 maxdiff = diff;
			 maxdiffk = k;
		     }
		}

		/* Reject the largest outlier, if it's above the threshold */
		if (maxdiff > asn->crthresh) {
		    nrej++;
		    mask[maxdiffk] = 0;
		}

	    } /* go back and recompute mean and stdv */
	}

	/* Add up the number of samples used */
	for (k=0; k<nsamp; k++)
	     *ngood   += mask[k];

	/* If at least one good sample was found, reset the output DQ */
	if (*ngood > 0) {
	    dq[0] = 0;
	    if (*badpix & ZEROSIG)
		dq[0] += ZEROSIG;
	    if (*badpix & SOURCE)
		dq[0] += SOURCE;
	    *badpix = dq[0];
	}

	free (mask);
}

/* N_WTMEAN: Compute the error-weighted mean of pixel values.
** Both the clipped mean (which excludes the minimum and maximum values)
** and the full mean are computed. The error in the full mean is also
** computed.
*/

void n_wtmean (float *sci, float *err, float *time, int *mask, int nsamp,
	       float *mean, float *stdv, float *efftime, float *cmean) {

/* Arguments:
**	sci	i: input SCI pixel values
**	err	i: input ERR pixel values
**	time	i: input TIME pixel values
**	mask	i: mask vector
**	nsamp	i: number of input values
**	mean	o: error-weighted mean
**	stdv	o: uncertainty in the mean
**	efftime	o: effective exposure time
**	cmean	o: clipped mean
*/

	/* Local variables */
	int k;				/* loop index */
	int ngood;			/* number of good samples */
	int nerr;			/* number of good error values */
	int mink, maxk;			/* min/max value indexes */
	double sums, sumt, sumw;	/* sum of sci, time, and weight */
	double wt;			/* weight value */

	/* Initialize returned values */
	*mean    = 0;
	*stdv    = 0;
	*efftime = 0;
	*cmean   = 0;

	/* Initialize accumulators */
	sums = 0;
	sumt = 0;
	sumw = 0;

	/* Find out how many good samples and error values there are */
	ngood = 0;
	nerr  = 0;
	for (k=0; k<nsamp; k++) {
	     if (mask[k] > 0) {
		 ngood++;
		 if (err[k] > 0)
		     nerr++;
	     }
	}

	/* If there are no good samples, just return */
	if (ngood == 0)
	    return;

	/* If there are no good error values, compute unweighted mean */
	if (nerr == 0) {
	    for (k=0; k<nsamp; k++) {
		 if (mask[k] > 0) {
		     sums += sci[k];
		     sumt += time[k];
		     sumw += 1.0;
		 }
	    }
	    *mean    = sums / sumw;
	    *efftime = sumt;
	    *stdv    = sqrt (1.0/sumw);
	    *cmean   = *mean;
	    return;
	}

	/* Handle 2 or fewer good samples as a special case */
	if (ngood <= 2) {

	    /* Compute the weighted sum of the non-masked pixel values,
	    ** integration times, and the sum of the weights */
	    for (k=0; k<nsamp; k++) {
		 if (mask[k] > 0 && err[k] != 0) {
		     wt    = 1.0 / (err[k]*err[k]);
		     sums += sci[k]  * wt;
		     sumt += time[k];
		     sumw += wt;
		 }
	    }

	    /* Compute the weighted means and error */
	    if (sumw > 0) {
		*mean    = sums / sumw;
		*efftime = sumt;
		*stdv    = sqrt (1.0/sumw);
	    } else {
		*mean    = 0;
		*efftime = 0;
		*stdv    = 0;
	    }

	    /* For this special case set the clipped mean equal to the mean */
	    *cmean = *mean;

	/* This section for 3 or more good samples */
	} else {

	    /* Initialize the min and max to the first good sample */
	    for (k=0; k<nsamp && (mask[k]==0 || err[k]==0); k++) ;
	    mink = k; maxk = k;

	    /* Find the min and max of the good samples */
	    for (k=0; k<nsamp; k++) {
		 if (mask[k] > 0 && err[k] != 0) {
		     if (sci[k] < sci[mink]) mink = k;
		     if (sci[k] > sci[maxk]) maxk = k;
		 }
	    }

	    /* Compute the sum of the good samples 
	    ** with the min and max excluded */
	    for (k=0; k<nsamp; k++) {
		 if (k != mink && k != maxk && mask[k] > 0 && err[k] != 0) {
		     wt    = 1.0 / (err[k]*err[k]);
		     sums += sci[k]  * wt;
		     sumt += time[k];
		     sumw += wt;
		 }
	    }

	    /* Compute the clipped weighted mean */
	    if (sumw > 0)
		*cmean = sums / sumw;
	    else
		*cmean = 0;

	    /* Add min and max into sums */
	    wt    = 1.0 / (err[mink]*err[mink]);
	    sums += sci[mink]  * wt;
	    sumt += time[mink];
	    sumw += wt;
	    wt    = 1.0 / (err[maxk]*err[maxk]);
	    sums += sci[maxk]  * wt;
	    sumt += time[maxk];
	    sumw += wt;

	    /* Compute the weighted mean and error in the mean */
	    if (sumw > 0) {
		*mean    = sums / sumw;
		*efftime = sumt;
		*stdv    = sqrt (1.0/sumw);
	    } else {
		*mean    = 0;
		*efftime = 0;
		*stdv    = 0;
	    }
	}

}

