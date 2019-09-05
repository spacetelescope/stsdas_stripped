# include <stdio.h>
# include <stdlib.h>
# include "msstat.h"

# define  MIN_NBINS  3  /* min bins from which midpt estimation will work */
# define  GS_MITER   5  /* max number of rebinning operations             */

# if defined(NATIVE_IRAF)
# define max(a,b) ((a) > (b) ? (a) : (b))
# define min(a,b) ((a) < (b) ? (a) : (b))
# endif


/*   E_MIDMOD -  Computes median and mode.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   15 Jul 96  -  Implementation taken straight from gstatistics (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *   03 Jun 10  -  Fixed declaration of e_hmode in e_midmod (HAB)
 *
 */

int e_midmod (Control *con, int hdu, float *midpt, float *mode) {

	int          i;

	int e_median (Control *, int, float *);
	int e_hmode  (Control *, int, float *);

	for (i = 0; i < con->nstats; i++) {
	    if (con->stats[i] == MIDPT) {
	        if (e_median (con, hdu, midpt))
	            return (1);
	    }
	}

	for (i = 0; i < con->nstats; i++) {
	    if (con->stats[i] == MODE) {
	        if (e_hmode (con, hdu, mode))
	            return (1);
	    }
	}

	return (0);
}



int e_median (Control *con, int hdu, float *midpt) {

	int	i, lo, hi;
	float	*ihgm;
	float	h1, hdiff, hnorm;

	/* If not enough or invalid data, do nothing. */
	if (con->accum[hdu].nbins    < MIN_NBINS || 
            con->accum[hdu].ceiling  < con->accum[hdu].floor ||
	    con->accum[hdu].binWidth <= 0.0F)
	    return (1);

	/* Alloc space for normalized histogram. */
	ihgm = (float *) calloc(con->accum[hdu].nbins, sizeof(float));
	if (ihgm == NULL) {
	    e_error ("Cannot allocate memory for histogram computation.");
	    return (1);
	}

	/* Integrate histogram and normalize. */
	ihgm[0] = (float)con->accum[hdu].histogram[0];
	for (i = 1; i < con->accum[hdu].nbins; i++)
	    ihgm[i] = con->accum[hdu].histogram[i] + ihgm[i-1];
	hnorm = ihgm[con->accum[hdu].nbins-1];
	if (!(hnorm > 0.0F)) {         /* hnorm should not be zero. */
	    free (ihgm);
	    e_error ("Cannot compute histogram.");
	    return (1);
	}
	for (i = 0; i < con->accum[hdu].nbins; i++)
	    ihgm[i] /= hnorm;

	/* Initialize the low and high bin numbers. */
	lo = -1;
	hi =  0;

	/* Search for the point which divides the integral in half. */
	for (i = 0; i < con->accum[hdu].nbins; i++) {
	    if (ihgm[i] > 0.5F)
		break;
	    lo = i;
	}
	hi = lo + 1;

	/* Approximate the histogram. */
	h1 = con->accum[hdu].floor + lo * con->accum[hdu].binWidth;
	if (lo < 0)
	    hdiff = ihgm[hi];
	else
	    hdiff = ihgm[hi] - ihgm[lo];
	if (hdiff == 0.0F)
	    *midpt = h1;
	else if (lo == 0)
	    *midpt = h1 + 0.5F / hdiff * con->accum[hdu].binWidth; 
	else
	    *midpt = h1 + (0.5F - ihgm[lo]) / hdiff * con->accum[hdu].binWidth;
	*midpt += con->accum[hdu].binWidth;

	free (ihgm);
	return (0);
}


int e_hmode (Control *con, int hdu, float *mode) {

	Bool	done;             /* Termination flag                */
	long    *hgm;             /* Rebinned histogram              */
	int	min_nbins;        /* Min acceptable number of bins   */
	float	mmode[GS_MITER];  /* Array holding modes             */
	int	i, j;             /* Loop indexes                    */
	int	iter_count;       /* Iteration count                 */
	float	rhmin;            /* Minimum histogram value	     */
	int	rbins;            /* # of bins in rebinned histogram */
	float	rhwidth;          /* Rebinned resolution             */
	int	temp1, temp2;

	void e_gmode (long *, float, int, float, float *);

	/* If not enough or invalid data, do nothing. */
	if (con->accum[hdu].nbins    <  MIN_NBINS             || 
            con->accum[hdu].ceiling  <= con->accum[hdu].floor ||
	    con->accum[hdu].binWidth <= 0.0F)
	    return (1);

	/* Alloc temporary histogram. */
	rhmin   = con->accum[hdu].floor;
	rbins   = con->accum[hdu].nbins;
	rhwidth = con->accum[hdu].binWidth;
	hgm = (long *) malloc (rbins * sizeof(long));
	for (i=0, j=0; i<rbins; hgm[i++] = con->accum[hdu].histogram[j++]);

	/* Compute first mode estimate from original histogram. */
	e_gmode (hgm, rhmin, rbins, rhwidth, &mmode[0]);
	iter_count = 1;

	temp1     = 2 * (int) (  pow ((float)(con->accum[hdu].npix-1), 0.4F) );
	temp2     =     (int) ( (float)(con->accum[hdu].npix) / 10.0F );
	min_nbins = max (temp1, temp2);

	/* Iterate. Rebin in successive coarser bins and recompute mode. */
	done = False;
	do {
	    rbins = rbins / 2;
	    rhwidth = rhwidth * 2.0F;
	    for (i = 0; i < rbins; i++) {
	    	j = 2 * i;
	    	hgm[i] =  hgm[j] + hgm[j+1];
	    }
	    e_gmode (hgm, rhmin, rbins, rhwidth, &mmode[iter_count]);
	    if (rhwidth < 0.01F * con->accum[hdu].stddev || 
                rbins > min_nbins )
	    	iter_count++;
	    else 
		done = True;
	} while (!done && iter_count < GS_MITER);

	/* Result is average of all estimates. */
	*mode      = 0.0F;
	iter_count = min (iter_count, GS_MITER);
	for (j = 0; j < iter_count; *mode += mmode[j++]);
	*mode /= (float)iter_count;

	free (hgm);
	return (0);
}



void e_gmode (long *x, float rhmin, int rbins, float rhwidth , float *mode) {

	int	i;               /* Loop index                 */
	int	bpeak;           /* Bin index containing peak  */
	float	hpeak;           /* Histogram peak value       */
	float	dh1, dh2, denom;

	/* Find the three points surrounding the histogram max. */
	hpeak = x[0];
	bpeak = 0;
	for (i = 1; i < rbins; i++) {
	    if (x[i] > hpeak) {
		hpeak = x[i];
		bpeak = i;
	    }
	}
	bpeak = max (1, bpeak-1);

	/* If the maximum is in the first bin return the midpoint of the bin.*/
	if (bpeak == 0) {
	    *mode = rhmin + 0.5F * rhwidth;
	    return;
	}

	/* If the maximum is in the last bin return the midpoint of the bin. */
	if (bpeak == rbins-1) {
	    *mode = rhmin + (rbins - 0.5F) * rhwidth;
	    return;
	}

	/* Do a parabolic interpolation to find the peak. */
	dh1 = x[bpeak+1] - x[bpeak];
	dh2 = x[bpeak+1] - x[bpeak+2];
	denom = dh1 + dh2;
	if (denom != 0.0F) {
	    *mode = bpeak + 1.0F + 0.5F * (dh2 - dh1) / denom;
	    *mode = rhmin + 0.5F * rhwidth + (*mode - 1.0F) * rhwidth;
	} else
	    *mode = rhmin + ((float)(bpeak) + 0.5F) * rhwidth;
}


