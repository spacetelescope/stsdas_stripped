# include <math.h>
# include <float.h>
# include "biaseq.h"

/*   FINDJUMPS  --  Find bias jumps within image quadrants.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-2000	Implementation.
**
*/

int findJumps (SingleNicmosGroup *bias, int cam, short bitmask, int filtsz,
	       float thresh, int *jmplim, int *njmps) {

/* Arguments:
**	bias	i: bias image
**	cam	i: camera number
**	bitmask	i: DQ bit mask
**	filtsz	i: median filter size
**	thresh	i: jump detection threshold
**	jmplim	o: array of jump limits
**	njmps	o: number of jumps
*/

	/* Local variables */
	int i, j, q;			/* loop indexes */
	float mean, median, mode;	/* image statistics */
	float stdv, min, max;		/* image statistics */
	float *vect, *fvect;		/* median vectors */

	/* Function declarations */
	int n_stats (SingleNicmosGroup *, int, int, int, int, float, float,
		     short, float *, float *, float *, float *, float *,
		     float *);
	void vect_filt (float *, int, int, float *);
	void vect_stats (float *, float, int, float *);

	/* Allocate memory for row/column vectors */
	vect  = (float *)calloc(NIC_QSIZE, sizeof(float));
	fvect = (float *)calloc(NIC_QSIZE, sizeof(float));
	for (i=0; i<NIC_QSIZE; i++)
	     fvect[i] = vect[i] = 0.0;

	/* Loop over quadrants, accumulating median of rows/columns */
	for (q=0; q<4; q++) {

	     /* Compute median of each row/column in quadrant: for cam 1,
	     ** compute median of each row, for cam 2 and 3, compute
	     ** median of each column. */
	     if (cam == 1) {
		 for (j=0, i=QYI[q]; i<=QYF[q]; i++, j++) {
		      if (n_stats (bias, QXI[q], QXF[q], i, i, -FLT_MAX,FLT_MAX,
			  bitmask, &mean, &median, &mode, &stdv, &min, &max))
			  return (1);
		      vect[j] += median;
		 }
	     } else {
		 for (j=0, i=QXI[q]; i<=QXF[q]; i++, j++) {
		      if (n_stats (bias, i, i, QYI[q], QYF[q], -FLT_MAX,FLT_MAX,
			  bitmask, &mean, &median, &mode, &stdv, &min, &max))
			  return (1);
		      vect[j] += median;
		 }
	     }
	}

	for (i=0; i<NIC_QSIZE; i++)
	     vect[i] /= 4;

	/* Median filter the vector of medians */
	vect_filt (vect, NIC_QSIZE, filtsz, fvect);

	/* Compute "gradient" of filtered vector */
	for (i=0; i<NIC_QSIZE-1; i++)
	     fvect[i] = fvect[i] - fvect[i+1];

	/* Compute clipped stdev of gradient vector */
	vect_stats (fvect, 5.0, 10, &stdv);

	/* Set start of first region to pixel 0 */
	*njmps = 1;
	jmplim[0] = 0;

	/* Mark any pixel locations that are above jump threshold value */
	for (i=0; i<NIC_QSIZE-1; i++) {
	     if (fabs(fvect[i]) > thresh*stdv) {
		 jmplim[2*(*njmps-1)+1] = i;		/* end of region */
		 jmplim[2*(*njmps-1)+2] = i+1;		/* start of next */
		 (*njmps)++;
	     }
	}

	/* Set end of last region to last pixel in quadrant */
	jmplim[2*(*njmps-1)+1] = NIC_QSIZE-1;

	/* Free memory */
	free (vect);
	free (fvect);

	/* Successful return */
	return (0);
}

void vect_filt (float *in, int in_len, int width, float *out) {

	/* Local variables */
	int i, j;		/* loop indexes */
	int npts;		/* number of points in array */
	int half_width;		/* half width of filter */
	float *arr;		/* work array */

	/* Function declarations */
	int sort (float *, int);
	float findMedian (float *, int);

	/* Allocate memory for work array */
	arr = (float *)calloc(width, sizeof(float));

	half_width = (int)(width/2);

	/* Loop through vector, computing median around each point */
	for (i=0; i<in_len; i++) {
	     npts = 0;
	     for (j=i-half_width; j<=i+half_width; j++) {
		  if (j>=0 && j<in_len) {
		      arr[npts] = in[j];
		      npts++;
		  }
	     }
	     sort(arr-1, npts);
	     out[i] = findMedian (arr, npts);
	}

	free (arr);
}

void vect_stats (float *vect, float nsigrej, int maxiter, float *stdv) {

	/* Local variables */
	int i;			/* Loop index */
	float mean, median;	/* Statistics */
	float last_med;		/* Previous median value */
	float low, upp;		/* Rejection limits */

	/* Function declarations */
	void get_vstats (float *, float, float, float *, float *, float *);

	/* Compute initial stats */
	get_vstats (vect, -1.0e30, 1.0e30, &mean, &median, stdv);
	last_med = median;

	/* Iterate on the stats, rejecting low/high values */
	for (i=1; i<=maxiter; i++) {

	     low = mean - nsigrej*(*stdv);
	     upp = mean + nsigrej*(*stdv);

	     get_vstats (vect, low, upp, &mean, &median, stdv);

	     /* Convergence test */
	     if (median == last_med)
		 break;

	     last_med = median;
	}
}

void get_vstats (float *vect, float low, float upp, float *mean, float *median,
		 float *stdv) {

	/* Local variables */
	int i;			/* loop index */
	int npix;		/* number of pixels */
	double sum, sum2;	/* accumulators */
	float val;		/* temp value */
	float *arr;		/* work array */

	/* Function declarations */
	int sort (float *, int);
	float findMedian (float *, int);

	/* Allocate work array */
	arr = (float *)calloc(NIC_QSIZE,sizeof(float));

	/* Initialize */
	npix = 0;
	sum = sum2 = 0.0;

	/* Fill accumulators */
	for (i=0; i<NIC_QSIZE; i++) {
	     val = vect[i];
	     if (val > low && val < upp) {
		 arr[npix] = val;
		 sum      += val;
		 sum2     += val*val;
		 npix++;
	     }
	}

	/* Compute stats */
	if (npix < 2) {

	    *mean   = 0.0;
	    *median = 0.0;
	    *stdv   = 0.0;

	} else {

	    *mean = sum / npix;
	    sort(arr-1, npix);
	    *median = findMedian (arr, npix);
	    *stdv = npix/(npix-1.) * (sum2/npix - (*mean)*(*mean));
	    if (*stdv >= 0)
		*stdv = sqrt (*stdv);
	    else
		*stdv = 0.0;
	}

	/* Free memory */
	free (arr);

}

