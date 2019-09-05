# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_IMSTAT: This file contains a collection of routines that compute means
** and medians of images or 1-d lists of data, and also do iterative clipping
** (rejection) of data in images or 1-d lists.
**
** The routines are:
**
**	n_imagemean	Compute the mean and std dev of an image, excluding
**			DQ flagged pixels.
**	n_imagemedian	Compute the median and ave dev of an image, excluding
**			DQ flagged pixels.
**	n_mean2d	Compute the mean and std dev of an image, excluding
**			DQ flagged pixels, and applying iterative sigma clipping
**	n_median2d	Compute the median and ave dev of an image, excluding
**			DQ flagged pixels, and applying iterative sigma clipping
**	n_mean1d	Compute the mean and std dev of a 1-d array, excluding
**			values flagged by an input mask array.
**	n_clip2d	Clip (mask) outliers in an image. 
**	n_clip1d	Clip (mask) outliers in a 1-d array.
**
** Revision history:
** H.Bushouse	July 1997	Created for Version 2.2
** H.Bushouse	09-Mar-1998	Moved sort routine to n_statcalc (Version 2.2)
** H.Bushouse	02-Jun-1998	Fixed bug in use of sort routine for computing
**				medians - must pass arr as "arr-1" because
**				Num.Rec. routines assume 1-indexed arrays
**				(Version 2.2)
*/

/* N_IMAGEMEAN: Compute the mean and standard deviation of an image.
** Flagged pixels are excluded.
*/

void n_imagemean (SingleNicmosGroup *im, float *mean, float *stddev) {

/* Arguments:
**	im	i: image
**	mean	o: mean value
**	stddev	o: standard deviation
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int npix;		/* number of pixels */
	float pix;		/* pixel value */
	double sum, sum2;	/* sum and sum squared */

	/* Inititalize */
	npix    = 0;
	sum     = 0;
	sum2    = 0;
	*mean   = 0;
	*stddev = 0;

	/* Loop through the image accumulating the sum and sum squared
	** of all the good pixel values. */
	for (j = 0; j < im->sci.data.ny; j++) {
	     for (i = 0; i < im->sci.data.nx; i++) {
		  if (DQPix(im->dq.data,i,j) == 0) {
		      pix   = Pix(im->sci.data,i,j);
		      sum  += pix;
		      sum2 += pix*pix;
		      npix++;
		  }
	     }
	}

	/* Compute the mean */
	if (npix > 0) {
	    sum = sum / npix;
	    *mean = sum;
	}

	/* Compute the std deviation */
	if (npix > 1) {
	    *stddev = npix/(npix-1.) * (sum2/npix - sum*sum);
	    if (*stddev > 0)
		*stddev = sqrt(*stddev);
	    else
		*stddev = 0.0;
	}
}

/* N_IMAGEMEDIAN: Compute the median and average deviation of an image.
** Flagged pixels are excluded.
*/

int n_imagemedian (SingleNicmosGroup *im, float *median, float *avedev) {

/* Arguments:
**	im	i: image
**	median	o: median value
**	avedev	o: average deviation
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int ngood;		/* number of good pixels */
	float *arr;		/* array of good pixel values */
	double sum;		/* sum of deviations */

	/* Function definitions */
	int sort (unsigned long, float *);

	/* Allocate memory for the temporary array */
	arr = (float *) calloc(im->sci.data.nx*im->sci.data.ny, sizeof(float));

	/* Loop through the image accumulating the good pixel values */
	ngood = 0;
	sum   = 0;
	for (j = 0; j < im->sci.data.ny; j++) {
	     for (i = 0; i < im->sci.data.nx; i++) {
		  if (DQPix(im->dq.data,i,j) == 0) {
		      arr[ngood] = Pix(im->sci.data,i,j);
		      ngood++;
		  }
	     }
	}

	/* Compute the median and average deviation */
	if (ngood == 0) {
	    *median = 0;
	    *avedev = 0;
	} else if (ngood == 1) {
	    *median = arr[0];
	    *avedev = 0;
	} else {

	    /* Sort the good values */
	    if (sort((unsigned long)ngood, arr-1)) {
		free (arr);
		return (status);
	    }

	    /* Find the median value */
	    *median = arr[(int)ngood/2];

	    /* Compute the average deviation */
	    for (i = 0; i < ngood; i++)
		 sum += fabs(arr[i] - (*median));
	    *avedev = sum / ngood;
	}

	/* Free local memory */
	free (arr);

	/* Successful return */
	return (status = 0);

}

/* N_MEAN2D: Compute the mean and stdev of a 2-d array.
** Uses only unmasked pixels.
*/

void n_mean2d (SingleNicmosGroup *image, float clip, float *mean, float *stdv) {

/* Arguments:
**	image	i: input image
**	clip	i: clipping threshold
**	mean	o: mean value of image
**	stdv	o: std dev of image
*/

	/* Local variables */
	int i, j;			/* loop indexes */
	int ngood;			/* number of good pixels used */
	float val;			/* pixel value */
	double sum, sum2;		/* sum and sum-squared */
	int nrej;			/* number of rejected pixels */
	int nrej_last;			/* number of rej pixels on last iter */
	Bool first;			/* first iteration */

	/* Initialize the accumulators */
	*mean = 0;
	*stdv = 0;
	first = True;
	nrej_last = 0;

	_loop:

	ngood = 0;
	sum   = 0;
	sum2  = 0;

	/* Loop through the image, accumulating the unmasked pixel values */
	nrej = 0;
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {
		  if (DQPix(image->dq.data,i,j) == 0) {
		      val = Pix(image->sci.data,i,j);
		      if (clip != 0 && (*stdv) != 0) {
			  if (fabs(val-(*mean)) > clip*(*stdv)) {
			      nrej++;
			      continue;
			  }
		      }
		      sum  += val;
		      sum2 += val*val;
		      ngood++;
		  }
	     }
	}

	/* Compute the mean of the good pixel values */
	if (ngood > 0)
	    *mean = sum / ngood;
	else
	    *mean = 0;

	/* Compute the std deviation */
	if (ngood > 1) {
	    *stdv = ngood/(ngood-1.) * (sum2/ngood-(*mean)*(*mean));
	    if (*stdv > 0)
		*stdv = sqrt (*stdv);
	    else
		*stdv = 0;
	} else
	    *stdv = 0;

	/* Iterate if necessary */
	if (clip != 0 && (first || nrej > nrej_last)) {
	    first = False;
	    nrej_last = nrej;
	    goto _loop;
	}

}

/* N_MEDIAN2D: Compute the median and avedev of a 2-d array.
** Uses only unmasked pixels.
*/

int n_median2d (SingleNicmosGroup *im, float clip, float *median,
		float *stdv) {

/* Arguments:
**	im	i: input image
**	clip    i: clipping threshold
**	median	o: median value of image
**	stdv	o: ave dev of image
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int ngood;		/* number of good pixels */
	int nrej;		/* number of rejected pixels */
	int nrej_last;		/* last number of rejected pixels */
	float val;		/* pixel value */
	float *arr;		/* array of good pixel values */
	double sum;		/* sum of deviations */
	Bool first;		/* first iteration */

	/* Function definitions */
	int sort (unsigned long, float *);

	/* Allocate memory for the temporary array */
	arr = (float *) calloc(im->sci.data.nx*im->sci.data.ny, sizeof(float));

	*median = 0;
	*stdv   = 0;
	first = True;
	nrej_last = 0;

	_loop:

	/* Loop through the image accumulating the good pixel values */
	nrej  = 0;
	ngood = 0;
	for (j = 0; j < im->sci.data.ny; j++) {
	     for (i = 0; i < im->sci.data.nx; i++) {
		  if (DQPix(im->dq.data,i,j) == 0) {
		      val = Pix(im->sci.data,i,j);
		      if (clip != 0 && (*stdv) != 0) {
			  if (fabs(val-(*median)) > clip*(*stdv)) {
			      nrej++;
			      continue;
			  }
		      }
		      arr[ngood] = val;
		      ngood++;
		  }
	     }
	}

	/* Compute the median and sigma */
	if (ngood == 0) {
	    *median = 0;
	    *stdv   = 0;
	} else if (ngood == 1) {
	    *median = arr[0];
	    *stdv   = 0;
	} else {

	    /* Sort the good values */
	    if (sort((unsigned long)ngood, arr-1)) {
		free (arr);
		return (status);
	    }

	    /* Find the median value */
	    *median = arr[(int)ngood/2];

	    /* Compute the average deviation */
	    sum = 0;
	    for (i = 0; i < ngood; i++)
		 sum += fabs(arr[i] - (*median));
	    *stdv  = sum / ngood;
	}

	/* Iterate if necessary */
	if (clip != 0 && (first || nrej > nrej_last)) {
	    first = False;
	    nrej_last = nrej;
	    goto _loop;
	}

	/* Free local memory */
	free (arr);

	/* Successful return */
	return (status = 0);

}

/* N_MEAN1D: Compute the mean and stdev of a 1-D array.
** Uses only unmasked pixels in the array.
*/

void n_mean1d (int nval, float *vals, int *mask, float *mean, float *stdv) {

/* Arguments:
**	nval	i: number of values
**	vals	i: list of values
**	mask	i: list of masks
**	mean	o: mean of values
**	stdv	o: std dev of values
*/

	/* Local variables */
	int i;			/* loop index */
	int ngood;		/* number of good (unmasked) values */
	double sum, sum2;	/* accumulators */

	/* Initialize the accumulators */
	ngood = 0;
	sum   = 0;
	sum2  = 0;

	/* Compute the sum and sum-squared */
	for (i = 0; i < nval; i++) {
	     if (mask[i] == 1) {
	     ngood++;
	     sum   += vals[i];
	     sum2  += vals[i]*vals[i];
	     }
	}

	/* Compute the mean */
	if (ngood > 0)
	    *mean = sum / ngood;
	else
	    *mean = 0;

	/* Compute the std deviation */
	if (ngood > 1) {
	    *stdv  = ngood/(ngood-1.) * (sum2/ngood - (*mean)*(*mean));
	    if (*stdv > 0)
		*stdv = sqrt (*stdv);
	    else
		*stdv = 0;
	} else
	    *stdv = 0;

}

/* N_CLIP2D: Clip off (mask) outliers in a 2-D array. The clipping
** threshold is based on the mean and stdev of unmasked pixels in the array.
*/

void n_clip2d (SingleNicmosGroup *im, ShortTwoDArray *mask, float clip,
	       float mean, float stdv, int *nrej) {

/* Arguments:
**	im	 i: input image
**	mask	io: badpix mask array
**	clip	 i: clipping threshold
**	mean	 i: mean value of image
**	stdv	 i: std dev of image
**	nrej	 o: rejection counter
*/

	/* Local variables */
	int i, j;			/* loop indexes */
	float diff;			/* difference between pixel and mean */

	/* Initialize the rejection counter */
	*nrej = 0;

	/* If the std dev is zero, just return */
	if (stdv == 0)
	    return;

	/* Otherwise, loop through the image, masking any outliers */
	for (j = 0; j < im->sci.data.ny; j++) {
	     for (i = 0; i < im->sci.data.nx; i++) {

		  /* Compute the difference between the pixel value
		  ** and the mean; masked values are set to zero */
		  diff = fabs(Pix(im->sci.data,i,j) - mean) / stdv *
			 PPix(mask,i,j);

		  /* If it's more than thesh*sigma out, mask it */
		  if (diff > clip) {
		      (*nrej)++;
		      PPix(mask,i,j) = 0;
		  }
	     }
	}
}

/* N_CLIP1D: Clip off (mask) outliers in a 1-D array. The clipping
** threshold is based on the mean and stdev of unmasked pixels in the array.
*/

void n_clip1d (int nval, float *vals, int *mask, float clip, float mean,
	       float stdv, int *nrej) {

/* Arguments:
**	nval	 i: number of values
**	vals	 i: list of values
**	mask	io: list of masks
**	clip	 i: clipping threshold
**	mean	 i: mean of values
**	stdv	 i: std deviation of values
**	nrej	 o: rejection counter
*/

	/* Local variables */
	int i;		/* loop index */
	float diff;	/* difference */

	/* Initialize the rejection counter */
	*nrej = 0;

	/* If the std dev is zero, then just return */
	if (stdv == 0)
	    return;

	/* Otherwise, loop through the values, rejecting outliers */
	for (i = 0; i < nval; i++) {
	     diff = fabs(vals[i] - mean) / stdv;
	     if (diff > clip) {
		 nrej++;
		 mask[i] = 0;
	     }
	}

}

