# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

# define THRESH 3.0	/* default bar detection threshold */

/* N_DOBARS: Calls the BARSCORR routine for a MultiAccum image.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doBars (NicInfo *nic, MultiNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be bars corrected
*/

	/* Local variables */

	/* Functions definitions */
	int n_barscorr (NicInfo *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Print info to processing log */
	if (nic->BARS.corr == PERFORM) {
	    sprintf (MsgText, "BARSCORR detection threshold set at %g sigma",
		     nic->barthresh);
	    n_message (MsgText);
	}

	/* Use default detection threshold if no user input */
	if (nic->barthresh == 0) {
	    nic->barthresh = THRESH;

	    sprintf (MsgText, "Using BARSCORR detection threshold of %g sigma (default)", nic->barthresh);
	    n_message (MsgText);
	}
	/* Call barscorr for each readout */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->BARS.corr == PERFORM) {
		 if (n_barscorr (nic, &(input->group[nic->group-1])))
		     return (status);
	     }

	     n_calReport (&nic->BARS, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
	}

	/* Successful return */
	return (status = 0);

}

# define MIN(a,b)	(a < b ? a : b)
# define MAX(a,b)	(a > b ? a : b)

static void quad_ave (SingleNicmosGroup *);
static void quad_med (SingleNicmosGroup *, SingleNicmosGroup *, Bool);
static void get_stats (SingleNicmosGroup *, int, int, Bool, int, float *,
		       float *, float *, float *, float *, float *);
static void accum (SingleNicmosGroup *, int, int, int *, float *, float *);
static void quad_stats (float *, int, float, float *, float *);
static void stats (float *, int, float *, float *);

/* N_BARSCORR: Search for and flag pixels in a NICMOS image affected by bars.
** Only the DQ extension of the input image is modified.
**
** Revision history:
** H.Bushouse	09-Oct-1998	Initial implementation (Version 3.3)
** H.Bushouse	04-Nov-1998	Changed from computing row/column medians
**				while searching the image to producing an
**				entire median filtered image at the start
**				(Version 3.3)
** H.Bushouse	16-Sep-1999	Moved static function prototypes to head of
**				file (to satisfy DEC/Alpha/OpenVMS compiler)
**				(Version 3.3)
** H.Bushouse	21-Jun-2000	Changed row_search from type int to Bool (V4.0)
*/

int n_barscorr (NicInfo *nic, SingleNicmosGroup *input) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be bars corrected
*/

	/* Local variables */
	SingleNicmosGroup image;	/* temp image */
	SingleNicmosGroup image2;	/* temp image */
	Bool row_search;		/* search direction indicator */

	/* Functions definitions */
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_findBars (SingleNicmosGroup *, SingleNicmosGroup *, Bool, int,
			 float, int);


	/* Set the proper row vs. col search pattern based on camera */
	if (nic->camera == 1)
	    row_search = False;
	else
	    row_search = True;

	/* Copy the input image to a temp image */
	if (n_copyGroup (&image, input))
	    return (status);
	if (n_copyGroup (&image2, input))
	    return (status);

	/* Average the 4 quadrants together to increase S/N */
	quad_ave (&image);

	/* Median filter the quad-averaged image */
	quad_med (&image, &image2, row_search);

	/* Search for and flag bars */
	n_findBars (input, &image2, row_search, nic->camera, nic->barthresh,
		    nic->group);

	/* Free the temp images */
	freeSingleNicmosGroup (&image);
	freeSingleNicmosGroup (&image2);

	/* Successful return */
	return (status = 0);

}

/* N_FINDBARS: Find and flag pixels that are affected by the bars. */

void n_findBars (SingleNicmosGroup *input, SingleNicmosGroup *image,
		 Bool row_search, int camera, float thresh, int group) {
/* Arguments:
**	input	  io: image to be bars corrected
**	image	   i: image that has been quad_ave'd and median filtered
**	row_search i: row or column oriented search indicator
**	camera	   i: camera number
**	thresh	   i: bar detection threshold
*/

	/* Local variables */
	int i, j, k;			/* loop indexes */
	Bool bar_found;			/* flag */
	int start, end;			/* bar start & end pixel indexes */
	int n_bar;			/* number of bar pixels */
	int n_bar_contig;		/* number of continguous bar pixels */
	int n_normal;			/* number of normal pixels */
	float median1, median2, median3;/* median pixel values */
	float sigma1, sigma2, sigma3;	/* sigma values */
	float diff1, diff2;		/* median difference values */

        /* Loop over the image lines */
	for (j = 0; j < 126; j++) {
 
             /* Scan through the line */
             bar_found = False;
             for (i = 0; i < 128; i++) {
 
                 /* Get the statistics for the current set of lines */
		 get_stats (image, i, j, row_search, camera, &median1,
			    &median2, &median3, &sigma1, &sigma2, &sigma3);

		 /* Take difference of the high & low lines relative
		 ** to the middle line */
		 diff1 = median2 - median1;
		 diff2 = median3 - median2;

		 /* Bar detection is based on the first line being lower than
		 ** normal, and the 3rd one being higher than normal */
                 if (sigma1 > 0.0 && sigma2 > 0.0 && sigma3 > 0.0 &&
		     diff2 > thresh*sqrt(sigma3*sigma3+sigma2*sigma2) &&
		     diff1 > thresh*sqrt(sigma1*sigma1+sigma2*sigma2)) {

		     /* Check to make sure we aren't seeing the top of a
		     ** zeroth-read induced inverted bar */
		     if (j >= 2) {
			 get_stats (image, i, j-2, row_search, camera, &median1,
				    &median2, &median3, &sigma1, &sigma2,
				    &sigma3);
			 if (median1 - median3 > diff2 + diff1 - sigma2)
			     continue;
		     }

		     /* Check to make sure we aren't seeing the bottom of a
		     ** zeroth-read induced inverted bar */
		     if (j <= 123) {
			 get_stats (image, i, j+2, row_search, camera, &median1,
				    &median2, &median3, &sigma1, &sigma2,
				    &sigma3);
			 if (median1 - median3 > diff2 + diff1 - sigma2)
			     continue;
		     }

		     /* It's a legitimate candidate bar */
                     bar_found = True;
                     break;
                 }
             }
 
	     /* Figure out how far the bar extends along the image line */
             if (bar_found) {

		 /* First look back down the line to lower pixel indexes */
                 start = i;
		 n_bar = 0;
		 n_bar_contig = 0;
		 n_normal = 0;
		 for (k=i-1; k >= 0; k--) {

		      /* Get statistics for this region */
		      get_stats (image, k, j, row_search, camera, &median1,
				 &median2, &median3, &sigma1, &sigma2, &sigma3);

		      /* Take the difference of the high and low lines */
		      diff1 = median3 - median1;

		      /* Look for a large difference between these lines */
		      if (sigma1 > 0.0 && sigma2 > 0.0 && sigma3 > 0.0 &&
			  diff1 > thresh*sqrt(sigma2*sigma2)) {
			  n_bar++;
			  n_bar_contig++;
		      } else {
			  n_bar_contig = 0;
			  n_normal++;
		      }
		      if (n_bar > n_normal || n_bar_contig > 9) {
			  start = k;
			  n_bar = 0;
			  n_normal = 0;
		      }
                 }
 
		 /* Now look forward along the line to higher pixel indexes */
                 end = i;
		 n_bar = 0;
		 n_bar_contig = 0;
		 n_normal = 0;
                 for (k=i+1; k < 128; k++) {

		      /* Compute statistics for this region */
		      get_stats (image, k, j, row_search, camera, &median1,
				 &median2, &median3, &sigma1, &sigma2, &sigma3);

		      /* Take the difference of the high and low lines */
		      diff1 = median3 - median1;

		      /* Look for a large difference between these lines */
		      if (sigma1 > 0.0 && sigma2 > 0.0 && sigma3 > 0.0 &&
			  diff1 > thresh*sqrt(sigma2*sigma2)) {
			  n_bar++;
			  n_bar_contig++;
		      } else {
			  n_bar_contig = 0;
			  n_normal++;
		      }
		      if (n_bar > n_normal || n_bar_contig > 9) {
			  end = k;
			  n_bar = 0;
			  n_normal = 0;
		      }
                 }
 
		 /* Make sure it isn't just a little artifact in the image */
		 if (end - start < 15)
		     continue;

		 /* Report the bar characteristics */
		 if (row_search) {
		     sprintf (MsgText,
		   "BARSCORR bar found in imset %d row %d from pixel %d to %d",
			      group, j+1, start+1, end+1);
		 } else {
		     sprintf (MsgText,
		"BARSCORR bar found in imset %d column %d from pixel %d to %d",
			      group, j+1, start+1, end+1);
		 }
		 n_message (MsgText);
 
		 /* Set flags in the input DQ image */
                 for (k=start; k <= end; k++) {

		      if (row_search) {
		      /* Quad 1 */
                      DQSetPix (input->dq.data, k, j,
				DQPix(input->dq.data,k,j) | BADPIX);
                      DQSetPix (input->dq.data, k, j+2,
				DQPix(input->dq.data,k,j+2) | BADPIX);

		      /* Quad 2 */
                      DQSetPix (input->dq.data, k+128, j,
				DQPix(input->dq.data,k+128,j) | BADPIX);
                      DQSetPix (input->dq.data, k+128, j+2,
				DQPix(input->dq.data,k+128,j+2) | BADPIX);

		      /* Quad 3 */
                      DQSetPix (input->dq.data, k, j+128,
				DQPix(input->dq.data,k,j+128) | BADPIX);
                      DQSetPix (input->dq.data, k, j+130,
				DQPix(input->dq.data,k,j+130) | BADPIX);

		      /* Quad 4 */
                      DQSetPix (input->dq.data, k+128, j+128,
				DQPix(input->dq.data,k+128,j+128) | BADPIX);
                      DQSetPix (input->dq.data, k+128, j+130,
				DQPix(input->dq.data,k+128,j+130) | BADPIX);

		      } else {
		      /* Quad 1 */
                      DQSetPix (input->dq.data, j, k,
				DQPix(input->dq.data,j,k) | BADPIX);
                      DQSetPix (input->dq.data, j+2, k,
				DQPix(input->dq.data,j+2,k) | BADPIX);

		      /* Quad 2 */
                      DQSetPix (input->dq.data, j, k+128,
				DQPix(input->dq.data,j,k+128) | BADPIX);
                      DQSetPix (input->dq.data, j+2, k+128,
				DQPix(input->dq.data,j+2,k+128) | BADPIX);

		      /* Quad 3 */
                      DQSetPix (input->dq.data, j+128, k,
				DQPix(input->dq.data,j+128,k) | BADPIX);
                      DQSetPix (input->dq.data, j+130, k,
				DQPix(input->dq.data,j+130,k) | BADPIX);

		      /* Quad 4 */
                      DQSetPix (input->dq.data, j+128, k+128,
				DQPix(input->dq.data,j+128,k+128) | BADPIX);
                      DQSetPix (input->dq.data, j+130, k+128,
				DQPix(input->dq.data,j+130,k+128) | BADPIX);
		      }
		 }
 
                 /* Skip the next 2 image lines */
                 j += 2;
             }
        }
}

/* QUAD_AVE: Average the 4 quadrants of an image, replacing the lower-left
** quadrant of the input image with the computed average. Sources and
** discrepant pixel values are rejected by clipping the max value for
** each pixel.
*/

static void quad_ave (SingleNicmosGroup *input) {

/* Arguments:
**	input	io: image to have the quadrants averaged
*/

	/* Local variables */
	int i, j;		/* pixel indexes */
	int npts;		/* number of pixels in average */
	float maxv;		/* max value of pixels in average */
	float mean, sigma;	/* mean and sigma of pixels in average */
	float *arr;		/* temp array for computing average */

	/* Allocate local memory for averaging array */
	arr = (float *)calloc(4,sizeof(float));

	/* Loop over the input image quadrant */
	for (j=0; j < 128; j++) {
	     for (i=0; i < 128; i++) {

		  npts = 0; maxv = -32000.0;

		  /* Accumulate pixel values into the temp array */
		  accum (input, i,     j,     &npts, &maxv, arr);
		  accum (input, i+128, j,     &npts, &maxv, arr);
		  accum (input, i,     j+128, &npts, &maxv, arr);
		  accum (input, i+128, j+128, &npts, &maxv, arr);

		  /* Compute mean and average deviation of values for this
		  ** pixel, excluding the max value from both quantities */
		  quad_stats (arr, npts, maxv, &mean, &sigma);
			  
		  /* Store results in image */
		  Pix(input->sci.data,i,j)  = mean;
		  Pix(input->err.data,i,j)  = sigma;
		  Pix(input->smpl.data,i,j) = npts;
		  if (npts > 0)
		      DQSetPix(input->dq.data,i,j,0);
		  else
		      DQSetPix(input->dq.data,i,j,32);
	     }
	}

	/* Free local memory */
	free (arr);
}

/* QUAD_MED: Median filter the lower-left quadrant of the input image,
** storing the results in a new output image.
*/

static void quad_med (SingleNicmosGroup *in_image, SingleNicmosGroup *out_image,
		      Bool row_search) {

/* Arguments:
**	in_image   i: input image
**	out_image  o: output median-filtered image
**	row_search i: row or column filter direction indicator
*/

	/* Local variables */
	int i, j, k;		/* pixel indexes */
	int npts;		/* number of pixels in median stack */
	float maxv;		/* max value in median stack */
	float median, sigma;	/* median and sigma of stack */
	float *arr;		/* temp array for computing median and sigma */

	/* Allocate local memory for temp array */
	arr = (float *)calloc(128,sizeof(float));

	/* Loop through the lower-left quadrant of the input image */
	for (j = 0; j < 128; j++) {
             for (i = 0; i < 128; i++) {

		  npts = 0; maxv = -32000.0;

		  /* Accumulate pixel values into median array */
		  for (k = MAX(i-4,0); k < MIN(i+5,128); k++) {
		       if (row_search)
			   accum (in_image, k, j, &npts, &maxv, arr);
		       else
			   accum (in_image, j, k, &npts, &maxv, arr);
		  }

		  /* Compute the median and sigma */
		  stats (arr, npts, &median, &sigma);

		  /* Store the results in the output image */
		  if (row_search) {
		      Pix(out_image->sci.data,i,j) = median;
		      Pix(out_image->err.data,i,j) = sigma;
		  } else {
		      Pix(out_image->sci.data,j,i) = median;
		      Pix(out_image->err.data,j,i) = sigma;
		  }
	      }
	}

	/* Free local memory */
	free (arr);
}

/* QUAD_STATS: Compute statistics (mean and average deviation) for the
** four values from each quadrant at a given pixel index. The max value
** of the four is not used to compute either the mean or the average
** deviation.
*/

static void quad_stats (float *arr, int npts, float maxv, float *mean,
			float *avedev) {

/* Arguments:
**	arr	i: array with input pixel values
**	npts	i: number of pixel values used in statistics
**	maxv	i: max of input values
**	mean	o: mean of the input values (excluding the max)
**	avedev	o: average deviation about the mean (excluding the max)
*/

	/* Local variables */
	int i;			/* pixel index */
	float sum;		/* sum of pixel values */

	/* Check for trivial cases */
	if (npts == 0) {
	    *mean = 0.0;
	    *avedev = 0.0;
	} else if (npts == 1) {
	    *mean = arr[0];
	    *avedev = 0.0;

	/* Compute the mean and average deviation */
	} else {
	    sum = 0.0;
	    for (i=0; i < npts; i++) {
		 if (arr[i] != maxv)
		     sum += arr[i];
	    }
	    *mean = sum / (npts-1);

	    sum = 0.0;
	    for (i=0; i < npts; i++) {
		 if (arr[i] != maxv)
		     sum += fabs(arr[i] - (*mean));
	    }
	    *avedev = sum / (npts-1);
	}
}

/* ACCUM: Accumulate non-flagged pixel values into temp array */

static void accum (SingleNicmosGroup *image, int i, int j, int *npts,
		   float *maxv, float *arr) {

/* Arguments:
**	image	i: input image
**	i,j	i: pixel indexes
**	npts	o: number of pixel values accepted
**	maxv	o: max pixel value
**	arr	o: array of accumulated values
*/

	/* Local variables */
	float val;			/* pixel value */

	/* Make sure we're within the image quadrant boundaries */
	if (i < 0 || i > 255 || j < 0 || j > 255)
	    return;

	/* If the pixel is not flagged, add it's value to the accumulators */
	if (DQPix(image->dq.data,i,j) == 0) {
	    val = Pix(image->sci.data,i,j);
	    arr[(*npts)] = val;
	    (*npts)++;

	    /* Save the max pixel value */
	    if (val > (*maxv))
		*maxv = val;
	}
}

/* STATS: Compute median and average deviation of values in accumulator
**	  array.
**
** Revision history:
** H.Bushouse	16-Jun-2000	Swapped order of arguments in call to sort
**				routine to conform with mods in n_statcalc
**				(Version 4.0)
*/

static void stats (float *arr, int npts, float *median, float *avedev) {

/* Arguments:
**	arr	i: array of input pixel values
**	npts	i: number of input values
**	median	o: median of pixel values
**	avedev	o: average deviation of pixel values
*/

	/* Local variables */
	int i;			/* pixel index */
	float sum;		/* sum of pixel values */

	/* Function definitions */
	int sort (float *, int);

	/* Check for trivial cases */
	if (npts == 0) {
	    *median = 0.0;
	    *avedev = 0.0;
	} else if (npts == 1) {
	    *median = arr[0];
	    *avedev = 0.0;
	} else {

	    /* Sort the values in the input array */
	    if (sort(arr-1, npts))
		return;

	    /* Find the median */
	    if (2*(npts/2) == npts)
		*median = (arr[npts/2-1] + arr[npts/2]) / 2.0;
	    else
		*median = arr[npts/2];

	    /* Compute the average deviation, excluding the max value */
	    sum = 0.0;
	    for (i = 0; i < npts-1; i++)
		 sum += fabs(arr[i] - (*median));
	    *avedev = sum / (npts-1);
	
	}
}

/* GET_STATS: Get the statistics stored in the median filtered image */

static void get_stats (SingleNicmosGroup *image, int i, int j, Bool row_search,
		       int camera, float *median1, float *median2,
		       float *median3, float *sigma1, float *sigma2,
		       float *sigma3) {

/* Arguments:
**	image	   i: median filtered image
**	i,j	   i: pixel indexes
**	row_search i: row/column search direction indicator
**	camera	   i: camera number
**	median[123]	o: median of rows 1,2,3
**	sigma[123]	o: sigma of rows 1,2,3
*/

	/* Retrieve median and sigma from appropriate pixels in
	** the median filtered input image */
	if (row_search) {
	    *median1 = Pix(image->sci.data,i,j);
	    *median2 = Pix(image->sci.data,i,j+1);
	    *median3 = Pix(image->sci.data,i,j+2);
	    *sigma1 =  Pix(image->err.data,i,j);
	    *sigma2 =  Pix(image->err.data,i,j+1);
	    *sigma3 =  Pix(image->err.data,i,j+2);

	    /* Swap values for dark/bright lines for camera 3 */
	    if (camera == 3) {
		*median1 = Pix(image->sci.data,i,j+2);
		*median3 = Pix(image->sci.data,i,j);
		*sigma1  = Pix(image->err.data,i,j+2);
		*sigma3  = Pix(image->err.data,i,j);
	    }

	} else {
	    *median1 = Pix(image->sci.data,j,i);
	    *median2 = Pix(image->sci.data,j+1,i);
	    *median3 = Pix(image->sci.data,j+2,i);
	    *sigma1 =  Pix(image->err.data,j,i);
	    *sigma2 =  Pix(image->err.data,j+1,i);
	    *sigma3 =  Pix(image->err.data,j+2,i);
	}

}

