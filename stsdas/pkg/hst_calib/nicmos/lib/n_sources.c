# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include "nicmos.h"

# define BKG_THRESH	5.0
# define SOURCE_THRESH	4.5
# define GROW_PIXELS	4

/* N_IDSOURCES: Identify sources in a NICMOS image.
**
**
** Revision history:
** H.Bushouse	17-May-2000	Ported from CALNICB routine.
**
*/

int n_idSources (SingleNicmosGroup *input) {

/* Arguments:
**	input	 i: input image
*/

	/* Local variables */
	float median;		/* median of image */
	float sigma;		/* sigma (ave dev) of image */

	/* Function definitions */
	int  n_median2d  (SingleNicmosGroup *, float, float *, float *);
	void n_setFlags  (SingleNicmosGroup *, float, float);
	void n_growFlags (SingleNicmosGroup *);

	/* Compute the median and sigma of the image */
	if (n_median2d (input, (float)BKG_THRESH, &median, &sigma))
	    return (1);

	/* Locate and flag sources in the image */
	n_setFlags (input, median, sigma);

	/* Grow the source flags to neighboring pixels */
	n_growFlags (input);

	/* Successful return */
	return (0);
}

/* N_SETFLAGS: Locate and flag image pixels that have a source. This is
** accomplished by simply flagging all pixels that are SOURCE_THRESH*sigma
** above the median level of the image. This requires that the median
** level and sigma have already been computed for the image. */

void n_setFlags (SingleNicmosGroup *image, float median, float sigma) {

/* Arguments:
**	image	io: input image
**	median	 i: image median
**	sigma	 i: image sigma
*/

	/* Local variables */
	int i, j;			/* loop indexes */

	/* Loop over the image, flagging all pixels that are more than
	** SOURCE_THRESH*sigma above the median level. */
	for (j=0; j < image->sci.data.ny; j++) {
	     for (i=0; i < image->sci.data.nx; i++) {

		  /* Only look at pixels not already flagged as bad */
		  if (!(DQPix(image->dq.data,i,j) & 1023)) {
		      if (Pix(image->sci.data,i,j)-median > SOURCE_THRESH*sigma)
			  DQSetPix(image->dq.data,i,j,
			  DQPix(image->dq.data,i,j) | N_SOURCE);
		  }
	     }
	}

}

/* N_GROWFLAGS: Expand source flags to surrounding pixels to fully mask
** sources. Also remove flags on isolated (single pixel) noise spikes.
*/

void n_growFlags (SingleNicmosGroup *image) {

/* Arguments:
**	image	io: input image
*/

	/* Local variables */
	int i, j, ii, jj;		/* loop indexes */
	int neighbors;			/* number of flagged neighbors */
	int *tmp1, *tmp2;		/* pointers to tmp arrays */

	/* Allocate memory for the temporary arrays */
	tmp1 = NULL;
	tmp2 = NULL;
	tmp1 = (int *) calloc (image->sci.data.nx*image->sci.data.ny,
			       sizeof(int));
	tmp2 = (int *) calloc (image->sci.data.nx*image->sci.data.ny,
			       sizeof(int));

	/* Loop over image and determine how many neighbors of each
	** source-flagged pixel are also source-flagged */
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {

		  neighbors = 0;

		  /* Examine source-flagged pixels */
		  if (DQPix(image->dq.data,i,j) & N_SOURCE) {

		      /* Search the 8 pixels around this pixel */
		      for (jj = j-1; jj <= j+1; jj++) {
			   for (ii = i-1; ii<= i+1; ii++) {

				/* Make sure we're in the image */
				if (jj < 0 || jj >= image->sci.data.ny ||
				    ii < 0 || ii >= image->sci.data.nx)
				    continue;

				/* Tally the neighboring source pixels */
				if (DQPix(image->dq.data,ii,jj) & N_SOURCE)
				    neighbors++;

			   }
		      }

		      /* Flag this pixel in tmp array if it has neighbors */
		      if (neighbors > 3)
			  tmp1[j*image->sci.data.nx+i] = N_SOURCE;

		  }
	     }
	}

	/* Loop over the image again, expanding the flags around the
	** surviving source-flagged pixels */
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {

		  /* Look for source-flagged pixels */
		  if (tmp1[j*image->sci.data.nx+i] == N_SOURCE) {

		      /* Set source flags in neighboring pixels */
		      for (jj = j-1; jj <= j+1; jj++) {
			   for (ii = i-1; ii <= i+1; ii++) {

				/* Make sure we're in the image */
				if (jj < 0 || jj >= image->sci.data.ny ||
				    ii < 0 || ii >= image->sci.data.nx)
				    continue;

				if (GROW_PIXELS == 8 ||
				    ( (ii==i && (jj>=j-1 || jj<=j+1)) ||
				      (jj==j && (ii>=i-1 || ii<=i+1)) ))
				    tmp2[jj*image->sci.data.nx+ii] = N_SOURCE;
			   }
		      }
		  }
	     }
	}

	/* Now copy the source flags from the tmp array to the input image
	** and unflag any pixels that did not survive the source selection. */
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {

		  if (tmp2[j*image->sci.data.nx+i] == N_SOURCE) {
		      DQSetPix(image->dq.data,i,j,
			       DQPix(image->dq.data,i,j) | N_SOURCE);
		  } else {
		      if (DQPix(image->dq.data,i,j) & N_SOURCE)
			  DQSetPix(image->dq.data,i,j,
				   DQPix(image->dq.data,i,j)-N_SOURCE);
		  }
	     }
	}

	/* Free the memory for the temporary array */
	free (tmp1);
	free (tmp2);

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
	int sort (float *, int);
	float findMedian (float *, int);

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
	    if (sort(arr-1, ngood)) {
		free (arr);
		return (1);
	    }

	    /* Find the median value */
	    *median = findMedian(arr-1, ngood);

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
	return (0);

}

