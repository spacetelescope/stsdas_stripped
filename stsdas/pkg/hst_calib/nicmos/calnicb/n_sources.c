# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define BKG_THRESH	5.0
# define SOURCE_THRESH	4.5
# define GROW_PIXELS	4

/* N_IDSOURCES: Identify sources in a NICMOS image.
**
** Revision history:
** H.Bushouse	Oct 1996	Written for Build 2
** H.Bushouse	07-Jul-1997	Modified n_idSources to call n_median2d instead
**				of n_getBkg (Version 2.2)
** H.Bushouse	19-Mar-1998	Modified to work on only one image at a time;
**				removed AsnInfo argument, and changed input
**				from AsnImages to SingleNicmosGroup; added use
**				of new n_pixOK function	(Version 2.2)
** H.Bushouse	03-Jun-1998	Modified n_setFlags and n_growFlags to set
**				Source flags without clearing existing Zerosig
**				flags (Version 2.2)
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
	    return (status);

	/* Locate and flag sources in the image */
	n_setFlags (input, median, sigma);

	/* Grow the source flags to neighboring pixels */
	n_growFlags (input);

	/* Successful return */
	return (status = 0);
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

	/* Function definitions */
	Bool n_pixOK (SingleNicmosGroup *, int, int);

	/* Loop over the image, flagging all pixels that are more than
	** SOURCE_THRESH*sigma above the median level. */
	for (j=0; j < image->sci.data.ny; j++) {
	     for (i=0; i < image->sci.data.nx; i++) {

		  /* Only look at pixels not already flagged as bad */
		  if (n_pixOK (image,i,j)) {
		      if (Pix(image->sci.data,i,j)-median > SOURCE_THRESH*sigma)
			  DQSetPix(image->dq.data,i,j,
			  DQPix(image->dq.data,i,j) | SOURCE);
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
		  if (DQPix(image->dq.data,i,j) & SOURCE) {

		      /* Search the 8 pixels around this pixel */
		      for (jj = j-1; jj <= j+1; jj++) {
			   for (ii = i-1; ii<= i+1; ii++) {

				/* Make sure we're in the image */
				if (jj < 0 || jj >= image->sci.data.ny ||
				    ii < 0 || ii >= image->sci.data.nx)
				    continue;

				/* Tally the neighboring source pixels */
				if (DQPix(image->dq.data,ii,jj) & SOURCE)
				    neighbors++;

			   }
		      }

		      /* Flag this pixel in tmp array if it has neighbors */
		      if (neighbors > 3)
			  tmp1[j*image->sci.data.nx+i] = SOURCE;

		  }
	     }
	}

	/* Loop over the image again, expanding the flags around the
	** surviving source-flagged pixels */
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {

		  /* Look for source-flagged pixels */
		  if (tmp1[j*image->sci.data.nx+i] == SOURCE) {

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
				    tmp2[jj*image->sci.data.nx+ii] = SOURCE;
			   }
		      }
		  }
	     }
	}

	/* Now copy the source flags from the tmp array to the input image
	** and unflag any pixels that did not survive the source selection. */
	for (j = 0; j < image->sci.data.ny; j++) {
	     for (i = 0; i < image->sci.data.nx; i++) {

		  if (tmp2[j*image->sci.data.nx+i] == SOURCE) {
		      DQSetPix(image->dq.data,i,j,
			       DQPix(image->dq.data,i,j) | SOURCE);
		  } else {
		      if (DQPix(image->dq.data,i,j) & SOURCE)
			  DQSetPix(image->dq.data,i,j,
				   DQPix(image->dq.data,i,j)-SOURCE);
		  }
	     }
	}

	/* Free the memory for the temporary array */
	free (tmp1);
	free (tmp2);

}

