# include <math.h>
# include <float.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include "pedsub.h"

/*   SPREAD  --  Compute spread of pixel values in a NICMOS image quadrant
**		 after subtracting a trial pedestal value.
**
**	Based on SPREAD routine from Roeland van der Marel's
**	"unpedestal" program.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	28-June-1999	Implementation
**
*/

float spread (PedInfo *info, SingleNicmosGroup *input,
	      SingleNicmosGroup *Flatim, float pedval) {

/* Arguments:
**	info	i: task info structure
**	input	i: input image
**	Flatim	i: flatfield image
**	pedval	i: trial pedestal value
*/

	/* Local variables */
	int npts;			/* number of good pixels in quadrant */
	float *arr;			/* sorted pixel array */
	float *bin;			/* histogram bins */
	float binsize;			/* histogram bin size */
	float mode;			/* mode of pixel values */
	float sigma;			/* gaussian sigma */
	SingleNicmosGroup subimg;	/* pedestal-subtract image */

	/* Function declarations */
	int  n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	void transform (PedInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
			float, int, SingleNicmosGroup *);
	int quadsort (SingleNicmosGroup *, int, int, int, int, short, float *,
		      int *);
	float findMode (float *, int);
	void mkhist (float *arr, int npts, float binsize, float mode,
		     float *bin);
	void gaussfit (float *, int , float, float, float, float *);
	float *vector (int nl, int nh);
	void free_vector (float *v, int nl, int nh);
	
	/* Copy the input image to a temp image */
	if (n_copyGroup (&subimg, input))
	    return (0);

	/* Subtract trial pedestal value from input image quadrant
	** and apply desired filter to result */
	transform (info, input, Flatim, pedval, info->Filter, &subimg);

	/* Allocate an array to store the sorted pixel values */
	arr = (float *)calloc((info->qx2[info->quad]-info->qx1[info->quad]+1)*
			      (info->qy2[info->quad]-info->qy1[info->quad]+1),
			      sizeof(float));
        if (arr == NULL) {
            sprintf (MsgText, "Memory allocation failure in spread");
            n_error (MsgText);
            return (0);
        }

	/* Sort the pixel values for this quadrant */
	if (quadsort (&subimg, info->qx1[info->quad], info->qx2[info->quad],
		      info->qy1[info->quad], info->qy2[info->quad],
		      info->BitMask, arr, &npts)) {
	    free (arr);
	    return (0);
	}

	/* Compute the mode of the quadrant */
	mode = findMode (arr, npts);

	/* Get an initial estimate of the Gaussian dispersion of the
	** filtered image, using percentiles. */
	sigma = 0.5 * (arr[(int)((0.5+0.3413)*(float)npts)] -
		       arr[(int)((0.5-0.3413)*(float)npts)] );

	/* Allocate a vector for the histogram bins and set the bin size */
	bin = vector (-NBIN, NBIN);
	binsize = BINMAX*sigma / (float)(NBIN);

	/* Sort the pixel values into the histogram bins */
	mkhist (arr, npts, binsize, mode, bin);

	/* Fit a Gaussian to the binned histogram;
	** first assign initial guesses, then call GAUSSFIT */
	gaussfit (bin, NBIN, binsize, 1.0, 0.0, &sigma);

	free (arr);
	free_vector (bin,-NBIN,NBIN);
	freeSingleNicmosGroup (&subimg);

	/* Return Gaussian width */
	return (sigma);

}

int quadsort (SingleNicmosGroup *in, int x1, int x2, int y1, int y2,
	      short mask, float *arr, int *npts) {

	/* Local variables */
	int i, j;			/* pixel indexes */

	/* Function declarations */
	int sort (float *, int);

	/* Copy unflagged pixels into sort array */
	for ((*npts)=0, j=y1; j<=y2; j++) {
	     for (i=x1; i<=x2; i++) {
		  if (!(mask & DQPix(in->dq.data,i,j))) {
		      arr[(*npts)] = Pix(in->sci.data,i,j);
		      (*npts)++;
		  }
	     }
	}

	/* Check for an empty array */
	if ((*npts) == 0) {
	    sprintf (MsgText, "No unrejected pixels in quadsort");
	    n_error (MsgText);
	    return (1);
	}

	/* Sort the pixel values */
	if (sort(arr-1, *npts))
	    return (1);

	return (0);

}
	
void mkhist (float *arr, int npts, float binsize, float mode, float *bin) {

	int i, ibin;

	/* Initialize the bin values */
	for (i = -NBIN; i <= NBIN; i++)
	     bin[i] = 0.0;

	/* Sort the pixel values into the bins */
	for (i = 0; i < npts; i++) {
	     ibin = (int)(0.5+fabs(arr[i]-mode)/binsize);
	     if (arr[i] < mode)
		 ibin = -ibin;

	     if (abs(ibin) <= NBIN)
		 bin[ibin] += 1;
	}

	/* Normalize the histogram bins */
	for (i = -NBIN; i <= NBIN; i++)
	     bin[i] /= (float)(npts*binsize);

}

