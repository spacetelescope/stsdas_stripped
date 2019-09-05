# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <float.h>
# include "pedsub.h"

/*   TRANSFORM  --  Subtract pedestal and apply transform to a
**		    NICMOS image quadrant.
**
**	Based on TRANSFORM routine from Roeland van der Marel's
**	"unpedestal" program.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	01-July-1999	Implementation
**
*/

void transform (PedInfo *info, SingleNicmosGroup *input,
	        SingleNicmosGroup *Flatim, float pedval, int filter,
	        SingleNicmosGroup *out) {

/* Arguments:
**	info	i: task info structure
**	input	i: input image
**	Flatim	i: flatfield image
**	pedval	i: pedestal value
**	filter	i: filter mode
**	out	o: output (transformed) image
*/

	/* Local variables */
	int i, j;	/* pixel indexes */

	/* Function declarations */
	void imgFilter (PedInfo *, SingleNicmosGroup *, int);

	/* Subtract the pedestal times the flatfield value from
	** the current image quadrant */
	for (j = QYI[info->quad]; j <= QYF[info->quad]; j++) {
	     for (i = QXI[info->quad]; i <= QXF[info->quad]; i++) {
		  Pix(out->sci.data,i,j) = Pix(input->sci.data,i,j) -
					   pedval*Pix(Flatim->sci.data,i,j);
		  DQSetPix(out->dq.data,i,j, (DQPix(input->dq.data,i,j) |
					     DQPix(Flatim->dq.data,i,j)));
	     }
	}

	/* Apply requested filtering, if any */
	if (filter != NONE)
	    imgFilter (info, out, filter);

}

/*  IMGFILTER  -- Filter a NICMOS image.
**
**    The requested type of filtering is applied to a NICMOS image quadrant.
**    The filter types are either a median or an unsharp mask (which is
**    computed by subtracting the median filtered image from the original
**    image).
*/

void imgFilter (PedInfo *info, SingleNicmosGroup *img, int filter) {

/* Arguments:
**	info	i: task info structure
**	img    io: image
**	filter	i: filter type
*/

	/* Local variables */
	int i, j;		/* pixel indexes */
	float **medimg;		/* median filtered image array */

	/* Function declarations */
	void imgMedian (PedInfo *, SingleNicmosGroup *, float **);
	float **matrix (int, int, int, int);
	void free_matrix (float **, int, int, int, int);

	/* Allocate memory for temp arrays */
	medimg = matrix (QXI[info->quad], QXF[info->quad], QYI[info->quad],
			 QYF[info->quad]);

	/* Compute median-filtered image */
	imgMedian (info, img, medimg);

	/* Replace the original image with the appropriate filtered values:
	** if filter=MEDIAN, the output is median filtered,
	** if filter=MASK, the output is unsharp masked (input-median) */
	if (filter == MEDIAN) {
	    for (j=QYI[info->quad]; j <= QYF[info->quad]; j++) {
		 for (i=QXI[info->quad]; i <= QXF[info->quad]; i++) {
		      Pix(img->sci.data,i,j) = medimg[i][j];
		 }
	    }
	} else if (filter == MASK) {
	    for (j=QYI[info->quad]; j <= QYF[info->quad]; j++) {
		 for (i=QXI[info->quad]; i <= QXF[info->quad]; i++) {
		      Pix(img->sci.data,i,j) -= medimg[i][j];
		 }
	    }
	}

	/* Free local memory */
	free_matrix (medimg, QXI[info->quad], QXF[info->quad],
		     QYI[info->quad], QYF[info->quad]);

}

/*  IMGMEDIAN  -  Image median filter.
**
**    Median filter a NICMOS image quadrant.
*/

# define SETD(a)  (a > 0 ? 1 : (-1))
# define UNSETD(a) (a > 0 ? 1 : 0)

void imgMedian (PedInfo *info, SingleNicmosGroup *img, float **medimg) {

/* Arguments:
**	info	i: task info structure
**	img	i: input image
**	medimg	o: median-filtered image
*/

	/* Local variables */
	int i, j, k, l, ii, jj;	/* pixel indexes */
	int x1, x2, y1, y2;
	int nmed;		/* number of points in median array */
	int nmed2;
	int nbox;
	int xwid, cen, beg, end, xdl1, xdl2;
	float *arr, *arr2;	/* work array */
	int *darr;

	/* Function declarations */
	float nrselect (int, int, float *);

	/* Allocate memory for local work arrays */
	nbox = 2*info->MMax+1;
	arr  = (float *)calloc(nbox*nbox, sizeof(float));
	arr2 = (float *)calloc(nbox*nbox, sizeof(float));
	darr = (int *)calloc(nbox*nbox, sizeof(int));

	/* Loop over the image quadrant */
	for (j = QYI[info->quad]; j <= QYF[info->quad]; j++) {
	     for (i = QXI[info->quad]; i <= QXF[info->quad]; i++) {

	     /* For interior locations, discard values that have dropped
	     ** off the left side of the box and insert new values that
	     ** have come into the right side. */
	     if (i >  QXI[info->quad]+info->MMax &&
		 i <= QXF[info->quad]-info->MMax &&
		 j >= QYI[info->quad]+info->MMax &&
		 j <= QYF[info->quad]-info->MMax) {
 
	         x1 = i-info->MMax;
	         x2 = i+info->MMax;
	         y1 = j-info->MMax;
	         y2 = j+info->MMax;

	         /* Reshuffle the arrays to drop left column */
	         for (jj = 0; jj < nbox; jj++) {
		      for (ii = 1; ii < nbox; ii++) {
		           arr[ii+nbox*jj-1] = arr[ii+nbox*jj];
		           darr[ii+nbox*jj-1] = UNSETD(darr[ii+nbox*jj]);
		      }
	         }

	         /* Load new right column into arrays */
	         ii = x2;
	         for (jj = y1; jj <= y2; jj++) {
		      arr[ii-x1+nbox*(jj-y1)] = Pix(img->sci.data,ii,jj);
		      darr[ii-x1+nbox*(jj-y1)] = DQPix(img->dq.data,ii,jj) &
					     info->BitMask;
	         }

	         /* Set flags for pixels to mask within inner box radius */
	         for (jj = j-info->MMin+1; jj <= j+info->MMin-1; jj++) {
		      for (ii = i-info->MMin+1; ii <= i+info->MMin-1; ii++) {
		           darr[ii-x1+nbox*(jj-y1)] = 
			   SETD(darr[ii-x1+nbox*(jj-y1)]);
		      }
	         }


	     }

	     /* For edge locations, reload the entire box */
	     else {

	         nmed = 0;

	         /* Loop over the box centered on this pixel */
	         x1 = MAX(i-info->MMax,QXI[info->quad]);
	         x2 = MIN(i+info->MMax,QXF[info->quad]);
	         y1 = MAX(j-info->MMax,QYI[info->quad]);
	         y2 = MIN(j+info->MMax,QYF[info->quad]);
	         for (jj = y1; jj <= y2; jj++) {
		      for (ii = x1; ii <= x2; ii++) {
		           arr[nmed] = Pix(img->sci.data,ii,jj);
		           darr[nmed]=DQPix(img->dq.data,ii,jj)&info->BitMask;
		           nmed++;
		      }
	         }

	         /* Set flags for pixels to mask within inner box radius */
	         xwid = x2 - x1 + 1;
	         xdl1 = MIN(i-x1,info->MMin-1); xdl2 = MIN(x2-i,info->MMin-1);
	         cen = MIN(i-x1,info->MMax) + xwid*MIN(j-y1,info->MMax);
	         beg = cen - xdl1 - xwid*MIN(j-y1,info->MMin-1);
	         end = cen + xdl2 + xwid*MIN(y2-j,info->MMin-1);
	         for (k = beg; k <= end; k+=xwid) {
		      for (l = k; l <= k+xdl1+xdl2; l++) {
		           darr[l] = SETD(darr[l]);
		      }
	         }

	     }

	     /* Copy the original data to work array without flagged pixels */
	     nmed2 = 0;
	     for (k = 0; k < nmed; k++) {
		  if (darr[k] == 0) {
		      arr2[nmed2] = arr[k];
		      nmed2++;
		  }
	     }

	     /* Compute median of box region */
	     if (2*(nmed2/2)==nmed2)
		 medimg[i][j] = 0.5 * (nrselect(nmed2/2,nmed2,arr2-1) +
				 nrselect((nmed2/2)+1,nmed2,arr2-1));
	     else
		 medimg[i][j] = nrselect((nmed2+1)/2,nmed2,arr2-1);

	     }
	}

	/* Free local arrays */
	free (arr);
	free (arr2);
	free (darr);

}

