# include <float.h>
# include "biaseq.h"

/*   SUBBIAS  --  Subtract bias from first difference image.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	08-May-2000	Implementation.
**
*/

int subBias (SingleNicmosGroup *bias, SingleNicmosGroup *fdiff, int cam,
	     short bitmask, int *reglim, int nregs) {

/* Arguments:
**	bias		i: bias image
**	fdiff	       io: first differences image
**	cam		i: camera number
**	bitmask		i: DQ rejection mask
**	reglim		i: bias region limits
**	nregs		i: number of bias regions
*/

	/* Local variables */
	int i, j, k, q;			/* loop indexes */
	int i1, i2, j1, j2;		/* bias jump region limits */
	float mean, median, mode;	/* image statistics */
	float stdv, min, max;		/* image statistics */

	/* Function declarations */
	int n_iterstat (SingleNicmosGroup *, int, int, int, int, float, float,
			short, float, int, float *, float *, float *, float *,
			float *, float *);

	/* Loop over image quadrants */
	for (q = 0; q < 4; q++) {

	     /* Compute and subtract the median of each jump
	     ** region in this quadrant of the bias image */
	     for (k=0; k<nregs; k++) {

		  if (cam==1) {
		      i1 = QXI[q];
		      i2 = QXF[q];
		      j1 = QYI[q] + reglim[2*k];
		      j2 = QYI[q] + reglim[2*k+1];
		  } else {
		      i1 = QXI[q] + reglim[2*k];
		      i2 = QXI[q] + reglim[2*k+1];
		      j1 = QYI[q];
		      j2 = QYF[q];
		  }

		  /* Compute the clipped median of the region */
		  if (n_iterstat (bias, i1, i2, j1, j2, -FLT_MAX, FLT_MAX,
		      bitmask, 5.0, 10, &mean, &median, &mode, &stdv, &min,
		      &max))
		      return (1);

		  /* Subtract the median from the region */
		  for (j = j1; j <= j2; j++) {
		       for (i = i1; i <= i2; i++) {
			    Pix(fdiff->sci.data,i,j) -= median;
		       }
		  }
	     }
	}

	return (0);
}

