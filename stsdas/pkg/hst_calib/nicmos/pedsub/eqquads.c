# include "math.h"
# include "stdio.h"
# include "stdlib.h"
# include "float.h"
# include "pedsub.h"

/* EQQUADS --  Equalize image quadrants in a NICMOS image.
**
**	Based on the "EQUALQUAD" routine from Roeland van der Marel's
**	"unpedestal" program.
**
**
**
** This routine determines the constants that must be added to each quadrant
** of a NICMOS image to make the image continuous across the quadrant
** boundaries.
**
** If the parameter EqFlat=0, then the constant that is determined for each
** quadrant is simply added to the image. If EqFlat=1, then the constant 
** multiplied by the flatfield is added to the image. In the latter case,
** the routine is called several times so as to allow convergence to be
** reached. 
**
** For each quadrant the routine does either a polynomial fit to the rows and
** columns near a quadrant edge or simply computes the median or mode of the
** pixel values near a quadrant edge. If performing a polynomial fit, it
** extrapolates the fits for adjacent quadrants to the actual edge, and the
** difference of the two extrapolations is calculated. The squared differences
** along all edges are accumulated. Offsets are added to each quadrant until
** the differences are minimized. The minimization is done using an "amoeba"
** (downhill simplex) method.
**
** Revision history:
** ----------------
** H. Bushouse		19-Aug-1999	Implementation
** H. Bushouse		19-Jan-2000	Replaced original fit method with
**					amoeba-based routine "dcquad" from
**					I. Busko's "dccorr" task.
** H. Bushouse		14-Jun-2000	Added saving of offset values in new
**					DCValue members of info structure.
**
*/

int EqQuads (PedInfo *info, SingleNicmosGroup *input, SingleNicmosGroup *Flatim,	     FILE *fp, Bool verbose) {

/* Arguments:
**	info	i: task info structure
**	input  io: input image
**	Flatim	i: flatfield image
**	fp	i: log file pointer
**	verbose	i: verbose output switch
*/

	/* Local variables */
	int i, j;			/* pixel indexes */
	int iter;			/* iteration loop index */
	int Niter;			/* iteration number */
	float mean, median, mode, stdv, /* image statistics */
	      min, max;
	float p[5];			/* quadrant offsets */
	float modenew[5], mednew[5];	/* new quadrant modes and medians */
	float rejfrac;			/* outlier rejection fraction */
	float threshold;		/* rejection threshold */
	float of1, of2, of3;		/* quadrant offsets */

	/* Function declarations */
	void dcquad (float *, short *, short, int, int, float, float, int,
		     float *, float *, float *);
	int  n_stats (SingleNicmosGroup *, int, int, int, int, float, float,
		      short, float *, float *, float *, float *, float *,
		      float *);

	/* Write a message to the screen. Also set the number of iterations.
	** In the case EqFlat=1, iterate this routine several times,
	** to make sure one converges. */
	if (info->EqFlat) {
	    Niter = 3;
	    if (verbose) {
		sprintf (MsgText,
		"  Adding a constant times flatfield to each quadrant to\n");
		n_message (MsgText);
		sprintf (MsgText,
		"  make transitions continuous at all quadrant boundaries:\n");
		n_message (MsgText);
	    }
	} else {
	    Niter = 1;
	    if (verbose) {
		sprintf (MsgText,
		"  Adding a constant to each quadrant to make\n");
		n_message (MsgText);
		sprintf (MsgText,
		"  transitions continuous at all quadrant boundaries:\n");
		n_message (MsgText);
	    }
	}

	/* Set rejection fraction and threshold */
	rejfrac = 20.0;
	threshold = 100.0;

	/* Start the loop over the multiple iterations */
	for (iter=1; iter <= Niter; iter++) {

	     /* Compute and minimize quadrant offsets */
	     dcquad (input->sci.data.data, input->dq.data.data, info->BitMask,
		     info->EqPix1, info->EqPix2, rejfrac, threshold,
		     info->EqOrder, &of1, &of2, &of3);

	     /* Extract offsets from dcquad structure */
	     p[0] = (of1+of2+of3)/3.0;
	     p[1] = of3 - p[0];
	     p[2] = of2 - p[0];
	     p[3] = 0.0 - p[0];
	     p[4] = of1 - p[0];

	     /* Save the accumulated offsets */
	     info->DCValue[0] += p[1];
	     info->DCValue[1] += p[2];
	     info->DCValue[2] += p[3];
	     info->DCValue[3] += p[4];

	     /* Add computed offsets to image quadrants. If EqFlat is
	     ** is turned on, multiply the offsets by the Flat image. */
	     for (info->quad=0; info->quad<4; info->quad++) {
		  if (info->EqFlat) {
		      for (j=QYI[info->quad]; j<=QYF[info->quad]; j++) {
			   for (i=QXI[info->quad];i<=QXF[info->quad];i++){
				Pix(input->sci.data,i,j) += p[info->quad+1] *
						     Pix(Flatim->sci.data,i,j);
			   }
		      }
		  } else {
		      for (j=QYI[info->quad]; j<=QYF[info->quad]; j++) {
			   for (i=QXI[info->quad];i<=QXF[info->quad];i++){
				Pix(input->sci.data,i,j) += p[info->quad+1];
			   }
		      }
		  }
	     }

	     /* Compute basic statistics for the corrected image:
	     ** For each quadrant, get the mode and the median of the data.
	     ** Use only the pixels that are not masked. */
	     for (info->quad=0; info->quad<4; info->quad++) {
		  if (n_stats (input, info->qx1[info->quad],
			       info->qx2[info->quad], info->qy1[info->quad],
			       info->qy2[info->quad], -FLT_MAX, FLT_MAX,
			       info->BitMask, &mean, &median, &mode, &stdv,
			       &min, &max))
		      return (1);
		  modenew[info->quad+1] = mode;
		  mednew[info->quad+1]  = median;
	     }

	     /* Write the results to the screen and the log file */
	     if (verbose) {
		 sprintf(MsgText, "  Quad  correction  new mode  new median\n");
		 n_message (MsgText);
		 for (info->quad=0; info->quad<4; info->quad++) {
		      sprintf (MsgText, "%6d %9.6f   %9.6f %9.6f\n",
			       info->quad+1, p[info->quad+1],
			       modenew[info->quad+1], mednew[info->quad+1]);
		      n_message (MsgText);
		 }
	     }
	     if (info->KeepLog) {
		 fprintf (fp, "\n#quad  correction  new mode  new median\n");
		 for (info->quad=0; info->quad<4; info->quad++) {
		      fprintf (fp, "#%4d %9.6f   %9.6f %9.6f\n", info->quad+1,
			       p[info->quad+1], modenew[info->quad+1],
			       mednew[info->quad+1]);
		 }
	     }

	} /* End of loop over iterations */

	return (0);

}

