# include <stdio.h>
# include <float.h>
# include "pedsky.h"

/*   FINDPED  --  Compute pedestal signal in a NICMOS image.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	18-May-1999	Implementation
**
*/

int findPed (TaskInfo *info, SingleNicmosGroup *input, Bool verbose) {

/* Arguments:
**	info   io: task info structure
**	input	i: input image
**	verbose	i: verbose output switch
*/

	/* Local variables */
	int quad;					/* quadrant index */
	float mean, median, mode, stdv, min, max;	/* image statistics */

	/* Function declarations */
	int n_iterstat (SingleNicmosGroup *, int, int, int, int, float, float,
			short, float, int, float *, float *, float *, float *,
			float *, float *);

	/* Loop over image quadrants */
	for (quad = 0; quad < 4; quad++) {

	     /* Compute statistics of quadrant */
	     if (n_iterstat (input, info->qx1[quad], info->qx2[quad],
			     info->qy1[quad], info->qy2[quad], -FLT_MAX,
			     FLT_MAX, info->BitMask, 5.0, 10, &mean, &median,
			     &mode, &stdv, &min, &max))
		 return (1);

	     /* Save median as pedestal value */
	     info->PedValue[quad] = median;

	     if (verbose) {
		 sprintf (MsgText, "  Quadrant %d pedestal estimate = %9.7g\n",
			  quad+1, info->PedValue[quad]);
		 n_message (MsgText);
	     }
	}

	return (0);
}

