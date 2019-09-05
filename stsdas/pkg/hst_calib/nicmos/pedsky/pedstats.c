# include <float.h>
# include "pedsky.h"

/*   PEDSTATS  --  Compute statistics for a pedestal-subtracted NICMOS image.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	18-May-1999	Implementation
**
*/

int pedStats (TaskInfo *info, SingleNicmosGroup *input) {

/* Arguments:
**	info   io: task info structure
**	input	i: input image
*/

	/* Local variables */
	float mean, median, mode, stdv, min, max;	/* image statistics */
	SingleNicmosGroup ringim;			/* filtered image */

	/* Function declarations */
	int n_iterstat (SingleNicmosGroup *, int, int, int, int, float, float,
			short, float, int, float *, float *, float *, float *,
			float *, float *);
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	int n_rmedian (SingleNicmosGroup *, SingleNicmosGroup *, float, float,
		       float, float);

	/* Median filter the subtracted image, if requested */
	if (info->doRingMedian) {

	    if (n_copyGroup (&ringim, input))
		return (1);

	    if (n_rmedian (input, &ringim, info->RingInner, info->RingOuter,
			   1, 0))
		return (1);

	    /* Compute statistics on the filtered image */
	    if (n_iterstat (&ringim, info->statlim[0], info->statlim[1],
			    info->statlim[2], info->statlim[3], -FLT_MAX,
			    FLT_MAX, info->BitMask, 5.0, 10, &mean, &median,
			    &mode, &stdv, &min, &max))
		return (1);

	    freeSingleNicmosGroup (&ringim);

	} else {

	    /* Compute statistics on the unfiltered image */
	    if (n_iterstat (input, info->statlim[0], info->statlim[1],
			    info->statlim[2], info->statlim[3], -FLT_MAX,
			    FLT_MAX, info->BitMask, 5.0, 10, &mean, &median,
			    &mode, &stdv, &min, &max))
		return (1);
	}

	/* Save stddev */
	info->rms = stdv;

	/* Successful return */
	return (0);
}

