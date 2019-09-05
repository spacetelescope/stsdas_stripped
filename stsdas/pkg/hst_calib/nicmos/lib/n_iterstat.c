# include <float.h>
# include "nicmos.h"

/*   N_ITERSTAT  --  Do iterative image statistics computation.
**
**	Based on IRAF CL script "iterstat.cl" by Mark Dickinson.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	01-Feb-2000	Implementation.
**
*/

int n_iterstat (SingleNicmosGroup *input, int x1, int x2, int y1, int y2, 
		float lower, float upper, short dqmask, float nsigrej,
		int maxiter, float *mean, float *median, float *mode,
		float *stdv, float *min, float *max) {

	/* Local variables */
	int   iter;
	float ll, ul;
	float last_med;

	/* Function declarations */
	int n_stats (SingleNicmosGroup *, int, int, int, int, float, float,
		     short, float *, float *, float *, float *, float *,
		     float *);

	/* Compute initial statistics */
	if (n_stats (input, x1, x2, y1, y2, lower, upper, dqmask,
		     mean, median, mode, stdv, min, max))
	    return (1);
	last_med = (*median);

	/* Loop over rejection cycles */
	iter = 1;
	while (iter <= maxiter) {

	       /* Reset rejection limits */
	       ll = (*mean) - nsigrej * (*stdv);
	       ul = (*mean) + nsigrej * (*stdv);
	       if (lower != -FLT_MAX && ll < lower) ll = lower;
	       if (upper !=  FLT_MAX && ul > upper) ul = upper;

	       /* Recompute statistics */
	       if (n_stats (input, x1, x2, y1, y2, ll, ul, dqmask,
			    mean, median, mode, stdv, min, max))
		   return (1);

	       /* Exit loop if result has converged */
	       if ((*median) == last_med)
		   break;

	       /* Set params for next iteration */
	       last_med = (*median);
	       iter++;
	}

	return (0);

}

