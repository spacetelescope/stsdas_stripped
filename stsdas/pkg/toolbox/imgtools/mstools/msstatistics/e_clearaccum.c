# include <stdio.h>
# include <float.h>
# include "msstat.h"


/*  E_CLEARACCUM  -  Clear accumulators.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   07 Jun 96  -  Implementation (IB)
 *   11 Jul 96  -  Histogram deallocation (IB)
 *
 */

void e_clearAccum (Control *con) {

	int  i;

	for (i = 0; i < MAX_HDUS; i++) {
	    con->accum[i].min      =  DBL_MAX;
	    con->accum[i].max      = -DBL_MAX;
	    con->accum[i].sum      = 0.0;
	    con->accum[i].sum2     = 0.0;
	    con->accum[i].sum3     = 0.0;
	    con->accum[i].sum4     = 0.0;
	    con->accum[i].sumxw    = 0.0;
	    con->accum[i].sumww    = 0.0;
	    con->accum[i].npix     = 0L;
	    con->accum[i].floor    = 0.0F;
	    con->accum[i].ceiling  = 0.0F;
	    con->accum[i].binWidth = 0.0F;
	    con->accum[i].nbins    = 0;
	    if (con->accum[i].histogram != NULL) {
	        free (con->accum[i].histogram);
	    }
	    con->accum[i].histogram = NULL;
	}
}
