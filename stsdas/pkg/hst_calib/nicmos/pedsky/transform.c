# include "pedsky.h"

/*   TRANSFORM  --  Subtract sky and pedestal signal from a NICMOS image,
**		    and compute residuals of result.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	18-May-1999	Implementation
**
*/

int transform (TaskInfo *info, SingleNicmosGroup *input,
	       SingleNicmosGroup *Flat, Bool verbose) {

/* Arguments:
**	info   io: task info structure
**	input  io: input image
**	Flat	i: flatfield image
**	verbose	i: verbose output switch
*/

	/* Function declarations */
	void subSky   (TaskInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	int  findPed  (TaskInfo *, SingleNicmosGroup *, Bool);
	void subPed   (TaskInfo *, SingleNicmosGroup *);
	int  pedStats (TaskInfo *, SingleNicmosGroup *);

	/* Subtract sky */
	subSky (info, input, Flat);

	/* Compute pedestal values for each quadrant */
	if (findPed (info, input, verbose))
	    return (1);

	/* Subtract the pedestal values from each quadrant */
	subPed (info, input);

	/* Get statistics for resulting image */
	if (pedStats (info, input))
	    return (1);

	/* Successful return */
	return (0);

}

