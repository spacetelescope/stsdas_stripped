# include <stdio.h>
# include <string.h>
# include <ximio.h>
# include "pedsky.h"

/*   N_SINGLEFILE  --  Process one file.
**
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**
*/

int n_singleFile (TaskInfo *info, char *infile, char *outfile, Bool verbose) {

	/* Local variables */
	SingleNicmosGroup Input;	/* Input image */
	SingleNicmosGroup Flat;		/* Flatfield image */
	ShortTwoDArray    oldDQ;	/* Original DQ array */

	/* Function declarations */
	int  n_idSources (SingleNicmosGroup *);
	int  n_getFlat (char *, SingleNicmosGroup *, SingleNicmosGroup *, Bool);
	void n_invFlat (SingleNicmosGroup *);
	int  n_unFlat (SingleNicmosGroup *, SingleNicmosGroup *);
	int  n_reFlat (SingleNicmosGroup *, SingleNicmosGroup *);
	int  findSky_Quick (TaskInfo *, SingleNicmosGroup *,
			    SingleNicmosGroup *, Bool);
	int  findSky_Iter  (TaskInfo *, SingleNicmosGroup *,
			    SingleNicmosGroup *, Bool);
	int  findPed (TaskInfo *, SingleNicmosGroup *, Bool);
	void subSky (TaskInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	void subPed (TaskInfo *, SingleNicmosGroup *);
	int  storeSky (TaskInfo *, SingleNicmosGroup *);
	int  storePed (TaskInfo *, SingleNicmosGroup *);
	int  n_history (TaskInfo *, SingleNicmosGroup *, char *);
	int  n_copyDQ (ShortTwoDArray *, ShortTwoDArray *);
	void n_replaceDQ (ShortTwoDArray *, ShortTwoDArray *);

	/* Read the input file. Only IMSET #1 is read. */
	initSingleNicmosGroup (&Input);
	if (getSingleNicmosGroup (infile, 1, &Input))
	    return (1);

	/* Keep a copy of the input DQ array if the user doesn't
	** want to keep the source flags set by this task. */
	if (!info->keepFlags) {
	    if (n_copyDQ (&oldDQ, &Input.dq.data))
		return (1);
	}

	/* Flag sources in the input image */
	if (n_idSources (&Input))
	    return (1);

	/* Load the FLATFIELD image. Also invert the Flat image
	** data for easier use later in the program. */
	if (n_getFlat (info->FlatName, &Input, &Flat, verbose))
	    return (1);
	n_invFlat (&Flat);

	/* Check the flatfielding status of the input file and undo
	** the correction if necessary */
	if (n_unFlat (&Input, &Flat))
	    return (1);

	/* Find and/or subtract sky, if requested */
	if (info->SkyMode != NONE) {

	    /* Find the optimal sky value */
	    if (info->SkyMode == QUICK) {
		if (findSky_Quick (info, &Input, &Flat, verbose))
		    return (1);
	    } else if (info->SkyMode == ITER) {
		if (findSky_Iter  (info, &Input, &Flat, verbose))
		    return (1);
	    }

	    /* Subtract the sky */
	    subSky (info, &Input, &Flat);

	}

	/* Find pedestal levels, if not determined by sky routine */
	if (info->SkyMode == CONSTANT || info->SkyMode == NONE) {
	    if (findPed (info, &Input, verbose))
		return (1);
	}

	/* Subtract the pedestal level in each quadrant */
	subPed (info, &Input);

	/* If we had to undo the flatfield correction earlier,
	** reapply it now. */
	if (n_reFlat (&Input, &Flat))
	    return (1);

	/* If the user didn't want to keep the source flags, copy the
	** original DQ array back into output image */
	if (!info->keepFlags)
	    n_replaceDQ (&Input.dq.data, &oldDQ);

	/* Add processing history records to header */
	if (n_history (info, &Input, outfile))
	    return (1);
	if (storeSky (info, &Input))
	    return (1);
	if (storePed (info, &Input))
	    return (1);

	/* Save the final results */
	if (verbose) {
	    sprintf (MsgText, "  Saving final results to file %s\n", outfile);
	    n_message (MsgText);
	}
	if (putSingleNicmosGroup (outfile, 1, &Input, 0))
	    return (1);

	/* Free the data for this file. */
	freeSingleNicmosGroup (&Input);
	freeSingleNicmosGroup (&Flat);

	return (0);
}

