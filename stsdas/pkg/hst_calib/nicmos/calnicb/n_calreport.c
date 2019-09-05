# include <stdio.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_CALREPORT: Reports calibration step information to processing log and to
** output SCI header history keywords. Also updates the calibration indicator
** and reference file pedigree keywords in the primary header.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	01-Dec-1997	Modified to use	new ref.dummy variable.
**				(Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 2.2.1)
*/

int n_calReport (CalStep *step, SingleNicmosGroup *image) {

/* Arguments:
**	step	 i: calibration step info structure
**	image	io: science image
*/

	/* Local variables */
	char history[72+1];	/* history record */

	/* Add history records to SCI image header and update the cal step
	** indicator in the primary header */
	if (step->corr == PERFORM) {
	    sprintf (history, "%s performed using %s", step->swname,
		     step->ref.name);
	    if (addHistoryKw (&(image->sci.hdr), history) == -1)
		return (status = 1);

	    if (putKeyS (image->globalhdr, step->indname, "PERFORMED", ""))
		return (status = 1);

	} else if (step->corr == SKIP) {

	    if (step->ref.name[0] != '\0' && step->ref.dummy) {
		sprintf (history, "%s skipped because %s is dummy",
			 step->swname, step->ref.name);
	    } else
		sprintf (history, "%s skipped", step->swname);

	    if (addHistoryKw (&(image->sci.hdr), history) == -1)
		return (status = 1);

	    if (putKeyS (image->globalhdr, step->indname, "SKIPPED", ""))
		return (status = 1);

	} else if (step->corr == OMIT) {

	    if (putKeyS (image->globalhdr, step->indname, "OMITTED", ""))
		return (status = 1);

	    return (status = 0);
	}

	/* Add Pedigree and Descrip as history records in SCI image header
	** and update the PDGR keyword in the primary header */
	if (step->ref.pedigree[0] != '\0') {
	    sprintf (history, "  %s", step->ref.pedigree);
	    if (addHistoryKw (&(image->sci.hdr), history) == -1)
		return (status = 1);
	}

	if (putKeyS (image->globalhdr, step->pdname, step->ref.pedigree, ""))
	    return (status = 1);

	if (step->ref.descrip[0] != '\0') {
	    sprintf (history, "  %s", step->ref.descrip);
	    if (addHistoryKw (&(image->sci.hdr), history) == -1)
		return (status = 1);
	}

	/* Successful return */
	return (status = 0);

}

