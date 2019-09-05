# include <stdio.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_CALREPORT: Reports calibration step information to processing log
** and to output SCI header history keywords. Also updates the calibration
** indicator and reference file pedigree keywords in the primary header.
*/

int n_calReport (CalStep *step, int group, Hdr *scihdr, Hdr *prihdr) {

/* Arguments:
**	step	io: calibration step information structure
**	group	 i: data group number
**	scihdr	io: science image header
**	prihdr	io: primary header
*/

	/* Function definitions */
	void updateTrailer (CalStep *);
	int updateSciHdr (CalStep *, Hdr *);
	int updatePriHdr (CalStep *, Hdr *);

	/* Write calibration step and reference file info to history
	** records in science image header */
	if (updateSciHdr (step, scihdr))
	    return (status);

	/* Write processing information to trailer file */
	if (group == 1)
	    updateTrailer (step);

	/* Update calibration indicator and reference file pedigree
	** keywords in primary header */
	if (group == 1) {
	    if (updatePriHdr (step, prihdr))
		return (status);
	}

	/* Successful return */
	return (status = 0);
}

void updateTrailer (CalStep *step) {

/* Arguments:
**	step	i: calibration step information
*/

	/* Local variables */
	char text[80];

	/* Report calibration step info to screen and trailer file */
	if (step->corr == PERFORM) {

	    sprintf (MsgText, "%s performed", step->swname);

	    if (step->ref.name[0] != '\0') {
		sprintf (text, " using %s", step->ref.name);
		strcat (MsgText, text);
	    }

	    n_message (MsgText);

	} else if (step->corr == SKIP) {

	    sprintf (MsgText, "%s skipped", step->swname);

	    if (step->ref.name[0] != '\0' && step->ref.dummy) {
		sprintf (text, "; %s is dummy", step->ref.name);
		strcat (MsgText, text);
	    }

	    n_warn (MsgText);

	}

	return;
}

/* UPDATESCIHDR: Check whether pedigree indicates that the calibration
   file is dummy, and log history info accordingly. From one to three
   history records will be logged: the status of the calibration step
   (i.e. "omitted", "skipped", or "performed"), the value of the pedigree
   keyword, and the value of the descrip keyword.
*/
 
int updateSciHdr (CalStep *step, Hdr *scihdr) {
 
/* Arguments:
**	step	 i: calibration step information structure
**	scihdr	io: science extension image header
*/

        /* Local variables */
        char *history;         /* history record */
 
        /* Function definitions */
        int savHist (char *);
	int putHist (Hdr *);
 
	history = (char *)calloc(SZ_LIN+1,sizeof(char));
	history[0] = '\0';

	if (step->corr == PERFORM) {

	    if (step->ref.name[0] != '\0') {
		sprintf (history, "%s performed using %s", step->swname,
			 step->ref.name);
	    } else {
		sprintf (history, "%s performed", step->swname);
	    }

	} else if (step->corr == SKIP) {

	    if (step->ref.name[0] != '\0' && step->ref.dummy) {
		sprintf (history, "%s skipped because %s is dummy",
			 step->swname, step->ref.name);
	    } else {
		sprintf (history, "%s skipped", step->swname);
	    }

	} else if (step->corr == OMIT) {
	    return (status = 0);
	}

	if (savHist (history))
	    return (status);

	/* Save pedigree as history */
	if (step->ref.pedigree[0] != '\0') {
	    sprintf (history, "  %s", step->ref.pedigree);
	    if (savHist (history))
		return (status);
	}
 
	/* Save descrip as history */
	if (step->ref.descrip[0] != '\0') {
	    sprintf (history, "  %s", step->ref.descrip);
	    if (savHist (history))
		return (status);
	}
 
        /* Write the history records to the science image header */
        if (putHist (scihdr))
	    return (status);
 
	free (history);

	/* Successful return */
        return (status = 0);
}

/* UPDATEPRIHDR: Update the values of the calibration indicator and
** reference file pedigree keywords in the primary header */

int updatePriHdr (CalStep *step, Hdr *prihdr) {

/* Arguments:
	step	 i: calibration step information
	prihdr	io: primary header
*/

	/* Update calibration indicator keyword if step was performed */
	if (step->corr == PERFORM) {
	    if (putKeyS (prihdr, step->indname, "PERFORMED", ""))
		return (status = 1);

	    /* Update the reference file pedigree keyword for this step */
	    if (step->ref.pedigree[0] != '\0') {
		if (putKeyS (prihdr, step->pdname, step->ref.pedigree, ""))
		    return (status = 1);
	    }
	}

	/* If this step was performed in a previous run, don't set
	** the keyword value back to SKIPPED or OMITTED */
	if (step->done == PERFORMED)
	    return (status = 0);

	if (step->corr == SKIP) {
	    if (putKeyS (prihdr, step->indname, "SKIPPED", ""))
		return (status = 1);

	    /* Update the reference file pedigree keyword for this step */
	    if (step->ref.pedigree[0] != '\0') {
		if (putKeyS (prihdr, step->pdname, step->ref.pedigree, ""))
		    return (status = 1);
	    }

	} else if (step->corr == OMIT) {
	    if (putKeyS (prihdr, step->indname, "OMITTED", ""))
		return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}
