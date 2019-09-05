# include <stdio.h>
# include <ximio.h>
# include "biaseq.h"

/*   N_SINGLEFILE  --  Process data from one file.
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	25-Mar-1999	Implementation.
**
*/

int n_singleFile (char *infile, char *outfile, BiasInfo *info, Bool verbose) {

/* Arguments:
**	infile	i: input file name
**	outfile	i: output file name
**	info   io: task information structure
**	verbose	i: verbose output switch
*/

	/* Local variables */
	Hdr               pheader;	/* input image primary header */
	MultiNicmosGroup  input;	/* input image */

	/* Function definitions */
	int n_getPriHdr (char *, Hdr *);
	int n_getRawData (char *, MultiNicmosGroup *, int);
	int doBiasCorr (BiasInfo *, MultiNicmosGroup *, Bool);
	int n_history (BiasInfo *, MultiNicmosGroup *, char *);
	int n_putMultiCalData (MultiNicmosGroup *, char *);

	/* Get input's primary header. */
	if (n_getPriHdr (infile, &pheader))
            return (1);

	/* Get number of IMSETs. */
	if (getKeyI (&pheader, "NEXTEND", &(info->nimsets))) {
            n_error ("Error reading NEXTEND keyword.");
	    return (1);
	} else {
	    info->nimsets /= 5;
	}

	/* Get camera number. */
	if (getKeyI (&pheader, "CAMERA", &(info->camera))) {
	    n_error ("Error reading CAMERA keyword.");
	    return (1);
	}

	/* Close input primary header. */
	freeHdr (&pheader);

	/* Read the whole input file */
	if (verbose) {
	    sprintf (MsgText, "  Reading input data for %s\n", infile);
	    n_message (MsgText);
	}
	if (n_getRawData (infile, &input, info->nimsets))
	    return (1);

	/* Do the bias correction on this file */
	if (doBiasCorr (info, &input, verbose))
	    return (1);

	/* Add processing history record to header */
	if (n_history (info, &input, outfile))
	    return (1);

	/* Save the final results */
	if (verbose) {
	    sprintf (MsgText, "  Saving results to file %s\n", outfile);
	    n_message (MsgText);
	}
	if (n_putMultiCalData (&input, outfile))
	    return (1);

	/* Free the data for this file. */
	freeMultiNicmosGroup (&input);

	/* Successful return */
	return (0);

}

