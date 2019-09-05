# include <stdio.h>
# include <string.h>
# include <ximio.h>
# include "pedsub.h"

/*   N_SINGLEFILE  --  Process one file.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Implementation
**
*/

int n_singleFile (PedInfo *info, char *infile, char *outfile, FILE *fp,
		  Bool verbose) {

/* Arguments:
**	info	io: task info structure
**	infile	 i: input file name
**	outfile	 i: output file name
**	fp	 i: log file pointer
**	verbose	 i: verbose output switch
*/

	/* Local variables */
	char Kw[40];			/* string keyword value */
	SingleNicmosGroup input;	/* input image */
	SingleNicmosGroup Flatim;	/* flatfield image */

	/* Function declarations */
	int n_getFlat (char *, SingleNicmosGroup *, SingleNicmosGroup *, Bool);
	int findBestPed (PedInfo *, SingleNicmosGroup *,
			 SingleNicmosGroup *, FILE *, Bool);
	int EqQuads (PedInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
		     FILE *, Bool);
	int n_history (PedInfo *, SingleNicmosGroup *, char *);
	int storePed (PedInfo *, SingleNicmosGroup *);

	/* Read the input file. Only IMSET #1 is read. */
	initSingleNicmosGroup (&input);
	if (getSingleNicmosGroup (infile, 1, &input)) {
	    sprintf (MsgText, "Can't read input file %s", infile);
	    n_error (MsgText);
	    return (1);
	}

	/* Initialize the pedestal values */
	info->PedValue[0] = 0; info->PedValue[1] = 0;
	info->PedValue[2] = 0; info->PedValue[3] = 0;
	info->DCValue[0]  = 0; info->DCValue[1]  = 0;
	info->DCValue[2]  = 0; info->DCValue[3]  = 0;

	/* Check the input image to make sure that it has been flatfielded */
	Kw[0] = '\0';
	if (getKeyS (input.globalhdr, "FLATDONE", Kw)) {
	    sprintf (MsgText, "Can't read FLATDONE keyword from %s", infile);
	    n_error (MsgText);
	    return (1);
	}

	if (strncmp (Kw, "PERFORMED", 9) != 0) {
	    sprintf (MsgText,
		     "Image %s has not been flatfielded; can't process",infile);
	    n_warn  (MsgText);
	    freeSingleNicmosGroup (&input);
	    return (0);
	}

	/* Load the FLATFIELD image */
	if (n_getFlat (info->FlatName, &input, &Flatim, verbose))
	    return (1);

	/* Find the best pedestal values for the image */
	if (findBestPed (info, &input, &Flatim, fp, verbose))
	    return (1);

	/* Equalize the image quadrants, if requested */
	if (info->EqQuads) {
	    if (EqQuads (info, &input, &Flatim, fp, verbose))
		return (1);
	}

	/* Write history records to header */
	if (n_history (info, &input, outfile))
	    return (1);
	if (storePed (info, &input))
	    return (1);

	/* Save the final results */
	if (verbose) {
	    sprintf (MsgText, "  Saving final results to file %s\n", outfile);
	    n_message (MsgText);
	}
	if (putSingleNicmosGroup (outfile, 1, &input, 0))
	    return (1);

	/* Free the data for this file. */
	freeSingleNicmosGroup (&Flatim);
	freeSingleNicmosGroup (&input);

	return (0);
}

