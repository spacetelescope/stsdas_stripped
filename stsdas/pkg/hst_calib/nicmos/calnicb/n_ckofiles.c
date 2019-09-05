# include <stdio.h>

# include <hstio.h>	/* defines HST I/O data structures */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_CKOUTPUTFILES: Checks for the existence of all output MOS and SPT
** files to be produced by CALNICB and if they already exist either
** deletes them or notifies the user with an error, depending on the
** setting of the imclobber environment variable. If imclobber is set
** "yes" existing output files will be deleted; otherwise an error
** message will be issued and CALNICB will stop.
**
** Revision history:
** H.Bushouse	06-May-1997	Written for Build 2 (Version 2.0)
*/

int n_ckOutputFiles (AsnInfo *asn) {

/* Arguments:
**	asn	i: association info structure
*/

	/* Local variables */
	int i;			/* loop index */
	int num_exist;		/* number of existing output files */
	int file_stat;		/* file status */
	char fname[SZ_NAME+1];	/* file name */

	/* Function definitions */

	/* Loop through the list of output images */
	num_exist = 0;
	for (i = asn->nmembers; i < asn->tmembers; i++) {

	     /* Construct the MOS file name for this output image */
	     fname[0] = '\0';
	     sprintf (fname, "%s%s", asn->member[i].name, "_mos.fits");

	     /* Check to see if it already exists */
	     file_stat = ckNewFile (fname);
	     if (file_stat == -1) {
		 sprintf (MsgText, "Existing output file \"%s\" was deleted",
			  fname);
		 n_warn  (MsgText);
	     } else if (file_stat > 0) {
		 sprintf (MsgText, "Output file \"%s\" already exists", fname);
		 n_error (MsgText);
		 num_exist++;
	     }

	     /* Construct the SPT file name for this output image */
	     fname[0] = '\0';
	     sprintf (fname, "%s%s", asn->member[i].name, "_spt.fits");

	     /* Check to see if it already exists */
	     file_stat = ckNewFile (fname);
	     if (file_stat == -1) {
		 sprintf (MsgText, "Existing output file \"%s\" was deleted",
			  fname);
		 n_warn  (MsgText);
	     } else if (file_stat > 0) {
		 sprintf (MsgText, "Output file \"%s\" already exists", fname);
		 n_error (MsgText);
		 num_exist++;
	     }
	}

	/* Error return */
	if (num_exist > 0)
	    return (status = 1);

	/* Successful return */
	else
	    return (status = 0);

}

