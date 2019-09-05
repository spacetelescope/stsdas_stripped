# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_CALNICB: Main CALNICB routine for performing pipeline processing of
** associated datasets. Reads the input association (ASN) table, the
** input images, the required calibration reference data files, calls the
** various calibration steps, and writes the output (mos) datasets and
** updated association (ASC) table.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	March 1997	Skip bck calculations for Darks (Version 0.1.5)
** H.Bushouse	30-Jan-1997	Added use of HSTIO ckNewFile function
**				(Version 2.0)
** H.Bushouse	27-Feb-1997	Added use of asn.docombine flag to control
**				calling of n_combNiter, instead of relying on
**				value of asn.niter (Version 2.0)
** H.Bushouse	10-Apr-1997	Moved in n_calReport from n_background so that
**				it still gets called when bck steps are skipped
**				for Darks (Version 0.1.5)
** H.Bushouse	28-Apr-1997	Removed mosnum from n_putMosData argument list
**				(Version 0.1.6)
** H.Bushouse	29-Apr-1997	Pass NicInfo to n_combNiter and n_mosaic; only
**				call n_idSources and n_background for external
**				(EXT) images (Version 2.0)
** H.Bushouse	06-May-1997	Use new function n_ckOutputFiles to check for
**				existence of output MOS and SPT files
**				(Version 2.0)
** H.Bushouse	01-Dec-1997	Added subbkg, usemeanbkg, crthresh, and xcwin
**				as input arguments; removed AsnInfo and mosnum
**                              from n_putMosData argument list (Version 2.2)
** H.Bushouse	17-Mar-1998	Added readbkg and readoffs as input arguments
**				(Version 2.2)
** H.Bushouse	19-Mar-1998	Added looping over individual images for call to
**				n_idSources; changed n_idSources definition
**				(Version 2.2)
** H.Bushouse	03-Jun-1998	Moved call to n_combNiter to follow n_idSources
**				so that xcorr routine has source-flagged pixels
**				to work with (Version 2.2)
*/

int n_calnicb (char *tname, char *subbkg, char *usemeanbkg, char *readbkg,
	       char *readoffs, float crthresh, int xcwin) {

/* Arguments:
**	tname		i: input association table name
**	subbkg		i: subtract scalar background flag
**	usemeanbkg	i: mean background flag
**	readbkg		i: read bkg values flag
**	readoffs	i: read image offsets flag
**	crthresh	i: CR rejection threshold
**	xcwin		i: xcorr window search half width
*/

	/* Local variables */
	clock_t start, finish;
	time_t  now;
	char time_stamp[30];

	int file_stat;			/* file status */
	int i;				/* loop index */
	NicInfo nic;             	/* Observation info */
	AsnInfo asn;			/* Association info */
	AsnImages input;		/* Input association images */
	SingleNicmosGroup back;		/* Background reference image */
	SingleNicmosGroup mosaic;	/* Mosaic image */

	/* Function definitions */
	int  n_getAsnTable (AsnInfo *, NicInfo *);
	int  n_ckOutputFiles (AsnInfo *);
	int  n_getInputData (AsnInfo *, AsnImages *);
	int  n_getRefData (NicInfo *, SingleNicmosGroup *);
	int  n_setup (NicInfo *, AsnImages *, AsnInfo *);
	int  n_combNiter (NicInfo *, AsnInfo *, AsnImages *);
	int  n_idSources (SingleNicmosGroup *);
	int  n_background (AsnInfo *, AsnImages *, SingleNicmosGroup *,
			   CalStep *);
	int  n_mosaic (NicInfo *, AsnInfo *, AsnImages *, int,
		       SingleNicmosGroup *);
	int  n_statcalc (SingleNicmosGroup *);
	int  n_putMosData (SingleNicmosGroup *);
	int  n_mkSPT (AsnInfo *, int);
	int  n_putAscTable (AsnInfo *, CalStep *);
	void n_mkNames (char *, char *);
	void n_initAsnInfo (AsnInfo *);
	void n_initRefData (SingleNicmosGroup *);
	void n_freeRefData (SingleNicmosGroup *);
	void n_freeAsnImages (AsnImages *);
	void n_freeAsnInfo (AsnInfo *);

	/* Post error handler */
	push_hstioerr (errchk);

	/* Print startup message */
	now = time(NULL);
	strftime (time_stamp, sizeof time_stamp, "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
	sprintf (MsgText,
		 "Starting CALNICB -- version %s -- %s", CALNICB_VERSION,
		  time_stamp);
	n_message (MsgText);

	/* Initialize the ASN info structure */
	n_initAsnInfo (&asn);

	/* Copy input table name to ASN info structure */
	strcpy (asn.asn_table, tname);

	/* Build complete input and output table names */
	n_mkNames (asn.asn_table, asn.asc_table);
        sprintf (MsgText, "Input: %s    Output: %s", asn.asn_table,
		 asn.asc_table);
        n_message (MsgText);

	/* Check to see if the output table already exists */
	file_stat = ckNewFile (asn.asc_table);
	if (file_stat == -1) {
	    sprintf (MsgText, "Existing output file \"%s\" was deleted",
		     asn.asc_table);
	    n_warn  (MsgText);
	} else if (file_stat > 0) {
	    sprintf (MsgText, "Output file \"%s\" already exists",
		     asn.asc_table);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Copy input arguments into AsnInfo structure */
	if (strncmp(subbkg, "no", 2) == 0)
	    asn.subbkg = False;
	if (strncmp(usemeanbkg, "no", 2) == 0)
	    asn.usemeanbkg = False;
	if (strncmp(readbkg, "yes", 3) == 0)
	    asn.readbkg = True;
	if (strncmp(readoffs, "yes", 3) == 0)
	    asn.readoffs = True;
	asn.crthresh = crthresh;
	asn.xcwin    = xcwin;

	/* Initialize the reference data pointers */
	n_initRefData (&back);

	start = clock();   /* "The clock has started!" */
 
	/*-----------------------------------------------------------------*/

	/* Read the Association Table information */
	if (n_getAsnTable (&asn, &nic))
	    return (status);

	/* Check for existence of output files */
	if (n_ckOutputFiles (&asn))
	    return (status);

	/* Load all input data files listed in the ASN table */
	if (n_getInputData (&asn, &input)) {
	    n_freeAsnImages (&input);
	    return (status);
	}

	/* Setup processing information */
	if (n_setup (&nic, &input, &asn)) {
	    n_freeAsnImages (&input);
	    return (status);
	}

	/* Load calibration reference data */
	if (n_getRefData (&nic, &back)) {
	    n_freeAsnImages (&input);
	    n_freeRefData (&back);
	    return (status);
	}

	/* Identify sources in the images (external observations only) */
	if (nic.obs_type == EXT) {

	    sprintf (MsgText, "Searching for sources in each image ...");
	    n_message (MsgText);

	    for (i=0; i < asn.nmembers; i++) {
		 if (asn.member[i].patpos != 0) {
		     if (n_idSources (&(input.member[i]))) {
			 n_freeAsnImages (&input);
			 n_freeRefData (&back);
			 return (status);
		     }
		 }
	    }
	}

	/* Combine NUMITER exposures at each pointing */
	if (asn.docombine) {
	    if (n_combNiter (&nic, &asn, &input)) {
		n_freeAsnImages (&input);
		n_freeRefData (&back);
		return (status);
	    }
	}

	/* Remove the background signal (external observations only) */
	if (nic.obs_type == EXT) {
	    if (n_background (&asn, &input, &back, &nic.BACK)) {
		n_freeAsnImages (&input);
		n_freeRefData (&back);
		return (status);
	    }
	}

	/* Free memory for the reference data */
	n_freeRefData (&back);

	/* Mosaic the images at each pointing */
	for (i=0; i < asn.nummos; i++) {

	     if (n_mosaic (&nic, &asn, &input, i, &mosaic)) {
		 n_freeAsnImages (&input);
		 return (status);
	     }

	     /* Compute statistics for the mosaic image */
	     if (n_statcalc (&mosaic)) {
		 n_freeAsnImages (&input);
		 freeSingleNicmosGroup (&mosaic);
		 return (status);
	     }

	     /* Write the Mosaic image */
	     if (n_putMosData (&mosaic)) {
		 n_freeAsnImages (&input);
		 freeSingleNicmosGroup (&mosaic);
		 return (status);
	     }

	     /* Make an SPT file to go with the mosaic image */
	     if (n_mkSPT (&asn, i)) {
		 n_freeAsnImages (&input);
		 freeSingleNicmosGroup (&mosaic);
		 return (status);
	     }

	     /* Free memory for the mosaic image */
	     freeSingleNicmosGroup (&mosaic);

	}

	/* Free memory for the input data */
	n_freeAsnImages (&input);

	/* Write the output Association (ASC) table */
	if (n_putAscTable (&asn, &nic.BACK))
	    return (status);

	/* Free memory for the ASN member info */
	n_freeAsnInfo (&asn);

	/*-----------------------------------------------------------------*/

	/* We're done! */
	now = time(NULL);
	strftime (time_stamp, sizeof time_stamp, "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
	sprintf (MsgText,
		 "Finished CALNICB -- %s", time_stamp);
	n_message (MsgText);
        finish = clock();
        /*printf (" [calibration steps took %6.2f seconds to execute]\n",
                (double)(finish-start)/CLOCKS_PER_SEC);*/
 
	/* Successful return */
	return (status = 0);
}

/* N_MKNAMES: Build the complete input and output file names from
** whatever the user supplied as arguments */

void n_mkNames (char *infile, char *outfile) {

/* Arguments:
**	infile	io: input file name
**	outfile	io: output file name
*/

	/* Local variables */
	Bool ext1;			/* Was a file extension specified? */
	Bool ext2;
	Bool suffix;			/* Was a file suffix specified? */
	char root[SZ_NAME];		/* root name of input file */
	int  rlen;			/* length of root name */
	char fitsext1[] = ".fits";	/* FITS file extension (default) */
	char fitsext2[] = ".fit";	/* FITS file extension (alternate) */
	char asnsuf[] = "_asn";		/* ASN file suffix */
	char ascsuf[] = "_asc";		/* ASC file suffix */

	/* Initialize */
	ext1   = False;
	ext2   = False;
	suffix = False;
	strcpy (root, infile);

	/* Search for "fits" extension on input file name */
	rlen = strlen(root);
	if (strncmp (root+rlen-5, fitsext1, 5) == 0) {
	    ext1 = True;
	    root[rlen-5] = '\0';
	    rlen = strlen (root);

	/* Search for "fit" extension on input file name */
	} else if (strncmp (root+rlen-4, fitsext2, 4) == 0) {
	    ext2 = True;
	    root[rlen-4] = '\0';
	    rlen = strlen (root);
	}

	/* Search for "asn" suffix on input file name */
	if (strncmp (root+rlen-4, asnsuf, 4) == 0) {
	    suffix = True;
	    root[rlen-4] = '\0';
	    rlen = strlen (root);
	}
	

	/* Add any missing pieces to the input ASN file name */
	if (suffix && !ext1 && !ext2) {
	    strcat (infile, fitsext1);
	    ext1 = True;
	} else if (!suffix && !ext1 && !ext2) {
	    strcat (infile, asnsuf);
	    strcat (infile, fitsext1);
	    ext1 = True;
	}

	/* Create the ASC table name */
	strcpy (outfile, root);
	strcat (outfile, ascsuf);
	if (ext1)
	    strcat (outfile, fitsext1);
	else if (ext2)
	    strcat (outfile, fitsext2);

}
