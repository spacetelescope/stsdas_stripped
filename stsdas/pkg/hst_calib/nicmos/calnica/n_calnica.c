# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

static void prStartUp (void);
static void prShutDown (void);
static void mkNames (char *, char *, char *);
static void mkPrefix (char *, char *);
static int  ckOutputFile (char *);

/* N_CALNICA: Main CALNICA routine for performing instrumental calibration
** of individual NICMOS datasets. Reads the input (raw) dataset, the
** required calibration reference data files, calls the various calibration
** steps, and writes the output (cal and ima) datasets.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Oct.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	27-Jan-1997	Use new ckNewFile function (Version 2.1)
** H.Bushouse	25-Apr-1997	Upgrade logic in n_mkNames (Version 2.2)
** H.Bushouse	13-Jun-1997	Pass NOISFILE to n_docalib2; don't free ref
**				data until after n_docalib2 (Version 2.3)
** H.Bushouse	25-Jul-1997	Added zsig image for use in new n_zsigcalc
**				routine; changed NicInfo.bunit from scalar to
**				vector;	moved call to n_getGroupInfo out of
**				n_calnica into n_getRawData; reversed order
**				of processing so that zeroth read donefirst
**				(Version 3.0)
** H.Bushouse	11-Sep-1997	Added cr_thresh and zs_thresh as input
**				arguments; changed "group" to "imset" in
**				messages (Version 3.0)
** H.Bushouse	01-Oct-1997	No longer pass NOISFILE to n_docalib2 and move
**				free ref data back to before n_docalib2
**				(Version 3.0)
** H.Bushouse	17-Dec-1997	Modified n_mkNames to handle Targ Acq rwb, rwf,
**				clb, and clf file name suffixes (Version 3.1)
** H.Bushouse	13-Feb-1998	Removed use of zsig image in this routine and
**				as argument to n_InstCal (Version 3.2)
** H.Bushouse	01-Oct-1998	Added samp_rej as input argument (Version 3.3)
** H.Bushouse	07-Oct-1998	Wrote the functions prStartUp, prShutDown, and
**				ckOutputFile (Version 3.3)
** H.Bushouse	15-Oct-1998	Added bar_thresh as input argument (Version 3.3)
** H.Bushouse	16-Sep-1999	Moved static function prototypes to head of
**				file (to satisfy DEC/Alpha/OpenVMS compiler)
**				(Version 3.3)
** H.Bushouse	12-Jan-2000	Restructured to eliminate loop over groups,
**				doing looping within individual calibration
**				steps instead (Version 4.0)
** H.Bushouse	20-Jun-2000	Replaced n_InstCal and n_docalib2 routines with
**				combined n_doCalib routine. Added void to
**				functions with no arguments. Removed nic from
**				n_putCalData, n_putMultiCalData, n_freeRefData
**				argument lists (Version 4.0)
** R.Jedrzeje   14-Jun-2001     Changed prototype & call for n_getPriHdr to
**                              reflect new argument list
*/

int n_calnica (char *raw_file, char *cal_file, float cr_thresh,
	       float zs_thresh, float bar_thresh, int samp_rej,
               int writedark) {

/* Arguments:
**	raw_file	i:input raw file name
**	cal_file	i:output cal file name
**	cr_thresh	i: CR rejection threshold
**	zs_thresh	i: Zero-read signal threshold
**	bar_thresh	i: Bar detection threshold
**	samp_rej	i: Number of initial samples to reject
**      writedark       i: Write dynamically-generated dark?
*/

	/* Local variables */
	NicInfo nic;             	/* Observation info */
	Hdr PriHdr;			/* Primary header */
	MultiNicmosGroup  allinput;	/* Input image data */
	SingleNicmosGroup crimage;	/* CR reject output image */
	SingleNicmosGroup zoff;		/* MULTIACCUM zero-read image */
	SingleNicmosGroup mask;		/* Bad pixel mask image */
	SingleNicmosGroup nois;		/* Read-noise image */
	SingleNicmosGroup dark;		/* Dark current image */
	NlinData	  nlin;		/* Linearity coefficients */
	SingleNicmosGroup flat;		/* Flat field image */
	PhotData          phot;		/* Photometry parameters */
	char ima_file[SZ_NAME+1];	/* Intermediate MultiAccum file name */
	char prefix[SZ_NAME+1];         /* File name prefix      */

	/* Function definitions */
	void n_initRefData (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
	     SingleNicmosGroup *, NlinData *, SingleNicmosGroup *,
	     SingleNicmosGroup *, PhotData *);
	int  n_getPriHdr (char *, Hdr *);
	int  n_setup (NicInfo *, Hdr *);
	int  n_getRawData (NicInfo *, MultiNicmosGroup *);
	int  n_getRefData (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
	     SingleNicmosGroup *, NlinData *, SingleNicmosGroup *,
	     SingleNicmosGroup *, PhotData *);
	int  n_doCalib (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *,
	     SingleNicmosGroup *, SingleNicmosGroup *, SingleNicmosGroup *,
	     NlinData *, SingleNicmosGroup *, PhotData *, SingleNicmosGroup *);
	void n_freeRefData (SingleNicmosGroup *, SingleNicmosGroup *,
	     SingleNicmosGroup *, NlinData *, SingleNicmosGroup *,
	     SingleNicmosGroup *);
	int  n_putMultiCalData (MultiNicmosGroup *, char *);
	int  n_putCalData (SingleNicmosGroup *, char *);

	/* Post error handler */
	push_hstioerr (errchk);

	/* Print startup message giving CALNICA version number and time */
	prStartUp ();

	/* Get the prefix from the input file name and copy it to the
           NicInfo structure                                            */
	mkPrefix (raw_file, prefix);
	strcpy (nic.prefix, prefix);

	/* Build the complete input and output file names */
	mkNames (raw_file, ima_file, cal_file);
        sprintf (MsgText, "Input: %s   Output: %s", raw_file, cal_file);
        n_message (MsgText);

	/* Copy the input filename into the NicInfo structure */
	strcpy (nic.filename, raw_file);

	/* Check if either of the output files already exists */
	if (ckOutputFile (cal_file))
	    return (status);
	if (ckOutputFile (ima_file))
	    return (status);

	/* Copy input arguments into NicInfo structure */
	nic.crthresh  = cr_thresh;
	nic.zsthresh  = zs_thresh;
	nic.barthresh = bar_thresh;
	nic.samp_rej  = samp_rej;
	nic.writedark = writedark;

	/* Initialize the reference data structures */
	n_initRefData (&nic, &zoff, &mask, &nois, &nlin, &dark, &flat, &phot);

	/* Read the primary header of the input file */
	if (n_getPriHdr (raw_file, &PriHdr))
	    return (status);

	/* Do setup based on keyword values in primary header */
	if (n_setup (&nic, &PriHdr)) {
	    freeHdr (&PriHdr);
	    return (status);
	}

	/* Free the primary header */
	freeHdr (&PriHdr);

	/* Load all input data groups */
	sprintf (MsgText, "Reading data from %s ...", nic.filename);
	n_message (MsgText);
	if (n_getRawData (&nic, &allinput))
	    return (status);

	/* Load calibration reference data */
	sprintf (MsgText, "Reading reference data ...");
	n_message (MsgText);

	if (n_getRefData (&nic, &zoff, &mask, &nois, &nlin, &dark, &flat,
			  &phot)) {
	    freeMultiNicmosGroup (&allinput);
	    n_freeRefData (&zoff, &mask, &nois, &nlin, &dark, &flat);
	    return (status);
	}

	/* Apply the calibration steps */
	if (n_doCalib (&nic, &allinput, &zoff, &mask, &nois, &dark, &nlin,
		       &flat, &phot, &crimage)) {
	    freeMultiNicmosGroup (&allinput);
	    n_freeRefData (&zoff, &mask, &nois, &nlin, &dark, &flat);
	    freeSingleNicmosGroup (&crimage);
	    return (status);
	}

	/* Rearrange the output file names for MultiACCUM data */
	if (nic.obsmode == MULTIACCUM) {
	    strcpy (raw_file, cal_file);
	    strcpy (cal_file, ima_file);
	    strcpy (ima_file, raw_file);
	}

	/* Write the calibrated data to the output file */
    
	sprintf (MsgText, "Writing calibrated data to %s ...", cal_file);
	n_message (MsgText);
	if (n_putMultiCalData (&allinput, cal_file)) {
	    freeMultiNicmosGroup (&allinput);
	    freeSingleNicmosGroup (&crimage);
        return (status);
	}

	/* Write the MultiACCUM final image data to output file */
	if (nic.obsmode == MULTIACCUM) {
	    sprintf (MsgText, "Writing final image to %s ...", ima_file);
	    n_message (MsgText);
	    if (nic.CRID.corr == PERFORM) {
		if (n_putCalData (&crimage, ima_file)) {
		    freeSingleNicmosGroup (&crimage);
		    return (status);
		}
		freeSingleNicmosGroup (&crimage);
	    } else {
		if (n_putCalData (&(allinput.group[0]), ima_file)) {
		    freeMultiNicmosGroup (&allinput);
		    return (status);
		}
	    }
	}

	/* Free memory for the input data groups */
	freeMultiNicmosGroup (&allinput);

	/* We're done! Print shut down message */
	prShutDown ();

	/* Successful return */
	return (status = 0);
}

/* PRSTARTUP: Print CALNICA startup message, including version number
** and current time.
*/

static void prStartUp (void) {

	/* Local variables */
	time_t  now;			/* Current date/time */
	char time_stamp[30];		/* Time stamp string */

	/* Get current time and print message */
	now = time(NULL);
	strftime (time_stamp, sizeof(time_stamp), "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
	sprintf (MsgText,
		 "Starting CALNICA -- version %s -- %s", CALNICA_VERSION,
		  time_stamp);
	n_message (MsgText);
}

/* PRSHUTDOWN: Print CALNICA shutdown message, including current time.
*/

static void prShutDown (void) {

	/* Local variables */
	time_t  now;			/* Current date/time */
	char time_stamp[30];		/* Time stamp string */

	/* Get current time and print message */
	now = time(NULL);
	strftime (time_stamp, sizeof(time_stamp), "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
	sprintf (MsgText, "Finished CALNICA -- %s", time_stamp);
	n_message (MsgText);
}

/* MKPREFIX: Build the prefix from the input file name */
void mkPrefix (char *rawfile, char inroot[]) {

        int rlen;
        strcpy (inroot, rawfile);

        /* Search for "fits" extension on input file name */
        rlen = strlen(inroot);
        if (strncmp (inroot+rlen-5, ".fits", 5) == 0) {
            inroot[rlen-5] = '\0';
            rlen = strlen (inroot);

        /* Search for "fit" extension on input file name */
        } else if (strncmp (inroot+rlen-4, ".fit", 4) == 0) {
            inroot[rlen-4] = '\0';
            rlen = strlen (inroot);
        }

        /* Search for "raw" suffix on input file name */
        if (strncmp (inroot+rlen-4, "_raw", 4) == 0) {
            inroot[rlen-4] = '\0';
            rlen = strlen (inroot);

        /* Search for "raw1" suffix on input file name */
	} else if (strncmp (inroot+rlen-5, "_raw1", 4) == 0) {
            inroot[rlen-5] = '\0';
            rlen = strlen (inroot);

        /* Search for "rwb" suffix on input file name */
        } else if (strncmp (inroot+rlen-4, "_rwb", 4) == 0) {
            inroot[rlen-4] = '\0';
            rlen = strlen (inroot);

        /* Search for "rwf" suffix on input file name */
        } else if (strncmp (inroot+rlen-4, "_rwf", 4) == 0) {
            inroot[rlen-4] = '\0';
            rlen = strlen (inroot);
        }
	return;
}



/* MKNAMES: Build the complete input and output file names from
** whatever the user supplied as arguments */

static void mkNames (char *rawfile, char *imafile, char *calfile) {

/* Arguments:
**	rawfile	io: input raw file name
**	imafile	 o: output intermediate file name (used for MULTIACCUM only)
**	calfile	io: output cal file name
*/

	/* Local variables */
	Bool ext1;			/* Was a "fits" extension specified? */
	Bool ext2;			/* Was a "fit"  extension specified? */
	Bool israw;			/* Was a raw file suffix specified? */
	Bool isrwb;			/* Was a rwb file suffix specified? */
	Bool isrwf;			/* Was a rwf file suffix specified? */
	Bool isclb;			/* Was a clb file suffix specified? */
	Bool isclf;			/* Was a clf file suffix specified? */
	char inroot[SZ_NAME];		/* root name of input file */
	char outroot[SZ_NAME];		/* root name of output file */
	int  rlen;			/* length of root name */
	char fitsext1[] = ".fits";	/* FITS file extension (default) */
	char fitsext2[] = ".fit";	/* FITS file extension (alternate) */
	char rawsuf[] = "_raw";		/* raw file suffix */
	char imasuf[] = "_ima";		/* intermediate file suffix */
	char calsuf[] = "_cal";		/* cal file suffix */
	char rwbsuf[] = "_rwb";		/* raw targ acq bck file suffix */
	char rwfsuf[] = "_rwf";		/* raw targ acq flt file suffix */
	char clbsuf[] = "_clb";		/* cal targ acq bck file suffix */
	char clfsuf[] = "_clf";		/* cal targ acq flt file suffix */

	/* Initialize */
	ext1   = False;
	ext2   = False;
	israw = False;
	isrwb = False;
	isrwf = False;
	isclb = False;
	isclf = False;
	strcpy (inroot,  rawfile);
	strcpy (outroot, calfile);

	/* Search for "fits" extension on input file name */
	rlen = strlen(inroot);
	if (strncmp (inroot+rlen-5, fitsext1, 5) == 0) {
	    ext1 = True;
	    inroot[rlen-5] = '\0';
	    rlen = strlen (inroot);

	/* Search for "fit" extension on input file name */
	} else if (strncmp (inroot+rlen-4, fitsext2, 4) == 0) {
	    ext2 = True;
	    inroot[rlen-4] = '\0';
	    rlen = strlen (inroot);
	}

	/* Search for "raw" suffix on input file name */
	if (strncmp (inroot+rlen-4, rawsuf, 4) == 0) {
	    israw = True;
	    inroot[rlen-4] = '\0';
	    rlen = strlen (inroot);

	/* Search for "rwb" suffix on input file name */
	} else if (strncmp (inroot+rlen-4, rwbsuf, 4) == 0) {
	    isrwb = True;
	    inroot[rlen-4] = '\0';
	    rlen = strlen (inroot);

	/* Search for "rwf" suffix on input file name */
	} else if (strncmp (inroot+rlen-4, rwfsuf, 4) == 0) {
	    isrwf = True;
	    inroot[rlen-4] = '\0';
	    rlen = strlen (inroot);
	}
	
	/* Add any missing pieces to the input raw file name */
	if ((israw || isrwb || isrwf) && !ext1 && !ext2) {
	    strcat (rawfile, fitsext1);
	    ext1 = True;
	} else if (!israw && !isrwb && !isrwf && !ext1 && !ext2) {
	    strcat (rawfile, rawsuf);
	    strcat (rawfile, fitsext1);
	    ext1 = True;
	}

	/* If no output name was specified, build the cal and ima file
	** file names from the input root name */
	if (calfile[0] == '\0') {

	    strcpy (calfile, inroot);
	    strcpy (imafile, inroot);

	} else {

	    /* Search for "fits" extension on output file name */
	    rlen = strlen (outroot);
	    if (strncmp (outroot+rlen-5, fitsext1, 5) == 0) {
		ext1 = True;
		ext2 = False;
		outroot[rlen-5] = '\0';
		rlen = strlen (outroot);

	    /* Search for "fit" extension on output file name */
	    } else if (strncmp (outroot+rlen-4, fitsext2, 4) == 0) {
		ext1 = False;
		ext2 = True;
		outroot[rlen-4] = '\0';
		rlen = strlen (outroot);
	    }

	    /* Search for "cal" suffix on output file name */
	    if (strncmp (outroot+rlen-4, calsuf, 4) == 0) {
		outroot[rlen-4] = '\0';
		rlen = strlen (outroot);

	    /* Search for "clb" suffix on output file name */
	    } else if (strncmp (outroot+rlen-4, clbsuf, 4) == 0) {
		isclb = True;
		outroot[rlen-4] = '\0';
		rlen = strlen (outroot);

	    /* Search for "clf" suffix on output file name */
	    } else if (strncmp (outroot+rlen-4, clfsuf, 4) == 0) {
		isclf = True;
		outroot[rlen-4] = '\0';
		rlen = strlen (outroot);
	    }

	    strcpy (calfile, outroot);
	    strcpy (imafile, outroot);
	}

	/* Build the cal and ima file names from their roots */
	if (isrwb || isclb) {
	    strcat (imafile, imasuf);
	    strcat (calfile, clbsuf);
	} else if (isrwf || isclf) {
	    strcat (imafile, imasuf);
	    strcat (calfile, clfsuf);
	} else {
	    strcat (imafile, imasuf);
	    strcat (calfile, calsuf);
	}

	if (ext1) {
	    strcat (calfile, fitsext1);
	    strcat (imafile, fitsext1);
	} else if (ext2) {
	    strcat (calfile, fitsext2);
	    strcat (imafile, fitsext2);
	}

}

/* CKOUTPUTFILE: Check for the existence of an output file. If the file
** already exists and the system environment variable "imclobber" is set to
** "yes", the file will be deleted and a warning issued. If the file exists,
** but "imclobber" is not set to "yes", an error is returned.
*/

static int ckOutputFile (char *filename) {

	/* Local variables */
	int  file_stat;			/* Output file status */

	file_stat = ckNewFile(filename);
	if (file_stat == -1) {
	    sprintf(MsgText, "Existing output file \"%s\" was deleted",
		    filename);
	    n_warn (MsgText);
	} else if (file_stat > 0) {
	    sprintf (MsgText, "Output file \"%s\" already exists", filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

