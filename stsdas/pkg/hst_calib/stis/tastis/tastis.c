/* C program that analyzes STIS Target Acquisition.      */
/* It makes use of the the imio.h functions.             */
/* Written by R. Katsanis & R. Downes, 17-February-1998  */
/* Based on the original IDL code to analize target      */
/* acquisition 'stis_ta.pro' written by George Hartig.   */
/* 10-March-98: currently reads both the raw and the spt */
/*              files.                                   */

/* Changes made by Phil Hodge, 21-April-2000:
1. Read PEDESTAL from the primary header of the raw file instead of from
the data portion of the spt extension.  This solves the problem of negative
pedestal values.
2. If at least one of the acquisitions did not succeed, then print a
message to stderr (if compiled as a host program) or set acq_success in
the parameter file to no (if compiled as a native IRAF task).
3. Compare the flux in the confirmation image with 0.9 times the maximum in
the ACQ/PEAK scan.  The messages printed to the standard output have been
changed to mention this factor as "90%".
4. Move the "# include" statements from tastis.h to tastis.c.
5. Replace the nearest-integer macro.
6. Move calls to c_imunmap for imraw7 and imspt1 to the sections where the
images were opened.  Also call imunmap separately for the spt primary header.
7. Add FATAL_ERROR macro for out of memory, and use this value as an exit code
if there was an error.
8. Initialize domfgs and subfgs strings to null.
9. Split the test on the quad filter (for setting the filter shift offset)
into four separate tests.
*/

/* Changes made by Phil Hodge, 17-June-2002:
In ReadKeywords, read the PROPAPER keyword.  If the value ends in "E1",
use this value for the aperture name; otherwise, read the APERTURE keyword,
as was done before.
In PrintWarnings, for an ACQ/PEAK check whether the maximum occurs at an
endpoint of a linear scan.  If so, print a warning message.
Check for command-line argument '-u'; if this is present, update the
ACQSTAT keyword in the primary header of the input file.  Note that this
keyword is set independently for each input file.
Remove the summary message to stderr.
*/

/* Change made by Phil Hodge, 26-June-2002:
In PrintWarnings, change the test (and messages) regarding the ratio of
fluxes in the maximum checkbox in the coarse and fine stages.  The previous
criterion was 10% (i.e. between 0.9 and 1.1); change this to 25%.
*/

/* Change made by Phil Hodge, 4-December-2002:
The loop over the input files was off by one (both start and end) when
the -u option was specified.  If there was more than one file in the
argument list, this resulted in tastis attempting to open "-u" as a file,
and the last file in the list was omitted.
*/

/* Change made by Phil Hodge, 27-December-2002:
keywords->visit was a double, but the value could actually contain
letters (e.g. "1A"), so change visit to a string.  Also change the
local variable linenum in ReadKeywords to a string.
*/

/* Changes made by Phil Hodge, 30-May-2003:
Change the lower limit for the ratio of flux in the confirmation image
to the maximum flux in the ACQ/PEAK scan from 0.9 to 0.8; add a maximum
value of 2.0 for this ratio.  The macros for these limits are LOW_FLUX_CUTOFF
and HIGH_FLUX_CUTOFF respectively.  Change the "success" message to refer to
both these limits.
Add a test that the flux level (above pedestal) in the confirmation image
is above a minimum value.  This minimum depends on whether the ACQ/PEAK was
done in imaging or spectroscopic mode (based on OBSTYPE); the macros for
the minimum flux levels are MIN_IMAGING_FLUX and MIN_SPECTROSCOPIC_FLUX
respectively.
Use keywords->pedestal instead of keywords->counts2 for the pedestal value.
In PrintWarnings, delete the check on the flux being minimum at the end
of a scan.
Change the value of ACQSTAT from ' ' to 'OK' if there was no problem.
Add a test for saturation, for both ACQ and ACQ/PEAK, using GOODMAX.
Add a test for the lamp being off, for ACQ, using GOODMAX.
Change the calling sequence of ReadKeywords to include both imraw7 and
imspt1.  Set imraw7 to zero for ACQ/PEAK.  Open the spt file for both ACQ
and ACQ/PEAK.
Get OCSTDFX from the spt file, and check it for the value TDFDown (which
would indicate that the take-data flag was down), and in that case set
DATAFLAG = 'TDF_Down'.
Delete the WriteError function.
Add no_spt to the calling sequences of PrintOutput and PrintWarnings,
instead of using a bit in badacq.
*/

/* Change made by Phil Hodge, 5-September-2003:
Check PROPAPER for "D1" in addition to "E1".
*/

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>
# include <c_iraf.h>
# include <xclio.h>
# include <ximio.h>

# include "tastis.h"

# define FATAL_ERROR     2
# define LOW_FLUX_CUTOFF    (0.8)
# define HIGH_FLUX_CUTOFF   (2.0)
# define MAX_GOODMAX        (32000.)	/* higher would be saturated */
# define MIN_GOODMAX        (1900.)	/* lower implies lamp was not on */
# define MIN_IMAGING_FLUX           (1250.)
# define MIN_SPECTROSCOPIC_FLUX     (20000.)

/* These are possible values for the bit mask badacq. */
# define BAD_ACQ           1	/* any problem */
# define BAD_SLEW          2	/* ACQ only */
# define BAD_LAMP_LOW      4	/* ACQ only */
# define BAD_RATIO_HIGH    8	/* ACQ or ACQ/PEAK */
# define BAD_RATIO_LOW    16	/* ACQ or ACQ/PEAK */
# define BAD_SATURATED    32	/* ACQ or ACQ/PEAK */
# define BAD_FLUX         64	/* ACQ/PEAK only */
# define BAD_END         128	/* ACQ/PEAK only */
# define BAD_TDF         256	/* take-data flag was down */

static int     TargetAcqAnalysis (char *, int, int *);
static void    ReadKeywords (IRAFPointer, IRAFPointer, IRAFPointer,
			 IRAFPointer, IRAFPointer, imageinfo *);
static void    CalculateSlews (imageinfo *);
static void    PrintOutput (imageinfo *, int, int *, int *);
static double  arcseconds (double);
static double  V2coord (double,double);
static double  V3coord (double,double);
static void    PrintWarnings (imageinfo *, int, int *, int *);
static int     tastis_immap (char *, char *, IRAFPointer *);


# if defined (NATIVE_IRAF)
IRAFTASK(tastis) {
# else
int main (int argc, char **argv) {
# endif

	int i;
	int          update;                  /* modify input file? */
	int          start;                   /* index of first input file name */
	char         input[IRAF_SZ_LINE];     /* list of input files */
	IRAFPointer  imlist;
	char         infile[SZ_FNAME];        /* one input image name */
	int          nfiles;                  /* total number of input files */
	int          acqstatus;
	int          status;


	c_irafinit (argc, argv);

	acqstatus = 0;			/* initial values */
	imlist = 0;
	update = 0;
	start = 1;

	# if defined (NATIVE_IRAF)
	/* 'update' is ignored */
	c_clgstr ("input", input, IRAF_SZ_LINE-1);
	imlist = c_imtopen (input);
	nfiles = c_imtlen (imlist);
	if (nfiles < 1) {
	    printf ("Null input, or no file matches input template.\n");
	    return;
	}
	# else
	if ( argc < 2 ) {
	    printf ("syntax: tastis [-u] file1 file2 ...\n");
	    exit (FATAL_ERROR);
	}
	if (strcmp (argv[1], "-u") == 0) {
	    start = 2;
	    update = 1;
	}
	if ( update && argc < 3 ) {
	    printf ("syntax: tastis [-u] file1 file2 ...\n");
	    exit (FATAL_ERROR);
	}

	/* Allow either '*raw.fits' or 'file1 file2 ...' */
	if (argc-start == 1) {
	    imlist = c_imtopen (argv[start]);
	    nfiles = c_imtlen (imlist);
	} else {
	    nfiles = argc - start;
	}
	if (nfiles < 1) {
	    printf ("Null input, or no file matches input template.\n");
	    exit (FATAL_ERROR);
	}
	# endif

	/* Analyze each datafile that was listed on the command line. */
	/* Loop over all input files. */

	# if defined (NATIVE_IRAF)

	for (i = start;  i < start+nfiles;  i++) {

	    if (c_imtgetim (imlist, infile, SZ_FNAME-1) < 1) {
		printf ("Error parsing input string `%s'\n", input);
		return;
	    }

	    if (status = TargetAcqAnalysis (infile, update, &acqstatus)) {
		if (status == FATAL_ERROR)
		    return;
		continue;
	    }
	}

	# else

	for (i = start;  i < start+nfiles;  i++) {

	    if (imlist > 0) {
		if (c_imtgetim (imlist, infile, SZ_FNAME-1) < 1) {
		    printf ("Error parsing input string `%s'\n", argv[1]);
		    exit (FATAL_ERROR);
		}
	    } else {
		strcpy (infile, argv[i]);
	    }

	   if (status = TargetAcqAnalysis (infile, update, &acqstatus)) {
		if (status == FATAL_ERROR)
		    exit (FATAL_ERROR);
		continue;
	   }
	}

	# endif

	if (imlist > 0)
	   c_imtclose (imlist);

	# if defined (NATIVE_IRAF)
	    /* Set a flag in the par file.  This will be yes if every ACQ and
	       ACQ/PEAK was OK, and it will be no if any appeared to be bad.
	    */
	    if (acqstatus == 0)
		c_clputb ("acq_success", 1);
	    else
		c_clputb ("acq_success", 0);
	# else
	    exit (acqstatus);
	# endif
}

static int TargetAcqAnalysis (char *image, int update, int *acqstatus) {

	IRAFPointer  imraw0, imraw1, imraw4, imraw7, imspt1, imspt0;
	int          size, i, status;
	char         rawfile[SZ_FNAME],
		     sptfile[SZ_FNAME],
		     obsmode[FITS_LINE];

	FILE         *fp;

	/* This is a bit mask; the acq or acq/peak was bad if any bit is set.*/
	int          badacq = 0;	/* acq status for current image */
	/* This is true if the support file was missing. */
	int          no_spt = 0;

	imageinfo    *keywords;


	/* Build the appropriate string for raw & spt files spt file. */
	strcpy (rawfile, image);
	size = strlen(rawfile) - 9;
	for (i=0; i<size; i++)
	    sptfile[i] = rawfile[i];
	sptfile[i] = '\0';
	strcat (sptfile, "_spt.fits");   /* 1st sci spt file */

	if ( (fp = fopen (sptfile, "r")) != NULL )
	    fclose (fp);
	else
	    no_spt = 1;

	/* Allocate space for structure 'keywords'.*/
	keywords = calloc(1,sizeof(imageinfo));
	if ( keywords == NULL ) {
	    printf ("out of memory!\n");
	    return (FATAL_ERROR);
	}

	/* Open input file */
	if ( status = tastis_immap (image, "[0]", &imraw0) )
	    return (status);


	/* Assign defaults, in case these are not needed or don't exist. */
	imraw7 = 0;
	imspt1 = 0;
	/* Select only ACQ and ACQ/PEAK. */
	c_imgstr (imraw0, "obsmode", obsmode, FITS_LINE-1);
	if (strcmp(obsmode, "ACQ") == 0 || strcmp(obsmode, "ACQ/PEAK") == 0) {
	    if ( status = tastis_immap (rawfile, "[1]", &imraw1) )
		return (status);
	    if ( status = tastis_immap (rawfile, "[4]", &imraw4) )
		return (status);
	    if ( strcmp(obsmode, "ACQ") == 0 ) {
		if ( status = tastis_immap (rawfile, "[7]", &imraw7) )
		    return (status);
	    }
	    if (no_spt) {
		if (strcmp (obsmode, "ACQ/PEAK") == 0) {
		    printf (
	"Can't find %s (required for ACQ/PEAK) ... skipping\n", sptfile);
		    return (1);
		}
	    } else if (status = tastis_immap (sptfile, "[1]", &imspt1)) {
		return (status);
	    }

	    ReadKeywords (imraw0, imraw1, imraw4, imraw7, imspt1, keywords);

	    CalculateSlews (keywords);

	    /* Read dominant & subdominant FGS from keywords DGESTAR & SGESTAR
	       in primary header of spt file.
	    */
	    if (no_spt) {		/* spt file does not exist? */
		keywords->domfgs[0] = '\0';
		keywords->subfgs[0] = '\0';
	    } else {
		if ( status = tastis_immap (sptfile, "[0]", &imspt0) )
		    return (status);
		c_imgstr (imspt0, "dgestar", keywords->domfgs, FITS_LINE-1);
		c_imgstr (imspt0, "sgestar", keywords->subfgs, FITS_LINE-1);
		c_imunmap (imspt0);
	    }

	    PrintOutput (keywords, no_spt, &badacq, acqstatus);
	    c_imunmap (imraw1);
	    c_imunmap (imraw4);
	    if (imraw7 != 0)
		c_imunmap (imraw7);
	    if (imspt1 != 0)
		c_imunmap (imspt1);
	}

	/* Close primary header image. */
	c_imunmap (imraw0);

	if (update) {
	    /* Update keywords in the input primary header to indicate
	       which tests succeeded and which failed.
	    */
	    strcpy (rawfile, image);
	    strcat (rawfile, "[0]");
	    imraw0 = c_immap (rawfile, IRAF_READ_WRITE, 0);
	    if (c_iraferr()) {
		printf ("Warning:  Can't update keyword(s) in %s\n", image);
		clear_cvoserr();
	    } else {

		if (badacq)
		    c_imastr (imraw0, "acqstat", "FAILED");
		else
		    c_imastr (imraw0, "acqstat", "OK");

		if (strcmp (keywords->obsmode, "ACQ") == 0) {

		    if (badacq & BAD_RATIO_HIGH)
			c_imastr (imraw0, "acq_rat", "HIRATIO");
		    else if (badacq & BAD_RATIO_LOW)
			c_imastr (imraw0, "acq_rat", "LORATIO");
		    else
			c_imastr (imraw0, "acq_rat", "OKRATIO");

		    if (badacq & BAD_SLEW)
			c_imastr (imraw0, "acq_slew", "BIGSLEW");
		    else
			c_imastr (imraw0, "acq_slew", "OK_SLEW");

		    if (badacq & BAD_SATURATED)
			c_imastr (imraw0, "acq_sat", "SAT");
		    else
			c_imastr (imraw0, "acq_sat", "UNSAT");

		    if (badacq & BAD_LAMP_LOW)
			c_imastr (imraw0, "acq_lamp", "LO_LAMP");
		    else
			c_imastr (imraw0, "acq_lamp", "OK_LAMP");

		} else {		/* ACQ/PEAK */

		    if (badacq & BAD_RATIO_HIGH)
			c_imastr (imraw0, "acqp_rat", "HIRATIO");
		    else if (badacq & BAD_RATIO_LOW)
			c_imastr (imraw0, "acqp_rat", "LORATIO");
		    else
			c_imastr (imraw0, "acqp_rat", "OKRATIO");

		    if (badacq & BAD_FLUX)
			c_imastr (imraw0, "acqp_flx", "LO_FLUX");
		    else
			c_imastr (imraw0, "acqp_flx", "OK_FLUX");

		    if (badacq & BAD_SATURATED)
			c_imastr (imraw0, "acqp_sat", "SAT");
		    else
			c_imastr (imraw0, "acqp_sat", "UNSAT");

		    if (badacq & BAD_END)
			c_imastr (imraw0, "acqp_end", "HI_END");
		    else
			c_imastr (imraw0, "acqp_end", "OK_END");
		}

		if (badacq & BAD_TDF)
		   c_imastr (imraw0, "dataflag", "TDFDown");
		else if (no_spt)
		   c_imastr (imraw0, "dataflag", "UNKNOWN");
		else
		   c_imastr (imraw0, "dataflag", "TDF_Up");

		c_imunmap (imraw0);
	    }
	}

	/* Deallocates memory for structures. */
	free (keywords);

	return(0);

}


static void ReadKeywords (IRAFPointer imraw0, IRAFPointer imraw1,
    IRAFPointer imraw4, IRAFPointer imraw7, IRAFPointer imspt1,
	imageinfo *keywords) {

/* arguments:
IRAFPointer imraw0   imio pointer for primary header of input raw file
IRAFPointer imraw1   imio pointer for extension 1 of raw file
IRAFPointer imraw4   imio pointer for extension 4 of raw file
IRAFPointer imraw7   imio pointer for extension 7 of raw file if ACQ, or 0
IRAFPointer imspt1   imio pointer for extension 1 of spt file if ACQ/PEAK, or 0
imageinfo *keywords  structure for keyword values
*/

	int     centera1, centera2, ngoodpix, i, j, offset;

	double  goodmean;
	char    linenum[FITS_LINE];

	int     *idwell;
	int     lenstr;	/* length of propaper keyword value */


	/* Read keywords from primary header. */
	c_imgstr (imraw0, "rootname", keywords->rootname, FITS_LINE-1);
	c_imgstr (imraw0, "obsmode",  keywords->obsmode, FITS_LINE-1);
	c_imgstr (imraw0, "obstype",  keywords->obstype, FITS_LINE-1);
	c_imgstr (imraw0, "propaper", keywords->aperture, FITS_LINE-1);
	/* For the aperture name, use PROPAPER if its value ends in "E1"
	   or "D1"; otherwise, use APERTURE.
	*/
	lenstr = strlen (keywords->aperture);
	if (lenstr < 2 || (strcmp (keywords->aperture+lenstr-2, "E1") != 0 &&
	                   strcmp (keywords->aperture+lenstr-2, "D1") != 0))
	    c_imgstr (imraw0, "aperture", keywords->aperture, FITS_LINE-1);
	c_imgstr (imraw0, "targname", keywords->targname, FITS_LINE-1);
	c_imgstr (imraw0, "tdateobs", keywords->tdateobs, FITS_LINE-1);
	c_imgstr (imraw0, "ttimeobs", keywords->ttimeobs, FITS_LINE-1);
	c_imgstr (imraw0, "opt_elem", keywords->optelem, FITS_LINE-1);
	c_imgstr (imraw0, "linenum", linenum, FITS_LINE-1);
	keywords->proposid = c_imgeti (imraw0, "proposid");
	keywords->sizaxis1 = c_imgeti (imraw0, "sizaxis1");
	keywords->sizaxis2 = c_imgeti (imraw0, "sizaxis2");
	keywords->texptime = c_imgetd (imraw0, "texptime");
	keywords->biaslev  = c_imgetd (imraw0, "biaslev");
	centera1 = c_imgeti (imraw0, "centera1");
	centera2 = c_imgeti (imraw0, "centera2");
	ngoodpix = c_imgeti (imraw1, "ngoodpix");
	goodmean = c_imgetd (imraw1, "goodmean");

	/* Read keywords from the ACQ headers. */
	if ( strcmp(keywords->obsmode, "ACQ") == 0 ) {
	    c_imgstr (imraw0, "acqtype",  keywords->acqtype, FITS_LINE-1);
	    if ( strcmp(keywords->acqtype, "POINT") == 0 )
		strcpy (keywords->search, "FLUX CENTROID");
	    else
		c_imgstr (imraw0, "centmeth", keywords->search, FITS_LINE-1);
	    keywords->box_step = c_imgeti (imraw0, "checkbox");
	    keywords->counts1  = c_imgetd (imraw1, "maxchcnt");
	    keywords->counts2  = c_imgetd (imraw4, "maxchcnt");
	    keywords->pedestal = 0.;		/* not used for ACQ */
	    keywords->goodmax1 = 0.;		/* not used for ACQ */
	    keywords->goodmax2 = c_imgetd (imraw4, "goodmax");
	    keywords->goodmax3 = c_imgetd (imraw7, "goodmax");
	    keywords->targax1  = c_imgetd (imraw1, "targa1");
	    keywords->targay1  = c_imgetd (imraw1, "targa2");
	    keywords->targax4  = c_imgetd (imraw4, "targa1");
	    keywords->targay4  = c_imgetd (imraw4, "targa2");
	    keywords->apera1   = c_imgetd (imraw7, "apera1");
	    keywords->apera2   = c_imgetd (imraw7, "apera2");
	    keywords->aperlka1 = c_imgetd (imraw7, "aperlka1");
	    keywords->aperlka2 = c_imgetd (imraw7, "aperlka2");
	}

	/* Read keywords from the ACQ/PEAK primary header and from the
	   spt extension header.
	*/
	if ( strcmp(keywords->obsmode, "ACQ/PEAK") == 0 ) {
	    c_imgstr (imraw0, "peakcent", keywords->peakcent, FITS_LINE-1);
	    c_imgstr (imraw0, "pksearch", keywords->search, FITS_LINE-1);
	    keywords->box_step = c_imgeti (imraw0, "numsteps");
	    keywords->peakstep = c_imgetd (imraw0, "peakstep");
	    /* From spt[1] header */
	    keywords->otaslwa1 = c_imgeti (imspt1, "otaslwa1");
	    keywords->otaslwa2 = c_imgeti (imspt1, "otaslwa2");
	}
	if (imspt1 == 0)
	    strcpy (keywords->ocstdfx, "unknown");
	else
	    c_imgstr (imspt1, "ocstdfx", keywords->ocstdfx, FITS_LINE-1);

	/* Extract visit & expnum from 'linenum'.  (27 Dec 2002, PEH) */
	for (i = 0;  linenum[i] != '.' && linenum[i] != '\0';  i++)
	    keywords->visit[i] = linenum[i];
	keywords->visit[i] = '\0';
	if (linenum[i] == '\0')
	    keywords->expnum = 0;
	else
	    keywords->expnum = atof (linenum+i+1);

	/* Calculate corner from 'centera' & sizaxis'. */
	keywords->corner1 = centera1 - keywords->sizaxis1/2;
	keywords->corner2 = centera2 - keywords->sizaxis2/2;

	/* Calculate coarse, fine local axis & reference aperture locations. */
	/* For ACQs only.  */
	if ( strcmp(keywords->obsmode, "ACQ")  == 0 ) {
	   keywords->coarse1  = keywords->targax1 - (keywords->corner1 - 1) + 1;
	   keywords->coarse2  = keywords->targay1 - (keywords->corner2 - 1) + 1;
	   keywords->fine1    = keywords->targax4 - (keywords->corner1 - 1) + 1;
	   keywords->fine2    = keywords->targay4 - (keywords->corner2 - 1) + 1;
	   keywords->refaper1 = keywords->apera1  - (keywords->corner1 + 31) +1;
	   keywords->refaper2 = keywords->apera2  - (keywords->corner2 + 34) +1;
	   if ( keywords->box_step > 3 ) {
		offset = (keywords->box_step + 1)/2;
		keywords->refaper1 = keywords->refaper1 - offset;
		keywords->refaper2 = keywords->refaper2 - offset;
	   }

	}

	/* Calculate post-slew flux,
	   get pedestal (from raw file) & dwell fluxes.
	   For ACQ/PEAKs only. */
	if ( strcmp(keywords->obsmode, "ACQ/PEAK") == 0 ) {
	    keywords->counts1 = ngoodpix * goodmean;   /* post-slew flux */
	    keywords->counts2 = 0.;		/* not used for ACQ/PEAK */
	    keywords->pedestal = c_imgetd (imraw0, "pedestal");
	    keywords->goodmax1 = c_imgetd (imraw1, "goodmax");
	    keywords->goodmax2 = 0.;		/* not used for ACQ/PEAK */
	    keywords->goodmax3 = 0.;		/* not used for ACQ/PEAK */

	    /* Read dwell fluxes from 4th extension of raw file. */
	    keywords->naxis1 = c_imglen (imraw4,1);
	    keywords->naxis2 = c_imglen (imraw4,2);
	    /* Note that j is one-indexed. */
	    for (j=1; j<=keywords->naxis2; j++) {
		idwell = c_imgl2i (imraw4,j);
		for (i=0;  i<keywords->naxis1;  i++)
		    (keywords->dwell)[j-1][i] = idwell[i];
	    }
	}
}



static void  CalculateSlews (imageinfo *keywords) {

	double  offset;

	double  finalx, finaly, x;


	/* Slew calculation for ACQs.  */

	if ( strcmp(keywords->obsmode, "ACQ") == 0 ) {

	/* Define all possible apertures for ACQs.  */
	if ( strcmp(keywords->aperture, "F25NDQ1") == 0 )
	    offset = -1.24840;
	else if ( strcmp(keywords->aperture, "F25NDQ2") == 0 )
	    offset = -1.24840;
	else if ( strcmp(keywords->aperture, "F25NDQ3") == 0 )
	    offset = -1.24840;
	else if ( strcmp(keywords->aperture, "F25NDQ4") == 0 )
	    offset = -1.24840;
	else if ( strcmp(keywords->aperture, "F28X50LP") == 0 )
	    offset = -1.26850;
	else if ( strcmp(keywords->aperture, "F28X50OIII") == 0 )
	    offset = -1.26850;
	else if ( strcmp(keywords->aperture, "F28X50OII") == 0 )
	    offset = -1.31570;
	else if ( strcmp(keywords->aperture, "F25ND3") == 0 )
	    offset = -1.24840;
	else if ( strcmp(keywords->aperture, "F25ND5") == 0 )
	    offset = -1.24840;
	else
	    offset = 0.0;

	/* Slews in pixels. */
	keywords->a1coarse_pix = keywords->targax1 - offset - keywords->aperlka1 + 1;
	keywords->a2coarse_pix = keywords->targay1 - keywords->aperlka2 + 1;
	keywords->a1fine_pix   = keywords->targax4 - offset - keywords->apera1;
	keywords->a2fine_pix   = keywords->targay4 - keywords->apera2;
	keywords->a1total_pix  = keywords->a1coarse_pix + keywords->a1fine_pix;
	keywords->a2total_pix  = keywords->a2coarse_pix + keywords->a2fine_pix;

	/* Slews in arcseconds.  */
	keywords->a1coarse_arc = arcseconds (keywords->a1coarse_pix);
	keywords->a2coarse_arc = arcseconds (keywords->a2coarse_pix);
	keywords->a1fine_arc   = arcseconds (keywords->a1fine_pix);
	keywords->a2fine_arc   = arcseconds (keywords->a2fine_pix);
	keywords->a1total_arc  = arcseconds (keywords->a1total_pix);
	keywords->a2total_arc  = arcseconds (keywords->a2total_pix);
	keywords->V2coarse     = V2coord (keywords->a2coarse_arc,
					  keywords->a1coarse_arc);
	keywords->V3coarse     = V3coord (keywords->a1coarse_arc,
					  keywords->a2coarse_arc);
	keywords->V2fine       = V2coord (keywords->a2fine_arc,
					  keywords->a1fine_arc);
	keywords->V3fine       = V3coord (keywords->a1fine_arc,
					  keywords->a2fine_arc);
	keywords->V2total      = V2coord (keywords->a2total_arc,
					  keywords->a1total_arc);
	keywords->V3total      = V3coord (keywords->a1total_arc,
					  keywords->a2total_arc);

	} else {

	    /* Slew calculations for ACQ/PEAKs. */
	    if ( strcmp(keywords->search, "LINEARAXIS2") == 0 ) {
		finalx = (keywords->box_step/2)*keywords->peakstep /
			(PLATESCALE*1000.0);
		finaly = 0.0;
	    } else if ( strcmp(keywords->search, "LINEARAXIS1") == 0 )  {
		finalx = 0.0;
		finaly = (keywords->box_step/2)*keywords->peakstep /
			(PLATESCALE*1000.0);
	    } else if ( strcmp(keywords->search, "SPIRAL") == 0 ) {
		x = modf(sqrt(keywords->box_step)/2, &finaly);
		finaly = (-1)*finaly * keywords->peakstep / (PLATESCALE*1000.0);
		x = modf(sqrt(keywords->box_step)/2, &finalx);
		finalx = (-1)*finalx * keywords->peakstep / (PLATESCALE*1000.0);
	    }

	    /* Final slews in pixels. */
	    keywords->a1total_pix = keywords->otaslwa1/10.0 + finaly;
	    keywords->a2total_pix = keywords->otaslwa2/10.0 + finalx;
	    if ( strcmp(keywords->search, "SPIRAL") == 0 ) {
		if (keywords->a2total_pix > -0.05 &&
		    keywords->a2total_pix < 0.05) {
		    keywords->a2total_pix = 0.0;
		}
		if (keywords->a1total_pix > -0.05 &&
		    keywords->a1total_pix < 0.05) {
		    keywords->a1total_pix = 0.0;
		}
	    }

	    /* Rounding up the pixel values up to the decimal place. */
	    keywords->a1total_pix = NINT (keywords->a1total_pix);
	    keywords->a2total_pix = NINT (keywords->a2total_pix);


	    /* Slews in arcseconds.  */
	    keywords->a1total_arc = arcseconds (keywords->a1total_pix);
	    keywords->a2total_arc = arcseconds (keywords->a2total_pix);
	    keywords->V2total = V2coord (keywords->a2total_arc,
				         keywords->a1total_arc);
	    keywords->V3total = V3coord (keywords->a1total_arc,
				         keywords->a2total_arc);
	}

}


static void PrintOutput (imageinfo *keywords, int no_spt, int *badacq,
		int *acqstatus) {

	int      i, j;


	/* Print Top header. */
	for (i=1; i<80; i++)
	    printf ("%s", "=");
	printf ("\n");

	if ( strcmp(keywords->obsmode, "ACQ") == 0 )
	    printf ("%8s       HST/STIS    MIRVIS     %7s             ACQ/%s\n",
		keywords->rootname, keywords->aperture, keywords->acqtype);
	else
	    printf ("%8s       HST/STIS    %s        %7s             ACQ/PEAK-UP\n",
		keywords->rootname, keywords->optelem, keywords->aperture);

	printf ("prop: %4d      visit: %s    line: %.0f   target: %s\n",
	    keywords->proposid, keywords->visit, keywords->expnum,
	    keywords->targname);

	printf ("obs date, time: %8s    %8s   exposure time: %5.2f\n",
	    keywords->tdateobs, keywords->ttimeobs, keywords->texptime);


	if ( strcmp(keywords->domfgs, "") != 0 ||
	    strcmp(keywords->subfgs, "") != 0 )
	    printf ("dom GS/FGS: %s    sub-dom GS/FGS: %s\n", keywords->domfgs,
			keywords->subfgs);


	if ( strcmp(keywords->obsmode, "ACQ") == 0 )
	    printf ("ACQ params:     bias sub: %.0f   checkbox: %d      method: %s\n",
		keywords->biaslev, keywords->box_step, keywords->search);
	else
	    printf ("ACQ params:     bias sub: %.0f                     method: %s\n",
		keywords->biaslev, keywords->peakcent);

	printf ("subarray (axis1,axis2):   size=(%d,%d)          corner=(%d,%d)\n",
		keywords->sizaxis1, keywords->sizaxis2, keywords->corner1,
		keywords->corner2 );

	for (i=1; i<80; i++)
	    printf ("%s", "-");
	printf ("\n");


	/* Print rest of output according to data type: ACQ or ACQ/PEAK. */
	if ( strcmp(keywords->obsmode, "ACQ") == 0 ) {
	    printf ("Coarse locate phase:           Target flux in max checkbox (DN): %.0f\n\n", keywords->counts1);
	    printf ("                       global          local\n");
	    printf ("                    axis1 axis2     axis1 axis2\n");
	    printf ("Target location:    %4.1f  %4.1f    %4.1f  %4.1f\n\n",
			keywords->corner1 + keywords->coarse1 -1,
			keywords->corner2 + keywords->coarse2 -1,
			keywords->coarse1,  keywords->coarse2);
	   printf ("                    axis1 axis2     axis1  axis2         V2      V3\n");
	   printf ("                      (pixels)        (arcsec)            (arcsec)\n");
	   printf ("Estimated slew:     %4.1f  %4.1f    %6.3f %6.3f       %6.3f %6.3f\n",
			keywords->a1coarse_pix, keywords->a2coarse_pix,
			keywords->a1coarse_arc, keywords->a2coarse_arc,
			keywords->V2coarse, keywords->V3coarse);

	    /* Print slews */

	    for (i=1; i<80; i++)
		printf ("%s", "-");
	    printf ("\n");

	    printf ("Fine locate phase:            Target flux in max checkbox (DN): %.0f\n\n", keywords->counts2);
	    printf ("                       global            local\n");
	    printf ("                    axis1 axis2     axis1 axis2\n");
	    printf ("Target location:    %4.1f  %4.1f    %4.1f  %4.1f\n",
			keywords->corner1 + keywords->fine1 -1,
			keywords->corner2 + keywords->fine2 -1,
			keywords->fine1,  keywords->fine2);
	    printf ("Ref ap location:    %4.1f  %4.1f    %4.1f  %4.1f\n\n",
			keywords->apera1 + 1, keywords->apera2 + 1,
			keywords->refaper1, keywords->refaper2);
	    printf ("                    axis1 axis2     axis1  axis2         V2      V3\n");
	    printf ("                      (pixels)        (arcsec)           (arcsec)\n");
	    printf ("Estimated slew:     %4.1f  %4.1f     %6.3f %6.3f      %6.3f %6.3f\n",
		keywords->a1fine_pix, keywords->a2fine_pix, keywords->a1fine_arc,
		keywords->a2fine_arc, keywords->V2fine, keywords->V3fine);


	    /* Print slews */

	    for (i=1; i<80; i++)
		printf ("%s", "-");
	    printf ("\n");

	    printf ("Total est. slew:    %4.1f  %4.1f    %6.3f %6.3f        %6.3f %6.3f\n",
		keywords->a1total_pix, keywords->a2total_pix, keywords->a1total_arc,
		keywords->a2total_arc, keywords->V2total, keywords->V3total);
	    for (i=1; i<80; i+=1)
		printf ("%s", "-");
	    printf ("\n");
	    PrintWarnings (keywords, no_spt, badacq, acqstatus);

	} else {

	    printf ("Scan type: %s                  Step size (mas): %.0f\n",
			keywords->search, keywords->peakstep);
	    if ( strcmp(keywords->search, "SPIRAL") == 0 )
		printf ("axis 1 -->,  axis 2 ^\n");
	    printf ("\n");

	    /* Print here the dwell point values */
	    if ( strcmp(keywords->search, "LINEARAXIS2") == 0 ) {
		for (i=0; i<keywords->naxis2; i++) {
		    for (j=keywords->naxis1; j>0;  j--)
			printf ("%10d", (keywords->dwell)[i][j-1]);
		}
	    } else {
		for (i=keywords->naxis2; i>0; i--) {
		    for (j=0; j<keywords->naxis1; j++)
			printf ("%10d", (keywords->dwell)[i-1][j]);
		    printf ("\n");
		}
	    }
	    printf ("\n\n");

	    printf ("                    axis1 axis2     axis1  axis2         V2   V3\n");
	    printf ("                      (pixels)        (arcsec)           (arcsec)\n");
	    printf ("Estimated slew:     %4.1f  %4.1f    %6.3f %6.3f     %6.3f %6.3f\n",
			keywords->a1total_pix, keywords->a2total_pix,
			keywords->a1total_arc, keywords->a2total_arc,
			keywords->V2total, keywords->V3total);
	    printf ("Flux in post-slew confirmation image (%.0f) - Pedestal (%.0f) = %.0f DN\n",
			keywords->counts1, keywords->pedestal,
			keywords->counts1 - keywords->pedestal);
	    for (i=1; i<80; i+=1)
		printf ("%s", "-");
	    printf ("\n");
	    PrintWarnings (keywords, no_spt, badacq, acqstatus);
	}

	for (i=1; i<80; i+=1)
	    printf ("%s", "=");
	printf ("\n");
}

static void PrintWarnings (imageinfo *keywords, int no_spt, int *badacq,
		int *acqstatus) {

	double  ratio, max;
	/* flux (above pedestal) in confirmation image */
	double  flux;
	/* ratio of flux in confirmation image to maximum flux in ACQ/PEAK */
	double  flux_ratio;
	int     i, j;
	int     i_max, j_max;		/* index of maximum value in peakup */
	/* flags to indicate that the max value was at edge or end of scan */
	int     max_at_end;

	max_at_end = 0;		/* initial value */

	if ( strcmp (keywords->ocstdfx, "TDFDown") == 0 ) {
	    printf (
	"Telemetry indicates that the intended exposures may not have\n");
	    printf (
	"been performed.  Check the images for signal.\n\n");
	    *badacq |= BAD_TDF;
	}
	if (no_spt) {		/* spt file does not exist? */
	    printf (
"This output lacks some information because the spt.fits file\n");
	    printf ("is not present in the directory.\n\n");
	}

	/* ACQ warnings. */
	if ( strcmp(keywords->obsmode, "ACQ") == 0 ) {
	    if ((keywords->a1fine_pix < -4.0  || keywords->a1fine_pix > 4.0)  ||
		(keywords->a2fine_pix < -4.0  || keywords->a2fine_pix > 4.0)) {
		printf (
"The fine slew (to center the target in the reference aperture) is larger\n");
		printf (
"than 4 pixels.  This may indicate a problem with your acquisition.\n\n");
		*badacq |= BAD_SLEW;
	    }

	    /* Ratio of flux in max checkbox in fine & coarse stages. */
	    ratio = keywords->counts2 / keywords->counts1;
	    if ( ratio < 0.75  ||  ratio > 1.25 ) {
		printf (
"The fluxes in the maximum checkbox in the fine and coarse stages differ\n");
		printf (
"by more than 25%%.  This may indicate a problem with your acquisition.\n\n");
		if (ratio < 0.75)
		    *badacq |= BAD_RATIO_LOW;
		else
		    *badacq |= BAD_RATIO_HIGH;
	    }

	    if (keywords->goodmax2 > MAX_GOODMAX) {
		*badacq |= BAD_SATURATED;
		printf (
"Saturation of pixels in the second image may have affected\n");
		printf ("the final centering.\n\n");
	    }

	    if (keywords->goodmax3 < MIN_GOODMAX) {
		*badacq |= BAD_LAMP_LOW;
		printf (
"The flux in the third image of the ACQ is lower than the typical value for\n");
		printf (
"the lamp; the image should be checked to see if the lamp was illuminated.\n\n");
	    }

	    if ( *badacq == 0 ) {
		printf (
"Your ACQ appears to have succeeded, as the fluxes in the coarse\n");
		printf (
"and fine stages agree within 25%% and the fine slews were less than\n");
		printf ("4 pixels as expected\n\n");
	    }
	}

	/* ACQ/PEAK warnings. */

	if ( strcmp(keywords->obsmode, "ACQ/PEAK") == 0 ) {
	    /* Calculate maximum flux in the peakup */
	    max = 0.0;
	    i_max = -1;
	    j_max = -1;
	    for (j = 0; j < keywords->naxis2; j++) {
		for (i = 0;  i < keywords->naxis1;  i++) {
		    if ((keywords->dwell)[j][i] > max) {
			max = (keywords->dwell)[j][i];
			i_max = i;
			j_max = j;
		    }
		}
	    }

	    /* subtract pedestal */
	   flux = (keywords->counts1 - keywords->pedestal);
	   flux_ratio = flux / max;

	   if (flux_ratio < LOW_FLUX_CUTOFF) {
		printf (
"The flux in the confirmation image is only %2.0f%% of the maximum flux\n",
			    flux_ratio * 100.);
		printf (
"in the ACQ/PEAK scan.  Percentages below %2.0f%% often indicate problems\n",
		LOW_FLUX_CUTOFF * 100.);
		printf ("in the ACQ/PEAK.\n\n");
		*badacq |= BAD_RATIO_LOW;
	   }

	   if (flux_ratio > HIGH_FLUX_CUTOFF) {
		printf (
"The flux in the confirmation image is %2.0f%% greater than the maximum flux\n",
			    (flux_ratio - 1.) * 100.);
		printf (
"in the ACQ/PEAK scan.  An excess greater than %3.0f%% indicates\n",
			(HIGH_FLUX_CUTOFF - 1.) * 100.);
		printf ("problems in the ACQ/PEAK.\n\n");
		*badacq |= BAD_RATIO_HIGH;
	   }

	   if (keywords->goodmax1 > MAX_GOODMAX) {
		*badacq |= BAD_SATURATED;
		printf (
"Some pixels in the confirmation image were saturated.  If saturation also\n");
		printf (
"occurred in any of the peakup steps, it may have affected the centering.\n\n");
	   }

	   /* Check that the flux level (above pedestal) in the confirmation
	      image is above a minimum value.
	   */
	   if (strcmp (keywords->obstype, "IMAGING") == 0) {
		if (flux < MIN_IMAGING_FLUX) {
		    printf (
"The flux in the confirmation image is %2.0f%% of the recommended minimum\n",
			    flux / MIN_IMAGING_FLUX * 100.);
		    printf (
"of %.0f DN for a direct-light ACQ/PEAK.  The signal-to-noise in the\n",
			    MIN_IMAGING_FLUX);
		    printf (
"ACQ/PEAK may be inadequate for an accurate centering.\n\n");
		    *badacq |= BAD_FLUX;
		}
	    } else {
		if (flux < MIN_SPECTROSCOPIC_FLUX) {
		    printf (
"The flux in the confirmation image is %2.0f%% of the recommended minimum\n",
			    flux / MIN_SPECTROSCOPIC_FLUX * 100.);
		    printf (
"of %.0f DN for a dispersed-light ACQ/PEAK.  The signal-to-noise in\n",
			    MIN_SPECTROSCOPIC_FLUX);
		    printf (
"the ACQ/PEAK may be inadequate for an accurate centering.\n\n");
		    *badacq |= BAD_FLUX;
		}
	    }

	    /* Search for the word FAILED in keyword PEAKCENT. */
	    /* This will check if flux test failed. */
	    if ( strstr(keywords->peakcent, "FAILED") != NULL ) {
		printf (
"The ACQ/PEAK flux test failed, which means that no point in the peakup\n");
		printf (
"scan has a flux that is at least 30%% higher than any other point.  The\n");
		printf (
"ACQ/PEAK has failed, and the telescope has returned to the initial\n");
		printf ("position of the ACQ/PEAK\n\n");
		*badacq |= BAD_ACQ;
	    }

	    /* If first & last flux values in LINEAR scans are 0. */
	    if ( strcmp(keywords->search, "LINEARAXIS1") == 0 ) {
		if (i_max == 0 || i_max == keywords->naxis1 - 1) {
		   max_at_end = 1;
		   *badacq |= BAD_END;
	      }
	    } else if ( strcmp(keywords->search, "LINEARAXIS2") == 0 ) {
		if (j_max == 0 || j_max == keywords->naxis2 - 1) {
		   max_at_end = 1;
		   *badacq |= BAD_END;
		}
	    }

	    if (max_at_end) {
		printf (
"The maximum flux in the sequence occurred at one end.\n");
		printf (
"This may indicate that the target was beyond that end\n");
		printf (
"or that a neighboring object affected the acquisition.\n");
	    }

	    if ( *badacq ==  0 ) {
		printf (
"The confirmation image has a flux between %3.1f and %3.1f times the\n",
			LOW_FLUX_CUTOFF, HIGH_FLUX_CUTOFF);
		printf (
"maximum flux in the peakup, which is typical of a successful ACQ/PEAK.\n");
	    }
	}

	/* Note that acqstatus is set if badacq is bad, but acqstatus is not
	 reset if badacq is OK.
	*/
	if (*badacq)
	    *acqstatus = 1;
}


static double arcseconds (double x) {

	return (x * PLATESCALE);

}


static double V2coord (double x, double y) {

	return (COSTHETA*x + SINTHETA*y);
}


static double V3coord (double x, double y) {

	return (COSTHETA * x - SINTHETA * y);
}


static int tastis_immap (char *infile, char *extn, IRAFPointer *im) {

	char *filename;

	if ((filename = calloc (SZ_FNAME, sizeof(char))) == NULL ) {
	    printf ("Out of memory.\n");
	    return (FATAL_ERROR);
	}

	strcpy (filename, infile);
	strcat (filename, extn);

	*im = c_immap (filename, IRAF_READ_ONLY, 0);
	if ( c_iraferr() ) {
	    printf ("Can't find %s ... skipping\n", filename);
	    *im = 0;
	    return (c_iraferr());
	}

	free (filename);
	return (0);
}
