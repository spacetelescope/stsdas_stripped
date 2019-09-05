/* N_REFDATA: Contains various routines for reading reference data,
** as well as initializing and freeing the reference data structures.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Rewritten for Build 2 (Version 2.0)
** H.Bushouse	27-Jan-1997	Modified n_getNlinData to check for dummy ref
**				file before reading ref data. Modified
**				n_getDarkImage to check for dummy ref file
**				before reading ref exposure time list. Modified
**				n_getPhotData and n_getPedigree to set the new
**				ref.dummy variable. Modified n_getNlinData to
**				handle new data types for NlinData structure.
**				(Version 2.1)
** H.Bushouse	25-Apr-1997	Modified n_getZoffImage to read SAMPTIME for
**				for zeroth-read. (Version 2.2)
** H.Bushouse	13-Jun-1997	n_getRefData loads NOISFILE for use by CRIDCALC
**				(Version 2.3)
** H.Bushouse	31-Jul-1997	Added group number to n_getDarkImage arguments
**				to support new n_zsigcalc routine (Version 3.0)
** H.Bushouse	18-Aug-1997	Added NlinData.zsci and zerr elements to
**				n_initRefData, n_freeRefData, and n_getNlinData
**				routines (Version 3.0)
** H.Bushouse	02-Oct-1997	n_getRefData no longer loads NOISFILE for use
**				by CRIDCALC (Version 3.0)
** H.Bushouse	14-Jan-1998	Fixed bug in n_getDarkImage that was causing a
**				memory error for MultiAccums with NSAMP=2:
**				instead of keying on "ngroup" for release of
**				memory for "etime", use "nic->group".
**				(Version 3.1.1)
** H.Bushouse	09-Mar-1998	Modified n_getDarkImage to look for exposure
**				time matches only to the nearest 10th of a
**				second (Version 3.2)
** H.Bushouse	05-Oct-1998	Modified n_getRefData to load ref files needed
**				by non-primary steps. Moved part of the dummy
**				ref file handling out of n_getPedigree and into
**				n_getRefData (Version 3.3)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	05-Aug-1999	Modified n_getNlinData to load up to 3 coeff
**				images and up to 6 error images, to accomodate
**				2nd-order correction scheme (Version 3.3)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
** H.Bushouse	19-Jun-2000	Renamed "mismatch" variable to "nmisses" to
**				avoid name conflict with mismatch routine in
**				n_pedcorr.c. Added new routine freeNlinData and
**				updated n_freeRefData to use it. Removed free
**				for nic->dtimes from n_freeRedData as it is now
**				a static array. Removed nic from n_freeRefData
**				argument list. (Version 4.0)
** R.Jedrzeje   31-Jan-2002     Construct dynamic dark reference file name from
**                              input file name & put it in nic.dyndarkfile
**                              (n_getDarkTemp)
** R.Jedrzeje   07-Nov-2007     Check for temperature=0.0, and abort if so.
                                Check for _raw1.fits.  Version 4.2d23 for
                                ticket #129
** R.Jedrzeje   08-Jul-2008     Check for TFBCALC="COMPLETE", if so, use
                                TFBTEMP keyword instead of temperature from
                                _spt file.  Version 4.3.

** M. Sosey     26-Oct-2008    added new logic for temperature dependent flatfields
**							to be applied when the TFBTEMP is in range, if not the
**							the original flatfield is applied
**
**M. Sosey    29-Oct-2008 updated to allow for temperature dependent linear dark
**                        and ampglow images
**
**M. Sosey    12-Oct-2008   updated to get static temperature from tdd file for dark
**                          corrections if nothing else was in range
**.
**M. Sosey   19-May-2009    updated error message for TFBHIGH/TFBLOW not found in dark header
**                         to instruct the user to get an updated reference file and then
**                         exit the calnica run cleanly with a status error
**
**M. Sosey        July 14, 2009   Changed the value of ALLOWDIF from 0.01 to 0.001
**                                to correct dark selection in bright object mode
**                                see ticket#428 
**
*/

# include <math.h>
# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <xtables.h>	/* defines TABLE I/O functions */
# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

# define  FATAL   1
# define  WARNING 2

int miss_file;		/* missing ref file counter */
int miss_kwd;		/* missing keyword counter */
int nmisses;		/* ref data mismatch counter */

static int  ckOutputFile (char *);  /* To check whether we can write to dark */

/* N_GETREFDATA: Loads all reference data into memory. Checks for
** missing reference files and reference file data that do not match
** the observing mode of the science data being processed. Returns with
** an error status if any ref files are missing or incompatibile.
*/

int n_getRefData (NicInfo *nic, SingleNicmosGroup *zoff,
	SingleNicmosGroup *mask, SingleNicmosGroup *nois, NlinData *nlin,
	SingleNicmosGroup *dark, SingleNicmosGroup *flat, PhotData *phot) {

/* Arguments:
**	nic	io: NICMOS info structure
**	zoff	 o: MULTIACCUM zeroth-read image
**	mask	 o: bad pixel mask
**	nois	 o: read-noise image
**	nlin	 o: non-linearity image
**	dark	 o: dark current image
**	flat	 o: flat field image
**	phot	 o: photometry parameters
*/

	/* Function definitions */
	int n_getZoffImage (NicInfo *, SingleNicmosGroup *);
	int n_getMaskImage (NicInfo *, SingleNicmosGroup *);
	int n_getNoisImage (NicInfo *, SingleNicmosGroup *);
	int n_getDarkInfo  (NicInfo *, SingleNicmosGroup *);
	int n_getNlinData  (NicInfo *, NlinData *);
	int n_getFlatImage  (NicInfo *, SingleNicmosGroup *);
	int n_getPhotData  (NicInfo *, PhotData *);

	/* Initialize the error counters */
	miss_file = 0;
	miss_kwd  = 0;
	nmisses   = 0;

	/* Load the photometry parameters */
	if (nic->PHOT.corr == PERFORM) {
	    if (n_getPhotData (nic, phot))
		return (status);
	    if (nic->PHOT.ref.dummy) {
		nic->PHOT.corr = SKIP;
		sprintf (MsgText, "%s will be skipped", nic->PHOT.swname);
		n_warn  (MsgText);
	    }
	}
    
	/* Load the DARK image information */
	if (nic->DARK.corr == PERFORM || nic->NOIS.corr == PERFORM ||
	    nic->ZSIG.corr == PERFORM) {
	    if (n_getDarkInfo (nic, dark))
		return (status);
	    if (nic->DARK.ref.dummy) {
		if (nic->DARK.corr == PERFORM) {
		    nic->DARK.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->DARK.swname);
		    n_warn  (MsgText);
		}
		if (nic->NOIS.corr == PERFORM) {
		    nic->NOIS.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->NOIS.swname);
		    n_warn  (MsgText);
		}
		if (nic->ZSIG.corr == PERFORM) {
		    nic->ZSIG.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->ZSIG.swname);
		    n_warn  (MsgText);
		}
	    }
	}

	/* Load the MULTIACCUM zeroth-read image */
	if (nic->ZOFF.corr == PERFORM) {
	    if (n_getZoffImage (nic, zoff))
		return (status);
	}

	/* Load the bad pixel MASK image */
	if (nic->MASK.corr == PERFORM || nic->ZSIG.corr == PERFORM) {
	    if (n_getMaskImage (nic, mask))
		return (status);
	    if (nic->MASK.ref.dummy) {
		if (nic->MASK.corr == PERFORM) {
		    nic->MASK.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->MASK.swname);
		    n_warn  (MsgText);
		}
		if (nic->ZSIG.corr == PERFORM) {
		    nic->ZSIG.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->ZSIG.swname);
		    n_warn  (MsgText);
		}
	    }
	}

	/* Load the readNOISE image */
	if ((nic->NOIS.corr == PERFORM && nic->obsmode != RAMP) ||
	    nic->ZSIG.corr == PERFORM) {
	    if (n_getNoisImage (nic, nois))
		return (status);
	    if (nic->NOIS.ref.dummy) {
		if (nic->NOIS.corr == PERFORM) {
		    nic->NOIS.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->NOIS.swname);
		    n_warn  (MsgText);
		}
		if (nic->ZSIG.corr == PERFORM) {
		    nic->ZSIG.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->ZSIG.swname);
		    n_warn  (MsgText);
		}
	    }
	}

	/* Load the non-linearity coefficients */
	if (nic->NLIN.corr == PERFORM || nic->ZSIG.corr == PERFORM) {
	    if (n_getNlinData (nic, nlin))
		return (status);
	    if (nic->NLIN.ref.dummy) {
		if (nic->NLIN.corr == PERFORM) {
		    nic->NLIN.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->NLIN.swname);
		    n_warn  (MsgText);
		}
		if (nic->ZSIG.corr == PERFORM) {
		    nic->ZSIG.corr = SKIP;
		    sprintf (MsgText, "%s will be skipped", nic->ZSIG.swname);
		    n_warn  (MsgText);
		}
	    }
	}

	/* Load the FLAT field image */
	if (nic->FLAT.corr == PERFORM) {
	    if (n_getFlatImage (nic, flat))
		return (status);
	    if (nic->FLAT.ref.dummy) {
		nic->FLAT.corr = SKIP;
		sprintf (MsgText, "%s will be skipped", nic->FLAT.swname);
		n_warn  (MsgText);
	    }
	}


	/* Load the background model parameters */
	if (nic->BACK.corr == PERFORM) {
	  sprintf (MsgText, "Background model not yet implemented");
	  n_warn (MsgText);
	    /* no routine yet */
	}

	/* Error check */
	if (miss_file > 0) {
	    sprintf (MsgText, "Calibration reference file missing");
	    n_error (MsgText);
	    status = 1;
	} else if (nmisses > 0) {
	    sprintf (MsgText, "Incompatible reference data");
	    n_error (MsgText);
	    status = 1;
	} else if (miss_kwd > 0) {
	    status = 1;
	} else {
	    status = 0;
	}

	/* return */
	return (status);
}
 
/* N_GETZOFFIMAGE: Load the data for a MULTIACCUM zeroth-read image.
** This is simply the last image group in the input file being processed.
** It is not necessary to check for PEDIGREE and DESCRIP keywords or to
** check the image size, as with normal reference images.
*/

int n_getZoffImage (NicInfo *nic, SingleNicmosGroup *zoff) {

/* Arguments:
**	nic	i: NICMOS info structure
**	zoff 	o: zeroth-read image data
*/

	/* Initialize the image data structure */
	initSingleNicmosGroup (zoff);

	/* Read the data */
	if (getSingleNicmosGroup (nic->ZOFF.ref.name, nic->nsamp, zoff))
	    status = 1;
	if (hstio_err() || status) {
	    n_filerr (nic->ZOFF.ref.name);
	    freeSingleNicmosGroup (zoff);
	    miss_file += 1;
	    return (status = 0);	/* don't abort yet */
	}

	/* Read the exposure time for the zeroth-read */
	nic->exptime[nic->nsamp-1] = 0;
	if (getKeyD (&(zoff->sci.hdr), "SAMPTIME",
		     &(nic->exptime[nic->nsamp-1]))) {
	    n_kwerr ("SAMPTIME", nic->ZOFF.ref.name);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_GETMASKIMAGE: Load the bad pixel MASK reference file image and
** check to make sure it's the appropriate file for using with the
** science data being processed.
*/

int n_getMaskImage (NicInfo *nic, SingleNicmosGroup *mask) {

/* Arguments:
**	nic	i: NICMOS info structure
**	mask	o: bad pixel mask image
*/

	/* Local variables */
	Bool missing;		/* missing file flag */

	/* Function definitions */
	int n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void checkKeyI (char *, Hdr *, char *, int, int);

	/* Read the reference image */
	if (n_getRefImage (&nic->MASK, 1, mask, &missing))
	    return (status);

	/* Check for a missing file */
	if (missing) {
	    miss_file += 1;
	    return (status = 0);	/* don't abort yet */
	}

	/* Check the MASK file CAMERA number */
	checkKeyI (nic->MASK.ref.name, mask->globalhdr, "CAMERA", nic->camera,
		   FATAL);

	/* Successful return */
	return (status = 0);
}

/* N_GETNOISIMAGE: Load the readnoise reference file image and
** check to make sure it's the appropriate file for using with the
** science data being processed.
*/

int n_getNoisImage (NicInfo *nic, SingleNicmosGroup *nois) {

/* Arguments:
**	nic	i: NICMOS info structure
**	nois	o: read noise image
*/

	/* Local variables */
	Bool missing;		/* missing file flag */

	/* Function definitions */
	int n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void checkKeyI (char *, Hdr *, char *, int, int);
	void checkKeyS (char *, Hdr *, char *, char *, int);

	/* Read the reference image */
	if (n_getRefImage (&nic->NOIS, 1, nois, &missing))
	    return (status);

	/* Check for a missing file */
	if (missing) {
	    miss_file += 1;
	    return (status = 0);	/* don't abort yet */
	}

	/* Check the NOISE file CAMERA number */
	checkKeyI (nic->NOIS.ref.name, nois->globalhdr, "CAMERA", nic->camera,
		   FATAL);

	/* Check the NOISE file READOUT speed */
	checkKeyS (nic->NOIS.ref.name, nois->globalhdr, "READOUT", nic->readout,
		   FATAL);

	/* Check the NOISE file NREAD value */
	checkKeyI (nic->NOIS.ref.name, nois->globalhdr, "NREAD", nic->nread,
		   WARNING);

	/* Successful return */
	return (status = 0);
}

/* N_GETNLINDATA: Read linearity coefficients and their associated
** data quality flags and node values from the NLINFILE reference file.
** The coefficient, DQ, and node arrays are returned in a single NlinData 
** structure. The super zero read SCI and ERR arrays are also read and loaded
** into the NlinData structure. Also check to make sure that this reference
** file is the appropriate one to use with the science data being processed.
**
** Initially the correction used was a 1st-order polynomial, involving 2
** coefficient and 3 error images. This routine now accomodates a 2nd-order
** correction, using 3 coefficient and 6 error images. It is designed to be
** backwards-compatible with the 1st-order reference files by checking to
** see how many coeff arrays are present. Unused coeff and error arrays are
** zero-filled.
*/

int n_getNlinData (NicInfo *nic, NlinData *nlin) {

/* Arguments:
**	nic 	 i: NICMOS info structure
**	nlin	 o: data structure containing linearity coeff's and DQ flags
*/

	/* Local variables */
	int i, j, k;		/* loop indexes */
	IODescPtr im;		/* image pointer */
	int ncoeff = 0;		/* number of coeff arrays */

	/* Function definitions */
	int n_getPedigree (Hdr *, CalStep *);
	void checkKeyI (char *, Hdr *, char *, int, int);

	/* Allocate memory for the nlin data structure */
	nlin->coeff = (FloatHdrData *)calloc(3,sizeof(FloatHdrData));
	nlin->error = (FloatHdrData *)calloc(6,sizeof(FloatHdrData));
	nlin->dqual = (ShortHdrData *)calloc(1,sizeof(ShortHdrData));
	nlin->nodes = (FloatHdrData *)calloc(2,sizeof(FloatHdrData));
	nlin->zsci  = (FloatHdrData *)calloc(1,sizeof(FloatHdrData));
	nlin->zerr  = (FloatHdrData *)calloc(1,sizeof(FloatHdrData));
	if (nlin->coeff==NULL || nlin->error==NULL || nlin->dqual==NULL ||
	    nlin->nodes==NULL || nlin->zsci ==NULL || nlin->zerr ==NULL) {
	    sprintf (MsgText, "Can't allocate memory for NLIN ref data");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Open the nonlinearity reference file */
	im = openInputImage (nic->NLIN.ref.name, "", 0);
	if (hstio_err()) {
	    n_openerr (nic->NLIN.ref.name);
	    miss_file += 1;
	    return (status = 0);	/* don't abort yet */
	}

	/* Allocate memory for the file's primary header */
	nlin->globalhdr = (Hdr *)calloc(1,sizeof(Hdr));
	if (nlin->globalhdr == NULL) {
	    sprintf (MsgText, "Can't allocate memory for NLIN ref file header");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Read the primary header */
	getHeader (im, nlin->globalhdr);
	if (hstio_err()) {
	    n_readerr (nic->NLIN.ref.name);
	    closeImage (im);
	    freeHdr (nlin->globalhdr);
	    return (status = 1);
	}

	/* Close the image */
	closeImage (im);

	/* Read the reffile PEDIGREE and DESCRIP keywords */
	if (n_getPedigree (nlin->globalhdr, &nic->NLIN)) {
	    freeHdr (nlin->globalhdr);
	    return (status);
	}

	/* Check the NLIN file CAMERA number */
	checkKeyI (nic->NLIN.ref.name, nlin->globalhdr, "CAMERA", nic->camera,
		   FATAL);

	/* Check for a dummy file */
	if (nic->NLIN.ref.dummy)
	    return (status = 0);

	/* Read the NEXTEND keyword from the NLINFILE */
        if (getKeyI (nlin->globalhdr, "NEXTEND", &ncoeff)) {
            n_kwerr ("NEXTEND", nic->NLIN.ref.name);
            miss_kwd += 1;
            return (status = 0);
        }

	/* Determine the number of coefficient and error arrays present:
	** if nextend <= 10, assume there are 2 coeffs and 3 errors;
	** else assume there are 3 coeffs and 6 errors. */
	if (ncoeff <= 10)
	    ncoeff = 2;
	else
	    ncoeff = 3;

	/* Get the coefficient images from the NLINFILE */
	for (i=0; i < ncoeff; i++) { 
	     initFloatHdrData (&nlin->coeff[i]);
	     if (getFloatHD (nic->NLIN.ref.name, "COEF", i+1, &nlin->coeff[i]))
		 return (status=1);
	}

	/* Initialize undefined coeffs (up to 3) to zero */
	for (i=ncoeff; i < 3; i++) {
	     initFloatHdrData (&nlin->coeff[i]);
	     allocFloatHdrData (&nlin->coeff[i], SZ_NICIMG, SZ_NICIMG);
	     for (k=0; k < SZ_NICIMG; k++) {
		  for (j=0; j < SZ_NICIMG; j++)
		       Pix(nlin->coeff[i].data,j,k) = 0.0;
	     }
	}	

	/* Get the error images from the NLINFILE */
	ncoeff = (ncoeff + ncoeff*ncoeff) / 2;
	for (i=0; i < ncoeff; i++) {
	     initFloatHdrData (&nlin->error[i]);
	     if (getFloatHD (nic->NLIN.ref.name, "ERR", i+1, &nlin->error[i]))
		 return (status=1);
	}

	/* Initialize undefined errors (up to 6) to zero */
	for (i=ncoeff; i < 6; i++) {
	     initFloatHdrData (&nlin->error[i]);
	     allocFloatHdrData (&nlin->error[i], SZ_NICIMG, SZ_NICIMG);
	     for (k=0; k < SZ_NICIMG; k++) {
		  for (j=0; j < SZ_NICIMG; j++)
		       Pix(nlin->error[i].data,j,k) = 0.0;
	     }
	}	

	/* Get the DQ array from the NLINFILE */
	initShortHdrData (&nlin->dqual[0]);
	if (getShortHD (nic->NLIN.ref.name, "DQ", 1, &nlin->dqual[0]))
	    return (status=1);

	/* Get the node arrays from the NLINFILE */
	for (i=0; i<2; i++) {
	     initFloatHdrData (&nlin->nodes[i]);
	     if (getFloatHD (nic->NLIN.ref.name, "NODE", i+1, &nlin->nodes[i]))
		 return (status=1);
	}

	/* Get the super zero-read SCI array from the NLINFILE (Vsn 3.0) */
	initFloatHdrData (&nlin->zsci[0]);
	if (getFloatHD (nic->NLIN.ref.name, "ZSCI", 1, &nlin->zsci[0]))
	    return (status=1);

	/* Get the super zero-read ERR array from the NLINFILE (Vsn 3.0) */
	initFloatHdrData (&nlin->zerr[0]);
	if (getFloatHD (nic->NLIN.ref.name, "ZERR", 1, &nlin->zerr[0]))
	    return (status=1);

	/* Successful return */
	return (status = 0);
}

/* N_GETFLATIMAGE: Load the flat field reference file image and
** check to make sure it's the appropriate file to use with the
** science data being processed.
*/

int n_getFlatImage (NicInfo *nic, SingleNicmosGroup *flat) {

/* Arguments:
**	nic	i: NICMOS info structure
**	flat	o: flat field image
*/

	/* Local variables */
	Bool missing;		/* missing file flag */
    Hdr flatHdr; /*temp flat header info*/
	double tlow;
    double thigh;
    Bool flatMatch; /*flat match found*/
    
	/* Function definitions */
	int n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void checkKeyI (char *, Hdr *, char *, int, int);
	void checkKeyS (char *, Hdr *, char *, char *, int);
    int n_getImsetHdr(char *, Hdr *, char *, int);
    
	int extnum;
 
    flatMatch=1;
    sprintf(MsgText,"FLATMETH keyword = %s",nic->flatmeth);
    n_message(MsgText);
    
    if(strcmp(nic->flatmeth, "STATIC")) { /*if it's not static*/        
        
        
        /*Find the extension that should be used (with temp match)
         note that logic assuming a temp match file exists has already
         been implented in n_setup, so return an error if nothing 
         matches in the reference file
        */
        
        tlow=0.;
        thigh=0.;
        extnum=1;
        flatMatch=0;
        
        /*find the image extension to use*/               
		for(extnum==1; extnum <= nic->totFlatImages ; extnum++){
            if(!flatMatch){/*so that the lowest match is always used*/
            if(n_getImsetHdr(nic->FLAT.ref.name,&flatHdr,"SCI",extnum)){
                sprintf(MsgText,"Problem getting flat imset header");
                n_warn(MsgText);
                return(status=1);
            }
                                
            if(getKeyD(&flatHdr, "TFBLOW", &tlow)){
                sprintf (MsgText,"Unable to get TFBLOW keyword from %s,please update your reference file",nic->FLAT.ref.name);
                n_error (MsgText);
                return(status=1);
            }
            if(getKeyD(&flatHdr, "TFBHIGH", &thigh)){
                sprintf (MsgText,"Unable to get TFBHIGH keyword from %s,\nplease check or update reference file",nic->FLAT.ref.name);
                n_error (MsgText);
                return (status=1);
            }
            
            if(tlow <= nic->tfbtemp && nic->tfbtemp <= thigh){
                    nic->flatext=extnum;
                    flatMatch=1;
                    /*TDFGROUP in science header needs to be populated with this value as well*/
                    sprintf(MsgText,"Using extension %d for flatfield (%g < %g > %g)",extnum,tlow,nic->tfbtemp,thigh);
                    n_message(MsgText);
            }
            
            }
        }
        if(!flatMatch){
            sprintf (MsgText,"Unable to match Temperatures in TDFFILE (%g).",nic->tfbtemp);
            n_error (MsgText);
            return(status=1);
        }
    }
                
   /* Read the reference image */
   if (n_getRefImage (&nic->FLAT, nic->flatext, flat, &missing)){
	   return (status);
   }
    /* Check for a missing file */
   if (missing) {
       miss_file += 1;
       return (status = 0);	/* don't abort yet */
   }                
    /* Check the FLAT file CAMERA number */
    checkKeyI (nic->FLAT.ref.name, flat->globalhdr, "CAMERA", nic->camera,
    	   FATAL);

	/* Check the FLAT file FILTER name */
    checkKeyS (nic->FLAT.ref.name, flat->globalhdr, "FILTER", nic->filter,
    	   FATAL);
    
	/* Successful return */
    return (status = 0);

    
    
}
/* N_GETDARKINFO: Decide whether to load informatin from the static or dynamic dark
*/

int n_getDarkInfo (NicInfo *nic, SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark	 o: dark image
*/

	/* Function prototypes  */
	int n_getPriHdr (char *, Hdr *);
	int n_getStaticDarkInfo (NicInfo *, SingleNicmosGroup *);
	int n_getDynamicDarkInfo (NicInfo *); 

        /* Local variables */
        Hdr DarkHeader;
	char keyval[SZ_STRKWVAL+1];


	keyval[0] = '\0';

    /* Read the Dark reference file header */
	if (n_getPriHdr (nic->DARK.ref.name, &DarkHeader)) {
            return (status);
        }

    /* Check the value of the DARKMETH keyword */
	if (getKeyS (&DarkHeader, "DARKMETH", keyval)) {
	  sprintf (MsgText, "DARKMETH keyword not found");
	  n_warn (MsgText);
	  freeHdr (&DarkHeader);
	  if (n_getStaticDarkInfo (nic, dark)) {
	    sprintf (MsgText, "Error in n_getStaticDarkInfo");
	    n_error (MsgText);
	    return (status=1);
	  }
	  return (status);
	} else {
	    sprintf (MsgText, "DARKMETH keyword = %s", keyval);
	    n_message (MsgText);
	    if (strcmp (keyval, "TEMPERATURE-DEPENDENT")) {
	       freeHdr (&DarkHeader);
	       if (n_getStaticDarkInfo (nic, dark)) {
		        sprintf (MsgText, "Error in n_getStaticDarkInfo");
		        n_error (MsgText);
	       }
           return (status);
	    } else {
	       freeHdr (&DarkHeader);
	       n_getDynamicDarkInfo (nic);
	       return (status);
	    }
	}
}

/* N_GETSTATICDARKINFO: Load information from the DARKFILE reference file.
*/

int n_getStaticDarkInfo (NicInfo *nic, SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark	 o: dark image
*/
	      
	/* Local variables */
	int i;			/* loop index */
	char kword[8+1];	/* keyword name */
	Bool missing;		/* missing ref file flag */

	/* Function definitions */
	int  n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void checkKeyI (char *, Hdr *, char *, int,    int);
	void checkKeyS (char *, Hdr *, char *, char *, int);

	/* Copy the string "STATIC" into the nic.darkmeth structure member */
        strcpy (nic->darkmeth, "STATIC");

	/* Read the first group of the dark reference file */
	if (n_getRefImage (&nic->DARK, 1, dark, &missing))
	    return (status);

	/* Check for a missing file */
	if (missing) {
	    miss_file += 1;
	    return (status = 0);    /* don't abort yet */
	}

	/* Check the DARK file CAMERA number */
	checkKeyI (nic->DARK.ref.name, dark->globalhdr, "CAMERA",
		   nic->camera, FATAL);

	/* Check the DARK file NREAD number */
	checkKeyI (nic->DARK.ref.name, dark->globalhdr, "NREAD",
		   nic->nread, WARNING);

	/* Check the DARK file SAMP_SEQ value */
	checkKeyS (nic->DARK.ref.name, dark->globalhdr, "SAMP_SEQ",
		   nic->sampseq, FATAL);

	/* Check for a dummy file */
	if (nic->DARK.ref.dummy)
	    return (status = 0);

	/* Find out how many dark images are in this file */
	if (getKeyI (dark->globalhdr, "NUMEXPOS", &nic->ndarks)) {
	    n_kwerr ("NUMEXPOS", nic->DARK.ref.name);
	    return (status = 1);
	}

	/* Read the list of exposure times */
	for (i = 0; i < nic->ndarks; i++) {
	     sprintf (kword, "EXPOS_%d\0", i+1);
	     nic->dtimes[i] = 0;
	     if (getKeyD (dark->globalhdr, kword, &(nic->dtimes[i]))) {
		 n_kwerr (kword, nic->DARK.ref.name);
		 return (status = 1);
	     }
	}

	/* Successful return */
	return (status = 0);

}

/* N_GETDYNAMICDARKINFO: Load information from the TEMPFILE reference file.
*/

int n_getDynamicDarkInfo (NicInfo *nic) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark	 o: dark image
*/

      /* Function prototypes   */
      int n_getPriHdr (char *, Hdr *);

      /* Local variables  */
      Hdr DarkHeader;
      int i;
      char kword[8+1];
      double Temperature;

      /* Function definitions  */
      void checkKeyI (char *, Hdr *, char *, int, int);
      int n_getPedigree (Hdr *, CalStep *);
      float n_getDarkTemp (NicInfo *);

      strcpy (nic->darkmeth, "TEMPERATURE-DEPENDENT");

      if (n_getPriHdr (nic->DARK.ref.name, &DarkHeader))
	return (status);

      /* Check the DARK file CAMERA number  */
      checkKeyI (nic->DARK.ref.name, &DarkHeader, "CAMERA",
		 nic->camera, FATAL);

      /* Get dark file's pedigree  */
      if (n_getPedigree (&DarkHeader, &nic->DARK))
	return (status);

      /* check for a dummy file  */
      if (nic->DARK.ref.dummy)
	return (status=0);

      /* Find out how many different DELTATIMEs there are in this file */
      if (getKeyI (&DarkHeader, "NDELTA", &nic->ndelta)) {
	n_kwerr ("NDELTA", nic->DARK.ref.name);
	return (status=1);
      }

      /* Read the list of deltatimes  */
      for (i=0; i < nic->ndelta; i++) {
				sprintf (kword, "DELTA%d\0", i+1);
				nic->ddeltas[i] = 0;
				if (getKeyD (&DarkHeader, kword, &(nic->ddeltas[i]))) {
					n_kwerr (kword, nic->DARK.ref.name);
					return (status=1);
				}
      }
			freeHdr (&DarkHeader);

      /* Get the temperature that we'll use in the polynomial expression for the shading and
       photometry tables ONLY should be either TFBTEMP, TMPSETS or NDWTMP11 */
      Temperature = n_getDarkTemp (nic);
      
      /* Temperature will be 0.0 if there was a problem reading the SPT file */
      /* If this is the case, exit  */
      if (Temperature == 0.0) {
	    sprintf (MsgText, "Error accessing _spt file");
        n_message (MsgText);
        return (status=1);
      }
      nic->temperature = Temperature;
      
      if (nic->DARK.corr == PERFORM) {
        if (nic->writedark) {
	        strcpy (nic->dyndarkfile, nic->prefix);
	        strcat (nic->dyndarkfile, "_drk.fits");
      /* Check that this reference file doesn't already exist  */
            if (ckOutputFile (nic->dyndarkfile)) {
	            return (status);
  	        }
        }
      }
      /* Successful return  */
      return (status=0);
}


int n_getDarkImage (NicInfo *nic, SingleNicmosGroup *dark1, int ngroup) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark1	 o: dark current image
**	ngroup	 i: group number
*/

        /*  Function prototypes  */
        int n_getStaticDarkImage (NicInfo *, SingleNicmosGroup *, int);
		int n_getDynamicDarkImage (NicInfo *, SingleNicmosGroup *, int);

        /* Check for dark calculation method and call appropriate function  */
        if (!strcmp (nic->darkmeth, "STATIC")) {
          if (n_getStaticDarkImage (nic, dark1, ngroup))
            return (status);
        } else if (!strcmp (nic->darkmeth, "TEMPERATURE-DEPENDENT")) {
           if (n_getDynamicDarkImage (nic, dark1, ngroup))
            return (status);
        } else {
          sprintf (MsgText, "Unknown Dark calculation method");
					n_error (MsgText);
          return (status);
        }
				return (status=0);
}

# define ALLOWDIFF 0.001	/* Max allowed exptime difference */

int n_getStaticDarkImage (NicInfo *nic, SingleNicmosGroup *dark1, int ngroup) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark1	 o: dark current image
**	ngroup	 i: group number
*/

	/* Local variables */
	int i;					/* loop index */
	double etime_lower, etime_upper;	/* dark exposure times */
	SingleNicmosGroup dark2;		/* temporary dark image */
	Bool missing;

	/* Function definitions */
	int  n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void dark_interp (SingleNicmosGroup *, SingleNicmosGroup *, 
			  double, double, double);
	void dark_extrap (SingleNicmosGroup *, double, double);
	double fabs (double);

	/* Initialize frame interpolation information */
	nic->DarkType   = 0;
	nic->darkframe1 = 0;
	nic->darkframe2 = 0;

	/* If the exposure time is zero, still need to load data */
	/*
	if (nic->exptime[ngroup-1] <= 0)
	    return (status = 0);
	*/

	/* Find the appropriate match between science and dark exp time */
	if (nic->obsmode == MULTIACCUM &&
	    strncmp (nic->sampseq, "NONE", 4) != 0) {
	    /* If the science data are from a MultiAccum standard exposure
	    ** time sequence, then find the ref file group that matches the
	    ** exposure time of the science group. */
	    for (i = 0; i < nic->ndarks; i++) {
		if (fabs(nic->dtimes[i]-nic->exptime[ngroup-1]) <= ALLOWDIFF){
			nic->DarkType = MATCH;
			nic->darkframe1 = i+1;
			break;
		}
	   }

	    /* Make sure we found a match */
	    if (nic->darkframe1 == 0) {
		sprintf (MsgText, "Can't find matching dark time", \
               " in %s for group %d", nic->DARK.ref.name, ngroup);
		n_error (MsgText);
		return (status = 1);
	    }

	} else {

	    /* If the science data aren't from a MultiAccum standard exposure
	    ** time sequence, then find an exposure time that matches the
	    ** science data, or two times that bracket it */

		for (i = 0; i < nic->ndarks; i++) {
		  if (fabs(nic->dtimes[i]-nic->exptime[ngroup-1]) <= ALLOWDIFF){
			nic->DarkType = MATCH;
			nic->darkframe1 = i+1;
			break;
		  } else if (nic->dtimes[i] < nic->exptime[ngroup-1]) {
			nic->DarkType = INTERP;
			nic->darkframe1 = i+1;
			etime_lower = nic->dtimes[i];
		  } else if (nic->dtimes[i] > nic->exptime[ngroup-1] &&
		   nic->darkframe2 == 0) {
			nic->DarkType = INTERP;
			nic->darkframe2 = i+1;
			etime_upper = nic->dtimes[i];
		  }
		}
	}

	/* Reinitialize for loading next dark image */
	freeSingleNicmosGroup (dark1);
	initSingleNicmosGroup (dark1);

	/* If there's a dark image with a matching exposure time, load it. */
	if (nic->DarkType == MATCH) {
	    if (n_getRefImage (&nic->DARK, nic->darkframe1, dark1, &missing)) {
	        sprintf (MsgText, "Error in n_getRefImage, status = %d", status);
		n_error (MsgText);
		return (status);
	    }
	    if (missing) {
		miss_file += 1;
		return (status = 0);
	    }

	/* Otherwise, load two images and interpolate between them */
	} else if (nic->darkframe1 != 0  &&  nic->darkframe2 != 0) {
	    if (n_getRefImage (&nic->DARK, nic->darkframe1, dark1, &missing))
		return (status);
	    if (missing) {
		miss_file += 1;
		return (status = 0);
	    }

	    if (n_getRefImage (&nic->DARK, nic->darkframe2, &dark2, &missing))
		return (status);
	    if (missing) {
		miss_file += 1;
		return (status = 0);
	    }

	    dark_interp (dark1, &dark2, etime_lower, etime_upper,
		 nic->exptime[ngroup-1]);

	    freeSingleNicmosGroup (&dark2);

	/* Otherwise, extrapolate outside the available range */
	} else if (nic->darkframe1 !=0) {
		nic->DarkType = EXTRAP;
		if (n_getRefImage (&nic->DARK, nic->darkframe1, dark1, &missing))
			return (status);
		if (missing) {
			miss_file += 1;
			return (status = 0);
		}

		dark_extrap (dark1, etime_lower, nic->exptime[ngroup-1]);

	} else if (nic->darkframe2 !=0) {
		nic->DarkType = EXTRAP;
		if (n_getRefImage (&nic->DARK, nic->darkframe2, dark1, &missing)) 
			return (status);
		if (missing) {
			miss_file += 1;
			return (status = 0);
		}

		dark_extrap (dark1, etime_upper, nic->exptime[ngroup-1]);
	}

	/* Successful return */
	return (status = 0);

}

#define SHADCOLS 8      /* Number of columns in SHAD tables  */

int n_getDynamicDarkImage(NicInfo *nic, SingleNicmosGroup *dark1, int ngroup) {
  /* Get a dark image using the temperature-dependent dark method  */

  /* Function definitions  */
	void n_amulk (SingleNicmosGroup *, float);
	void n_aadd (SingleNicmosGroup *, SingleNicmosGroup *);
	float poly (float *, int, float);
	int n_copytoSingleNicmosGroup (SingleNicmosGroup *, FloatHdrData,
                                       NicInfo *, int);
    int n_getImsetHdr(char *filename, Hdr *imsetHdr, char *extname, int extver);
	void c_tbciga (int, int, int *, int [], int);
    int n_getPriHdr (char *, Hdr *);
    
	FloatHdrData xdark;
	SingleNicmosGroup dark2;
	IRAFPointer tp;                   /* Shading table pointer */
	IRAFPointer colptr[SHADCOLS];
	float exptime;
	float DeltaTime;
	float diff;
	float diffmin;
	int nreads;
	int nrows;
	int i;
	int col;
	int axlen[2];
	int ndims;
	int ncoeffs;
	float Temperature;
	char shadtab[SZ_NAME];
	char cGroupnum[5];
	int xstart, xend;
	int ystart, yend;
	int axisvary;
	int pstart, pend;
	int j;
	int x, y, p;
	float argument;
	float value;
    Hdr darkHdr;
    Hdr priHdr;
    float c1_amp;
    float c0_amp;
    float f;
    float logf;
    float c1_lin;
    float c0_lin;
    
    /* the tfb high/low temps are now specific to the group they are in */
    float tfblow;
    float tfbhigh;
    
	char colname[SHADCOLS][SZ_COLNAME+1] = {
	  "XSTART",
	  "XEND",
	  "YSTART",
	  "YEND",
	  "AXISVARY",
	  "PSTART",
	  "PEND",
	  "COEFF"};

	float *pcoeffs;
	float coeff[10];

	FitsKw kw;
 
        
   if (nic->tfbtemp > -1){ 
    /*get the tfblow and tfbhigh for the shading only out of the primary dark header*/
    if (n_getPriHdr (nic->DARK.ref.name, &priHdr)){
        sprintf(MsgText,"Problem getting primary header from dark");
        n_error(MsgText);
    }
    if(getKeyF(&priHdr,"TFBLOW",&tfblow)){
        sprintf(MsgText,"TFBLOW not in dark primary header, please update your reference file");
        n_error(MsgText);
        return(status=1);
    }
    if(getKeyF(&priHdr,"TFBHIGH",&tfbhigh)){
        sprintf(MsgText,"TFBHIGH not in dark primary header, please update your reference file");
        n_error(MsgText);
        return(status=1);
    }
    
    }
    	
	/* Open the dark reference file  */
	initFloatHdrData (&xdark);

	if (getFloatHD (nic->DARK.ref.name, "LIN", 1, &xdark)) {
	  sprintf (MsgText, "Error getting LIN group of dark image");
	  n_error (MsgText);
	  return (status);
	}

	/*  Need to convert to a SingleNicmosGroup  */
	if (n_copytoSingleNicmosGroup (dark1, xdark, nic, 1)) {
	  sprintf (MsgText, "Error converting LIN extension to SNG");
	  n_error (MsgText);
	  return (status);
	}

	freeFloatHdrData (&xdark);

	/* Multiply by the data image's exposure time  */
	exptime = nic->exptime[ngroup-1];
	n_amulk (dark1, exptime);

	/* Get the AMP extension from reference file  */
	initFloatHdrData (&xdark);

	if (getFloatHD (nic->DARK.ref.name, "AMPGLOW", 1, &xdark)) {
	  sprintf (MsgText, "Error getting AMPGLOW group of DARK image");
	  n_error (MsgText);
	  return (status);
	}

	/* Convert to SNG  */
	if (n_copytoSingleNicmosGroup (&dark2, xdark, nic, 1)) {
	  sprintf (MsgText, "Error converting AMPGLOW extension to SNG");
	  n_error (MsgText);
	  return (status);
	}

	/* Multiply AMPGLOW image by the number of reads for this group  
      If the TFBTEMP is valid, then also scale the TDD amp glow
      and linear dark images dependent on the camera
    */
    
    nic->ampScale=1.;
    nic->linScale=1.;
    
    if( nic->tfbtemp != -1){ /*only do this if there is an available TFBTEMP*/
        
        /*get some keywords from the tdd file*/
               
        tfblow=0.;
        tfbhigh=0.;
        c1_amp=0.;
        c0_amp=1.;
        
        if(n_getImsetHdr(nic->DARK.ref.name, &darkHdr, "AMPGLOW",1)){
            sprintf(MsgText,"Error getting  AMPGLOW header");
            n_error(MsgText);
            return(status=0);
        }
        if(getKeyF(&darkHdr,"C1_AMP",&c1_amp)){
            sprintf(MsgText,"Unable to get C1_AMP keyword value");
            n_warn(MsgText);
        }
        if(getKeyF(&darkHdr,"C0_AMP",&c0_amp)){
            sprintf(MsgText,"Unable to get C0_AMP keyword value");
            n_warn(MsgText);
        }
        
        if(getKeyF(&darkHdr,"TFBLOW",&tfblow)){
            sprintf(MsgText,"Unable to get TFBLOW from AMPGLOW extension header");
            n_warn(MsgText);
        }    
            
        if(getKeyF(&darkHdr,"TFBHIGH",&tfbhigh)){
            sprintf(MsgText,"Unable to get TFBHIGH from AMPGLOW extension header");
            n_warn(MsgText);
        }    
                    
        if(tfblow <= nic->tfbtemp && nic->tfbtemp <= tfbhigh){                    
            if(nic->camera ==1){
                f= c1_amp*(nic->tfbtemp -76.2479) + c0_amp  ;
            }
            if(nic->camera ==2){
                f= c1_amp*(nic->tfbtemp -76.1356) + c0_amp  ;
            }
            if(nic->camera ==3){
                f= c1_amp*(nic->tfbtemp -76.6528) + c0_amp;
            }    
            nic->ampScale = f;
            n_amulk(&dark2, f);
            
        } 
         
        /*now the linear image*/
                           
        tfblow=0.;
        tfbhigh=0.;
         
        if(n_getImsetHdr(nic->DARK.ref.name, &darkHdr, "LIN",1)){
            sprintf(MsgText,"Error getting  Linearity header");
            n_error(MsgText);
            return(status=0);
        }
       

        if(getKeyF(&darkHdr,"C1_LIN",&c1_lin)){
            sprintf(MsgText,"Unable to get C1_LIN keyword value");
            n_warn(MsgText);
        }
        if(getKeyF(&darkHdr,"C0_LIN",&c0_lin)){
            sprintf(MsgText,"Unable to get C0_LIN keyword value");
            n_warn(MsgText);
        }
        if(getKeyF(&darkHdr,"TFBLOW",&tfblow)){
            sprintf(MsgText,"Unable to get TFBLOW from LIN extension header");
            n_warn(MsgText);
        }    
        if(getKeyF(&darkHdr,"TFBHIGH",&tfbhigh)){
            sprintf(MsgText,"Unable to get TFBHIGH from LIN extension header");
            n_warn(MsgText);
        }    
        
        if(tfblow <= nic->tfbtemp && nic->tfbtemp <= tfbhigh){ 
            logf= (c1_lin / nic->tfbtemp) + c0_lin;
            f=pow(10,logf);
            nic->linScale=f;                       
            n_amulk(dark1,f);
        } 
        
    }
	nreads = nic->ngroups - ngroup;        

	n_amulk (&dark2, (float)nreads);

	/* Now add the ampglow and linear dark components together */
	n_aadd (dark1, &dark2);

	freeFloatHdrData (&xdark);

	/* Now we need to get the shading correction, which depends on  */
	/* the temperature obtained from either the spt file or tfbtemp */
    /* there is logic that puts the correct value in nic->temperature */

	/* First find which delta time in the dark reference file is    */
	/* closest to the delta time of our group                       */

	for (i=0; i < SZ_NICIMG; i++) {
	    for (j=0; j < SZ_NICIMG; j++) {
	      Pix(dark2.sci.data, j, i) = 0.0;
	      Pix(dark2.err.data, j, i) = 0.0;
	    }
	}

	DeltaTime = nic->delttime[nic->group-1];

	nic->DarkType = 0;
	nic->darkframe1 = 0;
	nic->darkframe2 = 0;

	diffmin = 1.0e6;
	for (i=0; i < nic->ndelta; i++) {
	  diff = fabs(nic->ddeltas[i] - DeltaTime);
	  if (diff <= ALLOWDIFF) {
	    nic->DarkType = MATCH;
	    nic->darkframe1 = i+1;
	    break;
	  } else {
	    if (diff < diffmin) {
	      diffmin = diff;
	      nic->darkframe1 = i+1;
	      nic->DarkType = NEAREST;
	    }
	  }
	}

	/* Make sure we found a match  */
	if (nic->darkframe1 == 0) {
	  sprintf (MsgText, 
		 "Can't find matching delta time in %s for group %d",
		 nic->DARK.ref.name, ngroup);
	  n_error (MsgText);
	  return (status=1);
	}


	/* Now get the table entry for that group  */
	/* Need to construct the filename for that binary table  */
	strcpy (shadtab, nic->DARK.ref.name);
	strcat (shadtab, "[SHAD,");
	sprintf (cGroupnum, "%d", nic->darkframe1);
	strcat (shadtab, cGroupnum);
	strcat (shadtab, "]");

	/* try opening the table  */
	tp = c_tbtopn (shadtab, 1, 0);
	if (c_iraferr()) {
	  n_openerr (shadtab);
	  miss_file += 1;
	  return (status=0);          /* Don't abort yet  */
	}

	/* Get pointers to the columns in SHADTAB  */
	for (col=0; col < SHADCOLS; col++) {
	  c_tbcfnd1 (tp, colname[col], &(colptr[col]));
	  if (c_iraferr() || colptr[col] == 0) {
	    sprintf (MsgText, "Can't find column %s in %s",
		     colname[col], shadtab);
	    n_error (MsgText);
	    c_tbtclo (tp);
	    return (status=1);
	  }
	}
	/* Get number of rows in SHADTAB  */
	nrows = 0;
	nrows = c_tbpsta (tp, TBL_NROWS);
	if (nrows <= 0) {
	  sprintf (MsgText, "Invalid number of rows in %s",
			 shadtab);
	  n_error (MsgText);
	  c_tbtclo (tp);
	  return (status=1);
	}

	/*  Create a SingleNicmosGroup to put the data   */

	/*  Process each row and fill the shading image   */
	/*  First get the number of coefficients          */
	c_tbciga (tp, colptr[7], &ndims, axlen, 2);
	if (ndims != 2) {
	  sprintf (MsgText, "Wrong dimensions for coefficients array");
	  n_error (MsgText);
	  return (status=1);
	}

	ncoeffs = axlen[0] * axlen[1];
	pcoeffs = (float *) calloc (ncoeffs, sizeof(float));
	for (i=0; i < nrows; i++) {
	  c_tbagtr (tp, colptr[7], i+1, pcoeffs, 1, ncoeffs);
	  c_tbegti (tp, colptr[0], i+1, &xstart);
	  c_tbegti (tp, colptr[1], i+1, &xend);
	  c_tbegti (tp, colptr[2], i+1, &ystart);
	  c_tbegti (tp, colptr[3], i+1, &yend);
	  c_tbegti (tp, colptr[4], i+1, &axisvary);
	  c_tbegti (tp, colptr[5], i+1, &pstart);
	  c_tbegti (tp, colptr[6], i+1, &pend);
	  for (j=0; j < axlen[1]; j++) {
	    coeff[j] = poly (pcoeffs+j*axlen[0], axlen[0], nic->temperature);
	  }

	  /* Fill the specified region of our array   */

	  /*  Consider the case where the polynomial variation is in   */
	  /*  the X direction first                                    */
	  if (axisvary == 1) {
	    /* All the columns look the same    */
	    for (y=ystart; y <= yend; y++) {
	      p = pstart;
	      for (x=xstart; x <= xend; x++) {
		argument = p;
		value = poly (&coeff[0], axlen[1], argument);
		Pix(dark2.sci.data, x-1, y-1) += value;
		p++;
	      }
	    }
	  } else if (axisvary == 2) {
	    /* When axisvary=2, all the rows look the same  */
	    p = pstart;
	    for (y=ystart; y <= yend; y++) {
	      argument = p;
	      value = poly (&coeff[0], axlen[1], argument);
	      for (x=xstart; x <= xend; x++) {
		Pix (dark2.sci.data, x-1, y-1) += value;
	      }
	      p++;
	    }
	  } else {
	    sprintf (MsgText, "Bogus value of AXISVARY: %d instead of 1 or 2",
		 axisvary);
	    n_error (MsgText);
	    return (status=1);
	  }

	}
	free (pcoeffs);

	n_aadd (dark1, &dark2);

	/*  Now get the static shading component    */
	initFloatHdrData (&xdark);

	if (getFloatHD (nic->DARK.ref.name, "STATIC", nic->darkframe1,
		&xdark)) {
	  sprintf (MsgText, "Error getting STATIC component of DARK image");
	  n_error (MsgText);
	  return (status=1);
	}

	/* Convert to SNG  */
	if (n_copytoSingleNicmosGroup (&dark2, xdark, nic,
               nic->darkframe1)) {
	  sprintf (MsgText, "Error converting STATIC component to SNG");
	  n_error (MsgText);
	  return (status=1);
	}

	/* Now add in the static shading component */
	n_aadd (dark1, &dark2); 

	freeFloatHdrData (&xdark);
	freeSingleNicmosGroup (&dark2);

	/* Add some items to the header, if they're not there already  */
	kw = findKw (dark1->globalhdr, "SAMP_SEQ");
	if (kw == NotFound) {
	  addStringKw (dark1->globalhdr, "SAMP_SEQ", nic->sampseq,
		       "MultiAccum exposure time sequence name");
	  addFloatKw (dark1->globalhdr, "EXPTIME", nic->exptime[0],
		      "exposure duration (seconds)--calculated");
	  addIntKw (dark1->globalhdr, "NREAD", nic->nread,
		    "ACCUM - number of initial and final readouts");
	  addIntKw (dark1->globalhdr, "NSAMP", nic->nsamp,
		    "RAMP, MULTI-ACCUM - number of samples");
	  addFloatKw (dark1->globalhdr, "DARKTEMP", nic->temperature,
		      "Dark temperature used");
	  addStringKw (dark1->globalhdr, "READOUT", nic->readout,
		       "detector array readout rate (FAST, SLOW)");
	  addFloatKw (&(dark1->sci.hdr), "DELTATIM",
		      nic->ddeltas[nic->darkframe1-1],
		      "integration time of this sample (sec)");
	}

	return (status=0);
}

/*  POLY:  return a float that represents the polynomial evaluation of   */
/*         polynomial with ncoeffs coefficients pcoeffs for argument x   */

float poly (float *pcoeffs, int ncoeffs, float x) {

  /*  Arguments:
  **  pcoeffs             i:   pointer to first float of coefficients
  **  ncoeffs             i:   number of coefficients
  **  x                   i:   argument of polynomial
  */

	float result;
	int i;
	result = pcoeffs[ncoeffs-1];
	for (i=ncoeffs-2; i>=0; i--) {
	  result = (result * x) + pcoeffs[i];
	}
	return result;
}

/* N_GETTEMPFROMSPT:  Get temperature from spt file if TFB keywords aren't */
/*                    valid                                                */

float n_getTempfromSPT (NicInfo *nic) {

  /* Local variables  */
  char suffix[] = "_spt.fits";
  char filename[SZ_NAME];
  char *tempkey;

  int lenname;
  int i;
  int temp100;

  double sum;
  double temp;
  float temperature;

  FloatHdrData sptdata;

  Hdr DarkHeader;
  Hdr DataHeader;

  /* Function prototypes   */
  int n_getPriHdr (char *, Hdr *);

  temperature = 0.0;

  /* Get data file name stored in nic structure  */
  strcpy (filename, nic->filename);
  lenname = strlen(filename);

  /* Decide which temperature keyword to get */
  if (n_getPriHdr (filename, &DataHeader)) {
    sprintf (MsgText,
	     "Unable to get primary header for %s",
	     filename);
    n_error (MsgText);
  }
  /* Get keyword from _spt file */
  /* Get input file name prefix */
  strcpy (filename, nic->prefix);

  /* Now put _spt.fits on the end               */
  strcat (filename, suffix);
	
  if (n_getPriHdr (nic->DARK.ref.name, &DarkHeader))
    return status;

  tempkey = (char *)calloc(SZ_NAME, sizeof(char));

  if (getKeyS (&DarkHeader, "TEMPKEY", tempkey)) {
    sprintf (MsgText,
	     "TEMPKEY keyword not found in Dark Reference file header");
    n_error (MsgText);
    freeHdr (&DarkHeader);
    return (0.0);
  } else {
    sprintf (MsgText, "Temperature key %s used", tempkey);
    n_message (MsgText);
  }

  freeHdr (&DarkHeader);

  /* Get the temperature characteristic of the whole image  */
  sum = 0.0;
  for (i=0; i<nic->ngroups; i++) {
    initFloatHdrData (&sptdata);
    if (getFloatHD (filename, "UDL", i+1, &sptdata)) {
      sprintf (MsgText,
	       "Unable to get SPT data in %s, group %d", filename, i+1);
      n_error (MsgText);
      return (0.0);
    }
    if (getKeyD (&(sptdata.hdr), tempkey, &temp)) {
      sprintf (MsgText, "Unable to get %s for group %d", tempkey, i+1);
      n_error (MsgText);
      return (0.0);
    }
    sum += temp;
    freeFloatHdrData (&sptdata);
  }
  freeFloatHdrData (&sptdata);
  sum = sum / (double) nic->ngroups;
  /*  Convert to 2 decimal places  */
  sum = sum * 100.0;
  temp100 = (int) sum;
  if ((sum - (double) temp100) >= 0.5) {
    temp100++;
  }
  temperature = 0.01 * (float) temp100;
  
  sprintf (MsgText,
	   "Temperature from _spt file = %.3f\n", temperature);
  n_message (MsgText);
  
  return temperature;
}



	  
/* N_GET_DARKTEMP:  Get temperature to use in polynomial expression for  */
/*                  shading correction, saved in nic->temperature  */


float n_getDarkTemp (NicInfo *nic) {

  /* Argument:
  ** nic                 i: Nicinfo structure
  */

  float temperature;
  char filename[SZ_NAME];
  char tfbkey[SZ_NAME];
  int lenname;
  char obsdate[SZ_STRKWVAL+1];
  float tfbhigh;
  float tfblow;
  float caltemp; /* temp to use if tfbtemp invalid, only for shading*/
  Hdr PriHdr;
  Hdr DarkHeader;
  
  /* Function prototypes   */
  int n_getPriHdr (char *, Hdr *);
  float n_getTempfromSPT (NicInfo *);
              
  temperature = 0.0;
  caltemp = -99.9;
  
  /* Try TFBDONE keyword first */
  /* If that doesn't work, try TFBCALC  */
  /* This logic has been moved to n_setup to populate nic->tfbtemp */
  tfblow=0.;
  tfbhigh=0.;
     
    if(nic->tfbtemp > -1){ /*temp from bias was run*/
                
        /*check if it's within tfblow->tfbhigh */
	    if (n_getPriHdr (nic->DARK.ref.name, &DarkHeader)) { 
            sprintf(MsgText,"Unable to get primary header from dark reference file: %s",nic->DARK.ref.name);
            n_error(MsgText);
            return(status=1);

        }
        
        if(getKeyF(&DarkHeader,"TFBLOW",&tfblow)){
            sprintf(MsgText,"TFBLOW not in primary dark header,please update reference file");
            n_error(MsgText);
            return(status=1);
         }
         
        if(getKeyF(&DarkHeader,"TFBHIGH",&tfbhigh)){
            sprintf(MsgText,"TFBHIGH not in primary dark header, please update reference file");
            n_error(MsgText);
            return(status=1);
            
        }

        if (tfblow <= nic->tfbtemp && nic->tfbtemp <= tfbhigh) {
            sprintf(MsgText,"Using TFBTEMP for dark shading(%g) ",nic->tfbtemp);
            n_message(MsgText);
            return nic->tfbtemp;
        } 

        /*check to see if the TEMPSETS is in range*/
        
        if(getKeyF(&DarkHeader,"TMPSETS",&caltemp)){
            sprintf(MsgText,"Problem getting TMPSETS from primary dark header");
            n_warn(MsgText);
        } 
        
        /*if caltemp is in range then use the value of TMPSETS*/
        
        if(tfblow <= caltemp && caltemp <= tfbhigh){
            sprintf(MsgText,"Using TMPSETS value for the dark shading: %g",caltemp);
            n_message(MsgText);    
            return caltemp;
        }  
    }   
        
     /* Fall through, get temperature from SPT file ,
       this temperature will only be used for the shading
       profile, the amp and lin will use scales of 1 when spt
       temps are grabbed 
     */

    sprintf (MsgText,"Getting temperature from _spt file for the shading correction");
    n_message (MsgText);
    temperature = n_getTempfromSPT (nic);
    return temperature;

                             

}

/* DARK_INTERP: Do a linear interpolation of two dark images */

void dark_interp (SingleNicmosGroup *d1, SingleNicmosGroup *d2, 
               double etime_d1, double etime_d2, double exptime) {

/* Arguments:
**	d1		io: dark image #1; overwritten by interpolated image
**	d2	 	 i: dark image #2
**	etime_d1   	 i: exposure time of dark image #1
**	etime_d2   	 i: exposure time of dark image #2
**	exptime		 i: desired exposure time of interpolated image
*/
	/* Local variables */
	float frac;

	/* Function definitions */
	void n_aadd  (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_asub  (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_amulk (SingleNicmosGroup *, float);

	/* Compute the fractional exposure time */
	frac = (exptime - etime_d1) / (etime_d2 - etime_d1);

	/* Interpolate the images */
	n_asub  (d2, d1);
	n_amulk (d2, frac);
	n_aadd  (d1, d2);
}

/* DARK_EXTRAP: Do a linear extrapolation of a dark image */

void dark_extrap (SingleNicmosGroup *d1, double etime, double exptime) {

/* Arguments:
**	d1		io: dark image; overwritten by extrapolated image
**	etime		 i: exposure time of dark image
**	exptime		 i: desired exposure time of extrapolated image
*/
	/* Local variables */
	float frac;

	/* Function definitions */
	void n_amulk (SingleNicmosGroup *, float);

	/* Compute the fractional exposure time */
	frac = exptime / etime;

	/* Extrapolate the image */
	n_amulk (d1, frac);
}

/* N_GETPHOTDATA: Read the PHOTTAB reference table and load the
** photometry parameters appropriate for the observing mode of the
** image being calibrated. The parameters will be recorded as header
** keywords in the calibrated output file.
*/

# define NCOLS		12	/* number of columns in PHOTTAB */
# define N_FLOAT_COLS	5	/* number of columns with float data type */
# define N_FLOAT_TCOLS	4	/* number of new columns with float data type */

int n_getPhotData (NicInfo *nic, PhotData *phot) {

/* Arguments:
**	nic	io: NICMOS info structure
**	phot	 o: photometry data
*/

	/* Local variables */
	int i;				/* loop index */
	Bool found;			/* PHOTMODE found in table? */
	Bool null[N_FLOAT_COLS];	/* null column entry flags */
	int nrows;			/* number of rows in PHOTTAB */
	int col, row;			/* loop indexes */
	IRAFPointer tp;			/* PHOTTAB table pointer */
	IRAFPointer colptr[NCOLS];	/* PHOTTAB column pointers */
	char pmode[SZ_STRKWVAL+1];	/* PHOTTAB photmode entry */
	char pedigree[SZ_STRKWVAL+1];	/* Pedigree keyword value */
	float pvals[N_FLOAT_COLS];	/* photometric values */
	float tvals[N_FLOAT_TCOLS];	/* photometric zero-point values */

    Hdr PhotHeader;
    int n_floatcols,n_cols;
    
					/* PHOTTAB column names */
	char colname[NCOLS][SZ_COLNAME+1] = {
		"PHOTMODE",
		"PHOTFLAM",
		"PHOTFNU",
		"PHOTZPT",
		"PHOTPLAM",
		"PHOTBW",
		"PEDIGREE",
		"DESCRIP",
        "PHOTREFT",
        "PHOTF_C0",
        "PHOTF_C1",
        "PHOTFERR",
	};
    
    /* Function prototypes  */
	int n_getPriHdr (char *, Hdr *);


	/* Initialize values */
	pvals[0]=0; pvals[1]=0; pvals[2]=0; pvals[3]=0; pvals[4]=0;
	tvals[0]=0; tvals[1]=0; tvals[2]=0; tvals[3]=0;

    n_cols = NCOLS;

	/* Initialize flag */
	found = False;

    /* Read in temperature limits from Primary header, if this
       is an updated PHOTTAB. */
	if (n_getPriHdr (nic->PHOT.ref.name, &PhotHeader))
	  return status;

	if (getKeyF(&PhotHeader, "TFBLOW", &phot->tfblow)) {
	  sprintf (MsgText,
		   "Unable to get TFBLOW keyword from PHOTTAB.");
	  n_warn (MsgText);

      /* If we have an old style PHOTTAB without TFB dependence,
         only try to read in the original 5 columns. */
      n_floatcols = 5;
      n_cols = 8;
      
	} else {
      getKeyF(&PhotHeader, "TFBHIGH", &phot->tfbhigh);
	  sprintf (MsgText,
		   "Temperature limits for photometric zero-point correction: %.3f to %.3f", phot->tfblow,phot->tfbhigh);
	  n_message (MsgText);
	}    
    freeHdr (&PhotHeader);
    
	/* Open the PHOTTAB reference table */
	tp = c_tbtopn (nic->PHOT.ref.name, 1, 0);
	if (c_iraferr()) {
	    n_openerr (nic->PHOT.ref.name);
	    miss_file += 1;
	    return (status = 0);	/* Don't abort yet */
	}

	/* Get pointers to columns in PHOTTAB */
	for (col = 0; col < n_cols; col++) {
	     c_tbcfnd1 (tp, colname[col], &(colptr[col]));
	     if (c_iraferr() || colptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s",
			  colname[col], nic->PHOT.ref.name);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 return (status = 1);
	     }
	}

	/* Find out how many rows are in PHOTTAB */
	nrows = 0;
	nrows = c_tbpsta (tp, TBL_NROWS);
	if (nrows <= 0) {
	    sprintf (MsgText, "Invalid number of rows in %s",
		     nic->PHOT.ref.name);
	    n_error (MsgText);
	    c_tbtclo (tp);
	    return (status = 1);
	}

	/* Build the PHOTMODE string from camera and filter/grating */
	sprintf (phot->mode, "NICMOS,%1d,", nic->camera);
	strncat (phot->mode, nic->filter, strlen(nic->filter));
	strncat (phot->mode, ",DN", 3);

	for (i=0; i<strlen(phot->mode); i++)
	     phot->mode[i] = toupper(phot->mode[i]);

	/* Read each row, searching for a matching PHOTMODE */
	for (row=1; row<=nrows; row++) {

	     /* Get the PHOTMODE value in this row */
	     c_tbegtt (tp, colptr[0], row, pmode, 19);
	     if (c_iraferr()) {
		 sprintf (MsgText, "Can't read PHOTMODE in row %d in %s", row,
			  nic->PHOT.ref.name);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 return (status = 1);
	     }

	     /* Does it match the PHOTMODE of the image? */
	     if (strncmp(phot->mode, pmode, strlen(pmode)) == 0) {
	        /* Yes! Found the right row */
		found = True;
		break;
	     }
	}

	if (found == True) {
	    /* Get the photometric parameters from this row */
	    c_tbrgtr (tp, &colptr[1], pvals, null, N_FLOAT_COLS, row);
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read row %d in %s", row,
			 nic->PHOT.ref.name);
		n_error (MsgText);
		c_tbtclo (tp);
		return (status = 1);
	    }

	    /* Get the PEDIGREE and DESCRIP values from this row */
	    nic->PHOT.ref.pedigree[0] = '\0';
	    c_tbegtt (tp, colptr[6], row, nic->PHOT.ref.pedigree, SZ_STRKWVAL);
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read PEDIGREE in row %d in %s", row,
			 nic->PHOT.ref.name);
		n_error (MsgText);
		c_tbtclo (tp);
		return (status = 1);
	    }

	    nic->PHOT.ref.descrip[0] = '\0';
	    c_tbegtt (tp, colptr[7], row, nic->PHOT.ref.descrip, SZ_STRKWVAL);
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read DESCRIP in row %d in %s", row,
			 nic->PHOT.ref.name);
		n_error (MsgText);
		c_tbtclo (tp);
		return (status = 1);
	    }

	    /* Is this a DUMMY reference file entry? */
	    pedigree[0] = '\0';
	    for (i=0; i<strlen(nic->PHOT.ref.pedigree); i++)
		 pedigree[i] = toupper(nic->PHOT.ref.pedigree[i]);
	    if (strncmp (pedigree, "DUMMY", 5) == 0) {
		nic->PHOT.ref.dummy = True;
		sprintf (MsgText, "PEDIGREE=DUMMY in %s", nic->PHOT.ref.name);
		n_warn (MsgText);
		c_tbtclo (tp);
		return (status = 0);
	    } else
		nic->PHOT.ref.dummy = False;

        if (n_cols == NCOLS){
	        /* Get the photometric parameters from this row */
	        c_tbrgtr (tp, &colptr[8], tvals, null, N_FLOAT_TCOLS, row);
	        if (c_iraferr()) {
		    sprintf (MsgText, "Can't read row %d in %s", row,
			     nic->PHOT.ref.name);
		    n_error (MsgText);
		    c_tbtclo (tp);
		    return (status = 1);
	        }
        }

	} else {
	    /* Report failure to find a PHOTMODE match */
	    sprintf (MsgText, "Can't find PHOTMODE \"%s\" in %s", phot->mode,
		     nic->PHOT.ref.name);
	    n_error (MsgText);
	    c_tbtclo (tp);
	    return (status = 1);
	}

	/* Close the PHOTTAB reference file */
	c_tbtclo (tp);

	/* Save the photometry parameters in the NICMOS info structure */
	phot->flam = pvals[0];
	phot->fnu  = pvals[1];
	phot->zpt  = pvals[2];
	phot->plam = pvals[3];
	phot->bw   = pvals[4];
    
    /* If this is a PHOTTAB with zero-point terms, store them away. */
    if (n_cols == NCOLS){
        phot->reft = tvals[0];
        phot->f_c0 = tvals[1];
        phot->f_c1 = tvals[2];
        phot->ferr = tvals[3];
    }
    
	/* Successful return */
	return (status = 0);
}

/* N_GETREFIMAGE: Load the data from a reference image. */

int n_getRefImage (CalStep *step, int extver, SingleNicmosGroup *image, 
		   Bool *missing) {

/* Arguments:
**	step	io: calibration step info structure
**	extver	 i: extension version number to load
**	image	 o: reference image data
**	missing	 o: missing file flag
*/

	/* Function definitions */
	int n_ckImgSize (SingleNicmosGroup *);
	int n_getPedigree (Hdr *, CalStep *);

	/* Initialize the image data structure */
	initSingleNicmosGroup (image);

	/* Read the data */
	if (getSingleNicmosGroup (step->ref.name, extver, image))
	    status = 1;
	if (hstio_err() || status) {
	    n_filerr (step->ref.name);
	    freeSingleNicmosGroup (image);
	    *missing = True;
	    return (status = 0);	/* Don't abort yet */
	} else {
	    *missing = False;
	}

	/* Check the size of the image */
	if (n_ckImgSize (image)) {
	    freeSingleNicmosGroup (image);
	    return (status);
	}

	/* Read the reffile PEDIGREE and DESCRIP keywords */
	if (n_getPedigree (image->globalhdr, step)) {
	    freeSingleNicmosGroup (image);
	    return (status);
	}

	/* Successful return */
	return (status = 0);
}

/* N_GETPEDIGREE: Read the PEDIGREE and DESCRIP keywords
** from a reference file header.
*/

int n_getPedigree (Hdr *refhdr, CalStep *step) {

/* Arguments:
**	refhdr	 i: reference file header structure
**	step	io: calibration step info structure
*/

	/* Local variables */
	int i;				/* loop index */
	char pedigree[SZ_STRKWVAL+1];	/* pedigree keyword value */

	/* Try to read the PEDIGREE keyword */
	if (getKeyS (refhdr, "PEDIGREE", step->ref.pedigree)) {
	    n_kwerr ("PEDIGREE", step->ref.name);
	    return (status = 1);
	}

	/* Is this a DUMMY reference file? */
	pedigree[0] = '\0';
	for (i = 0; i < strlen(step->ref.pedigree); i++)
	     pedigree[i] = toupper(step->ref.pedigree[i]);
	if (strncmp (pedigree, "DUMMY", 5) == 0) {
	    step->ref.dummy = True;
	    sprintf (MsgText, "PEDIGREE=DUMMY in %s", step->ref.name);
	    n_warn (MsgText);
	} else
	    step->ref.dummy = False;

	/* Try to read the DESCRIP keyword */
	if (getKeyS (refhdr, "DESCRIP",  step->ref.descrip))
	    status = 0;   /* not an error since it's not a required keyword */

	/* Successful return */
	return (status);
}

/* N_INITREFDATA: Initialize all reference data structures. */

void n_initRefData (NicInfo *nic, SingleNicmosGroup *zoff,
	SingleNicmosGroup *mask, SingleNicmosGroup *nois, NlinData *nlin,
	SingleNicmosGroup *dark, SingleNicmosGroup *flat, PhotData *phot) {

/* Arguments:
**	nic	io: NICMOS info structure
**	zoff	o: M-ACCUM zero-read image structure
**	mask	o: Bad pixel mask image structure
**	nois	o: Detector read-noise image structure
**	nlin	o: Non-linearity coefficients data structure
**	dark	o: Dark current image structure
**	flat	o: Flat field image structure
**	phot	o: Photometry data structure
*/

	/* Initialize single-group structures */
	initSingleNicmosGroup (zoff);
	initSingleNicmosGroup (mask);
	initSingleNicmosGroup (nois);
	initSingleNicmosGroup (dark);
	initSingleNicmosGroup (flat);

	/* Initialize non-linearity data structure */
	nlin->globalhdr = NULL;
	nlin->coeff     = NULL;
	nlin->error     = NULL;
	nlin->dqual     = NULL;
	nlin->nodes     = NULL;
	nlin->zsci	= NULL;
	nlin->zerr	= NULL;

	nic->ndarks   = 0;

	/* Initialize photometry data structure */
	phot->mode[0] = '\0';
	phot->flam    = 0;
	phot->fnu     = 0;
	phot->zpt     = 0;
	phot->plam    = 0;
	phot->bw      = 0;
    
    phot->reft    = 0;
    phot->f_c0    = 1;
    phot->f_c1    = 0;
    phot->ferr    = -1;
    phot->tfblow  = -1;
    phot->tfbhigh = -1;
    phot->zpscale = 1;
}

/* N_FREEREFDATA: Free all reference data structures and memory. */

void n_freeRefData (SingleNicmosGroup *zoff, SingleNicmosGroup *mask,
		    SingleNicmosGroup *nois, NlinData *nlin,
		    SingleNicmosGroup *dark, SingleNicmosGroup *flat) {

/* Arguments:
**	zoff	o: M-ACCUM zero-read image structure
**	mask	o: Bad pixel mask image structure
**	nois	o: Detector read-noise image structure
**	nlin	o: Non-linearity coefficients data structure
**	dark	o: Dark current image structure
**	flat	o: Flat field image structure
*/

	/* Function definitions */
	void freeNlinData (NlinData *);

	/* Free single-group structures */
	freeSingleNicmosGroup (zoff);
	freeSingleNicmosGroup (mask);
	freeSingleNicmosGroup (nois);
	freeSingleNicmosGroup (dark);
	freeSingleNicmosGroup (flat);

	/* Free non-linearity data structure */
	freeNlinData (nlin);

}

void freeNlinData (NlinData *nlin) {

	/* Local variables */
	int i;				/* loop index */

	/* Free non-linearity data structure */
	if (nlin->globalhdr != NULL)
	    free (nlin->globalhdr);
	if (nlin->coeff != NULL) {
	    for (i=0; i<3; i++)
		 freeFloatHdrData (&nlin->coeff[i]);
	    free (nlin->coeff);
	}
	if (nlin->error != NULL) {
	    for (i=0; i<6; i++)
		 freeFloatHdrData (&nlin->error[i]);
	    free (nlin->error);
	}
	if (nlin->dqual != NULL) {
	    freeShortHdrData   (&nlin->dqual[0]);
	    free (nlin->dqual);
	}
	if (nlin->nodes != NULL) {
	    freeFloatHdrData (&nlin->nodes[0]);
	    freeFloatHdrData (&nlin->nodes[1]);
	    free (nlin->nodes);
	}
	if (nlin->zsci  != NULL) {
	    freeFloatHdrData (&nlin->zsci[0]);
	    free (nlin->zsci);
	}
	if (nlin->zerr  != NULL) {
	    freeFloatHdrData (&nlin->zerr[0]);
	    free (nlin->zerr);
	}
}

void checkKeyI (char *filename, Hdr *header, char *keyword, int scival,
	       int severity) {

	/* Local variables */
	int keyval;

	keyval = 0;

	/* Read the keyword from the ref file */
	if (getKeyI (header, keyword, &keyval)) {
	    n_kwerr (keyword, filename);
	    miss_kwd += 1;
	    status = 0;
	    return;
	}

	/* Does it match the science image value? */
	if (keyval != scival) {
	    sprintf (MsgText, "%s %s=%d does not match science data",
		     filename, keyword, keyval);
	    if (severity == FATAL) {
		nmisses += 1;
		n_error (MsgText);
	    } else
		n_warn  (MsgText);
	}
}

void checkKeyS (char *filename, Hdr *header, char *keyword, char *scival,
	       int severity) {

	/* Local variables */
	char keyval[SZ_STRKWVAL+1];

	keyval[0] = '\0';

	/* Read the keyword from the ref file */
	if (getKeyS (header, keyword, keyval)) {
	    n_kwerr (keyword, filename);
	    miss_kwd += 1;
	    status = 0;
	    return;
	}

	/* Does it match the science image value? */
	if (strncmp (keyval, scival, strlen(scival)) != 0) {
	    sprintf (MsgText, "%s %s=\"%s\" does not match science data",
		     filename, keyword, keyval);
	    if (severity == FATAL) {
		nmisses += 1;
		n_error (MsgText);
	    } else
		n_warn  (MsgText);
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


