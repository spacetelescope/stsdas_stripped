# include <ctype.h>
# include <stdio.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

# define  ZTIME	0.203	/* default zeroth read eposure time (seconds) */
# define  FATAL   1
# define  WARNING 2

/* N_SETUP: Initializes and retrieves all information
** necessary for CALNICA processing of NICMOS data, e.g. header
** keywords containing calibration switches, reference file
** names, and observation-specific flags and indicators.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	25-Apr-1997	Modified n_ckStep to check for "FNF" in ref
**				file name keyword values (Version 2.2)
** H.Bushouse	12-Jun-1997	n_checkSteps loads NOISFILE info into CRIDCALC
**				cal step structure (Version 2.3)
** H.Bushouse	02-Jul-1997	Initialize ref.dummy=False in n_ckStep
**				(Version 3.0)
** H.Bushouse	18-Aug-1997	Modified n_sanity routine to force ZOFFCORR and
**				NLINCORR to be performed together (Version 3.0)
** H.Bushouse	02-Oct-1997	n_checkSteps no longer loads NOISFILE info into
**				CRIDCALC cal step structure (Version 3.0)
** H.Bushouse	13-Feb-1998	Added call in n_checkSteps to retrieve ZSIG
**				CalStep keywords; added checks for ZSIG in
**				n_sanity; added reading	of SAMPZERO keyword in
**				n_getInfo routine, and checking its value in
**				n_checkInfo (Version 3.2)
** H.Bushouse	17-Apr-1998	Added additional check in n_sanity for use of
**				ZSIGCORR - must do ZOFF, MASK, NOIS, and DARK
**				along with ZSIG (Version 3.2)
** H.Bushouse	05-May-1998	Modified n_checkSteps, n_getSwitch, and
**				n_getIndicator to handle missing ZSIGCORR and
**				ZSIGDONE keywords with a warning, instead of
**				an error (Version 3.2)
** H.Bushouse	01-Oct-1998	Modified n_sanity to check for reasonable value
**				of samp_rej parameter (Version 3.3)
** H.Bushouse	05-Oct-1998	Added use of severity argument to n_ckStep,
**				n_getSwitch, and n_getIndicator routines and
**				removed explicit check for errors in reading the
**				ZSIG keywords from these routines. Split the
**				n_ckStep routine into n_ckStep and n_ckFile.
**				Modified n_checkSteps to use new form of
**				n_ckStep and n_ckFile (Version 3.3)
** H.Bushouse	09-Oct-1998	Modified n_checkSteps to retrieve new BARSCORR
**				step info (Version 3.3)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	14-Jan-2000	Modified n_checkSteps to retrieve new BSEQCORR
**				and PEDECORR step info. Modified n_sanity to
**				check BSEQCORR dependencies (Version 4.0)
** H.Bushouse	03-Oct-2000	Removed initialization of RefDataLoaded from
**				n_setup routine (no longer used) (Version 4.0)
** R.Jedrzeje   31-Jan-2002     Removed BSEQCORR and PEDCORR steps
** R.Jedrzeje   22-Feb-2002     Check for TEMPFILE as dark ref file, use
**                              DARKFILE if absent or set to N/A (Version 4.1)
**
** W. Hack      26-Oct-2008   Warren updated the setup and nic structure to
** M. Sosey                record a value of -1 when then TFBTEMP that was reported
**						   by CALNICA is around and valid. I added in logic
**						  to the flatfield setup to see if the TDFFILE was around
**                        and then to check if the flats were in the range of
**						 the TFBTEMP - if not, calnica will use the older static
**						 flatfield file for the calibration
**M. Sosey      20-Nov-2008  fixed a bug in the FLATFIELD loop so that when tdftemp 
**					was missing but tdffile and flatfile were both there
**					the correct flat was used and the code didn't crash
*/

int n_setup (NicInfo *nic, Hdr *header) {

/* Arguments:
**	nic	io: NICMOS info structure
**	header	 i: input raw image header
*/

	/* Function definitions */
	int n_getInfo    (Hdr *, NicInfo *);
	int n_checkInfo  (NicInfo *);
	int n_checkSteps (Hdr *, NicInfo *);

	/* Get observation mode keywords from primary header */
	if (n_getInfo (header, nic))
	    return (status);

	/* Check the mode keywords for validity */
	if (n_checkInfo (nic))
	    return (status);

	/* Read and check all info associated with cal steps */
	if (n_checkSteps (header, nic))
	    return (status);

	/* Successful return */
	return (status = 0);
}
 
/* N_GETINFO: Reads observation flags and indicator keyword values 
** from primary image header.
*/

int n_getInfo (Hdr *hdr, NicInfo *nic) {

/* Arguments:
**	hdr	i: header structure
**	nic	o: NICMOS info structure
*/

	/* Local variables */
	char keyval[SZ_STRKWVAL+1];		/* string keyword value */
    float temperature;
    char tfbkey[SZ_STRKWVAL+1];

	/* Get keyword values from header */
	nic->instr[0] = '\0';
	if (getKeyS (hdr, "INSTRUME", nic->instr)) {
	    n_kwerr ("INSTRUME", nic->filename);
	    return (status = 1);
	}

	nic->camera = 0;
	if (getKeyI (hdr, "CAMERA", &nic->camera)) {
	    n_kwerr ("CAMERA", nic->filename);
	    return (status = 1);
	}

	nic->obsmode = 0;
	if (getKeyS (hdr, "OBSMODE", keyval)) {
	    n_kwerr ("OBSMODE", nic->filename);
	    return (status = 1);
	}
    
 
	/* Decode the OBSMODE string */
	if (strncmp (keyval, "ACCUM", 5) == 0)
	    nic->obsmode = ACCUM;
	else if (strncmp (keyval, "MULTIACC", 8) == 0)
	    nic->obsmode = MULTIACCUM;
	else if (strncmp (keyval, "RAMP", 4) == 0)
	    nic->obsmode = RAMP;
	else if (strncmp (keyval, "BRIGHTOB", 8) == 0)
	    nic->obsmode = BRIGHTOBJ;
	else if (strncmp (keyval, "ACQ", 3) == 0)
	    nic->obsmode = ACQ;
	else {
	    sprintf (MsgText,
		    "OBSMODE keyword value \"%s\" not valid in %s", keyval,
		     nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	nic->filter[0] = '\0';
	if (getKeyS (hdr, "FILTER", nic->filter)) {
	    n_kwerr ("FILTER", nic->filename);
	    return (status = 1);
	}

	nic->nread = 0;
	if (getKeyI (hdr, "NREAD", &nic->nread)) {
	    n_kwerr ("NREAD", nic->filename);
	    return (status = 1);
	}

	nic->nsamp = 0;
	if (getKeyI (hdr, "NSAMP", &nic->nsamp)) {
	    n_kwerr ("NSAMP", nic->filename);
	    return (status = 1);
	}

	nic->sampseq[0] = '\0';
	if (getKeyS (hdr, "SAMP_SEQ", nic->sampseq)) {
	    n_kwerr ("SAMP_SEQ", nic->filename);
	    return (status = 1);
	}

	nic->readout[0] = '\0';
	if (getKeyS (hdr, "READOUT", nic->readout)) {
	    n_kwerr ("READOUT", nic->filename);
	    return (status = 1);
	}

	nic->adcgain = 0;
	if (getKeyF (hdr, "ADCGAIN", &nic->adcgain)) {
	    n_kwerr ("ADCGAIN", nic->filename);
	    return (status = 1);
	}

	nic->sampzero = 0;
	if (nic->obsmode == MULTIACCUM) {
	    if (getKeyD (hdr, "SAMPZERO", &nic->sampzero)) {
		sprintf (MsgText, "Keyword \"SAMPZERO\" not found in %s;",
			 nic->filename);
		n_warn  (MsgText);
		sprintf (MsgText, "Default value of %g sec will be used",ZTIME);
		n_warn  (MsgText);
		nic->sampzero = ZTIME;
	    }
	}

    /* Get temperature-from-bias value from science header */
    /* Set default to -1 to indicate no valid temperature. */
    nic->tfbtemp = -1;
    temperature = -1;
    
    if (getKeyS (hdr, "TFBDONE", tfbkey)){
       if (getKeyS (hdr, "TFBCALC", tfbkey)){
        sprintf (MsgText,
	         "Unable to get TFB keyword from primary header");
        n_message (MsgText);
        }
    } 

    if (!strcmp(tfbkey, "PERFORMED")) {
      if (getKeyF(hdr, "TFBTEMP", &temperature)) {
	    sprintf (MsgText,
		     "Unable to get temperature from TFBTEMP keyword");
	    n_error (MsgText);
      } else {
	    sprintf (MsgText,
		     "Temperature from bias = %.3f", temperature);
	    n_message (MsgText);
      }
    }
    
    nic->tfbtemp = temperature;
	nic->ampScale=1.0;
    nic->linScale=1.0;
    
    	/* Successful return */
	return (status = 0);
}

/* N_CHECKINFO: Checks observation-specific flags and indicators for validity.
** Also builds PHOTMODE keyword string from other keyword values.
*/

int n_checkInfo (NicInfo *nic) {

/* Arguments:
**	nic	io: NICMOS info structure
*/

	/* Check instrument = NICMOS */
	if (strncmp (nic->instr, "NICMOS", 6) != 0 ) {
	    sprintf (MsgText, "INSTRUME keyword value \"%s\" not valid in %s",
		     nic->instr, nic->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for valid camera number */
	if (nic->camera < 1 || nic->camera > 3) {
	    sprintf (MsgText, "CAMERA keyword value \"%d\" not valid in %s",
		     nic->camera, nic->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for valid NREAD value */
	if (nic->nread < 1) {
	    sprintf (MsgText, "NREAD keyword value \"%d\" not valid in %s", 
		     nic->nread, nic->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for valid NSAMP value */
	if (nic->nsamp < 1) {
	    sprintf (MsgText, "NSAMP keyword value \"%d\" not valid in %s", 
		     nic->nsamp, nic->filename);
	    n_error (MsgText);
	    status = 1;
	} else {
	    if (nic->obsmode == MULTIACCUM)
		nic->ngroups = nic->nsamp;
	    else if (nic->obsmode == ACQ)
		nic->ngroups = 2;
	    else
		nic->ngroups = 1;
	}

	/* Check for valid ADCGAIN value */
	if (nic->adcgain <= 0) {
	    sprintf (MsgText, "ADCGAIN keyword value \"%g\" not valid in %s",
		     nic->adcgain, nic->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for valid SAMPZERO value */
	if (nic->obsmode == MULTIACCUM && nic->sampzero <= 0.0) {
	    sprintf (MsgText, "SAMPZERO keyword value \"%g\" not valid in %s",
		     nic->sampzero, nic->filename);
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}

/* N_CHECKSTEPS: Reads switches and reference file names for each 
** calibration step.
*/

int n_checkSteps (Hdr *hdr, NicInfo *nic) {
 
/* Arguments:
**	hdr	i: header structure
**	nic	o: NICMOS info structure
*/

	/* Local variables */
	int nsteps;		/* number of cal steps to perform */
	FitsKw kw;              /* Keyword variable               */
	char keyval[SZ_STRKWVAL+1];		/* string keyword value */
    Hdr  FlatHdr; /*this is to store the info from the TDFFLAT header*/
    double thigh;
    double tlow;
    
	/* Function definitions */
	int n_ckStep (NicInfo *, Hdr *, char *, char *, CalStep *, int *, int);
	int n_ckFile (NicInfo *, Hdr *, char *, char *, CalStep *);
	void n_sanity (NicInfo *, int *);
 
	/* Initialize counter */
	nsteps = 0;
    
	keyval[0] = '\0';

	/* Get ZOFFCORR step info */
	if (n_ckStep (nic, hdr, "ZOFFCORR", "ZOFFDONE", &nic->ZOFF, &nsteps,
		      FATAL))
	    return (status);
	strcpy (nic->ZOFF.ref.name, nic->filename);

	/* Get MASKCORR step info */
	if (n_ckStep (nic, hdr, "MASKCORR", "MASKDONE", &nic->MASK, &nsteps,
		      FATAL))
	    return (status);

	/* Get BIASCORR step info */
	if (n_ckStep (nic, hdr, "BIASCORR", "BIASDONE", &nic->BIAS, &nsteps,
		      FATAL))
	    return (status);

	/* Get NOISCALC step info */
	if (n_ckStep (nic, hdr, "NOISCALC", "NOISDONE", &nic->NOIS, &nsteps,
		      FATAL))
	    return (status);

	/* Get DARKCORR step info */
	if (n_ckStep (nic, hdr, "DARKCORR", "DARKDONE", &nic->DARK, &nsteps,
		      FATAL))
	    return (status);

	/* Get NLINCORR step info */
	if (n_ckStep (nic, hdr, "NLINCORR", "NLINDONE", &nic->NLIN, &nsteps,
		      FATAL))
	    return (status);

	/* Get FLATCORR step info */
	if (n_ckStep (nic, hdr, "FLATCORR", "FLATDONE", &nic->FLAT, &nsteps,
		      FATAL))
	    return (status);

	/* Get UNITCORR step info */
	if (n_ckStep (nic, hdr, "UNITCORR", "UNITDONE", &nic->UNIT, &nsteps,
		      FATAL))
	    return (status);

	/* Get PHOTCALC step info */
	if (n_ckStep (nic, hdr, "PHOTCALC", "PHOTDONE", &nic->PHOT, &nsteps,
		      FATAL))
	    return (status);

	/* Get CRIDCALC step info */
	if (n_ckStep (nic, hdr, "CRIDCALC", "CRIDDONE", &nic->CRID, &nsteps,
		      FATAL))
	    return (status);

	/* Get BACKCALC step info */
	if (n_ckStep (nic, hdr, "BACKCALC", "BACKDONE", &nic->BACK, &nsteps,
		      FATAL))
	    return (status);

	/* Get WARNCALC step info */
	if (n_ckStep (nic, hdr, "WARNCALC", "WARNDONE", &nic->WARN, &nsteps,
		      FATAL))
	    return (status);

	/* Get ZSIGCORR step info (Version 3.2) */
	if (n_ckStep (nic, hdr, "ZSIGCORR", "ZSIGDONE", &nic->ZSIG, &nsteps,
		      WARNING)) {

	    /* If an error occured, assume it is an old file that doesn't
	    ** have the ZSIGCORR/DONE keywords, and set them to defaults;
	    ** also check for special case of ZSIGCORR present, but ZSIGDONE
	    ** missing. */
	    if (nic->ZSIG.corr != PERFORM && nic->ZSIG.corr != OMIT) {
		if (nic->obsmode == MULTIACCUM && nic->NLIN.corr == PERFORM) {
		    nic->ZSIG.corr = PERFORM;
		    nsteps++;
		    sprintf (MsgText, "Default value \"PERFORM\" will be used");
		    n_warn (MsgText);
		} else {
		    nic->ZSIG.corr = OMIT;
		    sprintf (MsgText, "Default value \"OMIT\" will be used");
		    n_warn (MsgText);
		}
	    }
	    status = 0;
	}

	/* Get BARSCORR step info */
	if (n_ckStep (nic, hdr, "BARSCORR", "BARSDONE", &nic->BARS, &nsteps,
		      WARNING)) {
	    if (nic->BARS.corr != PERFORM && nic->BARS.corr != OMIT) {
		if (nic->obsmode == MULTIACCUM) {
		    nic->BARS.corr = PERFORM;
		    nsteps++;
		    sprintf (MsgText, "Default value \"PERFORM\" will be used");
		    n_warn (MsgText);
		} else {
		    nic->BARS.corr = OMIT;
		    sprintf (MsgText, "Default value \"OMIT\" will be used");
		    n_warn (MsgText);
		}
	    }
	    status = 0;
	}

	/* Get the MASKFILE info */
	if (nic->MASK.corr == PERFORM || nic->ZSIG.corr == PERFORM) {
	    if (n_ckFile (nic, hdr, "MASKFILE", "MASKPDGR", &nic->MASK))
		return (status);
	}

	/* Get the NOISFILE info */
	if ((nic->NOIS.corr == PERFORM && nic->obsmode != RAMP) ||
	     nic->ZSIG.corr == PERFORM) {
	    if (n_ckFile (nic, hdr, "NOISFILE", "NOISPDGR", &nic->NOIS))
		return (status);
	}

	/* Get the DARK reference file info */
	if (nic->DARK.corr == PERFORM || nic->NOIS.corr == PERFORM ||
	    nic->ZSIG.corr == PERFORM ) {

	  /* Do static dark calculation by default   */
	    strcpy (keyval, "N/A");

	  /* First check whether the TEMPFILE keyword exists  */
	    kw = findKw (hdr, "TEMPFILE");

	    /* If it exists, check that it's not set to N/A  */
	    if ( kw != NotFound) {
		if (getKeyS (hdr, "TEMPFILE", keyval)) {
		  sprintf (MsgText, "Error getting TEMPFILE keyword");
		  n_error (MsgText);
		}
	    }
	    if (strcmp (keyval, "N/A")) {
	      if (n_ckFile (nic, hdr, "TEMPFILE", "DARKPDGR", &nic->DARK))
	      	return (status);
	    } else {
	      if (n_ckFile (nic, hdr, "DARKFILE", "DARKPDGR", &nic->DARK))
	      	return (status);
	    }
	}

	/* Get the NLINFILE info */
	if (nic->NLIN.corr == PERFORM || nic->ZSIG.corr == PERFORM) {
	    if (n_ckFile (nic, hdr, "NLINFILE", "NLINPDGR", &nic->NLIN))
		return (status);
	}

	/* Get the FLATFILE info 
       If the TDFFILE exists and is populated with 
       something other than N/A than use it. Note that
       this also assumes that if the TDFFILE exists AND
       the TFBTEMP is valid, that there is a flat inside the
       range covering the temperature. Hmm, I could double
       check the temps here too, or I can check them in the
       later routine, but if they fail then i have to revert
       to the flatfile or return an error to the user. It's 
       nicer for me here.....
 
    */
	if (nic->FLAT.corr == PERFORM) {
        
		/*check if TDFFILE is populated*/
		strcpy(nic->flatmeth,"STATIC"); /*assume static to start*/
        nic->flatext=1; /*this is always true for static flat*/
        nic->totFlatImages=1;  /*update to NUMEXPOS if TDF*/
        
        kw = findKw (hdr, "TDFFILE");
   	    if ( kw != NotFound) {
		    if (getKeyS (hdr, "TDFFILE", keyval)) {
		        sprintf (MsgText, "Error getting TDFFILE keyword");
		        n_error (MsgText);
		    }
            if(strcmp(keyval,"N/A") && (strlen(keyval) != 1)){
                if(nic->tfbtemp > -1){
                    /*check the primary header of the tdffile for the temperature
                      range and validate against tfbtemp to make sure there
                      is a file that's vaild in there - read in the header
                      from keyval to check the range. 
                    */
                    if (n_getPriHdr (keyval, &FlatHdr)){
	                    return (status);
                    }
                    
                    if(getKeyD(&FlatHdr,"TFBHIGH",&thigh)){
                        sprintf (MsgText, "Error getting TFBHIGH keyword from TDFFILE,please check reference file");
		                n_error (MsgText);  
                    }
                   if(getKeyD(&FlatHdr,"TFBLOW",&tlow)){
                        sprintf (MsgText, "Error getting TFBLOW keyword from TDFFILE,please check reference file");
                        n_error (MsgText);    
                    }
                        
                    if(tlow <= nic->tfbtemp && nic->tfbtemp <= thigh){    
                        strcpy(nic->flatmeth,"TEMPERATURE-DEPENDENT");
                        
                        if(getKeyI(&FlatHdr,"NUMEXPOS",&nic->totFlatImages)){
                            sprintf (MsgText, "Error getting NUMEXPOS keyword from flatfield");
							n_error (MsgText);  
                        }                           
                       if (n_ckFile (nic, hdr, "TDFFILE", "FLATPDGR", &nic->FLAT)){
		                    return (status);
                        }

                    } else {
                    	if (n_ckFile (nic, hdr, "FLATFILE", "FLATPDGR", &nic->FLAT)){
                    		return (status); 
                        }
                    }   
                } else {
                    if (n_ckFile (nic, hdr, "FLATFILE", "FLATPDGR", &nic->FLAT)){
                    return (status);
                	}
                }
                
           } else {
                if (n_ckFile (nic, hdr, "FLATFILE", "FLATPDGR", &nic->FLAT)){
                    return (status);
                }
           }
        } else {
            if (n_ckFile (nic, hdr, "FLATFILE", "FLATPDGR", &nic->FLAT)){
                return (status);
            }
        }    
    }

	/* Get the PHOTTAB info */
	if (nic->PHOT.corr == PERFORM) {
	    if (n_ckFile (nic, hdr, "PHOTTAB", "PHOTPDGR", &nic->PHOT))
		return (status);
	}

	/* Get the BACKTAB info */
	if (nic->BACK.corr == PERFORM) {
	    if (n_ckFile (nic, hdr, "BACKTAB", "BACKPDGR", &nic->BACK))
		return (status);
	}

	/* Sanity checks */
	n_sanity (nic, &nsteps);

	/* Make sure there's something to do */
	if (nsteps < 1) {
	    sprintf (MsgText, "No calibration steps to perform");
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}

/* N_SANITY: Check the various combinations of calibration switch and
** indicator settings to make sure that they make sense. Don't allow
** combinations of steps that are not appropriate.
*/

void n_sanity (NicInfo *nic, int *nsteps) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	nsteps	io: number of calibration steps to perform
*/

	/* Don't allow ZSIGCORR for non-MultiACCUM datasets */
	if (nic->ZSIG.corr == PERFORM && nic->obsmode != MULTIACCUM) {
	    sprintf (MsgText, "ZSIGCORR only appropriate for MULTIACCUM data;");
	    n_warn (MsgText);
	    sprintf (MsgText, "ZSIGCORR will be omitted.");
	    n_warn (MsgText);
	    nic->ZSIG.corr = OMIT;
	    (*nsteps)--;
	}

	/* Don't allow ZOFFCORR for non-MultiACCUM datasets */
	if (nic->ZOFF.corr == PERFORM && nic->obsmode != MULTIACCUM) {
	    sprintf (MsgText, "ZOFFCORR only appropriate for MULTIACCUM data;");
	    n_warn (MsgText);
	    sprintf (MsgText, "ZOFFCORR will be omitted.");
	    n_warn (MsgText);
	    nic->ZOFF.corr = OMIT;
	    (*nsteps)--;
	}

	/* Check for reasonable value of SAMP_REJ parameter */
	if (nic->obsmode == MULTIACCUM && nic->CRID.corr == PERFORM) {
	    if (nic->samp_rej >= nic->nsamp) {
		sprintf (MsgText,
		"SAMP_REJ greater than or equal to total number of samples;");
		n_warn (MsgText);
		sprintf (MsgText, "All samples will be rejected in CRIDCALC");
		n_warn (MsgText);
	    }
	}

	/* Can't do UNITCORR before other instrumental corrections */
	if (nic->UNIT.done == PERFORMED) {
	    if (nic->ZSIG.corr == PERFORM ||
		nic->ZOFF.corr == PERFORM ||
		nic->BIAS.corr == PERFORM ||
		nic->NOIS.corr == PERFORM ||
		nic->DARK.corr == PERFORM ||
		nic->NLIN.corr == PERFORM) {
		sprintf (MsgText,
			 "Can't perform ZSIG, ZOFF, BIAS, NOIS, DARK, or NLIN");
		n_warn (MsgText);
		sprintf (MsgText,
			 "corrections if UNITCORR performed previously");
		n_warn (MsgText);
		sprintf (MsgText, "These steps will be omitted.");
		n_warn (MsgText);
		if (nic->ZSIG.corr == PERFORM) {
		    nic->ZSIG.corr = OMIT;
		    (*nsteps)--;
		}
		if (nic->ZOFF.corr == PERFORM) {
		    nic->ZOFF.corr = OMIT;
		    (*nsteps)--;
		}
		if (nic->BIAS.corr == PERFORM) {
		    nic->BIAS.corr = OMIT;
		    (*nsteps)--;
		}
		if (nic->NOIS.corr == PERFORM) {
		    nic->NOIS.corr = OMIT;
		    (*nsteps)--;
		}
		if (nic->DARK.corr == PERFORM) {
		    nic->DARK.corr = OMIT;
		    (*nsteps)--;
		}
		if (nic->NLIN.corr == PERFORM) {
		    nic->NLIN.corr = OMIT;
		    (*nsteps)--;
		}
	    }
	}

	/* Don't allow ZSIGCORR if ZOFFCORR or DARKCORR have been done */
	if (nic->ZSIG.corr == PERFORM) {
	    if (nic->ZOFF.done == PERFORMED || nic->DARK.done == PERFORMED) {
		sprintf (MsgText, "Can't perform ZSIGCORR if ZOFF and/or DARK");
		n_warn (MsgText);
		sprintf (MsgText, "steps have already been performed;");
		n_warn (MsgText);
		sprintf (MsgText, "ZSIGCORR will be omitted.");
		n_warn (MsgText);
		nic->ZSIG.corr = OMIT;
		(*nsteps)--;
	    }
	}

	/* Check BIASCORR for required dependencies */
	if (nic->BIAS.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before BIASCORR");
		n_warn (MsgText);
		sprintf (MsgText, "BIASCORR will be omitted.");
		n_warn (MsgText);
		nic->BIAS.corr = OMIT;
		(*nsteps)--;
	    }
	}

	/* Check NOISCORR for required dependencies */
	if (nic->NOIS.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before NOISCALC");
		n_warn  (MsgText);
		sprintf (MsgText, "NOISCALC will be omitted");
		n_warn  (MsgText);
		nic->NOIS.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode != MULTIACCUM &&
		nic->BIAS.corr != PERFORM && nic->BIAS.done != PERFORMED) {
		sprintf (MsgText, "BIASCORR must be performed before NOISCALC");
		n_warn  (MsgText);
		sprintf (MsgText, "NOISCALC will be omitted");
		n_warn  (MsgText);
		nic->NOIS.corr = OMIT;
		(*nsteps)--;
	    }
	}

	/* Check DARKCORR for required dependencies */
	if (nic->DARK.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before DARKCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "DARKCORR will be omitted");
		n_warn  (MsgText);
		nic->DARK.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode != MULTIACCUM &&
		nic->BIAS.corr != PERFORM && nic->BIAS.done != PERFORMED) {
		sprintf (MsgText, "BIASCORR must be performed before DARKCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "DARKCORR will be omitted");
		n_warn  (MsgText);
		nic->DARK.corr = OMIT;
		(*nsteps)--;
	    }
	}

	/* Check NLINCORR for required dependencies */
	if (nic->NLIN.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before NLINCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "NLINCORR will be omitted");
		n_warn  (MsgText);
		nic->NLIN.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.done == PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed with NLINCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "NLINCORR will be omitted");
		n_warn  (MsgText);
		nic->NLIN.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode != MULTIACCUM &&
		nic->BIAS.corr != PERFORM && nic->BIAS.done != PERFORMED) {
		sprintf (MsgText, "BIASCORR must be performed before NLINCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "NLINCORR will be omitted");
		n_warn  (MsgText);
		nic->NLIN.corr = OMIT;
		(*nsteps)--;
	    }

	    if (nic->DARK.corr != PERFORM && nic->DARK.done != PERFORMED &&
		nic->obsmode != BRIGHTOBJ) {
		sprintf (MsgText, "DARKCORR must be performed before NLINCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "NLINCORR will be omitted");
		n_warn  (MsgText);
		nic->NLIN.corr = OMIT;
		(*nsteps)--;
	    }
	}


	/* Check FLATCORR for required dependencies */
	if (nic->FLAT.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before FLATCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "FLATCORR will be omitted");
		n_warn  (MsgText);
		nic->FLAT.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode != MULTIACCUM &&
		nic->BIAS.corr != PERFORM && nic->BIAS.done != PERFORMED) {
		sprintf (MsgText, "BIASCORR must be performed before FLATCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "FLATCORR will be omitted");
		n_warn  (MsgText);
		nic->FLAT.corr = OMIT;
		(*nsteps)--;
	    }

	    if (nic->DARK.corr != PERFORM && nic->DARK.done != PERFORMED &&
		nic->obsmode != BRIGHTOBJ) {
		sprintf (MsgText, "DARKCORR must be performed before FLATCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "FLATCORR will be omitted");
		n_warn  (MsgText);
		nic->FLAT.corr = OMIT;
		(*nsteps)--;
	    }

	    if (nic->NLIN.corr != PERFORM && nic->NLIN.done != PERFORMED) {
		sprintf (MsgText, "NLINCORR must be performed before FLATCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "FLATCORR will be omitted");
		n_warn  (MsgText);
		nic->FLAT.corr = OMIT;
		(*nsteps)--;
	    }
	}

	/* Check UNITCORR for required dependencies */
	if (nic->UNIT.corr == PERFORM) {
	    if (nic->obsmode == MULTIACCUM &&
		nic->ZOFF.corr != PERFORM && nic->ZOFF.done != PERFORMED) {
		sprintf (MsgText, "ZOFFCORR must be performed before UNITCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "UNITCORR will be omitted");
		n_warn  (MsgText);
		nic->UNIT.corr = OMIT;
		(*nsteps)--;
	    } else if (nic->obsmode != MULTIACCUM &&
		nic->BIAS.corr != PERFORM && nic->BIAS.done != PERFORMED) {
		sprintf (MsgText, "BIASCORR must be performed before UNITCORR");
		n_warn  (MsgText);
		sprintf (MsgText, "UNITCORR will be omitted");
		n_warn  (MsgText);
		nic->UNIT.corr = OMIT;
		(*nsteps)--;
	    }
	}

}

/* N_CKSTEP: Reads switch and indicator keywords for an individual
** calibration step.
*/

int n_ckStep (NicInfo *nic, Hdr *hdr, char *calSwitch, char *calIndicator,
	      CalStep *step, int *nsteps, int severity) {

/* Arguments:
**	nic		i: NICMOS info structure
**	hdr		i: header structure containing keywords
**	calSwitch	i: calibration switch keyword name
**	calIndicator	i: calibration indicator keyword name
**	step		io: calibration step info structure
**	nsteps		io: number of steps to perform
**	severity	i: severity of missing keywords (FATAL or WARNING)
*/

	/* Function definitions */
	int n_getSwitch    (NicInfo *, Hdr *, CalStep *, int);
	int n_getIndicator (NicInfo *, Hdr *, CalStep *, int);

	/* Initialize the CalStep information */
	step->corr            = -1;
	step->done            = -1;
	step->swname[0]       = '\0';
	step->indname[0]      = '\0';
	step->pdname[0]       = '\0';
	step->ref.name[0]     = '\0';
	step->ref.pedigree[0] = '\0';
	step->ref.descrip[0]  = '\0';
	step->ref.dummy       = False;

	/* Copy the keyword names into the CalStep structure */
	strcpy (step->swname,  calSwitch);
	strcpy (step->indname, calIndicator);

	/* Get the switch value from the header */
	if (n_getSwitch (nic, hdr, step, severity))
	    return (status);
 
	/* Get the indicator value from the header */
	if (n_getIndicator (nic, hdr, step, severity))
	    return (status);

	/* Accumulate number of steps to perform */
	if (step->corr == PERFORM)
	    (*nsteps)++;

	/* Issue a warning if this step has already been performed */
	if (step->corr==PERFORM && step->done==PERFORMED) {
	    sprintf (MsgText, "%s has been performed previously on this data",
		     step->swname);
	    n_warn (MsgText);
	    sprintf (MsgText, "%s will be performed again", step->swname);
	    n_warn (MsgText);
	}

        /* Successful return */
        return (status = 0);
}

/* N_CKFILE: Reads reference file name for an individual calibration step.
*/

int n_ckFile (NicInfo *nic, Hdr *hdr, char *calFile, char *calPedigree,
	      CalStep *step) {

/* Arguments:
**	nic		i: NICMOS info structure
**	hdr		i: header structure containing keywords
**	calFile		i: calibration file keyword name
**	calPedigree	i: calibration file pedigree keyword name
**	step		io: calibration step info structure
*/

	/* Copy the keyword names into the CalStep structure */
	strcpy (step->pdname,  calPedigree);

	/* Get the reference file name from header */
	if (getKeyS (hdr, calFile, step->ref.name)) {
	    n_kwerr (calFile, nic->filename);
	    return (status = 1);
	}

	/* Is the reference file name blank or set to "FNF"? */
	/* NOTE: This section is currently disabled because all
	** BACKTAB names are set to null (don't have any yet).
	** When BACKTAB are put into use, this should be enabled. */
	/*
	if (step->ref.name[0] == '\0' || step->ref.name[0] == ' ') {
	    sprintf (MsgText, "%s keyword in %s is null", calFile,
		     nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	} else if (strncmp (step->ref.name, "FNF", 3) == 0) {
	*/
	if (strncmp (step->ref.name, "FNF", 3) == 0) {
	    sprintf (MsgText, "%s keyword value \"%s\" in %s is not valid",
		     calFile, step->ref.name, nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

        /* Successful return */
        return (status = 0);

}

/* N_GETSWITCH: Read the value of a calibration switch */

int n_getSwitch (NicInfo *nic, Hdr *hdr, CalStep *step, int severity) {

/* Arguments:
**	nic	i: NICMOS info structure
**	hdr  	i: header structure
**	step	io: calibration step info structure
**	severity i: severity of missing keyword
*/

	/* Local variables */
	char keyval[SZ_STRKWVAL+1];

	/* Get the string value of the keyword */
	keyval[0] = '\0';
	if (getKeyS (hdr, step->swname, keyval)) {
	    if (severity == FATAL)
		n_kwerr (step->swname, nic->filename);
	    else
		n_kwwarn (step->swname, nic->filename);
	    return (status = 1);
	}

	/* Check the value of the switch */
	if (strncmp (keyval, "PERFORM", 7) == 0)
	    step->corr = PERFORM;
	else if (strncmp (keyval, "OMIT", 4) == 0)
	    step->corr = OMIT;
	else {
	    sprintf (MsgText, "Keyword %s has invalid value \"%s\"",
		     step->swname, keyval);
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}
 
/* N_GETINDICATOR: Read the value of a calibration indicator */

int n_getIndicator (NicInfo *nic, Hdr *hdr, CalStep *step, int severity) {

/* Arguments:
**	nic	i: NICMOS info structure
**	hdr  	i: header structure
**	step	io: calibration step info structure
**	severity i: severity of missing keyword
*/

	/* Local variables */
	char keyval[SZ_STRKWVAL+1];

	/* Get the string value of the keyword */
	keyval[0] = '\0';
	if (getKeyS (hdr, step->indname, keyval)) {
	    if (severity == FATAL)
		n_kwerr (step->indname, nic->filename);
	    else
		n_kwwarn (step->indname, nic->filename);
	    return (status = 1);
	}

	/* Check the value of the indicator */
	if (strncmp (keyval, " ", 1) == 0 || keyval[0] == '\0')
	    step->done = BLANK;
	else if (strncmp (keyval, "OMITTED", 7) == 0)
	    step->done = OMITTED;
	else if (strncmp (keyval, "PERFORMED", 9) == 0)
	    step->done = PERFORMED;
	else if (strncmp (keyval, "SKIPPED", 7) == 0)
	    step->done = SKIPPED;
	else {
	    sprintf (MsgText, "Keyword %s has invalid value \"%s\"",
		     step->indname, keyval);
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}

