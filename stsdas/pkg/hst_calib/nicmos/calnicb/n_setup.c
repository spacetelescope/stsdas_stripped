# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS parameters */
# include "calnicb.h"	/* defines CALNICB parameters */

/* N_SETUP: Initializes and retrieves all information
** necessary for CALNICB processing of NICMOS data, e.g. header
** keywords containing calibration switches, reference file
** names, and observation-specific flags and indicators.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	Oct 1996	Build 2: modified n_getWCS to use new WCS
**				structure
** H.Bushouse	30-Jan-1997	Modified n_getGlobalInfo to read NUMITER
**				keyword; modified n_inventory so that it no
**				longer computes asninfo.nexp; modified
**				n_printinfo to print asninfo.niter; modified
**				n_getGlobalInfo to eliminate reading PATT_OFF,
**				PORIENT, DITHSIZE, CHOPSIZE keywords; modified
**				n_printinfo to longer print these keywords;
**				(Version 2.0)
** H.Bushouse	27-Feb-1997	Modified n_inventory to set "docombine" flag if
**				any pattern positions have multiple images
**				(Version 2.0)
** H.Bushouse	24-Mar-1997	Modified n_checkGlobalInfo to turn off ILLMCORR
**				for Darks; fixed pattern=none bug in n_inventory
**				(Version 0.1.5)
** H.Bushouse	10-Apr-1997	Modified n_inventory to fix bug in assignments
**				of images to mosaics for 4-CHOP and 8-CHOP
**				patterns (Version 0.1.5)
** H.Bushouse	29-Apr-1997	Modified n_getGlobalInfo to read IMAGETYP
**				keyword; modified n_checkGlobalInfo to use
**				imagetype to set obs_type and turn off ILLMCORR
**				for non-external images; changed
**				MemberInfo.imgtype to mtype in n_printinfo
**				(Version 2.0)
** H.Bushouse	06-May-1997	Changed single quotes (') to double (") in all
**				output strings (Version 2.0)
** H.Bushouse	03-Dec-1997	Modified n_setup to check for conflict between
**				usemeanbkg and pattern type (Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 2.2.1)
** H.Bushouse	10-May-1999	Modified n_getGlobalInfo to read new PATTERN1
**				and P1_NPTS keywords. Modified n_checkGlobalInfo
**				for new PATTERN1 keyword values. Modified
**				n_getMemberInfo to read new PATTSTEP keyword.
**				(Version 2.3)
** H.Bushouse	27-Sep-1999	Modified n_checkGlobalInfo to read P2_NPTS
**				keyword for new pattern syntax and use it to
**				compute total number of pattern positions.
**				(Version 2.4)
** H.Bushouse	04-Apr-2000	Modified n_getGlobalInfo to read TARGNAME
**				keyword for support of Post-SAA darks. Modified
**				n_checkGlobalInfo to check value of targname 
**				for POST_SAA_DARKs. (Version 2.4)
** H.Bushouse	06-Sep-2002	Updated n_checkGlobalInfo to add "SPIRAL" and
**				"LINE" as recognized pattern names and
**				generalize the use of P2_NPTS. (Version 2.5)
** R. Jedrzejewski  3-19-03     Modified n_checkGlobalInfo to set pattern type
**                              to DITH, pattern name to SPIRALDITH if the
**                              pattern isn't recognized as a NICMOS or
**                              generic pattern (Version 2.4)
** R. Jedrzejewski  4-25-03     Put back modifications from Version 2.5 that
**				were removed in Version 2.6 (Version 2.7)
*/

int n_setup (NicInfo *nic, AsnImages *input, AsnInfo *asn) {

/* Arguments:
**	nic	io: NICMOS info structure
**	input	 i: input association member images
**	asn	io: association info structure
*/
	/* Local variables */
	int i;				/* loop index */

	/* Function definitions */
	int  n_getGlobalInfo (Hdr *, NicInfo *, AsnInfo *);
	int  n_checkGlobalInfo (Hdr *, NicInfo *, AsnInfo *);
	int  n_getMemberInfo (SingleNicmosGroup *, AsnInfo *, MemberInfo *);
	void n_inventory (AsnInfo *);
	void n_printInfo (AsnInfo *);

	/* Get global observation mode keywords from primary header 
	** of first image in the association */
	if (n_getGlobalInfo (input->member[0].globalhdr, nic, asn))
	    return (status);

	/* Check the global keywords for validity */
	if (n_checkGlobalInfo (input->member[0].globalhdr, nic, asn))
	    return (status);

	/* Get member info from keywords in each input image */
	for (i=0; i < asn->nmembers; i++) {
	     if (n_getMemberInfo (&(input->member[i]), asn, &(asn->member[i])))
		 return (status);
	}

	/* Take inventory of what we've got */
	n_inventory (asn);

	/* Print a summary of association info */
	n_printInfo (asn);

	/* Check to make sure we've got names for all output files */
	if (asn->nmembers+asn->nummos != asn->tmembers) {
	    sprintf (MsgText, "Names not in assoc. table for all output files");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Check for possible conflict between scalar bkg subtraction mode
	** and pattern type; can't subtract individual bkgs for CHOP or
	** DITHCHOP patterns, only for NONE and DITH patterns. */
	if ((asn->patt_type == CHOP || asn->patt_type == DITHCHOP) &&
	    asn->usemeanbkg == False) {
	    sprintf (MsgText,
              "Can't subtract individual bcks for CHOP or DITH-CHOP patterns;");
	    n_warn (MsgText);
	    sprintf (MsgText,
		     "Mean scalar background will be subtracted instead");
	    n_warn (MsgText);
	    asn->usemeanbkg = True;
	}
	    
	/* Successful return */
	return (status = 0);
}
 
/* N_GETGLOBALINFO: Reads observation flags and indicator keyword values 
** from primary image header.
*/

int n_getGlobalInfo (Hdr *hdr, NicInfo *nic, AsnInfo *asn) {

/* Arguments:
**	hdr	 i: header structure
**	nic	io: NICMOS info structure
**	asn	io: association info structure
*/

	/* Get the observing mode keyword values from header */
	nic->instr[0] = '\0';
	if (getKeyS (hdr, "INSTRUME", nic->instr)) {
	    n_kwerr ("INSTRUME", asn->member[0].name);
	    return (status = 1);
	}

	nic->camera = 0;
	if (getKeyI (hdr, "CAMERA", &nic->camera)) {
	    n_kwerr ("CAMERA", asn->member[0].name);
	    return (status = 1);
	}

	nic->filter[0] = '\0';
	if (getKeyS (hdr, "FILTER", nic->filter)) {
	    n_kwerr ("FILTER", asn->member[0].name);
	    return (status = 1);
	}

	nic->imagetype[0] = '\0';
	if (getKeyS (hdr, "IMAGETYP", nic->imagetype)) {
	    n_kwerr ("IMAGETYP", asn->member[0].name);
	    return (status = 1);
	}

	nic->targname[0] = '\0';
	if (getKeyS (hdr, "TARGNAME", nic->targname)) {
	    n_kwerr ("TARGNAME", asn->member[0].name);
	    return (status = 1);
	}

	/* Get the association-related keywords from the header */
	asn->niter = 0;
	if (getKeyI (hdr, "NUMITER", &asn->niter)) {
	    n_kwerr ("NUMITER", asn->member[0].name);
	    return (status = 1);
	}

	asn->newpattkeys = False;
	asn->pattern[0] = '\0';
	if (getKeyS (hdr, "PATTERN", asn->pattern)) {

	    /* Don't abort yet; check for new PATTERN1 keyword */
	    if (getKeyS (hdr, "PATTERN1", asn->pattern)) {
		n_kwerr ("PATTERN and PATTERN1", asn->member[0].name);
		return (status = 1);
	    }
	    asn->newpattkeys = True;
	}

	asn->numpos = 0;
	if (!strcmp (asn->pattern, "NONE") == 0) {
	    if (asn->newpattkeys) {
		if (getKeyI (hdr, "P1_NPTS", &asn->numpos)) {
		    n_kwerr ("P1_NPTS", asn->member[0].name);
		    return (status = 1);
		}
	    } else {
		if (getKeyI (hdr, "NUMPOS", &asn->numpos)) {
		    n_kwerr ("NUMPOS", asn->member[0].name);
		    return (status = 1);
		}
	    }
	}

	/* Successful return */
	return (status = 0);
}

/* N_CHECKGLOBALINFO: Checks flags and indicators for validity. */

int n_checkGlobalInfo (Hdr *hdr, NicInfo *nic, AsnInfo *asn) {

/* Arguments:
**	hdr	 i: header structure
**	nic	io: NICMOS info structure
**	asn	io: Association info structure
*/

	/* Local variables */
	int p2_npts;			/* P2_NPTS keyword value */
	char p2_name[SZ_STRKWVAL+1];	/* PATTERN2 keyword value */
	/* Check instrument = NICMOS */
	if (strncmp (nic->instr, "NICMOS", 6) != 0 ) {
	    sprintf (MsgText, 
		     "INSTRUME keyword value \"%s\" not valid in %s",
		     nic->instr, asn->member[0].name);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for valid camera number */
	if (nic->camera < 1 || nic->camera > 3) {
	    sprintf (MsgText, 
		     "CAMERA keyword value \"%d\" not valid in %s",
		     nic->camera, asn->member[0].name);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check for recognized IMAGETYP */
	if (strcmp (nic->imagetype, "DARK") == 0 ||
	    strcmp (nic->filter, "BLANK") == 0)
	    nic->obs_type = DARK;
	else if (strcmp (nic->imagetype, "IFLAT") == 0 ||
		 strcmp (nic->imagetype, "FLAT") == 0)
	    nic->obs_type = IFLAT;
	else if (strcmp (nic->imagetype, "EFLAT") == 0)
	    nic->obs_type = EFLAT;
	else
	    nic->obs_type = EXT;

	/* Check for Post-SAA-Dark target name */
	if (strcmp (nic->targname, "POST-SAA-DARK") == 0)
	    nic->obs_type = POST_SAA_DARK;

	/* Turn off background removal for Darks and Flats */
	if (nic->obs_type != EXT && nic->BACK.corr == PERFORM) {
	    nic->BACK.corr = OMIT;
	    sprintf (MsgText, "ILLMCORR will be OMITTED for Darks and Flats");
	    n_message (MsgText);
	}

	/* Check for recognized PATTERN name */
	if (strcmp (asn->pattern, "NONE") == 0) {
	    asn->patt_type = NONE;
	} else if (strcmp (asn->pattern, "SPIRAL") == 0 ||
		   strcmp (asn->pattern, "SPIRAL-DITH") == 0 ||
		   strcmp (asn->pattern, "NIC-SPIRAL-MAP") == 0 ||
		   strcmp (asn->pattern, "NIC-SPIRAL-DITH") == 0) {
	    asn->patt_type = DITH;
	    asn->patt_name = SPIRALDITH;
	} else if (strcmp (asn->pattern, "SQUARE-WAVE-DITH") == 0 ||
		   strcmp (asn->pattern, "NIC-SQUARE-WAVE-DITH") == 0) {
	    asn->patt_type = DITH;
	    asn->patt_name = SQUAREWAVEDITH;
	} else if (strcmp (asn->pattern, "LINE") == 0 ||
                   strcmp (asn->pattern, "XSTRIP-DITH") == 0 ||
		   strcmp (asn->pattern, "NIC-XSTRIP-DITH") == 0 ||
		   strcmp (asn->pattern, "NIC-MAP") == 0) {
	    asn->patt_type = DITH;
	    asn->patt_name = XSTRIPDITH;
	} else if (strcmp (asn->pattern, "YSTRIP-DITH") == 0 ||
		   strcmp (asn->pattern, "NIC-YSTRIP-DITH") == 0) {
	    asn->patt_type = DITH;
	    asn->patt_name = YSTRIPDITH;
	} else if (strcmp (asn->pattern, "ONE-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-ONE-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SKY-ONE-CHOP") == 0) {
	    asn->patt_type = CHOP;
	    asn->patt_name = ONECHOP;
	} else if (strcmp (asn->pattern, "TWO-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-TWO-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SKY-TWO-CHOP") == 0) {
	    asn->patt_type = CHOP;
	    asn->patt_name = TWOCHOP;
	} else if (strcmp (asn->pattern, "FOUR-CHOP") == 0) {
	    asn->patt_type = CHOP;
	    asn->patt_name = FOURCHOP;
	} else if (strcmp (asn->pattern, "EIGHT-CHOP") == 0) {
	    asn->patt_type = CHOP;
	    asn->patt_name = EIGHTCHOP;
	} else if (strcmp (asn->pattern, "SPIRAL-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SPIRAL-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SKY-SPIRAL-DITH-CHOP") == 0) {
	    asn->patt_type = DITHCHOP;
	    asn->patt_name = SPIRALDITHCHOP;
	} else if (strcmp (asn->pattern, "XSTRIP-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-XSTRIP-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SKY-XSTRIP-DITH-CHOP") == 0) {
	    asn->patt_type = DITHCHOP;
	    asn->patt_name = XSTRIPDITHCHOP;
	} else if (strcmp (asn->pattern, "YSTRIP-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-YSTRIP-DITH-CHOP") == 0 ||
		   strcmp (asn->pattern, "NIC-SKY-YSTRIP-DITH-CHOP") == 0) {
	    asn->patt_type = DITHCHOP;
	    asn->patt_name = YSTRIPDITHCHOP;
	} else {
          /* If it's not a recognized pattern, it must be an instrument
          ** specific pattern (e.g. ACS-WFC-DITHER-LINE).  In that case,
          ** just pretend it's a spiral dither.  */
	    sprintf (MsgText,
		    "PATTERN keyword value \"%s\" not recognized in %s",
		     asn->pattern, asn->member[0].name);
	    n_message (MsgText);
            sprintf (MsgText,
                     "Using PATTERN type of DITH, PATTERN name of SPIRALDITH");
            n_message (MsgText);
            asn->patt_type = DITH;
            asn->patt_name = SPIRALDITH;

/* That takes care or every possibility, so we don't need to
** return a non-zero status.  */

	}

	/* If we're using the new pattern keywords, we need to check for
	** existence of a secondary pattern and, if there is one, then
	** retrieve the value of the P2_NPTS keyword and multiply it by
	** P1_NPTS to get the total number of positions to expect in
	** the whole pattern. */

	if (asn->newpattkeys) { 

	    p2_name[0] = '\0';
            if (getKeyS (hdr, "PATTERN2", p2_name)) {
                /* Absence of this keyword is not an error */
                status = 0;

	    } else {

		p2_npts = 0;
		if (getKeyI (hdr, "P2_NPTS", &p2_npts)) {
		    n_kwerr ("P2_NPTS", asn->member[0].name);
		    return (status = 1);
		}

	        if (p2_npts) asn->numpos *= p2_npts;
	    }

	}

	/* Check for valid NUMPOS value */
	if (asn->patt_type != NONE && asn->numpos < 2) {
	    sprintf (MsgText, 
		     "NUMPOS keyword value \"%d\" not valid in %s", 
		     asn->numpos, asn->member[0].name);
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}
 
/* N_GETMEMBERINFO: Reads keyword values specific to each assoc member. */

int n_getMemberInfo (SingleNicmosGroup *image, AsnInfo *asn, MemberInfo *mem) {

/* Arguments:
**	image	i: member image
**	asn	i: association info structure
**	mem	o: member info structure
*/

	/* Function definitions */
	int n_getWCS (Hdr *, MemberInfo *);

	/* Get PATTSTEP or PATT_POS keyword value from header */
	mem->patpos = 0;
	if (!strcmp (asn->pattern, "NONE") == 0) {
	    if (asn->newpattkeys) {
		if (getKeyI (image->globalhdr, "PATTSTEP", &mem->patpos)) {
		    n_kwerr ("PATTSTEP", mem->name);
		    return (status = 1);
		}
	    } else {
		if (getKeyI (image->globalhdr, "PATT_POS", &mem->patpos)) {
		    n_kwerr ("PATT_POS", mem->name);
		    return (status = 1);
		}
	    }
	}

	/* Get the three background estimate keyword values */
	mem->backest1 = 0;
	mem->backest2 = 0;
	mem->backest3 = 0;

	if (getKeyF (image->globalhdr, "BACKEST1", &mem->backest1)) {
	    n_kwerr ("BACKEST1", mem->name);
	    return (status = 1);
	}

	if (getKeyF (image->globalhdr, "BACKEST2", &mem->backest2)) {
	    n_kwerr ("BACKEST2", mem->name);
	    return (status = 1);
	}

	if (getKeyF (image->globalhdr, "BACKEST3", &mem->backest3)) {
	    n_kwerr ("BACKEST3", mem->name);
	    return (status = 1);
	}

	/* Get all WCS info from header */
	if (n_getWCS (&(image->sci.hdr), mem))
	    return (status);

	/* Check PATT_POS for valid value */
	if (asn->patt_type != NONE &&
	   (mem->patpos < 1 || mem->patpos > asn->numpos)) {
	    sprintf (MsgText, 
		     "PATT_POS keyword value \"%d\" not valid in %s", 
		     mem->patpos, asn->member[0].name);
	    n_error (MsgText);
	    status = 1;
	}

	/* Successful return */
	return (status);
}

/* N_GETWCS: Reads all WCS keywords from SCI image header */

int n_getWCS (Hdr *hdr, MemberInfo *mem) {

/* Arguments:
**	hdr	 i: image header
**	mem	io: member info structure
*/

	/* Read the CRPIX keywords */
	mem->wcs.crpix[0] = 0;
	if (getKeyF (hdr, "CRPIX1", &mem->wcs.crpix[0])) {
	    n_kwerr ("CRPIX1", mem->name);
	    return (status = 1);
	}

	mem->wcs.crpix[1] = 0;
	if (getKeyF (hdr, "CRPIX2", &mem->wcs.crpix[1])) {
	    n_kwerr ("CRPIX2", mem->name);
	    return (status = 1);
	}

	/* Read the CRVAL keywords */
	mem->wcs.crval[0] = 0;
	if (getKeyD (hdr, "CRVAL1", &mem->wcs.crval[0])) {
	    n_kwerr ("CRVAL1", mem->name);
	    return (status = 1);
	}

	mem->wcs.crval[1] = 0;
	if (getKeyD (hdr, "CRVAL2", &mem->wcs.crval[1])) {
	    n_kwerr ("CRVAL2", mem->name);
	    return (status = 1);
	}

	/* Read the CD matrix keywords */
	mem->wcs.cd[0][0] = 0;
	if (getKeyD (hdr, "CD1_1", &mem->wcs.cd[0][0])) {
	    n_kwerr ("CD1_1", mem->name);
	    return (status = 1);
	}

	mem->wcs.cd[0][1] = 0;
	if (getKeyD (hdr, "CD1_2", &mem->wcs.cd[0][1])) {
	    n_kwerr ("CD1_2", mem->name);
	    return (status = 1);
	}

	mem->wcs.cd[1][0] = 0;
	if (getKeyD (hdr, "CD2_1", &mem->wcs.cd[1][0])) {
	    n_kwerr ("CD2_1", mem->name);
	    return (status = 1);
	}

	mem->wcs.cd[1][1] = 0;
	if (getKeyD (hdr, "CD2_2", &mem->wcs.cd[1][1])) {
	    n_kwerr ("CD2_2", mem->name);
	    return (status = 1);
	}

	/* Read the CTYPE keywords */
	mem->wcs.ctype[0][0] = '\0';
	if (getKeyS (hdr, "CTYPE1", mem->wcs.ctype[0])) {
	    n_kwerr ("CTYPE1", mem->name);
	    return (status = 1);
	}

	mem->wcs.ctype[1][0] = '\0';
	if (getKeyS (hdr, "CTYPE2", mem->wcs.ctype[1])) {
	    n_kwerr ("CTYPE2", mem->name);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_INVENTORY: Take inventory of member image information */

void n_inventory (AsnInfo *asn) {

/* Arguments:
**	asn	io: association info structure
*/

	/* Local variables */
	int i;				/* loop index */
	int patpos;			/* pattern position number */

	/* If no pattern was used, set numpos and all patpos's to 1 */
	if (asn->patt_type == NONE) {
	    asn->numpos = 1;
	    for (i = 0; i < asn->nmembers; i++)
		 asn->member[i].patpos = 1;
	}

	/* Add up the number of object and sky images we've got:
	** if there's no pattern, call them all obj;
	** if it's a DITHER pattern, call them all obj;
	** if it's a CHOP pattern, odd-numbered ones are obj, even are sky;
	** if it's a DITHER-CHOP combination, odd are obj, even are sky. */

	asn->nobj = 0;
	asn->nsky = 0;

	for (i = 0; i < asn->nmembers; i++) {
	     if (asn->patt_type == NONE) {
		 asn->member[i].type = OBJ;
		 asn->nobj++;
	     } else if (asn->patt_type == DITH) {
		 asn->member[i].type = OBJ;
		 asn->nobj++;
	     } else if (asn->patt_type == CHOP || asn->patt_type == DITHCHOP) {
		 if (asn->member[i].patpos % 2 == 0) {
		     asn->member[i].type = SKY;
		     asn->nsky++;
		 } else {
		     asn->member[i].type = OBJ;
		     asn->nobj++;
		 }
	     }
	}

	/* Figure out which images go into which mosaic and how many
	** mosaics we'll be making; since these are the only output products
	** we'll always make at least 1, even if no pattern was used;
	** For DITHER patterns all images go into one mosaic;
	** For CHOP patterns the OBJ and each SKY image gets its own mosaic;
	** For DITHER-CHOP patterns, the OBJs go into one mosaic, and the
	**     the SKYs into the other. */

	if (asn->patt_type == NONE) {
	    for (i=0; i < asn->nmembers; i++)
		 asn->member[i].mospos = 1;
	} else if (asn->patt_type == DITH) {
	    for (i=0; i < asn->nmembers; i++)
		 asn->member[i].mospos = 1;
	} else if (asn->patt_type == CHOP && asn->patt_name == ONECHOP) {
	    for (i=0; i < asn->nmembers; i++) {
		 if (asn->member[i].patpos % 2 == 0)
		     asn->member[i].mospos = 2;
		 else
		     asn->member[i].mospos = 1;
	    }
	} else if (asn->patt_type == CHOP && asn->patt_name == TWOCHOP) {
	    for (i=0; i < asn->nmembers; i++) {
		 if (asn->member[i].patpos % 4 == 0)
		     asn->member[i].mospos = 3;
		 else if (asn->member[i].patpos % 2 == 0)
		     asn->member[i].mospos = 2;
		 else
		     asn->member[i].mospos = 1;
	    }
	} else if (asn->patt_type == CHOP && asn->patt_name == FOURCHOP) {
	    for (i=0; i < asn->nmembers; i++) {
		 patpos = asn->member[i].patpos;
		 while (patpos > 8) patpos -= 8;
		 if (patpos % 8 == 0)
		     asn->member[i].mospos = 5;
		 else if (patpos % 6 == 0)
		     asn->member[i].mospos = 4;
		 else if (patpos % 4 == 0)
		     asn->member[i].mospos = 3;
		 else if (patpos % 2 == 0)
		     asn->member[i].mospos = 2;
		 else
		     asn->member[i].mospos = 1;
	    }
	} else if (asn->patt_type == CHOP && asn->patt_name == EIGHTCHOP) {
	    for (i=0; i < asn->nmembers; i++) {
		 patpos = asn->member[i].patpos;
		 while (patpos > 16) patpos -= 16;
		 if (patpos % 16 == 0)
		     asn->member[i].mospos = 9;
		 else if (patpos % 14 == 0)
		     asn->member[i].mospos = 8;
		 else if (patpos % 12 == 0)
		     asn->member[i].mospos = 7;
		 else if (patpos % 10 == 0)
		     asn->member[i].mospos = 6;
		 else if (patpos %  8 == 0)
		     asn->member[i].mospos = 5;
		 else if (patpos %  6 == 0)
		     asn->member[i].mospos = 4;
		 else if (patpos %  4 == 0)
		     asn->member[i].mospos = 3;
		 else if (patpos %  2 == 0)
		     asn->member[i].mospos = 2;
		 else
		     asn->member[i].mospos = 1;
	    }
	} else if (asn->patt_type == DITHCHOP) {
	    for (i=0; i < asn->nmembers; i++) {
		 if (asn->member[i].patpos % 2 == 0)
		     asn->member[i].mospos = 2;
		 else
		     asn->member[i].mospos = 1;
	    }
	}

	/* Find the highest mosaic number that we need to make */
	asn->nummos = 1;
	for (i=0; i < asn->nmembers; i++)
	     if (asn->member[i].mospos > asn->nummos)
		 asn->nummos = asn->member[i].mospos;

	/* Count up the number of images at each pattern position
	** and in each mosaic */
	asn->posmems = (int *)calloc(asn->numpos, sizeof(int));
	asn->mosmems = (int *)calloc(asn->nummos, sizeof(int));
	for (i=0; i < asn->nmembers; i++) {
	     asn->posmems[asn->member[i].patpos-1]++;
	     asn->mosmems[asn->member[i].mospos-1]++;
	}

	/* Do we have to combine images at a single position? */
	asn->docombine = False;
	for (i=0; i < asn->numpos; i++)
	     if (asn->posmems[i] > 1) asn->docombine = True;

}

void n_printInfo (AsnInfo *asn) {

	int i;

	sprintf (MsgText,
		 "PATTERN: %s  NUMPOS: %d  NUMITER: %d  NTARG: %d  NBCK: %d",
		 asn->pattern, asn->numpos, asn->niter, asn->nobj, asn->nsky);
	n_message (MsgText);

	for (i=0; i < asn->nmembers; i++) {
	     sprintf (MsgText,
		 "Member %3d: %s  PattPosn: %2d  Mosaic: %1d  Type: %-12s",
		  i+1, asn->member[i].name, asn->member[i].patpos,
		  asn->member[i].mospos, asn->member[i].mtype);
	     n_message (MsgText);
	}
}
