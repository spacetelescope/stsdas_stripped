# include <stdio.h>
# include <math.h>
# include <string.h>
# include <c_iraf.h>
# include <ximio.h>
# include "msarith.h"


/*  N_FILEINFO  -  Detect instrument type, number of groups and 
                   NICMOS pixel units. 

    Unfortunately this information is spread among the primary header
    and the header of the SCI extension, thus this routine must open
    and close HDUs twice. 

    Since the instrument type is not known, no HSTIO high-level routines
    can be used. This version relies directly on IMIO cvos calls, but
    a later version should explore the independency from IRAF allowed
    by a more capable I/O interface.




    Revision history:
    ---------------
    12 Nov 96  -  Implementation (IB)
    14 May 97  -  If SCI,1 is not found, return silently (IB)
    24 Sep 98  -  If NEXTEND not found, set ngroups=1 if STIS (IB)
    19 Oct 99  -  Increase size of keyword string storage (IB)
    25 Mar 04  -  Add ACS support (as STIS; IB)
*/

int n_fileInfo (char *file, Instrument *inst, int *ngroups, Bool *cs) {

	char         keyw[90];
	char         dkeyw[90];
	IRAFPointer  im;
	int          err;

	int n_openXtension (char *, char *, IRAFPointer *);

	/* Open primary header. */
	if (n_openXtension (file, "[0]", &im)) {
	    sprintf (ErrText, "Cannot open file %s[0]", file);
	    n_error (ErrText);
	    return (1);
	}

	/* Look for INSTRUME keyword. */
	if (c_imaccf (im, "INSTRUME")) {
	    c_imgstr (im, "INSTRUME", keyw, 90);
	    if (strcmp (keyw, "NICMOS") == 0)
	        *inst = NICMOS;
	    else if (strcmp (keyw, "STIS") == 0)
	        *inst = STIS;
        else if (strcmp (keyw, "ACS") == 0)
            *inst = ACS;
        else if (strcmp (keyw, "COS") == 0)
            *inst = COS;
        else if (strcmp (keyw, "WFC3") == 0) {
	        c_imgstr (im, "DETECTOR", dkeyw, 90);
            if (strcmp (dkeyw, "IR") == 0)
                *inst = WFC3IR;
            else
                *inst = WFC3UV;
	    } else {
	        n_error ("Non-supported instrument.");
	        c_imunmap (im);
	        return (1);
	    }
	} else {
	    n_error ("No INSTRUME keyword.");
	    c_imunmap (im);
	    return (1);
	}

	/* Compute number of groups based on NEXTEND
           keyword and instrument type. */
	if (c_imaccf (im, "NEXTEND")) {
	    switch (*inst) {
            case WFC3IR:
	        case NICMOS: *ngroups = c_imgeti (im, "NEXTEND") / 5; break;
            case WFC3UV:
            case COS:
            case ACS:
	        case STIS:   *ngroups = c_imgeti (im, "NEXTEND") / 3; break;
	    }
	} else if (*inst == STIS) {
	        *ngroups = 1;   /* to handle some old STIS reference files */
	        sprintf (ErrText, 
                "No NEXTEND keyword, adopting 1 IMSET in file %s", file);
	        n_warn (ErrText);
	} else {
	    n_error ("No NEXTEND keyword.");
	    c_imunmap (im);
	    return (1);
	}


	/* Close primary header. */
	c_imunmap (im);

	/* Open science header to retrieve BUNIT value. It is assumed
           that EXTVER=1 exists. If the file was messed up by the user 
           and EXTVERs were renumbered, BUNIT can't be retrieved and
           is assumed to be in COUNTS units. This situation can only be
           aleviated by an I/O library function that returns the existing
           EXTVER numbers in the file.
        */
	err = n_openXtension (file, "[SCI,1]", &im);
	if (err == 2)
	    return (1);
	else if (err == 1)
	    *cs = False;
	else {

	    /* Is the input data in counts? */
	    if (c_imaccf (im, "BUNIT")) {
	        c_imgstr (im, "BUNIT", keyw, 90);
	        if (strncmp (keyw, "COUNTS/S", 8) == 0) 
	            *cs = True;
	        else
	            *cs = False;
	    } else
	        *cs = False;

	    /* Close science header. */
	    c_imunmap (im);
	}

	return (0);
}



/*  Routine to open a generic FITS extension. If the extension does not
    exist, return silently with error code set to 1. If the extension 
    exists but an open error occurs, issue an error message and return 
    with error code set to 2.
*/

int n_openXtension (char *file, char *ext, IRAFPointer *im) {

	char extension[SZ_NAME+1];

	c_imgcluster (file, extension, SZ_NAME);
	strcat (extension, ext);
	if (c_ximaccess (extension, IRAF_READ_ONLY)) {
	    *im = c_immap (extension, IRAF_READ_ONLY, (IRAFPointer)0);
	    if (c_iraferr()) {
	        sprintf (ErrText, "Cannot open file %s", extension); 
	        n_error (ErrText);
	        return (2);
	    }
	} else
	    return (1);

	return (0);
}

