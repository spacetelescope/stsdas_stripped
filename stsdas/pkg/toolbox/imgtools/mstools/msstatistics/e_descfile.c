# include <stdio.h>
# include <string.h>
# include "msstat.h"

/* File name extension types. */
enum extType_ { OIF_EXT, GEIS_EXT, FITS_EXT, NO_EXT };
typedef enum extType_ extType;


/*  E_DESCRIBEFILE  --  Sets the file type and associated information
 *                      based on file name extension and header keywords.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   07 Jun 96  -  Implementation (IB)
 *   28 Jun 96  -  Remove mandatory file name extension (IB)
 *   02 Jul 96  -  Explicit DQF name support (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   02 May 97  -  Added [0] suffix to FITS names to ensure acces to PHU (IB)
 *   18 Feb 98  -  Fixed handling of plain FITS files (IB)
 *   02 Jun 10  -  Fixed ftype assignments for WFC3UV and WFC3IR (HAB)
 *
 */

int e_describeFile (Control *con, char *dqfname) {

	IRAFPointer                 im;
	char        filename[SZ_FNAME];
	char           keyword[SZ_STR];
	char          dkeyword[SZ_STR];
	char            root[SZ_FNAME];
	char             ext[SZ_FNAME];
	char           dummy[SZ_FNAME];
	int                    nextend;
	int                     groups;
	int                          i;
	extType              extension;  /* Type of file name extension */
	Bool              groupKeyword;  /* Signal presence of */
	Bool               STISKeyword;  /* specific keywords  */
	Bool             NICMOSKeyword;  /* in primary header  */
	Bool                ACSKeyword;
	Bool                COSKeyword;
	Bool             WFC3IRKeyword;
	Bool             WFC3UVKeyword;
	Bool            nextendKeyword;

	int e_parseName (char *, char *, char *, int *, char *);
	void e_imunmap (IRAFPointer);

	nextend = 0;
	groups  = 0;

	/* Test for FITS file name extension. */
	if ((strcmp (con->inputExten, ".fit" ) == 0) || 
            (strcmp (con->inputExten, ".fits") == 0) ||
	    ( (strlen(con->inputExten) == 4  )  &&
              (con->inputExten[3]      == 'f')) )
	    extension = FITS_EXT;

	/* Test for ".??h" types. */
	else if ((strlen(con->inputExten) == 4  )  &&
                   (con->inputExten[3]      == 'h')) {
	    if ((con->inputExten[1] == 'i') && 
                (con->inputExten[2] == 'm'))
	        extension = OIF_EXT;
	    else
	        extension = GEIS_EXT;

	/* Test for no extension in file name. */
	} else if (strlen(con->inputExten) == 0)
	        extension = NO_EXT;
	else {
	    sprintf (MsgText,"Invalid extension in file name.");
	    e_error (MsgText);
	    return (1);
	}

	/* Look for certain keywords in primary header. */
	groupKeyword   = False;
	nextendKeyword = False;
	STISKeyword    = False;
	NICMOSKeyword  = False;
	ACSKeyword     = False;
	COSKeyword     = False;
	WFC3IRKeyword  = False;
	WFC3UVKeyword  = False;

	strcpy (filename, con->filename);
	strcat (filename, con->inputExten);
	if (extension == FITS_EXT)
	    strcat (filename, "[0]");
        im = c_immap (filename, IRAF_READ_ONLY, 0);
	if (c_iraferr())  {
	    e_IRAFerror();
	    return (1);
	}

	if (c_imaccf (im, "INSTRUME")) {
	    c_imgstr (im, "INSTRUME", keyword, SZ_STR);
	    if (strcmp (keyword, "NICMOS") == 0)
	        NICMOSKeyword = True;
	    if (strcmp (keyword, "STIS") == 0)
	        STISKeyword = True;
	    if (strcmp (keyword, "ACS") == 0)
	        ACSKeyword = True;
	    if (strcmp (keyword, "COS") == 0)
	        COSKeyword = True;
	    if (strcmp (keyword, "WFC3") == 0) {
    	    c_imgstr (im, "DETECTOR", dkeyword, SZ_STR);
            if (strcmp (dkeyword, "IR") == 0) {
    	        WFC3IRKeyword = True;
            } else {
                WFC3UVKeyword = True;
            }
        }
	}
	if (c_imaccf (im, "NEXTEND")) {
	    nextendKeyword = True;
	    nextend = c_imgeti (im, "NEXTEND");
	}
	if (c_imaccf (im, "GCOUNT")) {
	    groupKeyword = True;
	    groups = c_imgeti (im, "GCOUNT");
	}
	e_imunmap (im);

	/* This is the main logic for detecting file type. 
         * The NO_EXT case also handles FITS files with no name 
         * extension despite the fact that the current FITS kernel 
         * does not allow that. 
         */
	switch (extension) {
	    case OIF_EXT:  con->ftype = OIF;  break;
	    case GEIS_EXT: con->ftype = GEIS; break;
	    case FITS_EXT:
	        if (NICMOSKeyword)    con->ftype = NICMOS;
	        else if (STISKeyword) con->ftype = STIS;
	        else if (ACSKeyword)  con->ftype = ACS;
	        else if (COSKeyword)  con->ftype = COS;
	        else if (WFC3IRKeyword) con->ftype = WFC3IR;
	        else if (WFC3UVKeyword) con->ftype = WFC3UV;
	        else                  con->ftype = FITS;
	        break;
	    case NO_EXT:
	        if (groupKeyword)       con->ftype = GEIS;
	        else if (NICMOSKeyword) con->ftype = NICMOS;
	        else if (STISKeyword)   con->ftype = STIS;
	        else if (ACSKeyword)    con->ftype = ACS;
	        else if (COSKeyword)    con->ftype = COS;
	        else if (WFC3IRKeyword) con->ftype = WFC3IR;
	        else if (WFC3UVKeyword) con->ftype = WFC3UV;
	        else                    con->ftype = OIF;
	        break;
	}

	/* Set number of groups and other flags. In the STIS, ACS,
         * and NICMOS cases the number of groups is computed from the
         * NEXTEND keyword and the group size (number of FITS
         * extensions per group). 
         */
	switch (con->ftype) {
	    case OIF:    
	        con->ngroups   = 1;
	        con->errOK     = False;
	        con->groupSpec = 0;
	        break;
	    case FITS:    
	        con->ngroups   = 1;
	        con->errOK     = False;
	        break;
	    case GEIS:
	        con->ngroups = groups;
	        con->errOK   = False;
	        break;
	    case COS:
	    case WFC3UV:
	    case ACS:
	    case STIS:
	        con->ngroups   = nextend / SZ_GSTIS;
	        con->dqfOK     = True;
	        con->errOK     = True;
	        con->groupSpec = 0;
	        break;
	    case WFC3IR:
	    case NICMOS:
	        con->ngroups   = nextend / SZ_GNIC;
	        con->dqfOK     = True;
	        con->errOK     = True;
	        con->groupSpec = 0;
	        break;
	}

	/* This handles the case were a GEIS/OIF DQF was supplied through
         * an explicit input parameter. Supersedes all other DQF specs. 
         */
	if (strlen (dqfname) > 0) {
	    if (e_parseName (dqfname, root, ext, &i, dummy)) {
	        sprintf (MsgText, "Cannot parse file name %s", dqfname);
	        e_error (MsgText);
	        return (1);
	    }
	    strcpy (con->dqfname,  root);
	    strcpy (con->dqfExten, ext);
 	    con->dqfOK = True;
	} else 
	    strcpy (con->dqfname, con->filename);

	/* Test for the presence and consistency 
         * of DQFs for OIF and GEIS formats. 
         */
	if ((con->ftype == OIF)  ||
            (con->ftype == GEIS)) {

	    strcpy (filename, con->dqfname);
	    strcat (filename, con->dqfExten);
	    if (c_ximaccess (filename, IRAF_READ_ONLY)) {

	        /* Found a file with matching DQF name. */
	        con->dqfOK = True;

	        /* If GEIS format, see if # of groups is consistent. */
	        if (con->ftype == GEIS) {
	            im = c_immap (filename, IRAF_READ_ONLY, 0);
	            if (c_iraferr())  {
	                e_IRAFerror();
	                return (1);
	            }
	            if (c_imaccf (im, "GCOUNT") == 0) {
	                e_error ("No GCOUNT keyword.");
	                e_imunmap (im);
	                return (1);
	            }
	            if (c_imgeti (im, "GCOUNT") != con->ngroups) {
	                e_error ("Inconsistent # of groups in DQ file.");
	                e_imunmap (im);
	                return (1);
	            }
	            e_imunmap (im);
	        }
	    } else

	        /* No DQF was found. */
	        con->dqfOK = False;
	}

	return (0);
}
