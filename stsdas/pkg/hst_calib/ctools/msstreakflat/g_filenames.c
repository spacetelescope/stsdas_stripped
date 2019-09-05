# include <stdio.h>
# include <string.h>
# include "estreak.h"



/*  G_FILENAMES  --  Routines that handle file names. These are highly
 *                   instrument-dependent, since it is basically by
 *                   manipulating file names that instrument-dependent
 *                   I/O is removed from the higher level code.
 *
 *
 *  - Parses file name into root, suffix and extension; checks its validity.
 *  - Checks validity of output file name. 
 *  - Assembles support file name.
 *  - Creates file names with appropriate suffix, group and/or extension. 
 *    These names are used to directly access FITS extensions or GEIS groups.
 *
 *
 *
 *
 *    Revision history:
 *    ---------------
 *    10 May 96  -  Implementation (IB)
 *    21 May 96  -  WFPC support (IB)
 *    07 Oct 96  -  Revised after code review (IB)
 *
 */





/*  Parses file name into root, suffix and extension; checks its validity. */

int g_checkInputName (IOControl *ioc, char *name, char *root, char *suffix, 
                      char *extension) {

	char *dot, *underline;

	/* Parse the name. */
	strcpy (root, name);

	/* Extension is mandatory, 3 or 4 characters 
         * long and delimited by '.' 
         */
	if ((dot = strrchr (root, '.')) == NULL) {
	    sprintf (ErrText, "No extension in %s", name);
	    g_warn2 (ErrText);
	    return (1);
	} else {
	    strcpy (extension, dot);
	    if (strlen (extension) != 4 &&
	        strlen (extension) != 5) {
	        sprintf (ErrText, "Invalid extension in %s", name);
	        g_warn2 (ErrText);
	        return (1);
	    }
	    *dot = '\0';  /* Throws away extension from root string. */
	}

	/* Suffix is not mandatory, but if existent must be of the 
         * form "_XYZ". Other combinations beginning with "_" are not 
         * taken to be suffixes. 
         */
	if ((underline = strrchr (root, '_')) == NULL)
	    strcpy (suffix, "");
	else {
	    strcpy (suffix, underline);
	    if (strlen (suffix) != 4) 
	        strcpy (suffix, "");
	    else
	        *underline = '\0'; /* Throws away suffix from root string. */
	}

	/* Check validity of file name extension. */
	switch (ioc->instrument) {
	case NICMOS:
	    if ((strcmp (extension,".fit" ) != 0) && 
                (strcmp (extension,".fits") != 0)) {
	        sprintf (ErrText, "Invalid extension in %s", name);
	        g_warn2 (ErrText);
	        return (1);
	    }
	    break;
	case WFPC:  
	case WFPC2:
	    if (extension[3] != 'h') {
	        sprintf (ErrText, "Invalid extension in %s", name);
	        g_warn2 (ErrText);
	        return (1);
	    }
	    break;
	}
	return (0);
}




/*  Checks validity of output file name. Only extension is checked. */

int g_checkOutput (IOControl *ioc, char *name) {

        char   *dot;
	Bool  error;

	if ((dot = strrchr (name, '.')) == NULL) {
	    g_error ("No extension in output file name.");
	    return (1);
	}
	error = False;
	switch (ioc->instrument) {
	case NICMOS:
	    if ((strcmp (dot,".fit" ) != 0) &&
                (strcmp (dot,".fits") != 0)) error = True;
	    break;
	case WFPC:
	case WFPC2:
	    if (strcmp (dot, W_OUT) != 0) error = True;
	    break;
	}

	if (error) {
            g_error ("Invalid extension in output file name.");
            return (1);
	} else 
	    return (0);
}





/*  Assembles support file name. 
 *
 *  Names have forms as:
 *
 *      WFPC:    root + suffix    + W_SPTEXT    (Ex. ef01.shh)
 *      NICMOS:  root + N_SPTSUFF + extension   (Ex. ef01_spt.fits)
 *
 */

void g_supportName (IOControl *ioc, char *root, char *support) {

	strcpy (support, root);
	switch (ioc->instrument) {
	case NICMOS:
	    strcat (support, N_SPTSUFF);
	    strcat (support, ioc->inputExten);
	    break;
	case WFPC:  
	case WFPC2:  
	    strcat (support, ioc->inputSuffix); 
	    strcat (support, W_SPTEXT);
	    break;
	}
}



/*  Creates file names with appropriate suffix, group and/or extension. 
 *  These names are used to directly access FITS extensions or GEIS groups.
 *
 *  If 'tmp' is True, temporary image names are built. Otherwise, the
 *  original input image names are built.
 *
 */

void g_buildNames (IOControl *ioc, Bool tmp, char name[][SZ_NAME+SZ_STR]) {

	int               pri,i;
	char        str[SZ_STR];
	char  suffix[SZ_SUFFIX];

	if (tmp)
	    strcpy (suffix, TMPSUFF);
	else
	    strcpy (suffix, ioc->inputSuffix);

	switch (ioc->instrument) {

	case NICMOS:
	    strcpy (name[SHDR], ioc->image[ioc->current].filename);
	    strcat (name[SHDR], suffix);
	    strcat (name[SHDR], ioc->inputExten);
	    strcat (name[SHDR], "[0]");
	    /* This loop assumes header is extension 0. */
	    for (i = 1; i < MAX_ARRAYS; i++) {
	        strcpy (name[i], ioc->image[ioc->current].filename);
	        strcat (name[i], suffix);
	        strcat (name[i], ioc->inputExten);
	        strcat (name[i], "[");
	        switch (i) {
	            case SSCI: strcat (name[i], SCIEXTNAME); break;
	            case SERR: strcat (name[i], ERREXTNAME); break;
	            case SDQF: strcat (name[i], DQFEXTNAME); break;
	            case SSMP: strcat (name[i], SMPEXTNAME); break;
	            case SINT: strcat (name[i], INTEXTNAME); break;
	        }
	        strcat (name[i], ",");
	        strcat (name[i], EXTVER);
	        strcat (name[i], ",INHERIT-]");
	    }
	    break;

	case WFPC:
	case WFPC2:
	    if (tmp) {
	        /* Temporary images have no DQF or group structure. */
	        strcpy (name[SSCI], ioc->image[ioc->current].filename);
	        strcat (name[SSCI], suffix);
	        strcat (name[SSCI], ioc->inputExten);
	        strcpy (name[SDQF], "");
	    } else {
	        /* Input images need group spec and DQF name. */ 
	        strcpy (name[SSCI], "");
	        strcpy (name[SSCI], ioc->image[ioc->current].filename);
	        strcat (name[SSCI], suffix);
	        strcat (name[SSCI], ioc->inputExten);
	        sprintf (str, "[%d]", ioc->group+1);
	        strcat (name[SSCI], str);
	        strcpy (name[SDQF], ioc->image[ioc->current].filename);
	        strcat (name[SDQF], suffix);
	        strcat (name[SDQF], ioc->dqfExten);
	        strcat (name[SDQF], str);
	    }
	    /* WFPC images have none of these data associated with them. */
	    strcpy (name[SHDR], "");
	    strcpy (name[SERR], "");
	    strcpy (name[SSMP], "");
	    strcpy (name[SINT], "");
	    break;

	}
}



