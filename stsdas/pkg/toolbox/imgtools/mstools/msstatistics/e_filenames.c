# include <stdio.h>
# include <string.h>
# include "msstat.h"


/*  E_FILENAMES  --  Routines that handle file names. 
 *
 *
 *   - Parses file name into root, extension, group and section.
 *   - Creates file names with appropriate extension and other appendages. 
 *     These names are used to directly access FITS extensions, GEIS groups
 *     or plain OIF images. They may also be used in the output printout.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   07 Jun 96  -  Implementation (IB)
 *   02 Jul 96  -  Explicit DQF name support (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *   18 Feb 98  -  Fixed handling of plain FITS files (IB)
 *
 */




/*  Parses file name into root, extension, group and section. */

int e_parseName (char *name, char *root, char *ext, int *groupSpec, 
                 char *section) {

	char   *bracket, *colon, *dot;

	strcpy (root, name);
	*groupSpec = 0;
	strcpy (section, "");

	/* First, look for the presence of group and/or section. */
	if (root[strlen(root)-1] == ']') {

	    /* There is a bracket construct, look for the presence 
             * of a colon. That will define it as a section spec. 
             */
	    if ((colon = strrchr (root, ':')) != NULL) {

	        /* It is a section spec. Extract and store. */
	        if ((bracket = strrchr (root, '[')) != NULL) {
	            strcpy (section, bracket);
	            *bracket = '\0'; /* throws away the bracket construct. */
	        } else
	            return (1);
	    } else {

	        /* Bracket with no colon: this is a group spec. */
	        if ((bracket = strrchr (root, '[')) != NULL) {
	            root[strlen(root)-1] = '\0';
	            *groupSpec = atoi (bracket+1);
	            *bracket = '\0'; /* throws away the bracket construct. */
	        } else
	            return (1);
	    }

	    /* Now there might be a second bracket construct when there
             * are both group and section specifiers. 
             */
	    if (root[strlen(root)-1] == ']') {

	        /* This is a group spec. */
	        if ((bracket = strrchr (root, '[')) != NULL) {
	            root[strlen(root)-1] = '\0';
	            *groupSpec = atoi (bracket+1);
	            *bracket = '\0'; /* throws away the bracket construct. */
	        } else
	            return (1);
	    }
	}

	/* Now extract extension. Invalid extensions are ignored and do
         * not generate a parse error. They may generate an IRAF error. 
         */
	if ((dot = strrchr (root, '.')) == NULL)
	    strcpy (ext, "");
	else if ( (strcmp (dot, ".imh")  == 0) ||
	          (strcmp (dot, ".hhh")  == 0) ||
	          (strcmp (dot, ".fit")  == 0) ||
	          (strcmp (dot, ".fits") == 0) ||
	          (strcmp (dot, ".pl")   == 0) ||
	          (strcmp (dot+3, "h")   == 0) ||
	          (strcmp (dot+3, "f")   == 0)) {
	    strcpy (ext, dot);
	    *dot = '\0';  /* throws away extension from root string. */
	} else
	    strcpy (ext, "");

	return (0);
}




/*  Creates file names with appropriate extension, group and section.
 *  These names are used to directly access FITS extensions or GEIS groups.
 */


void e_buildNames (Control *con, HDUType type, char *name) {

	char     str[SZ_STR];

	strcpy (name, con->filename);

	switch (con->ftype) {

	case NICMOS:
	case STIS:
    case ACS:
    case COS:
    case WFC3IR:
    case WFC3UV:
	    strcat (name, con->inputExten);
	    switch (type) {
	        case PRIMARY: strcat (name, "[0]"); break;
	        case SCIENCE: strcat (name, "[SCI"); break;
	        case DQ:      strcat (name, "[DQ"); break;
	        case ERROR:   strcat (name, "[ERR"); break;
	        case TIME:    strcat (name, "[TIME"); break;
	        case SAMPLE:  strcat (name, "[SAMP"); break;
	    }
	    if (type != PRIMARY) {
	        sprintf (str, ",%d]", con->group);
	        strcat (name, str);
	    }
	    break;

	case GEIS:
	case OIF:
	    if (type == DQ) {
	        strcpy (name, con->dqfname);
	        strcat (name, con->dqfExten);
	    } else if (type == SCIENCE) {
	        strcat (name, con->inputExten);
	        if (con->ftype == GEIS) {
	            sprintf (str, "[%d]", con->group);
	            strcat (name, str);
	        }
	    } else {
	        strcpy (name, "");
	        return;
	    }
	    break;

	case FITS:
	    if (type == SCIENCE) {
	        strcat (name, con->inputExten);
	        if (con->groupSpec != 0) {
	            sprintf (str, "[%d]", con->groupSpec);
	            strcat (name, str);
	        }
	    } else {
	        strcpy (name, "");
	        return;
	    }
	    break;
	}

	strcat (name, con->section);
}

