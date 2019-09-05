# include <stdio.h>
# include <string.h>
# include "readnoise.h"


/*  RN_CHECKNAME  --  Check file name validity.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   31 Oct 96  -  Implementation (IB)
 *
 */

int rn_checkInputName (char *name) {

	char   *bracket;

	/* Check only for the presence of a bracket that denotes
         * a section or FITS extension spec. This test allows brackets
         * inside pathnames, VMS-style. 
         */

	if ((bracket = strrchr (name, ']')) != NULL) {
	    if (*(bracket + 1) == '\0') {
	        sprintf (MsgText, "No section/FITS extension allowed: %s", 
                         name); 
	        rn_error (MsgText);
	        return (1);
	    }
	}

	return (0);
}


