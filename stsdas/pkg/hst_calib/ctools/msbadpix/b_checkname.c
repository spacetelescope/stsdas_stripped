# include <stdio.h>
# include <string.h>
# include "nbadpix.h"


/*  B_CHECKNAME  --  Check file name validity.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   05 Aug 96  -  Implementation (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *
 */

int b_checkInputName (char *name) {

	char   *bracket;

	/* Check only for the presence of a bracket that denotes
         * a section or FITS extension spec. This test allows brackets
         * inside pathnames, VMS-style. 
         */

	if ((bracket = strrchr (name, ']')) != NULL) {
	    if (*(bracket + 1) == '\0')
	    return (1);
	}

	return (0);
}


