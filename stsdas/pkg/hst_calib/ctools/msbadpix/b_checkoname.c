# include <stdio.h>
# include <string.h>
# include "nbadpix.h"

/*    Checks if proper extension exists in output file name. 
 *    If not, appends a ".fits" extension.
 *
 *
 *
 *    Revision history:
 *    ---------------
 *    07 Feb 97  -  Implementation (IB)
 *
 */

int b_checkOutName (char *name) {

	char  *dot;
	int  valid;

	valid = 0;

	/* Attempt to detect a dot in file name string. */
	if ((dot = strrchr (name, '.')) != NULL) {

	    /* There is a dot, test if it is a valid extension. */
	    if (strcmp (dot, ".fit")  == 0 || strcmp (dot, ".fits") == 0)
	        valid = 1;
	}

	/* No valid extension was found. Append proper extension, 
           but only if there is enough space left on string. 
        */
	if (!valid) { 
	    if (strlen (name) > SZ_NAME) {
 	        b_error ("Cannot append .fits extension in output name.");
	        return (1);
	    }
	    strcat (name, ".fits");
	}
	return (0);
}


