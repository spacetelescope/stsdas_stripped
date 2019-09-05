# include <stdio.h>
# include <string.h>
# include "nicmos.h"

/*    N_CHECKOUTNAME: Checks if proper extension exists in output file name. 
**    If not, appends a ".fits" extension.
**
**
**
**    Revision history:
**    ---------------
**    H. Bushouse	25-Mar-1999	Implementation.
**
*/

int n_checkOutName (char *name) {

/* Arguments:
**	name	io: output file name
*/

	/* Local variables */
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
        ** but only if there is enough space left on string. */
	if (!valid) { 
	    if (strlen (name) > SZ_NAME) {
 	        n_error ("Cannot append .fits extension in output name.");
	        return (1);
	    }
	    strcat (name, ".fits");
	}
	return (0);
}

