# include <stdio.h>
# include <math.h>
# include <string.h>
# include "msarith.h"

/*    Checks if proper extension exists in output file name. 
      If not, sets extension to ".fits".



      Revision history:
      ---------------
      06 Feb 97  -  Implementation (IB)

*/

int n_checkOutName (arithControl *ac) {

	char  *dot;
	int  valid;

	valid = 0;

	/* Attempt to detect a dot in file name string. */
	if ((dot = strrchr (ac->outfile, '.')) != NULL) {

	    /* There is a dot, test if it is a valid extension. */
	    if (strcmp (dot, ".fit")  == 0 || strcmp (dot, ".fits") == 0)
	        valid = 1;
	}

	/* No valid extension was found. Append proper extension, 
           but only if there is enough space left on string. 
        */
	if (!valid) { 
	    if (strlen (ac->outfile) > SZ_NAME) {
 	        n_error ("Cannot append .fits extension in output name.");
	        return (1);
	    }
	    strcat (ac->outfile, ".fits");
	}
	return (0);
}


