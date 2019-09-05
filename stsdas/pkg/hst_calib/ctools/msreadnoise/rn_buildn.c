# include <stdio.h>
# include <string.h>
# include "readnoise.h"


/*   RN_BUILDNAMES  --  Create file names with appropriate appendages. 
 *
 *
 *
 *
 *    Revision history:
 *    ---------------
 *    31 Oct 96  -  Implementation (IB)
 *
 */


void rn_buildNames (Image *image, char *filename, Bool science, 
                   char *fullname) {

	switch (image->instrument) {

	case NICMOS:
	case STIS:
	    if (science)
	        sprintf (fullname , "%s[SCI,%d]", filename, image->extver);
	    else
	        sprintf (fullname , "%s[0]", filename);
	    break;

	default:
	    strcpy (fullname, "");
	    break;

	}

}



