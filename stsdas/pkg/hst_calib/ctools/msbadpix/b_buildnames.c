# include <stdio.h>
# include <string.h>
# include "nbadpix.h"


/*   B_BUILDNAMES  --  Create file names with appropriate appendages. 
 *
 *
 *
 *
 *    Revision history:
 *    ---------------
 *    05 Aug 96  -  Implementation (IB)
 *    11 Nov 96  -  Added STIS support (IB)
 *
 */


void b_buildNames (Image *image, char *filename, Bool science, 
                   char *fullname) {

	strcpy (fullname, filename);

	switch (image->instrument) {

	case NICMOS:
	case STIS:
	    if (science)
	        strcat (fullname, "[SCI,1]");
	    else
	        strcat (fullname, "[0]");
	    break;

	default:
	    strcpy (fullname, "");
	    break;

	}

}



