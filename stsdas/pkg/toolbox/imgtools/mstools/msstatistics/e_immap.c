# include <stdio.h>
# include <string.h>
# include "msstat.h"

/*   E_IMMAP  --  Assembles the proper file name to access the desired 
 *   HDU with a c_immap call; tests for the presence of that HDU and 
 *   either c_immap it or return a NULL pointer.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   18 Jun 96  -  Implementation (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *
 */

IRAFPointer e_immap (Control *con, HDUType type) {

	char          filename[SZ_FNAME];
	IRAFPointer                   im;

	void e_buildNames (Control *, HDUType, char *);

	e_buildNames (con, type, filename);
	if (strlen (filename) > 0) {
	    if (c_ximaccess (filename, IRAF_READ_ONLY)) {
	        im = c_immap (filename, IRAF_READ_ONLY, (IRAFPointer)0);
	        if (c_iraferr()) {
	            e_IRAFerror();
	            return ((IRAFPointer)NULL);
	        } else  {
	            return (im);
	        }
	    } else {
	        return ((IRAFPointer)NULL);
	    }
	} else  {
	    return ((IRAFPointer)NULL);
	}
}




/*  Place holder routine. */
 
void e_imunmap (IRAFPointer im) {

	c_imunmap (im);
}
