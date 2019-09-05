# include <stdio.h>
# include <string.h>
# include "msstat.h"


/*   E_OPENHDU  -  Opens HDU.
 *
 *
 *   Compressed files in the HSTIO sense cannot be accessed with a
 *   image section spec appended to the file name. Before opening
 *   the section, the full array is opened in order to make its
 *   header available. If then its physical dimensionality is found
 *   to be 2, the array is closed and reopened again with the section
 *   spec in place.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   06 Mar 97  -  Implementation (IB)
 *   23 Feb 98  -  Fixed null pointer bug (in Digital Unix only, IB) 
 */

IRAFPointer e_openHDU (Control *con, HDUType type) {

	IRAFPointer im;
	char section[SZ_FNAME];

	IRAFPointer e_immap (Control *, HDUType);
	void e_imunmap (IRAFPointer);

	strcpy (section, con->section);
	strcpy (con->section, "");
	im = e_immap (con, type);
	if (c_iraferr()) {
	    e_IRAFerror();
	    return ((IRAFPointer)NULL);
	}
	strcpy (con->section, section);

	if (im != (IRAFPointer)NULL) {
	    if (c_imgndim (im) != 0) {
	        e_imunmap (im);
	        im = e_immap (con, type);
	        if (c_iraferr()) {
	            e_IRAFerror();
	            return ((IRAFPointer)NULL);
	        }
	    }
	} else
	    return ((IRAFPointer)NULL);
	if (im != (IRAFPointer)NULL) {
	    if (c_imgndim (im) != 2 && c_imgndim (im) != 0) {
	         e_imunmap (im);
	        return ((IRAFPointer)NULL);
	    }
	} else 
	    return ((IRAFPointer)NULL);

	return (im);
}
