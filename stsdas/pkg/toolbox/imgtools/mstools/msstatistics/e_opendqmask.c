# include <stdio.h>
# include <string.h>
# include "msstat.h"

/*   E_OPENDQMASK  -  Opens DQ, mask and error files.
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
 *   11 Jul 96  -  Implementation (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs (IB)
 *
 */

int e_openDQMask (Control *con) {

	char section[SZ_FNAME];

	IRAFPointer e_immap (Control *, HDUType);
	void e_imunmap (IRAFPointer);
	void e_closeDQMask (Control *);

	if (con->dqfOK) {
	    strcpy (section, con->section);
	    strcpy (con->section, "");
	    con->dqf = e_immap (con, DQ);
	    if (c_iraferr()) {
	        e_IRAFerror();
	        return (1);
	    }
	    strcpy (con->section, section);
	    if (c_imgndim (con->dqf) != 0) {
	        e_imunmap (con->dqf);
	        con->dqf = e_immap (con, DQ);
	        if (c_iraferr()) {
	            e_IRAFerror();
	            return (1);
	        }
	    }
	    if (c_imgndim (con->dqf) != 2 && c_imgndim (con->dqf) != 0) {
	        e_closeDQMask (con);
	        return (1);
	    }
	}

	if (con->errOK) {
	    strcpy (section, con->section);
	    strcpy (con->section, "");
	    con->err = e_immap (con, ERROR);
	    if (c_iraferr()) {
	        e_IRAFerror();
	        return (1);
	    }
	    strcpy (con->section, section);
	    if (c_imgndim (con->err) != 0) {
	        e_imunmap (con->err);
	        con->err = e_immap (con, ERROR);
	        if (c_iraferr()) {
	            e_IRAFerror();
	            return (1);
	        }
	    }
	    if (c_imgndim (con->err) != 2 && c_imgndim (con->err) != 0) {
	        e_closeDQMask (con);
	        return (1);
	    }
	}

	return (0);
}


/*  Closes DQ and ERR. */

void e_closeDQMask (Control *con) {

	void e_imunmap (IRAFPointer);

	if (con->dqf != (IRAFPointer)NULL) e_imunmap (con->dqf);
	if (con->err != (IRAFPointer)NULL) e_imunmap (con->err);
}
