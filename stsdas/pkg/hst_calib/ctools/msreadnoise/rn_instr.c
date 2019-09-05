# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include <ximio.h>
# include "readnoise.h"


/*  RN_INSTRUMENT  --  Detect instrument type.
 *
 *  This is done now by looking into the INSTRUME keyword.
 *  Since this routine is called obviously *before* the instrument
 *  type is know, it cannot use the standard "encapsulated" I/O
 *  approach used elsewhere in this task. To keep generality the
 *  consulted image is closed before exiting, with no regard to
 *  the fact that the same image will have to be opened again
 *  during task execution.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   31 Oct 96  -  Implementation  (IB)
 *
 */

int rn_instrument (IRAFPointer list, Image *image) {

	char      filename[SZ_NAME];
	char            keyw[SZ_KW];
	IRAFPointer              im;

	int rn_checkInputName (char *);

	/* Read and rewind list. */
	c_imtgetim (list, filename, SZ_NAME);
	c_imtrew (list);

	/* Check file name validity. */
	if (rn_checkInputName (filename)) {
	    sprintf (MsgText, "Invalid name: %s", filename); 
	    rn_error (MsgText);
	    return (1);
	}

	/* Open primary header. Notice that we cannot do this
         * in a transparent way because the FITS inheritance
         * mechanism may be turned off and the plain file name
         * with no "[0]" suffix will not access the primary
         * header but the first extension instead. 
         */
	strcat (filename, "[0]");
	if (c_ximaccess (filename, IRAF_READ_ONLY)) {
	    im = c_immap (filename, IRAF_READ_ONLY, (IRAFPointer)0);
	    if (c_iraferr()) {
	        rn_IRAFerror();
	        sprintf (MsgText, "Cannot open file %s", filename); 
	        rn_error (MsgText);
	        return (1);
	    }
	} else {
	    sprintf (MsgText, "Cannot access file %s", filename); 
	    rn_error (MsgText);
	    return (1);
	}

	/* Look for INSTRUME keyword. */
	if (c_imaccf (im, "INSTRUME")) {
	    c_imgstr (im, "INSTRUME", keyw, SZ_KW);
	    if (strcmp (keyw, "NICMOS") == 0) 
	        image->instrument = NICMOS;
	    else if (strcmp (keyw, "STIS") == 0)
	        image->instrument = STIS;
	    else {
	        rn_error ("Non-supported instrument.");
	        c_imunmap (im);
	        return (1);
	    }
	} else {
	    rn_error ("No INSTRUME keyword.");
	    c_imunmap (im);
	    return (1);
	}

	/* Close primary header. */
	c_imunmap (im);
	return (0);
}
