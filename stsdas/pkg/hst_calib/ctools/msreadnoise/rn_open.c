# include <stdio.h>
# include <c_iraf.h>
# include "readnoise.h"



/*  RN_OPEN  --  Routines for opening primary header or science HDU.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   01 Nov 96  -  Implementation  (IB)
 *
 */


IRAFPointer rn_openPrimary (Image *image, char *name) {

	IRAFPointer       im;

	IRAFPointer rn_immap (Image *, char *, Bool);

	if ((im = rn_immap (image, name, False)) == (IRAFPointer)NULL) {
	    sprintf (MsgText, "Cannot open primary header in %s", name); 
	    rn_error (MsgText);
	    return ((IRAFPointer)NULL);
	} else
	    return (im);
}



IRAFPointer rn_openScience (Image *image, char *name) {

	IRAFPointer       im;

	IRAFPointer rn_immap (Image *, char *, Bool);

	if ((im = rn_immap (image, name, True)) == (IRAFPointer)NULL) {
	    sprintf (MsgText, "Cannot open science HDU in %s", name); 
	    rn_error (MsgText);
	    return ((IRAFPointer)NULL);
	} else
	    return (im);
}
