# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include <ximio.h>
# include "nbadpix.h"

# define  NKW    4        /* Maximum # of keywords tested by this routine */


/*  B_CHECKFILES  --  Check input images.
 *
 *  The first file in the list sets the parameter values which the remaining 
 *  files will be checked against. Errors result in task abort.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   05 Aug 96  -  Implementation (IB)
 *   11 Nov 96  -  Added STIS support (IB)
 *   23 Jun 97  -  Fixed NICMOS keyword name (IB)
 *
 */


int b_checkFiles (IRAFPointer list, Image *image, Bool verbose) {

	char       filename[SZ_NAME]; /* Current file's name */
	int                   imfile; /* Current image in input list */
	int                        i;
	char          keyword[SZ_KW]; /* Current keyword */
	char ref_keyword[NKW][SZ_KW]; /* Keyword value */

	/* This supports 4 instruments with NKW keywords per instrument,
         * ordered according to the ordering defined in nbadpix.h. 
         */
	char *base_keyword[4][NKW] = 
        {{"INSTRUME", "CAMERA",   "FILTER", "OBSMODE"},
         {"INSTRUME", "TARGNAME", "OPMODE", "CCDGAIN"},
         {"",         "",         "",       ""},
         {"",         "",         "",       ""}};

	/* Functions. */
	IRAFPointer b_immap (Image *, char *, Bool);
	int b_checkInputName (char *);
	int b_instrument (IRAFPointer, Image *);

	if (verbose)
	    b_message ("Checking input images...\n");

	/* Get the instrument type. */
	if (b_instrument (list, image))
	    return (1);

	/* Set instrument-dependent stuff. */
	switch (image->instrument) {
	case NICMOS:
	    image->ngroups = 1;
	    image->xsize   = NIC_XSIZE;
	    image->ysize   = NIC_YSIZE;
	break;
	case STIS:
	    image->ngroups = 1;
	    image->xsize   = STI_XSIZE;
	    image->ysize   = STI_YSIZE;
	break;
	}

	/* Loop over all input files. */
	for (imfile = 0; imfile< c_imtlen (list); imfile++) {

	    /* Read next file name in input list. */
	    c_imtgetim (list, filename, SZ_NAME);

	    if (verbose) {
	        sprintf (MsgText, "  %s\n", filename);
	        b_message (MsgText);
	    }

	    /* Check input file name. */
	    if (b_checkInputName (filename)) {
	        sprintf (MsgText, "Invalid name: %s", filename); 
	        b_error (MsgText);
	        return (1);
	    }

	    /* Open primary header. */
	    image->im = b_immap (image, filename, False);
	    if (image->im == (IRAFPointer)NULL) {
	        sprintf (MsgText, "Cannot open primary header in %s", 
                        filename); 
	        b_error (MsgText);
	        return (1);
	    }

	    /* Check keywords in (primary) header. */
	    for (i = 0; i < NKW; i++) {

	        /* Test keyword existence. */
	        if (!c_imaccf (image->im, 
                    base_keyword[image->instrument][i])) {
	            sprintf (MsgText,"No %s keyword in %s",
                             base_keyword[image->instrument][i], filename);
	            b_error (MsgText);
	            return (1);
	        }

	        /* Found, read and compare with reference. If this is
                 * the first file in the list, store as reference value
                 * for future comparisons.
                 */
	        c_imgstr (image->im, base_keyword[image->instrument][i], 
                          keyword, SZ_KW);
	        if (imfile > 0) {
	            if (strcmp (keyword, ref_keyword[i])) {
	                sprintf (MsgText, "Invalid %s keyword in %s.", 
                                base_keyword[image->instrument][i], filename);
	                b_error (MsgText);
	                return (1);
	            }
	        } else
	            strcpy (ref_keyword[i], keyword);
	    }

	    /* Close primary header and open science HDU. */
	    c_imunmap (image->im);
	    image->im = b_immap (image, filename, True);
	    if (image->im == (IRAFPointer)NULL) {
	        sprintf (MsgText, "Cannot open science HDU in %s", filename); 
	        b_error (MsgText);
	        return (1);
	    }

	    /* Check axis sizes. */
	    if (c_imgndim (image->im) != 2) {
	        sprintf (MsgText, "Non-2D array found in %s.", filename);
	        b_error (MsgText);
	        return (1);
	    } else if ((c_imglen(image->im, 1) != image->xsize) ||
                       (c_imglen(image->im, 2) != image->ysize)) {
	        sprintf (MsgText, "Wrong array size found in %s.", 
                         filename);
	        b_error (MsgText);
	        return (1);
	    }

	    /* Close science HDU. */
	    c_imunmap (image->im);
	}

	return (0);
}
