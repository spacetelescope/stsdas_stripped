# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include <ximio.h>
# include "readnoise.h"

# define  NKW    5   /* Maximum number of keywords tested by this routine */


/*  RN_CHECKFILES  --  Check input images.
 *
 *  The first file in the list sets the parameter values which the remaining 
 *  files will be checked against. Errors result in task abort.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   31 Oct 96  -  Implementation  (IB)
 *
 */


int rn_checkFiles (IRAFPointer list1, IRAFPointer list2, Image *image, 
                   Bool verbose) {

	char       filename1[SZ_NAME]; /* Current file's name */
	char       filename2[SZ_NAME];
	int                    imfile; /* Current image in input lists */
	int                         i;
	char          keyword1[SZ_KW]; /* Current keyword from list 1 */
	char          keyword2[SZ_KW]; /* Current keyword from list 2 */
	char  ref_keyword[NKW][SZ_KW]; /* Keyword value */

	/* This supports 4 instruments with NKW keywords per instrument,
         * ordered according to the ordering defined in readnoise.h.
         */
	char *base_keyword[4][NKW] = 
        {{"INSTRUME", "CAMERA",   "FILTER", "READOUT", "NREAD"},
         {"INSTRUME", "TARGNAME", "OPMODE", "CCDGAIN", ""     },
         {"",         "",         "",       "",        ""     },
         {"",         "",         "",       "",        ""     }};

	/* Functions. */
	IRAFPointer rn_openPrimary (Image *, char *);
	IRAFPointer rn_openScience (Image *, char *);
	int rn_checkInputName (char *);
	int rn_checkTimes (Image *);
	int rn_instrument (IRAFPointer, Image *);
	Bool rn_keywExist (IRAFPointer, char *);

	if (verbose)
	    rn_message ("Checking input images...\n");

	image->nimages = 0;

	/* Get the instrument type. */
	if (rn_instrument (list1, image))
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
	for (imfile = 0; imfile< c_imtlen (list1); imfile++) {

	    /* Read next file names in input lists. */
	    c_imtgetim (list1, filename1, SZ_NAME);
	    c_imtgetim (list2, filename2, SZ_NAME);

	    if (verbose) {
	        sprintf (MsgText, "  %s  %s\n", filename1, filename2);
	        rn_message (MsgText);
	    }

	    /* Check input file names. */
	    if (rn_checkInputName (filename1)) return (1);
	    if (rn_checkInputName (filename2)) return (1);

	    /* Open primary headers. */
	    if ((image->im1 = rn_openPrimary (image, filename1)) == 
                (IRAFPointer)NULL)
	        return (1);
	    if ((image->im2 = rn_openPrimary (image, filename2)) == 
                (IRAFPointer)NULL)
	        return (1);

	    /* Check keywords in primary headers. */
	    for (i = 0; i < NKW; i++) {
	        if (strlen (base_keyword[image->instrument][i]) > 0) {

	            /* Test keyword existence. */
	            if (!rn_keywExist (image->im1, 
                        base_keyword[image->instrument][i]))
	                return (1);
	            if (!rn_keywExist (image->im2, 
                        base_keyword[image->instrument][i]))
	                return (1);

	            /* Found, read and compare with reference. If this is
                     * the first file in the list, store as reference value
                     * for future comparisons.
                     */
	            c_imgstr (image->im1, base_keyword[image->instrument][i], 
                              keyword1, SZ_KW);
	            if (imfile > 0) {
	                if (strcmp (keyword1, ref_keyword[i])) {
	                    sprintf (MsgText, "Invalid %s keyword.", 
                                     base_keyword[image->instrument][i]);
	                    rn_error (MsgText);
	                    return (1);
	                }
	            } else
	                strcpy (ref_keyword[i], keyword1);

	            /* Now read same keyword from paired  
                     * file in second list and compare.
                     */
	            c_imgstr (image->im2, base_keyword[image->instrument][i], 
                              keyword2, SZ_KW);
	            if (strcmp (keyword1, keyword2)) {
	                sprintf (MsgText, "Non-matching %s keyword.", 
                                base_keyword[image->instrument][i]);
	                rn_error (MsgText);
	                return (1);
	            }

	            /* Custom tests for specific keywords. */
	            switch (image->instrument) {
	            case NICMOS:
	                if (strcmp (base_keyword[image->instrument][i],
                            "FILTER") == 0){
	                    if ((strcmp (keyword1, "BLANK") != 0) ||
	                        (strcmp (keyword2, "BLANK") != 0) ) {
	                        rn_error ("FILTER keyword is not BLANK.");
	                        return (1);
	                    }
	                }
	                break;
	            case STIS:
	                if (strcmp (base_keyword[image->instrument][i],
                            "TARGNAME") == 0){
	                    if (!(((strcmp (keyword1, "BIAS") == 0)   &&
	                           (strcmp (keyword2, "BIAS") == 0))  ||
                                  ((strcmp (keyword1, "DARK") == 0)   &&
                                   (strcmp (keyword2, "DARK") == 0)))){
	                        rn_error (
                                "TARGNAME keyword is not BIAS nor DARK.");
	                        return (1);
	                    }
	                }
	                break;
	            }
	        }
	    }

	    /* Close primary headers and open science HDUs. */
	    c_imunmap (image->im2);
	    c_imunmap (image->im1);
	    if ((image->im1 = rn_openScience (image, filename1)) == 
                (IRAFPointer)NULL)
	        return (1);
	    if ((image->im2 = rn_openScience (image, filename2)) == 
                (IRAFPointer)NULL)
	        return (1);

	    /* Test equality of exposure times. This assumes that
             * exposure time information is in the science HDU
             * header. */
	    if (rn_checkTimes (image)) return (1);

	    /* Check dimensionality and axis sizes. */
	    if ((c_imgndim (image->im1) != 2) ||
	        (c_imgndim (image->im2) != 2)) {
	        rn_error ("Non-2D array.");
	        return (1);
	    } else if (!((c_imglen(image->im1, 1) == image->xsize) &&
                         (c_imglen(image->im1, 2) == image->ysize) &&
                         (c_imglen(image->im2, 1) == image->xsize) &&
                         (c_imglen(image->im2, 2) == image->ysize))) {
	        rn_error ("Invalid image size.");
	        return (1);
	    }

	    /* Close science HDUs. */
	    c_imunmap (image->im2);
	    c_imunmap (image->im1);

	    /* One more successful image pair. */
	    image->nimages++;
	}

	return (0);
}




/*  Tests keyword existence. */

Bool rn_keywExist (IRAFPointer im, char *keyword) {

	if (!c_imaccf (im, keyword)) {
	    sprintf (MsgText,"No %s keyword.", keyword);
	    rn_error (MsgText);
	    return (False);
	} else
	    return (True);
}



/*  Checks exposure times. */

int rn_checkTimes (Image *image) {

	char   time1[SZ_KW];
	char   time2[SZ_KW];

	switch (image->instrument) {
	case NICMOS:
	    c_imgstr (image->im1, "SAMPTIME", time1, SZ_KW);
	    c_imgstr (image->im2, "SAMPTIME", time2, SZ_KW);
	    break;
	case STIS:
	    c_imgstr (image->im1, "EXPTIME", time1, SZ_KW);
	    c_imgstr (image->im2, "EXPTIME", time2, SZ_KW);
	    break;
	}
	if (strcmp (time1, time2) != 0) {
	    rn_error ("Exposure times do not match.");
	    return (1);
	} else
	    return (0);
}

