# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS parameters */
# include "calnicb.h"	/* defines CALNICB parameters */

# define  FATAL   1
# define  WARNING 2

int miss_file;		/* missing ref file counter */
int miss_kwd;		/* missing keyword counter */
int mismatch;		/* ref data mismatch counter */

/* N_REFDATA: Contains various routines for reading reference data,
** as well as initializing and freeing the reference data structures.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	06-May-1997	Changed single quotes (') to double (") in all
**				output strings; modified n_getPedigree to split
**				dummy ref file message to 2 lines (Version 2.0)
** H.Bushouse	01-Dec-1997	Modified n_getPedigree to set the new ref.dummy
**				variable (Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 2.2.1)
*/

/* N_GETREFDATA: Loads all reference data into memory. Checks for
** missing reference files and reference file data that do not match
** the observing mode of the science data being processed. Returns with
** an error status if any ref files are missing or incompatible.
*/

int n_getRefData (NicInfo *nic, SingleNicmosGroup *back) {

/* Arguments:
**	nic	i: NICMOS info structure
**	back	o: background illumination pattern image
*/

	/* Function definitions */
	int n_getIllmImage (NicInfo *, SingleNicmosGroup *);

	/* Initialize the error counters */
	miss_file = 0;
	miss_kwd  = 0;
	mismatch  = 0;

	/* Load the background image */
	if (nic->BACK.corr == PERFORM) {
	    if (n_getIllmImage (nic, back))
		return (status);
	}

	goto ErrCheck;

ErrCheck:
	/* Error check */
	if (miss_file > 0) {
	    sprintf (MsgText, "Calibration reference file missing");
	    n_error (MsgText);
	    status = 1;
	} else if (mismatch > 0) {
	    sprintf (MsgText, "Incompatible reference data");
	    n_error (MsgText);
	    status = 1;
	} else if (miss_kwd > 0) {
	    status = 1;
	} else {
	    status = 0;
	}

	/* return */
	return (status);

}

/* N_GETILLMIMAGE: Load the background illumination pattern reference image
** and check to make sure it's the appropriate file for using with the
** science data being processed.
*/

int n_getIllmImage (NicInfo *nic, SingleNicmosGroup *back) {

/* Arguments:
**	nic	i: NICMOS info structure
**	back	o: background illumination pattern image
*/

	/* Local variables */
	Bool missing;		/* missing file flag */

	/* Function definitions */
	int n_getRefImage (CalStep *, int, SingleNicmosGroup *, Bool *);
	void checkKeyI (char *, Hdr *, char *, int, int);
	void checkKeyS (char *, Hdr *, char *, char *, int);

	/* Read the reference image */
	sprintf (MsgText, "Reading background reference image %s",
		 nic->BACK.ref.name);
	n_message (MsgText);
	if (n_getRefImage (&nic->BACK, 1, back, &missing))
	    return (status);

	/* Check for a missing file */
	if (missing) {
	    miss_file += 1;
	    return (status = 0);	/* don't abort yet */
	}


	/* Check the BACK file camera number */
	checkKeyI (nic->BACK.ref.name, back->globalhdr, "CAMERA", nic->camera,
		   FATAL);

	/* Check the BACK file filter name */
	checkKeyS (nic->BACK.ref.name, back->globalhdr, "FILTER", nic->filter,
		   FATAL);

	/* Successful return */
	return (status = 0);
}

/* N_GETREFIMAGE: Load the data from a reference image. */
 
int n_getRefImage (CalStep *step, int extver, SingleNicmosGroup *image,
                   Bool *missing) {
 
/* Arguments:
**      step    io: calibration step info structure
**      extver   i: extension version number to load
**      image    o: reference image data
**      missing  o: missing file counter
*/
 
        /* Function definitions */
        int n_ckImgSize (SingleNicmosGroup *);
        int n_getPedigree (Hdr *, CalStep *);
 
        /* Initialize the image data structure */
        initSingleNicmosGroup (image);
 
        /* Read the data */
        if (getSingleNicmosGroup (step->ref.name, extver, image))
            status = 1;
        if (hstio_err() || status) {
            n_filerr (step->ref.name);
            freeSingleNicmosGroup (image);
            *missing = True;
            return (status = 0);        /* Don't abort yet */
        } else {
	    *missing = False;
	}
 
        /* Check the size of the image */
        if (n_ckImgSize (image)) {
            freeSingleNicmosGroup (image);
            return (status);
        }
 
        /* Read the reffile PEDIGREE and DESCRIP keywords */
        if (n_getPedigree (image->globalhdr, step)) {
            freeSingleNicmosGroup (image);
            return (status);
        }
 
        /* Successful return */
        return (status = 0);
}
 
/* N_GETPEDIGREE: Read the PEDIGREE and DESCRIP keywords
** from a reference file header.
*/
 
int n_getPedigree (Hdr *refhdr, CalStep *step) {
 
/* Arguments:
**      refhdr   i: reference file header structure
**      step    io: calibration step info structure
*/
 
        /* Try to read the PEDIGREE keyword */
        if (getKeyS (refhdr, "PEDIGREE", step->ref.pedigree)) {
            n_kwerr ("PEDIGREE", step->ref.name);
            return (status = 1);
        }
 
        /* Is this a DUMMY reference file? */
        if (strncmp (step->ref.pedigree, "DUMMY", 5) == 0) {
            step->corr = SKIP;
	    step->ref.dummy = True;
            sprintf (MsgText, "PEDIGREE=DUMMY in %s;", step->ref.name);
	    n_warn (MsgText);
	    sprintf (MsgText, "%s will be skipped", step->swname);
            n_warn (MsgText);
        } else
	    step->ref.dummy = False;
 
        /* Try to read the DESCRIP keyword */
        if (getKeyS (refhdr, "DESCRIP",  step->ref.descrip))
            status = 0;   /* not an error since it's not a required keyword */
 
        /* Successful return */
        return (status);
}
 
/* N_INITREFDATA: Initialize all reference data structures. */

void n_initRefData (SingleNicmosGroup *back) {

/* Arguments:
**	back	o: background illumination pattern image
*/

	/* Initialize single-group structures */
	initSingleNicmosGroup (back);

}

/* N_FREEREFDATA: Free all reference data structures and memory. */

void n_freeRefData (SingleNicmosGroup *back) {

/* Arguments:
**	back	o: background illumination pattern image
*/

	/* Free single-group structures */
	freeSingleNicmosGroup (back);

}

void checkKeyI (char *filename, Hdr *header, char *keyword, int scival,
	       int severity) {

	/* Local variables */
	int keyval;

	keyval = 0;

	/* Read the keyword from the ref file */
	if (getKeyI (header, keyword, &keyval)) {
	    n_kwerr (keyword, filename);
	    miss_kwd += 1;
	    status = 0;
	    return;
	}

	/* Does it match the science image value? */
	if (keyval != scival) {
	    sprintf (MsgText, "%s %s=%d does not match science data",
		     filename, keyword, keyval);
	    if (severity == FATAL) {
		mismatch += 1;
		n_error (MsgText);
	    } else
		n_warn  (MsgText);
	}
}

void checkKeyS (char *filename, Hdr *header, char *keyword, char *scival,
	       int severity) {

	/* Local variables */
	char keyval[SZ_STRKWVAL+1];

	keyval[0] = '\0';

	/* Read the keyword from the ref file */
	if (getKeyS (header, keyword, keyval)) {
	    n_kwerr (keyword, filename);
	    miss_kwd += 1;
	    status = 0;
	    return;
	}

	/* Does it match the science image value? */
	if (strncmp (keyval, scival, strlen(scival)) != 0) {
	    sprintf (MsgText, "%s %s=\"%s\" does not match science data",
		     filename, keyword, keyval);
	    if (severity == FATAL) {
		mismatch += 1;
		n_error (MsgText);
	    } else
		n_warn  (MsgText);
	}
}

