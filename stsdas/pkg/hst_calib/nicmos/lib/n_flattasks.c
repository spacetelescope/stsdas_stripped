# include <stdio.h>
# include <string.h>
# include "nicmos.h"

/*   N_FLATTASKS --  Contains various flatfield-related routines.
**
**   N_GETFLAT: Get the flatfield image for a science image
**   N_INVFLAT: Invert a flatfield image
**   N_UNFLAT: Undo the flatfielding if it has already been
**                applied to a science image
**   N_REFLAT: Reapply flatfield to a science image
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	04-May-1999	Implementation
**
*/

int n_getFlat (char *FlatName, SingleNicmosGroup *Input,
	       SingleNicmosGroup *Flat, Bool verbose) {

/* Arguments:
**	FlatName	i: flatfield file name
**	Input		i: input image
**	Flat		o: flatfield image
**	verbose		i: verbose output switch
*/

	/* Local variables */
	char *name;		/* file name */

	/* Allocate memory for file name */
	name = (char *)calloc(SZ_NAME, sizeof(char));

	/* Initialize the flat image data structure */
	initSingleNicmosGroup (Flat);

	/* If input flatfield name is blank, retrieve from image header */
	if (FlatName[0] == '\0' || FlatName[0] == ' ') {

	    if (getKeyS (Input->globalhdr, "FLATFILE", name)) {
		sprintf (MsgText, "Can't read FLATFILE keyword from %s",
			 Input->filename);
		n_error (MsgText);
		return (1);
	    }

	}

	/* Otherwise use name given by user */
	else {
	    strcpy (name, FlatName);
	}

	if (verbose) {
	    sprintf (MsgText, "  Using Flatfield %s\n", name);
	    n_message (MsgText);
	}

	/* Load the flatfield image */
	if (getSingleNicmosGroup (name, 1, Flat))
	    return (1);

	free (name);

	return (0);

}

void n_invFlat (SingleNicmosGroup *Flat) {

/* Arguments:
**	Flat	io: flatfield image
*/

	/* Local variables */
	int i, j;		/* Pixel indexes */

	/* Invert the SCI image data */
	for (j=0; j < Flat->sci.data.ny; j++) {
	     for (i=0; i < Flat->sci.data.nx; i++) {
		  if (Pix(Flat->sci.data,i,j) != 0)
		      Pix(Flat->sci.data,i,j) = 1.0 / Pix(Flat->sci.data,i,j);
	     }
	}

}

int n_unFlat (SingleNicmosGroup *Input, SingleNicmosGroup *Flat) {

/* Arguments:
**	Input	io: input image
**	Flat	 i: flatfield image
*/

	/* Local variables */
	int i, j;			/* Pixel indexes */
	char Kw[40];			/* String keyword value */
	Bool done;			/* Switch */

	/* Read the FLATDONE keyword from the input file header to see
	** if the data have already been flatfielded. */
	Kw[0] = '\0';
	if (getKeyS (Input->globalhdr, "FLATDONE", Kw)) {
	    sprintf (MsgText, "Can't read FLATDONE keyword from %s",
		     Input->filename);
	    n_error (MsgText);
	    return (1);
	}

	if (strncmp (Kw, "PERFORMED", 9) == 0)
	    done = True;
	else
	    done = False;

	/* If flatfielding has already been applied to the input image,
	** temporarily undo the correction. The Flat image has already
	** been inverted at this point, so to undo the flatfielding we
	** multiply the Input image by the Flat (rather than divide). */
	if (done) {
	    for (j=0; j < Input->sci.data.ny; j++) {
		 for (i=0; i < Input->sci.data.nx; i++) {
		      Pix(Input->sci.data,i,j) *= Pix(Flat->sci.data,i,j);
		 }
	    }
	}

	return (0);
}

int n_reFlat (SingleNicmosGroup *Input, SingleNicmosGroup *Flat) {

/* Arguments:
**	Input	io: input image
**	Flat	 i: flatfield image
*/

	/* Local variables */
	int i, j;			/* Loop indexes */
	char Kw[40];			/* String keyword value */
	Bool done;			/* Switch */

	/* Read the FLATDONE keyword from the input file header to see
	** if the data have already been flatfielded. */
	Kw[0] = '\0';
	if (getKeyS (Input->globalhdr, "FLATDONE", Kw)) {
	    sprintf (MsgText, "Can't read FLATDONE keyword from %s",
		     Input->filename);
	    n_error (MsgText);
	    return (1);
	}

	if (strncmp (Kw, "PERFORMED", 9) == 0)
	    done = True;
	else
	    done = False;

	/* If flatfielding was applied previously, reapply it now. */
	if (done) {
	    for (j=0; j < Input->sci.data.ny; j++) {
		 for (i=0; i < Input->sci.data.nx; i++) {
		      if (Pix(Flat->sci.data,i,j) != 0)
			  Pix(Input->sci.data,i,j) /= Pix(Flat->sci.data,i,j);
		      else
			  Pix(Input->sci.data,i,j) = 0.0;
		 }
	    }
	}

	return (0);
}

