# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define BKG_THRESH1 5.0
# define BKG_THRESH2 5.0
# define BKG_THRESH3 5.0

/* N_BACKGROUND: Compute and remove the background signal from
** NICMOS images.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	19-Feb-1997	Modified n_getBkgLevels to not check CALNICA
**				bck estimates if they're zero (Version 0.1.5)
** H.Bushouse	24-Mar-1997	Modified n_getBkgLevels to check for non-zero
**				bck value before using in division
**				(Version 0.1.5)
** H.Bushouse	10-Apr-1997	Moved n_calReport to n_calnicb so that it still
**				gets called when bck steps are skipped for darks
**				(Version 0.1.5)
** H.Bushouse	07-Jul-1997	Moved mean, median, and clip routines to new
**				n_imstat file; modified n_getBkg to no longer
**                              use n_initMask, n_mean2d, or n_clip2d, but use
**                              n_median2d instead (Version 2.2)
** H.Bushouse	02-Dec-1997	Added use of subbkg and usemeanbkg parameters;
**				split n_subBkg routine into n_subScalarBkg and
**				n_subBkgImg; modified n_getBkgLevels to report
**                              results for each image (Version 2.2)
** H.Bushouse	17-Mar-1998	Added use of asn->readbkg in n_background to
**				skip calculation of scalar backgrounds
**				(Version 2.2)
*/

int n_background (AsnInfo *asn, AsnImages *input,
		  SingleNicmosGroup *back_image, CalStep *back_step) {

/* Arguments:
**	asn		io: association info structure
**	input		io: input images
**	back_image	 i: background reference image
**	back_step	 i: calibration step structure
*/

	/* Local variables */
	int i;			/* loop index */

	/* Function definitions */
	int n_compBkg (AsnInfo *, AsnImages *);
	int n_subScalarBkg (AsnInfo *, AsnImages *);
	int n_subBkgImg (CalStep *, AsnInfo *, AsnImages *,
			 SingleNicmosGroup *);

	/* Do the scalar background subtraction, if requested */
	if (asn->subbkg) {

	    /* If the scalar background values were read from the
	    ** input assoc table, report their values. */
	    if (asn->readbkg) {
		sprintf (MsgText,
			 "Using scalar background levels from ASN table ...");
		n_message (MsgText);
		if (asn->usemeanbkg) {
		    sprintf (MsgText, "Mean background level = %4.3g",
			     asn->meanbkg);
		    n_message (MsgText);
		} else {
		    for (i = 0; i < asn->nmembers; i++) {
			 if (asn->member[i].patpos != 0) {
			     sprintf (MsgText, "  %s bck level = %4.3g",
				      asn->member[i].name, asn->member[i].bkg);
			     n_message (MsgText);
			 }
		    }
		}

	    /* Otherwise, compute the scalar background */
	    } else {
		if (n_compBkg (asn, input))
		    return (status);
	    }
		
	    /* Now, subtract the scalar background from all images */
	    if (n_subScalarBkg (asn, input))
		return (status);
	}

	/* Subtract the background reference image */
	if (n_subBkgImg (back_step, asn, input, back_image))
	    return (status);

	/* Successful return */
	return (status = 0);

}

/* N_COMPBKG: Compute the scalar background level for all
** the sky images in an association.
*/

int n_compBkg (AsnInfo *asn, AsnImages *input) {

/* Arguments:
**	asn	io: association info structure
**	input	 i: input images
*/

	/* Function definitions */
	int  n_getBkgLevels (AsnInfo *, AsnImages *);
	void n_getAvgLevel  (AsnInfo *, float *);

	/* Initialize */
	asn->meanbkg = 0;

	/* Compute the mean background level in each sky image */
	if (n_getBkgLevels (asn, input))
	    return (status);

	/* Compute the mean of the individual results */
	n_getAvgLevel (asn, &(asn->meanbkg));

	/* Successful return */
	return (status = 0);
}

/* N_GETBKGLEVELS: Compute the scalar background level in each of the
** sky images in an association.
*/

int n_getBkgLevels (AsnInfo *asn, AsnImages *input) {

/* Arguments:
**	asn	io: association info structure
**	input	 i: input images
*/

	/* Local variables */
	int i;				/* loop index */
	float diff;			/* computed - model difference */
	float stdev;			/* stdev of mean bkg */

	/* Function definitions */
	int n_getBkg (SingleNicmosGroup *, float *, float *);

	/* Announce start of background computation */
	sprintf (MsgText,
		 "Computing scalar background level for each image ...");
	n_message (MsgText);

	/* Are there any sky images to work with?; 
	** if not, use the target images */
	if (asn->nsky == 0) {
	    sprintf (MsgText, "No BCK images for background computation;");
	    n_warn (MsgText);
	    sprintf (MsgText, "Background will be computed from target images");
	    n_warn (MsgText);
	}

	/* Loop over the sky images in the association,
	** computing the mean background level for each. */
	for (i = 0; i < asn->nmembers; i++) {

	     stdev = 0;
	     asn->member[i].bkg = 0;
	     asn->member[i].bkgimg = False;

	     if (asn->member[i].patpos != 0 && 
		(asn->member[i].type == SKY || asn->nsky == 0)) {

		 /* Compute the bkg for this image */
		 if (n_getBkg (&(input->member[i]), &(asn->member[i].bkg),
			       &stdev))
		     return (status);

		 /* Mark the image as used */
		 asn->member[i].bkgimg = True;

		 /* Report result for this image */
		 sprintf (MsgText, "  %s bck level = %4.3g, sigma = %4.3g",
			  asn->member[i].name, asn->member[i].bkg, stdev);
		 n_message (MsgText);

		 /* If we're using target images, compare the computed
		 ** level with the CALNICA estimate */
		 if (asn->nsky == 0 && asn->member[i].backest1 != 0.0) {
		     diff = fabs(asn->member[i].bkg - asn->member[i].backest1);
		     if (asn->member[i].bkg != 0)
			 diff = diff / sqrt(fabs(asn->member[i].bkg));
		     if (diff > BKG_THRESH1) {
			 sprintf (MsgText,
			 "Bck level of %g for %s differs by > %g sigma",
			 asn->member[i].bkg, asn->member[i].name, BKG_THRESH1);
			 n_warn (MsgText);
			 sprintf (MsgText,
		"from CALNICA estimate of %g; will use CALNICA estimate",
			 asn->member[i].backest1);
			 n_warn (MsgText);

			 /* Reset stored bkg level */
			 asn->member[i].bkg = asn->member[i].backest1;
		     }
		 }
	     }
	}

	/* Successful return */
	return (status = 0);
}

/* N_GETBKG: Compute the scalar bkg level for an individual image */

int n_getBkg (SingleNicmosGroup *image, float *bkg, float *sigma) {

/* Arguments:
**	image	i: input image
**	bkg	o: bkg value
**	sigma	o: sigma of bkg value
*/

	/* Function definitions */
	int n_median2d (SingleNicmosGroup *, float, float *, float *);

	/* Initialize the results */
	*bkg   = 0;
	*sigma = 0;

	/* Compute the median and ave deviation of the image */
	if (n_median2d (image, (float)BKG_THRESH2, bkg, sigma))
	    return (status);

	/* Successful return */
	return (status = 0);
}

/* N_GETAVGLEVEL: Compute the average bkg level from the list of bkg
** levels for all the sky images in an association.
*/

void n_getAvgLevel (AsnInfo *asn, float *mean_bkg) {

/* Arguments:
**	asn		i: association info structure
**	mean_bkg	o: mean bkg value
*/
	/* Local variables */
	int i;			/* loop index */
	int nrej;		/* rejection counter */
	int nbkg;		/* number of bkg values */
	float *bkg;		/* bkg values vector */
	int *mask;		/* mask vector */
	float mean;		/* mean of bkg values */
	float stdv;		/* standard deviation of bkg values */

	/* Function definitions */
	void n_mean1d (int, float *, int *, float *, float *);
	void n_clip1d (int, float *, int *, float, float, float, int *);

	/* Initialize the result */
	*mean_bkg = 0;
	nbkg = 0;

	/* Allocate memory for the bkg and mask vectors */
	bkg  = (float *) calloc(asn->nmembers, sizeof(float));
	mask = (int *)   calloc(asn->nmembers, sizeof(int));

	/* Load the background and mask vectors */
	for (i = 0; i < asn->nmembers; i++) {
	     if (asn->member[i].bkgimg) {
		 bkg[nbkg] = asn->member[i].bkg;
		 mask[nbkg] = 1;
		 nbkg++;
	     }
	}

	/* For two or fewer values, just compute the straight mean */
	if (nbkg <= 2) {

	    n_mean1d (nbkg, bkg, mask, &mean, &stdv);

	/* Otherwise compute the mean with rejection of outliers */
	} else {

	    n_mean1d (nbkg, bkg, mask, &mean, &stdv);

	    n_clip1d (nbkg, bkg, mask, (float)BKG_THRESH3, mean, stdv, &nrej);

	    if (nrej > 0)
		n_mean1d (nbkg, bkg, mask, &mean, &stdv);
	}

	/* Free memory allocated locally */
	free (bkg);
	free (mask);

	/* Return the final mean value */
	*mean_bkg = mean;

	sprintf (MsgText, "Mean background level = %4.3g, stdev = %4.3g",
		 mean, stdv);
	n_message (MsgText);
}

/* N_SUBSCALARBKG: Subtract the scalar background value from all images in
** an association.
*/

int n_subScalarBkg (AsnInfo *asn, AsnImages *input) {

/* Arguments:
**	asn		 i: association info structure
**	input		io: association images
*/

	/* Local variables */
	int i;			/* loop index */
	char history[72+1];	/* history record */

	/* Function definitions */
	int n_asubk (SingleNicmosGroup *, float);

	if (asn->usemeanbkg) {
	    sprintf (MsgText,
		     "Subtracting MEAN background from all images ...");
	} else {
	    sprintf (MsgText,
		     "Subtracting INDIVIDUAL backgrounds from images ...");
	}
	n_message (MsgText);

	/* Loop through association images */ 
	for (i = 0; i < asn->nmembers; i++) {

	     /* Only work on the ones that haven't already been disposed of */
	     if (asn->member[i].patpos != 0) {

		 /* Subtract the scalar bkg level */
		 if (asn->usemeanbkg) {
		     n_asubk (&(input->member[i]), asn->meanbkg);
		     sprintf (history, "Subtracted constant bkg level = %g",
			      asn->meanbkg);
		 } else {
		     n_asubk (&(input->member[i]), asn->member[i].bkg);
		     sprintf (history, "Subtracted constant bkg level = %g",
			      asn->member[i].bkg);
		 }

		 if (addHistoryKw (&(input->member[i].sci.hdr), history) == -1)
		     return (status = 1);
	     }
	}

	/* Successful return */
	return (status = 0);
}

/* N_SUBBKGIMG: Subtract the background illumination pattern reference
** image from all images in an association.
*/

int n_subBkgImg (CalStep *step, AsnInfo *asn, AsnImages *input,
		 SingleNicmosGroup *back) {

/* Arguments:
**	step		 i: calibration step structure
**	asn		 i: association info structure
**	input		io: association images
**	back		 i: background pattern reference image
*/

	/* Local variables */
	int i;			/* loop index */

	/* Function definitions */
	int n_asub  (SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, SingleNicmosGroup *);

	/* Loop through association images */ 
	for (i = 0; i < asn->nmembers; i++) {

	     /* Only work on the ones that haven't already been disposed of */
	     if (asn->member[i].patpos != 0) {

		 /* Subtract the bkg reference image, if requested */
		 if (step->corr == PERFORM)
		     n_asub (&(input->member[i]), back);

		 /* Print history records and update header keywords */
		 if (n_calReport (step, &(input->member[i])))
		     return (status);
	     }
	}

	/* Successful return */
	return (status = 0);
}

