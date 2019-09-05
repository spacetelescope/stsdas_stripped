# include <math.h>
# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define TWOPI 6.283185307179586	/* 2 times PI */

# define DEBUG 0	/* print debug messages? */

/* N_MOSAIC: Build a mosaic image from NICMOS association members.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	Oct 1996	Modified n_mosHeader to update ROOTNAME and
**				ASN_MTYP keywords in MOS primary header, and
**				update ROOTNAME and EXPNAME in MOS extension
**				headers (Version 0.1.4)
** H.Bushouse	Oct 1996	Build 2: modified all routines to use new WCS
**				structure; modified n_mosHeader to set WCS of
**				output mosaic to center of image (Version 2.0)
** H.Bushouse	Jan 1997	Build 2: changed mosaic image from MultiGroup
**				to SingleGroup structure; removed n_combMosaic
**				routine; rewrote n_fillMosaic to directly
**				populate SingleGroup mosaic; changed name of
**				n_mosHeader to n_updtHeader (Version 2.0)
** H.Bushouse	28-Apr-1997	Modified n_mosHeader to construct MOS filename
**				and update FILENAME keyword (Version 0.1.6)
** H.Bushouse	29-Apr-1997	Pass in NicInfo; change MemberInfo.imgtype to
**				mtype; changed n_getOffsets to set offsets to
**				zero for non-external images (Version 2.0)
** H.Bushouse	06-May-1997	Moved header updates from n_copyHeader to
**				n_updtHeader; update CAL_VER keyword in
**				n_updtHeader (Version 2.0)
** H.Bushouse	12-Jun-1997	Modified n_fillMosaic so that it doesn't free
**				memory for first image in mosaic because it'll
**				be needed by later routines (Version 2.1)
** H.Bushouse	01-Dec-1997	Added AsnInfo to n_xcorr argument list in
**				n_getOffsets; added AsnInfo to n_aerrlcip
**				argument list in n_fillMosaic; modified
**				n_getOffsets to update predicted offsets based
**				on xcorr results for previous image in pattern
**				(Version 2.2)
** H.Bushouse	17-Mar-1998	Added use of asn->readoffs in n_mosaic to skip
**				call to n_getOffsets when image offsets are
**				read from input assoc table (Version 2.2)
** H.Bushouse	19-Mar-1998	Added call to n_idSources to reflag sources in
**				the mosaic images (Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 2.2.1)
** H.Bushouse	19-May-1999	Added calls to updMinMax routines to
**				n_updtHeader to update DATAMIN/MAX keywords
**				in each mos file extension header (Version 2.3)
** H.Bushouse	04-Apr-2000	Modified n_mosaic to only call n_idSources for
**				EXTernal obs_types; Added NicInfo as argument
**				to n_makeMosaic and n_copyHeader; Modified
**				n_copyHeader to use "saa" file name suffix for
**				Post-SAA dark mosaic products (Version 2.4)
**
**M. Sosey	    the mosaic header information was updated to include
**		        the full exposure time and expstart/expend times for the
** 				entire association instead of just the first member.
**
**
*/

int n_mosaic (NicInfo *nic, AsnInfo *asn, AsnImages *input, int mosnum,
	      SingleNicmosGroup *mosaic) {

/* Arguments:
**	nic	i: observation info structure
**	asn	i: association info structure
**	input	i: input images
**	mosnum  i: mosaic index number to build
**	mosaic	o: mosaic image
*/

	/* Local variables */
	int i, j;			/* loop indexes */

	/* Function definitions */
	int n_getOffsets (NicInfo *, AsnInfo *, int, AsnImages *);
	int n_getCorners (AsnInfo *, int);
	int n_makeMosaic (NicInfo *, AsnInfo *, int, AsnImages *,
			  SingleNicmosGroup *);
	int n_fillMosaic (AsnInfo *, int, AsnImages *, SingleNicmosGroup *);
	int n_updtHeader (AsnInfo *, int, SingleNicmosGroup *);
	int n_idSources  (SingleNicmosGroup *);

	/* Make sure we need to build a mosaic */
	if (asn->mosmems[mosnum] == 0)
	    return (status = 0);

	sprintf (MsgText, "Building mosaic %d:", mosnum+1);
	n_message (MsgText);

	/* If the image offsets were read from the input assoc table,
	** just report the values */
	if (asn->readoffs) {

	    sprintf (MsgText, "Using image offsets from ASN table ...");
	    n_message (MsgText);

	    for (i = 0; i < asn->nmembers; i++) {
		 if (asn->member[i].mospos == mosnum+1) {
 		     sprintf (MsgText, "  %s  dx=%7.2f  dy=%7.2f",
			      asn->member[i].name, asn->member[i].dx,
			      asn->member[i].dy);
		     n_message (MsgText);
		 }
	    }

	/* Otherwise, calculate the offsets for members of this mosaic */
	} else {
	    if (n_getOffsets (nic, asn, mosnum, input))
		return (status);
	}

	/* Determine the corner locations for each image in the mosaic */
	if (n_getCorners (asn, mosnum))
	    return (status);

	/* Make the empty mosaic image */
	if (n_makeMosaic (nic, asn, mosnum, input, mosaic))
	    return (status);

	/* Fill the mosaic with the image data */
	if (n_fillMosaic (asn, mosnum, input, mosaic))
	    return (status);

	/* Update the mosaic image header */
	if (n_updtHeader (asn, mosnum, mosaic))
	    return (status);

	/* Reflag sources in the mosaic image. This is only
	** done for external observations. */
	if (nic->obs_type == EXT) {

	    /* First, remove any existing source flags in the DQ image */
	    for (j=0; j < mosaic->sci.data.ny; j++) {
		 for (i=0; i < mosaic->sci.data.nx; i++) {
		      if (DQPix(mosaic->dq.data,i,j) & SOURCE)
			  DQSetPix(mosaic->dq.data,i,j,
				DQPix(mosaic->dq.data,i,j)-SOURCE);
		 }
	    }

	    /* Now do a fresh search for sources */
	    if (n_idSources (mosaic))
		return (status);
	}

	/* Successful return */
	return (status = 0);

}

/* N_GETOFFSETS: Compute the relative offsets (in pixel space) of the various
** images to be included in a mosaic. The first association image listed as
** a member of the mosaic is taken to be the "reference" image and the offsets
** for all other images are computed relative to the reference image. The
** reference image is assigned offsets of zero. The offsets are in the
** sense of the direction that an image is shifted on the sky relative to
** the reference image. Therefore an object at pixel coords of x,y in a
** shifted image will be at pixel coords x+xoff, y+yoff in the reference
** image.
*/

int n_getOffsets (NicInfo *nic, AsnInfo *asn, int mosnum, AsnImages *input) {

/* Arguments:
**	nic	 i: observation info structure
**	asn	io: association info structure
**	mosnum	 i: mosaic number
**	input	 i: input association images
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int prev_image;		/* image index */
	float xoff, yoff;	/* pixel offsets */
	float dx_pred, dy_pred;	/* predicted pixel offsets */

	/* Function definitions */
	int n_compOffset (MemberInfo *, MemberInfo *);
	int n_xcorr (AsnInfo *, SingleNicmosGroup *, SingleNicmosGroup *,
		     float *, float *);

	/* Set all offsets to zero for Darks and Flats */
	if (nic->obs_type != EXT) {
	    for (i=0; i < asn->nmembers; i++) {
		 if (asn->member[i].mospos == mosnum+1) {
		     asn->member[i].dx = 0;
		     asn->member[i].dy = 0;
		     sprintf (MsgText, "  %s  dx=%7.2f  dy=%7.2f",
			      asn->member[i].name, asn->member[i].dx,
			      asn->member[i].dy);
		     n_message (MsgText);
		 }
	    }

	/* Otherwise, calculate the offsets for images in this mosaic */
	} else {

	    /* Set offsets to zero for the first image in this mosaic */
	    for (i=0; i < asn->nmembers; i++) {
		 if (asn->member[i].mospos == mosnum+1) {
		     asn->member[i].dx = 0;
		     asn->member[i].dy = 0;
		     prev_image = i;
		     dx_pred = asn->member[i].dx;
		     dy_pred = asn->member[i].dy;
		     sprintf (MsgText, "  %s  dx=%7.2f  dy=%7.2f",
			      asn->member[i].name, asn->member[i].dx,
			      asn->member[i].dy);
		     n_message (MsgText);
		     break;
		 }
	    }

	    /* Calculate offsets for the rest */
	    for (j=i+1; j < asn->nmembers; j++) {
		 if (asn->member[j].mospos == mosnum+1) {

		     /* First compute the WCS offsets relative to
		     ** the reference image */
		     if (n_compOffset (&(asn->member[j]), &(asn->member[i])))
			 return (status);

		     /* Refine the offset by cross-correlating against
		     ** the previous image in the mosaic */
		     xoff = asn->member[j].dx - dx_pred;
		     yoff = asn->member[j].dy - dy_pred;
		     dx_pred = asn->member[j].dx;
		     dy_pred = asn->member[j].dy;
		     if (n_xcorr (asn, &(input->member[j]),
				  &(input->member[prev_image]), &xoff, &yoff))
			 return (status);
		     asn->member[j].dx = xoff + asn->member[prev_image].dx;
		     asn->member[j].dy = yoff + asn->member[prev_image].dy;

		     prev_image = j;

		     sprintf (MsgText, "  %s  dx=%7.2f  dy=%7.2f",
			      asn->member[j].name, asn->member[j].dx,
			      asn->member[j].dy);
		     n_message (MsgText);
		 }
	    }
	}

	/* Successful return */
	return (status = 0);

}

/* N_COMPOFFSET: Compute the offsets of one image relative to another
** based on their WCS information. */

int n_compOffset (MemberInfo *im1, MemberInfo *im2) {

/* Arguments:
**	im1	io: offset image info structure
**	im2	 i: reference image info structure
*/

	/* Local variables */
	float x, y;			/* pixel coords */
	double ra, dec;			/* RA and Dec values */

	/* Function definitions */
	int n_rd2xy (double, double, WCS *, float *, float *);
	int n_xy2rd (float, float, WCS *, double *, double *);

	/* Initialize the offsets to zero */
	im1->dx = 0;
	im1->dy = 0;

	/* First find the RA/Dec of this image for pixel coords
	** equal to that of the crpix1/crpix2 of the reference image.
	** This is necessary in case the two images don't use the same
	** reference pixel values. */
	x = im2->wcs.crpix[0];
	y = im2->wcs.crpix[1];
	if (n_xy2rd (x, y, &(im1->wcs), &ra, &dec))
	    return (status);

	/* Now compute the pixel coords of this RA/Dec relative to
	** the reference image */
	if (n_rd2xy (ra, dec, &(im2->wcs), &x, &y))
	    return (status);

	/* Store the pixel coords as an offset from the reference image */
	im1->dx = x - im2->wcs.crpix[0];
	im1->dy = y - im2->wcs.crpix[1];

	/* Successful return */
	return (status = 0);

}

/* N_RD2XY: Convert RA/Dec to x,y pixel coords for an image. */

int n_rd2xy (double ra, double dec, WCS *wcs, float *x, float *y) {

	/* Local variables */
	double ra0, dec0;
	double det, bottom, xi, eta;
	double cdinv1_1, cdinv1_2, cdinv2_1, cdinv2_2;

	/* Compute the determinant of the CD matrix */
	det = wcs->cd[0][0] * wcs->cd[1][1]  -  wcs->cd[0][1] * wcs->cd[1][0];
	if (det == 0) {
	    sprintf (MsgText, "Singular CD matrix in rd2xy routine");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Compute the inverse CD matrix */
	cdinv1_1 =   wcs->cd[1][1] / det;
	cdinv1_2 = - wcs->cd[0][1] / det;
	cdinv2_1 = - wcs->cd[1][0] / det;
	cdinv2_2 =   wcs->cd[0][0] / det;

	/* Convert RA/Dec from degrees to radians */
	ra0  = wcs->crval[0] * TWOPI / 360.0;
	dec0 = wcs->crval[1] * TWOPI / 360.0;
	ra             = ra  * TWOPI / 360.0;
	dec            = dec * TWOPI / 360.0;

	bottom = sin(dec)*sin(dec0) + cos(dec)*cos(dec0)*cos(ra-ra0);
	if (bottom == 0) {
	    sprintf (MsgText, "Unreasonable RA/Dec range in rd2xy routine");
	    n_error (MsgText);
	    return (status = 1);
	}

	xi  = cos(dec) * sin(ra-ra0) / bottom;
	eta = (sin(dec)*cos(dec0) - cos(dec)*sin(dec0)*cos(ra-ra0)) / bottom;

	/* Convert back to degrees */
	xi  = xi  * 360.0 / TWOPI;
	eta = eta * 360.0 / TWOPI;

	/* Compute pixel coords */
	*x = cdinv1_1 * xi + cdinv1_2 * eta + (double) wcs->crpix[0];
	*y = cdinv2_1 * xi + cdinv2_2 * eta + (double) wcs->crpix[1];

	/* Successful return */
	return (status = 0);
}

/* N_XY2RD: Convert x,y pixel coords to RA/Dec for an image. */

int n_xy2rd (float x, float y, WCS *wcs, double *ra, double *dec) {

	double xi,  eta;
	double ra0, dec0;

	xi  = wcs->cd[0][0] * (double) (x - wcs->crpix[0]) +
	      wcs->cd[0][1] * (double) (y - wcs->crpix[1]);
	eta = wcs->cd[1][0] * (double) (x - wcs->crpix[0]) +
	      wcs->cd[1][1] * (double) (y - wcs->crpix[1]);

	xi   = xi  * TWOPI / 360.0;
	eta  = eta * TWOPI / 360.0;

	ra0  = wcs->crval[0] * TWOPI / 360.0;
	dec0 = wcs->crval[1] * TWOPI / 360.0;

	*ra  = atan2(xi, cos(dec0)-eta*sin(dec0)) + ra0;
	*dec = cos(dec0)-eta*sin(dec0);
	*dec = (*dec)*(*dec) + xi*xi;
	*dec = atan2(eta*cos(dec0)+sin(dec0), sqrt(*dec));

	*ra  = *ra  * 360.0 / TWOPI;
	*dec = *dec * 360.0 / TWOPI;
	*ra = fmod (*ra, 360.0);
	if (*ra < 0)
	    *ra += 360.0;

	return (status = 0);
}

/* N_GETCORNERS: Compute the mosaic pixel coords corresponding to the
** bottom left corner of each member image.
*/

int n_getCorners (AsnInfo *asn, int mosnum) {

/* Arguments:
**	asn	io: association info structure
**	mosnum	 i: mosaic number that we're working on
*/

	/* Local variables */
	int i;				/* loop index */
	float xmin, ymin;		/* minimum offsets */

	/* Initialize the min offsets */
	xmin = 0;
	ymin = 0;

	/* Loop over the input images, finding the min x/y offsets */
	for (i = 0; i < asn->nmembers; i++) {

	     /* Find the ones that belong to this mosaic */
	     if (asn->member[i].mospos == mosnum+1) {

		 /* Find the min x/y offsets */
		 if (asn->member[i].dx < xmin) xmin = asn->member[i].dx;
		 if (asn->member[i].dy < ymin) ymin = asn->member[i].dy;
	     }
	}

	/* Loop over the input images, setting corners relative to mosaic.
	** The image with the minimum offsets defines the 0,0 mosaic
	** coords. */
	for (i = 0; i < asn->nmembers; i++) {

	     /* Find the ones that belong to this mosaic */
	     if (asn->member[i].mospos == mosnum+1) {

		 /* Set the corner locations in the mosaic image */
		 asn->member[i].xi = asn->member[i].dx - xmin;
		 asn->member[i].yi = asn->member[i].dy - ymin;

		 if (DEBUG)
		 printf (" image[%d]: xi=%g, yi=%g\n", i+1, asn->member[i].xi,
			 asn->member[i].yi);
	     }
	}

	/* Successful return */
	return (status = 0);

}

/* N_MAKEMOSAIC: Make an (empty) mosaic image.
** The image is constructed as a NICMOS SingleGroup structure.
*/

int n_makeMosaic (NicInfo *nic, AsnInfo *asn, int mosnum, AsnImages *input,
		  SingleNicmosGroup *mosaic) {

/* Arguments:
**	nic	 i: observation info structure
**	asn	 i: association info structure
**	mosnum	 i: mosaic number that we're building
**	input	 i: input association images
**	mosaic	 o: mosaic image
*/

	/* Local variables */
	int i;				/* loop index */
	int mosnx, mosny;		/* mosaic dimensions */
	float xmax, ymax;		/* max offsets */
	     
	/* Function definitions */
	int n_copyHeader (NicInfo *, AsnInfo *, int, AsnImages *,
			  SingleNicmosGroup *);

	/* Initialize */
	xmax = 0;
	ymax = 0;

	/* Find the max offsets for members of this mosaic */
	if (asn->mosmems[mosnum] > 1) {
	    for (i = 0; i < asn->nmembers; i++) {
		 if (asn->member[i].mospos == mosnum+1) {
		     if (asn->member[i].xi > xmax) xmax = asn->member[i].xi;
		     if (asn->member[i].yi > ymax) ymax = asn->member[i].yi;
		 }
	    }
	}

	/* Determine the size needed for the mosaic image */
	mosnx = (int)xmax + SZ_NICIMG;
	mosny = (int)ymax + SZ_NICIMG;

	if (DEBUG) {
	printf (" mosaic[%d] size: nx=%d, ny=%d, nimages=%d\n", mosnum+1,
		mosnx, mosny, asn->mosmems[mosnum]);
	}

	/* Initialize and allocate the SingleGroup mosaic image structure */
	initSingleNicmosGroup (mosaic);
	if (allocSingleNicmosGroup (mosaic, mosnx, mosny)) {
	    sprintf (MsgText, "Can't allocate mosaic image in makeMosaic");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Copy info into the mosaic image header */
	if (n_copyHeader (nic, asn, mosnum, input, mosaic))
	    return (status);

	/* Successful return */
	return (status = 0);

}

/* N_FILLMOSAIC: Fill the mosaic image with combined member data.
** Bilinear interpolation is performed for images that are not
** precisely registered on the mosaic image grid.
*/

int n_fillMosaic (AsnInfo *asn, int mosnum, AsnImages *input,
		  SingleNicmosGroup *mosaic) {

/* Arguments:
**	asn	i: association info structure
**	mosnum	i: mosaic number that we're building
**	input	i: association images
**	mosaic	o: mosaic image
*/

	/* Local variables */
	int i, j, k;			/* loop indexes */
	int nsamp;			/* number of samples */
	int nfirst;			/* index of first image */
	int badpix, ngood;		/* combine results */
	int *dq;			/* interpolated dq value */
	float x, y;			/* pixel coordinates */
	float mean, stdv, efftime;	/* combined pixel values */
	float *sci, *err, *time;	/* interpolated pixel values */
	char history[72+1];		/* history keyword string */

	/* Function definitions */
	int n_bilin_grp (SingleNicmosGroup *, float, float, float *, float *,
			 int *, float *);
	void n_aerrclip (AsnInfo *, int, float *, float *, int *, float *,
			 float *, float *, int *, int *, float *, int, int);

	/* Allocate memory for local arrays */
	sci  = NULL;
	err  = NULL;
	dq   = NULL;
	time = NULL;
	sci  = (float *) calloc(asn->mosmems[mosnum], sizeof(float));
	err  = (float *) calloc(asn->mosmems[mosnum], sizeof(float));
	dq   =   (int *) calloc(asn->mosmems[mosnum], sizeof(int));
	time = (float *) calloc(asn->mosmems[mosnum], sizeof(float));

	/* Loop through the mosaic image,
	** collecting and combining samples at each pixel  */
	for (j = 0; j < mosaic->sci.data.ny; j++) {
	for (i = 0; i < mosaic->sci.data.nx; i++) {

	     /* Initialize sample counter */
	     nsamp = 0;

	     /* Loop over the input images */
	     for (k = 0; k < asn->nmembers; k++) {

		  /* Find the ones that belong in this mosaic */
		  if (asn->member[k].mospos == mosnum+1) {

		      /* Find the ones that contribute to this pixel */
		      x = (float)i - asn->member[k].xi;
		      y = (float)j - asn->member[k].yi;

		      if (x >= 0 && x <= input->member[k].sci.data.nx-1 &&
			  y >= 0 && y <= input->member[k].sci.data.ny-1) {

			  /* Compute interpolated image values */
			  if (n_bilin_grp (&(input->member[k]), x, y,
				&sci[nsamp], &err[nsamp], &dq[nsamp],
				&time[nsamp]))
			      return (status);
			  nsamp++;
		      }
		  }
	     }

	     /* Combine the samples for this pixel */
	     n_aerrclip (asn, nsamp, sci, err, dq, time,
			 &mean, &stdv, &badpix, &ngood, &efftime, i, j);

	     /* Put the combined data into the mosaic image */
	     Pix(mosaic->sci.data,i,j)  = mean;
	     Pix(mosaic->err.data,i,j)  = stdv;
	     DQSetPix(mosaic->dq.data,i,j,badpix);
	     Pix(mosaic->smpl.data,i,j) = ngood;
	     Pix(mosaic->intg.data,i,j) = efftime;

	     /* Insert a DQ flag for pixels with no samples */
	     if (nsamp == 0)
		 DQSetPix(mosaic->dq.data,i,j,BADPIX);
	}
	}

	/* Add some history keywords to the mosaic image header
	** and free memory for images we no longer need. */
	nfirst = -1;
	for (k = 0; k < asn->nmembers; k++) {
	     if (asn->member[k].mospos == mosnum+1) {
		 if (nfirst == -1) {
		     nfirst = k;
		     sprintf (history, "Mosaiced images:");
		     if (addHistoryKw (&(mosaic->sci.hdr), history) == -1)
			 return (status = 1);
		 }
		 sprintf (history, "  %s", input->member[k].filename);
		 if (addHistoryKw (&(mosaic->sci.hdr), history) == -1)
		     return (status = 1);

		 if (k != nfirst) {
		     asn->member[k].patpos = 0;
		     asn->member[k].mospos = 0;
		     freeSingleNicmosGroup(&(input->member[k]));
		 }
	     }
	}

	/* Free memory for the local arrays */
	free (sci);
	free (err);
	free (dq);
	free (time);

	/* Succesful return */
	return (status = 0);

}

/* N_COPYHEADER: Create a header for the mosaic image by copying the
** header from the reference image for the mosaic. Also update pertinent
** keywords in the mosaic image header.
*/

int n_copyHeader (NicInfo *nic, AsnInfo *asn, int mosnum, AsnImages *input,
		  SingleNicmosGroup *mosaic) {

/* Arguments:
**	nic	 i: observation info structure
**	asn	 i: association info structure
**	mosnum	 i: mosaic number we're working on
**	input	 i: association images
**	mosaic	io: mosaic image
*/

	/* Local variables */
	int i;				/* loop index */
	int k;				/* mosaic index in ASN member list */
	char fname[SZ_NAME+1];		/* mosaic file full name */

	/* Find the index number of this mosaic within the list
	** of files in the ASN table */
	k = asn->nmembers + mosnum;

	/* Construct the mosaic output file name */
	fname[0] = '\0';
	if (nic->obs_type == POST_SAA_DARK)
	    sprintf (fname, "%s%s", asn->member[k].name, "_saa.fits");
	else
	    sprintf (fname, "%s%s", asn->member[k].name, "_mos.fits");
	mosaic->filename = (char *)calloc((strlen(fname)+1), sizeof(char));
	strcpy (mosaic->filename, fname);

	/* Set the group number */
	mosaic->group_num = 1;

	/* Find the first input image that belongs to this mosaic */
	for (i = 0; i < asn->nmembers; i++) {
	     if (asn->member[i].mospos == mosnum+1)
		 break;
	}

	/* Copy the global and extension headers from the input image */
        if (copyHdr(mosaic->globalhdr,     input->member[i].globalhdr))
	    return(status=1);
        if (copyHdr(&(mosaic->sci.hdr),  &(input->member[i].sci.hdr)))
	    return(status=1);
        if (copyHdr(&(mosaic->err.hdr),  &(input->member[i].err.hdr)))
	    return(status=1);
        if (copyHdr(&(mosaic->dq.hdr),   &(input->member[i].dq.hdr)))
	    return(status=1);
        if (copyHdr(&(mosaic->smpl.hdr), &(input->member[i].smpl.hdr)))
	    return(status=1);
        if (copyHdr(&(mosaic->intg.hdr), &(input->member[i].intg.hdr)))
	    return(status=1);

	/* Successful return */
	return (status = 0);

}

/* N_UPDTHEADER: Update pertinent keywords in the mosaic image header. */

int n_updtHeader (AsnInfo *asn, int mosnum, SingleNicmosGroup *mosaic) {

/* Arguments:
**	asn	 i: association info structure
**	mosnum	 i: mosaic number we're working on
**	mosaic	io: mosaic image
*/

	/* Local variables */
	int i, j, p;			/* loop indexes */
	char rname[SZ_NAME+1];		/* mosaic file root name */
	char version[SZ_STRKWVAL+1];	/* software version */
	float crpix1, crpix2;		/* WCS CRPIX values */
	double crval1, crval2;		/* WCS CRVAL values */
    double expstart, expend;
    float exptime;

	/* Function definitions */
	int n_xy2rd (float, float, WCS *, double *, double *);
	int updMinMaxf (FloatHdrData *);
	int updMinMaxs (ShortHdrData *);

    expstart = -1.0;
    expend = -1.0;
    exptime = -1.0;
    
	/* Update the "ROOTNAME" keyword */
	i = asn->nmembers + mosnum;
	rname[0] = '\0';
	strcpy (rname, asn->member[i].name);
	for (j = 0; j < strlen(rname); j++)
	     rname[j] = toupper(rname[j]);
	if (putKeyS (mosaic->globalhdr, "ROOTNAME", rname,""))
	    return (status = 1);

	/* Update the "FILENAME" keyword */
	if (putKeyS (mosaic->globalhdr, "FILENAME", mosaic->filename, ""))
	    return (status = 1);

	/* Update the "ASN_MTYP" keyword */
	if (putKeyS (mosaic->globalhdr, "ASN_MTYP", asn->member[i].mtype, ""))
	    return (status = 1);

	/* Update the "NEXTEND" keyword */
	if (putKeyI (mosaic->globalhdr, "NEXTEND", 5, ""))
	    return (status = 1);

	/* Update the "CALSTAGE" keyword */
	if (putKeyS (mosaic->globalhdr, "CALSTAGE", "CALNICB", ""))
	    return (status = 1);

	/* Update the "CAL_VER" keyword */
	sprintf (version, "Version %s", CALNICB_VERSION);
	if (putKeyS (mosaic->globalhdr, "CAL_VER", version, ""))
	    return (status = 1);

    if (mosnum > 0){
        for (p=0;p < asn->tmembers;p++){
            /* Match up mosaic with product from ASN table */
            if ((asn->member[p].bkgprod > 0) && (asn->member[p].bkgprod == mosnum)){
                exptime = asn->member[p].exptime;
                expstart = asn->member[p].expstart;
                expend = asn->member[p].expend;
                break;
            }
        }
    }
    if (expstart < 0) {
        /* We are working with a exp-targ product, so get the values
        from the global values. 
        */
        exptime = asn->exptime;
        expstart = asn->expstart;
        expend = asn->expend;
    }
    /* Update the "EXPTIME" keyword */
	sprintf (version, "Total exposure time for mosaic=", asn->exptime);
	if (putKeyF (mosaic->globalhdr, "EXPTIME", exptime, ""))
	    return (status = 1);

    /* Update the "EXPSTART" keyword */
	if (putKeyD (mosaic->globalhdr, "EXPSTART", expstart, ""))
	    return (status = 1);

    /* Update the "EXPEND" keyword */
	if (putKeyD (mosaic->globalhdr, "EXPEND", expend, ""))
	    return (status = 1);
    
    

	/* Update the ROOTNAME and EXPNAME keywords in
	** each extension header */
	if (putKeyS (&(mosaic->sci.hdr), "ROOTNAME",rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->sci.hdr), "EXPNAME", rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->err.hdr), "ROOTNAME",rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->err.hdr), "EXPNAME", rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->dq.hdr),  "ROOTNAME",rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->dq.hdr),  "EXPNAME", rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->smpl.hdr),"ROOTNAME",rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->smpl.hdr),"EXPNAME", rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->intg.hdr),"ROOTNAME",rname,""))
	    return (status = 1);
	if (putKeyS (&(mosaic->intg.hdr),"EXPNAME", rname,""))
	    return (status = 1);

	/* Update the DATAMIN/MAX keywords in each extension header */
	if (updMinMaxf (&mosaic->sci))
	    return (status);
	if (updMinMaxf (&mosaic->err))
	    return (status);
	if (updMinMaxs (&mosaic->dq))
	    return (status);
	if (updMinMaxs (&mosaic->smpl))
	    return (status);
	if (updMinMaxf (&mosaic->intg))
	    return (status);

	/* Update the WCS keywords:
	** First, set the CRPIX values to the center of the image */
	crpix1 = (float)(mosaic->sci.data.nx + 1) / 2.0;
	crpix2 = (float)(mosaic->sci.data.ny + 1) / 2.0;

	/* Update the CRPIX keywords in each extension header */
	if (putKeyF (&(mosaic->sci.hdr),  "CRPIX1",crpix1,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->sci.hdr),  "CRPIX2",crpix2,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->err.hdr),  "CRPIX1",crpix1,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->err.hdr),  "CRPIX2",crpix2,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->dq.hdr),   "CRPIX1",crpix1,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->dq.hdr),   "CRPIX2",crpix2,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->smpl.hdr), "CRPIX1",crpix1,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->smpl.hdr), "CRPIX2",crpix2,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->intg.hdr), "CRPIX1",crpix1,""))
	    return (status = 1);
	if (putKeyF (&(mosaic->intg.hdr), "CRPIX2",crpix2,""))
	    return (status = 1);

	/* Find the first input member that belongs to this mosaic */
	for (i = 0; i < asn->nmembers; i++) {
	     if (asn->member[i].mospos == mosnum+1)
		 break;
	}

	/* Now find the location of the new CRPIX relative to the
	** reference image */
	crpix1 = crpix1 - asn->member[i].xi;
	crpix2 = crpix2 - asn->member[i].yi;

	/* And then compute the CRVAL values for this location */
	if (n_xy2rd (crpix1, crpix2, &(asn->member[i].wcs), &crval1, &crval2))
	    return (status = 1);
		 
	/* Update the CRVAL keywords in each extension header */
	if (putKeyD (&(mosaic->sci.hdr),  "CRVAL1",crval1,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->sci.hdr),  "CRVAL2",crval2,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->err.hdr),  "CRVAL1",crval1,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->err.hdr),  "CRVAL2",crval2,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->dq.hdr),   "CRVAL1",crval1,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->dq.hdr),   "CRVAL2",crval2,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->smpl.hdr), "CRVAL1",crval1,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->smpl.hdr), "CRVAL2",crval2,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->intg.hdr), "CRVAL1",crval1,""))
	    return (status = 1);
	if (putKeyD (&(mosaic->intg.hdr), "CRVAL2",crval2,""))
	    return (status = 1);

	/* Successful return */
	return (status = 0);

}

