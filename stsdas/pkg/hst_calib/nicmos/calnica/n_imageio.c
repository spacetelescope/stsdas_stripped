# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <ximio.h>	/* defines IRAF imio functions */
# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structure */
# include "calnica.h"	/* defines CALNICA data structure */

/* N_IMAGEIO: Contains routines for reading and writing image
** data to be calibrated. These are the routines:
**
** n_getPriHdr : Reads the primary header of an input file.
**
** n_getRawData: Reads one group of raw data from input file.
**
** n_putCalData: Writes one group of calibrated data to output file.
**
** n_putMultiCalData: Writes multiple groups of calibrated data to output file.
**
** n_ckImgSize:  Checks the size of input images to make sure they're
**               what we expect for NICMOS.
**
** n_copyGroup: Copy the contents of one group to a new one.
**
** n_copytoSingleNicmosGroup:  Copies FloatHdrData structure to
**                             SingleNicmosGroup
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2
** H.Bushouse	28-Apr-1997	Added filename arg to updateHdr function
** H.Bushouse	28-Jul-1997	Moved call to n_getGroupInfo out of n_calnica
**				into n_getRawData (Version 3.0)
** H.Bushouse	10-Sep-1998	Fixed bug in n_copyGroup: instead of setting
**				the to and from filename pointers equal to one
**				another, allocate memory and do a strcpy for
**				the to->filename (Version 3.2.1)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	20-Jun-2000	Removed nic from n_putCalData,
**				n_putMultiCalData, and n_updateHdr argument
**				lists (Version 4.0)
** R. Jedrzeje  14-Jun-2001     Changed first argument to character string
**
**M. Sosey      26-Oct-2008  I added a new function (n_getImsetHdr 
**				to return the pointer to a specific imset header in an MEF
**


*/


/*N_GETIMSETHDR: Read in the header from a specific input file*/
int n_getImsetHdr(char *filename, Hdr *imsetHdr, char *extname, int extver){

/* Arguments:
**	filename	    i: file name of image
**	imsetHdr	    o: header structure
**  extname			i: the name of the extension ("SCI")
**  extver			i: the exposure number
*/

	/* Local variables */
	IODescPtr im;	/*file descriptor pointer*/
    
    im=openInputImage(filename,extname,extver);
    if(hstio_err()){
	    n_openerr (filename);
	    return (status = 1);
	}
    
    initHdr(imsetHdr);
	if (getHeader (im, imsetHdr))
	    status = 1;
	if (hstio_err() || status) {
	    n_readerr (filename);
	    closeImage (im);
	    freeHdr (imsetHdr);
	    return (status = 1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (status = 0);

}


/* N_GETPRIHDR: Read primary header from an input file.  */

int n_getPriHdr (char *filename, Hdr *PriHdr) {

/* Arguments:
**	filename	i: file name
**	PriHdr	        o: header structure
*/

	/* Local variables */
	IODescPtr im;		/* file descriptor pointer */

	/* Open the image */
	im = openInputImage (filename, "", 0);
	if (hstio_err()) {
	    n_openerr (filename);
	    return (status = 1);
	}

	/* Initialize the header data structure */
	initHdr (PriHdr);

	/* Read the header */
	if (getHeader (im, PriHdr))
	    status = 1;
	if (hstio_err() || status) {
	    n_readerr (filename);
	    closeImage (im);
	    freeHdr (PriHdr);
	    return (status = 1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (status = 0);
}

/* N_GETRAWDATA: Read raw data from input file. One group is read. */

int n_getRawData (NicInfo *nic, MultiNicmosGroup *in) {

/* Arguments:
**	nic	i: NICMOS info structure
**	in	o: input image data
*/

	/* Function definitions */
	int n_ckImgSize (SingleNicmosGroup *);
	int n_getGroupInfo (NicInfo *, SingleNicmosGroup *);

	/* Initialize the input data structure */
	initMultiNicmosGroup (in);

	/* Allocate the group data structures */
	if (allocMultiNicmosGroup (in, nic->ngroups))
	    return (status = 1);

	/* Read the global header */
	if (getMultiNicmosGroupHdr (nic->filename, in))
	    status = 1;
	if (hstio_err() || status) {
	    n_filerr (nic->filename);
	    freeMultiNicmosGroup (in);
	    return (status = 1);
	}

	/* Read the input data groups */
	for (nic->group = 1; nic->group <= nic->ngroups; nic->group++) {
	     if (getMultiNicmosGroup (in, nic->group-1, nic->group))
		 status = 1;
	     if (hstio_err() || status) {
		 n_filerr (nic->filename);
		 freeMultiNicmosGroup (in);
		 return (status = 1);
	     }

	     /* Check the image sizes */
	     if (n_ckImgSize (&(in->group[nic->group-1]))) {
		 freeMultiNicmosGroup (in);
		 return (status);
	     }

	     /* Get group-specific information from the file headers
	     ** and check for null input data */
	     if (n_getGroupInfo (nic, &(in->group[nic->group-1]))) {
		 freeMultiNicmosGroup (in);
		 return (status);
	     }
	}

	/* Successful return */
	return (status = 0);
}

/* N_PUTCALDATA: Write calibrated data to a single-group ouput file. */

int n_putCalData (SingleNicmosGroup *out, char *fname) {

/* Arguments:
**	out	i: image data to be written to file
**	fname	i: output file name
*/

	/* Function definitions */
	int n_updateHdr (SingleNicmosGroup *, char *);
	int updminmax (char *, int, SingleNicmosGroup *);

	/* Update output image header keywords */
	if (n_updateHdr (out, fname))
	    return (status);

	/* Write a single group header and data */
	if (putSingleNicmosGroup (fname, out->group_num, out, 0))
	    status = 1;
	if (hstio_err() || status) {
	    sprintf (MsgText, "Can't write to output image %s", fname);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Reset the DATAMIN/MAX keywords in the output file */
	if (updminmax (fname, out->group_num, out))
	    status = 1;
	if (hstio_err() || status) {
	    sprintf (MsgText, "Can't update DATAMIN/MAX in %s", fname);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_PUTMULTICALDATA: Write multiple groups of calibrated data to ouput file.
** The file is created and the primary header is written when writing the 
** first data group. */

int n_putMultiCalData (MultiNicmosGroup *out, char *fname) {

/* Arguments:
**	out	i: image data to be written to file
**	fname	i: output file name
*/

	/* Local variables */
	int i;				/* loop index */

	/* Function definitions */
	int n_updateHdr (SingleNicmosGroup *, char *);
	int updminmax (char *, int, SingleNicmosGroup *);

	/* Update output image header keywords */
	for (i = 0; i < out->ngroups; i++) {
	     if (n_updateHdr (&(out->group[i]), fname))
		 return (status);
	}

	/* Write the group headers and data */
	for (i = 0; i < out->ngroups; i++) {

	     if (putSingleNicmosGroup (fname, i+1, &(out->group[i]), 0))
		 status = 1;
	     if (hstio_err() || status) {
		 sprintf (MsgText, "Can't write to output image %s", fname);
		 n_error (MsgText);
		 return (status = 1);
	     }

	     /* Reset the DATAMIN/MAX keywords in the output file */
	     if (updminmax (fname, i+1, &(out->group[i])))
		 status = 1;
	     if (hstio_err() || status) {
		 sprintf (MsgText, "Can't update DATAMIN/MAX in %s", fname);
		 n_error (MsgText);
		 return (status = 1);
	     }
	}

	/* Successful return */
	return (status = 0);
}

/* UPDMINMAX: Reset the values of the DATAMIN/DATAMAX keywords in an image
** header. This is a kludge that is necessitated by the fact that the IRAF
** FITS image kernel is hardwired to overwrite the values of the DATAMIN/MAX
** keywords with the IRAF imio IM_MIN/IM_MAX values whenever it writes an
** image. So the previous values that we had computed and set in memory for
** DATAMIN/MAX are overwritten when HSTIO functions are used to write the
** image. We must reopen the image, get the IRAF image pointer out of the
** HSTIO file descriptor structure (so that we have access to the imhdr
** elements), set the values of IM_MIN/IM_MAX, and then close the image.
** The DATAMIN/MAX values get updated by the IRAF FITS kernel when the image
** is closed. */

int updminmax (char *fname, int extver, SingleNicmosGroup *image) {

/* Arguments:
**	fname	 i: file name
**	extver	 i: extension version number
**	image	io: image to be updated
*/

	/* Local variables */
	int i;				/* loop index */
	float datamin, datamax;		/* datamin/max values */
	IRAFPointer im;			/* image pointer */
	char tmp[60];			/* full file name */

	/* Loop over the 5 extensions in the group */
	for (i=1; i<=5; i++) {

	     /* Open the extension and read the datamin/max values that
	     ** we computed earlier and stored in memory */
	     strcpy (tmp, fname);
	     if (i==1) {
		 sprintf (&(tmp[strlen(tmp)]), "[sci,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     n_openerr (tmp);
		     return (status = 1);
		 }
	         getKeyF (&(image->sci.hdr),  "DATAMIN", &datamin);
	         getKeyF (&(image->sci.hdr),  "DATAMAX", &datamax);
	     } else if (i==2) {
		 sprintf (&(tmp[strlen(tmp)]), "[err,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     n_openerr (tmp);
		     return (status = 1);
		 }
	         getKeyF (&(image->err.hdr),  "DATAMIN", &datamin);
	         getKeyF (&(image->err.hdr),  "DATAMAX", &datamax);
	     } else if (i==3) {
		 sprintf (&(tmp[strlen(tmp)]), "[dq,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     n_openerr (tmp);
		     return (status = 1);
		 }
	         getKeyF (&(image->dq.hdr),   "DATAMIN", &datamin);
	         getKeyF (&(image->dq.hdr),   "DATAMAX", &datamax);
	     } else if (i==4) {
		 sprintf (&(tmp[strlen(tmp)]), "[samp,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     n_openerr (tmp);
		     return (status = 1);
		 }
	         getKeyF (&(image->smpl.hdr), "DATAMIN", &datamin);
	         getKeyF (&(image->smpl.hdr), "DATAMAX", &datamax);
	     } else if (i==5) {
		 sprintf (&(tmp[strlen(tmp)]), "[time,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     n_openerr (tmp);
		     return (status = 1);
		 }
	         getKeyF (&(image->intg.hdr), "DATAMIN", &datamin);
	         getKeyF (&(image->intg.hdr), "DATAMAX", &datamax);
	     }

	     /* Set the IRAF imio IM_MIN and IM_MAX values */
	     c_impmin (im, datamin);
	     c_impmax (im, datamax);

	     /* Reset the IRAF IM_LIMTIME to be greater than the IM_MTIME,
	     ** so that the DATAMIN/MAX keywords get updated. */
	     c_implimtime (im, c_imgmtime(im)+1);

	     /* Close the image */
	     c_imunmap (im);
	}

	/* Successful return */
	return (status = 0);
}

/* N_CKIMGSIZE: Check the size of all images in a group to make sure that
** they're the right size for a NICMOS image */

int n_ckImgSize (SingleNicmosGroup *in) {

/* Arguments:
**	in	i: input image group
*/

	/* Check the science image size */
	if (in->sci.data.nx != SZ_NICIMG || in->sci.data.ny != SZ_NICIMG) {
	    sprintf (MsgText, "%s SCI image not correct size", in->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check the error image size */
	if (in->err.data.nx != SZ_NICIMG || in->err.data.ny != SZ_NICIMG) {
	    sprintf (MsgText, "%s ERR image not correct size", in->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check the data quality image size */
	if (in->dq.data.nx != SZ_NICIMG || in->dq.data.ny != SZ_NICIMG) {
	    sprintf (MsgText, "%s DQ image not correct size", in->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check the number of samples image size */
	if (in->smpl.data.nx != SZ_NICIMG || in->smpl.data.ny != SZ_NICIMG) {
	    sprintf (MsgText, "%s SAMP image not correct size", in->filename);
	    n_error (MsgText);
	    status = 1;
	}

	/* Check the integration time image size */
	if (in->intg.data.nx != SZ_NICIMG || in->intg.data.ny != SZ_NICIMG) {
	    sprintf (MsgText, "%s TIME image not correct size", in->filename);
	    n_error (MsgText);
	    status = 1;
	}

	return (status);
}

/* N_COPYGROUP: Copy one group structure to another.
** Initializes and allocates the structure for the new group.
*/
 
int n_copyGroup (SingleNicmosGroup *to, SingleNicmosGroup *from) {
 
/* Arguments:
**      to      o: new group
**      from    i: group to be copied
*/
 
        /* Initialize and allocate new group structure */
        initSingleNicmosGroup  (to);
        if (allocSingleNicmosGroup 
		(to, from->sci.data.nx, from->sci.data.ny) == -1) {
            sprintf (MsgText, "in copyGroup; can't allocate new group");
            n_error (MsgText);
            return (status = 1);
        }
 
	/* Copy the filename */
	to->filename = (char *)calloc((strlen(from->filename)+1),sizeof(char));
	strcpy (to->filename, from->filename);

	/* Copy the group number */
	to->group_num = from->group_num;

        /* Copy the global and extension headers */
        if (copyHdr (to->globalhdr,   from->globalhdr))   return(status=1);
        if (copyHdr (&(to->sci.hdr),  &(from->sci.hdr)))  return(status=1);
        if (copyHdr (&(to->err.hdr),  &(from->err.hdr)))  return(status=1);
        if (copyHdr (&(to->dq.hdr),   &(from->dq.hdr)))   return(status=1);
        if (copyHdr (&(to->smpl.hdr), &(from->smpl.hdr))) return(status=1);
        if (copyHdr (&(to->intg.hdr), &(from->intg.hdr))) return(status=1);
 
        /* Copy the extension images data arrays */
        memcpy (to->sci.data.data, from->sci.data.data,
                from->sci.data.nx*from->sci.data.ny*sizeof(float));
        memcpy (to->err.data.data, from->err.data.data,
                from->sci.data.nx*from->sci.data.ny*sizeof(float));
        memcpy (to->dq.data.data,  from->dq.data.data,
                from->sci.data.nx*from->sci.data.ny*sizeof(short));
        memcpy (to->smpl.data.data,from->smpl.data.data,
                from->sci.data.nx*from->sci.data.ny*sizeof(short));
        memcpy (to->intg.data.data,from->intg.data.data,
                from->sci.data.nx*from->sci.data.ny*sizeof(float));
 
        /* Successful return */
        return (status = 0);
}


/* N_COPYTOSINGLENICMOSGROUP: Copy FloatHdrData structure to SingleNicmosGroup
** Initializes and allocates the structure for the new group.
*/
 
int n_copytoSingleNicmosGroup (SingleNicmosGroup *to, FloatHdrData from,
                               NicInfo *nic, int fromver) {
 
/* Arguments:
**      to      o: new group
**      from    i: FloatHdrData structure to be copied
**      filename i: File name
*/

        /*  Local variables   */
#
        Hdr PriHdr;
	char filename[SZ_NAME+1];
	strcpy (filename, nic->DARK.ref.name);
        /* Initialize and allocate new group structure */
	initSingleNicmosGroup  (to);

        if (allocSingleNicmosGroup 
		(to, from.data.nx, from.data.ny) == -1) {
            sprintf (MsgText, "in copyGroup; can't allocate new group");
            n_error (MsgText);
            return (status = 1);
        }
	

	/* Copy the group number */
	to->group_num = fromver;
	/* Get the primary header from the from structure  */
	if (n_getPriHdr (filename, &PriHdr)) {
	  sprintf (MsgText, "Error getting primary header");
	  n_error (MsgText);
	  return (status = 1);
	}
        /* Copy the global and extension headers */
        if (copyHdr (to->globalhdr,   &PriHdr))   return(status=1);
        if (copyHdr (&(to->sci.hdr),  &(from.hdr)))  return(status=1);
        /* Copy the extension images data arrays */
        memcpy (to->sci.data.data, from.data.data, 
                from.data.nx*from.data.ny*sizeof(float));
        memset (to->err.data.data, 0.0,
                from.data.nx*from.data.ny*sizeof(float));
        memset (to->dq.data.data,  0,
                from.data.nx*from.data.ny*sizeof(short));
        memset (to->smpl.data.data, 0,
                from.data.nx*from.data.ny*sizeof(short));
        memset (to->intg.data.data, 0.0,
                from.data.nx*from.data.ny*sizeof(float));
        /* Successful return */
        return (status = 0);
}

