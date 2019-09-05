# include <stdio.h>
# include <string.h>

# include <ximio.h>	/* defines IRAF imio functions */
# include "nicmos.h"

/* N_IMAGEIO: Contains routines for reading and writing image data.
** These are the routines:
**
** n_getPriHdr : Reads the primary header of a file.
**
** n_putPriHdr:  Write the primary header of a file.
**
** n_getRawData: Reads one group of raw data from input file.
**
** n_putCalData: Writes one group of calibrated data to output file.
**
** n_putMultiCalData: Writes multiple groups of calibrated data to output file.
**
** n_copyGroup: Copy the contents of one group to a new one.
**
** n_copyDQ: Copy the contents of one DQ array to a new one.
**
** n_replaceDQ: Copy the contents of one DQ array to an existing one.
**
** Revision history:
** ----------------
**
** H.Bushouse	26-Mar-1999	Initial implementation based on CALNICA routine
*/

/* N_GETPRIHDR: Read the primary header from a file. */

int n_getPriHdr (char *file, Hdr *PriHdr) {

/* Arguments:
**	file	i: file name
**	PriHdr	o: header structure
*/

	/* Local variables */
	IODescPtr im;		/* file descriptor pointer */

	/* Open the image */
	im = openInputImage (file, "", 0);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't open input file %s\n", file);
	    n_error (MsgText);
	    return (1);
	}

	/* Initalize the header data structure */
	initHdr (PriHdr);

	/* Read the header */
	if (getHeader (im, PriHdr))
	    return (1);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't read input file %s\n", file);
	    n_error (MsgText);
	    closeImage (im);
	    freeHdr (PriHdr);
	    return (1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (0);
}

/* N_PUTPRIHDR: Write the primary header to a file. */

int n_putPriHdr (char *file, Hdr *PriHdr) {

/* Arguments:
**	file	i: file name
**	PriHdr	i: header structure
*/

	/* Local variables */
	IODescPtr im;		/* file descriptor pointer */
	int file_stat;		/* file status */

	/* Check for existence of output file */
	file_stat = ckNewFile (file);
	if (file_stat == -1) {
	    sprintf (MsgText, "Existing output file \"%s\" was deleted", file);
	    n_warn  (MsgText);
	} else if (file_stat > 0) {
	    sprintf (MsgText, "Output file \"%s\" already exists", file);
	    n_error (MsgText);
	    return  (1);
	}

	/* Open the image; this also writes the header */
	im = openOutputImage (file, "", 0, PriHdr, 0, 0, FITSBYTE);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't open file %s", file);
	    n_error (MsgText);
	    return (1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (0);
}

/* N_GETRAWDATA: Read raw data from input file. All groups are read. */

int n_getRawData (char *filename, MultiNicmosGroup *in, int nimsets) {

/* Arguments:
**	filename i: file name
**	in	 o: input image data
**	nimsets  i: number of IMSETS in file
*/

	int i; /* loop index */

	/* Initialize the input data structure */
	initMultiNicmosGroup (in);

	/* Allocate the group data structures */
	if (allocMultiNicmosGroup (in, nimsets))
	    return (1);

	/* Read the global header */
	if (getMultiNicmosGroupHdr (filename, in))
	    return (1);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't read input file %s\n", filename);
	    n_error (MsgText);
	    freeMultiNicmosGroup (in);
	    return (1);
	}

	/* Read the input data groups */
	for (i = 1; i <= nimsets; i++) {
	     if (getMultiNicmosGroup (in, i-1, i))
		 return (1);
	     if (hstio_err()) {
		 sprintf (MsgText, "Can't read input file %s\n", filename);
		 n_error (MsgText);
		 freeMultiNicmosGroup (in);
		 return (1);
	     }
	}

	/* Successful return */
	return (0);
}

/* N_PUTCALDATA: Write calibrated data to a single-group ouput file. */

int n_putCalData (SingleNicmosGroup *out, char *fname) {

/* Arguments:
**	out	i: image data to be written to file
**	fname	i: output file name
*/

	/* Function definitions */
	int updminmax (char *, int, SingleNicmosGroup *);

	/* Write a single group header and data */
	if (putSingleNicmosGroup (fname, out->group_num, out, 0))
	    return (1);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't write to output image %s\n", fname);
	    n_error (MsgText);
	    return (1);
	}

	/* Reset the DATAMIN/MAX keywords in the output file */
	if (updminmax (fname, out->group_num, out))
	    return (1);
	if (hstio_err()) {
	    sprintf (MsgText, "Can't update DATAMIN/MAX in %s\n", fname);
	    n_error (MsgText);
	    return (1);
	}

	/* Successful return */
	return (0);
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
	int updminmax (char *, int, SingleNicmosGroup *);

	/* Write the group headers and data */
	for (i = 0; i < out->ngroups; i++) {

	     if (putSingleNicmosGroup (fname, i+1, &(out->group[i]), 0))
		 return (1);
	     if (hstio_err()) {
		 sprintf (MsgText, "Can't write to output image %s\n", fname);
		 n_error (MsgText);
		 return (1);
	     }

	     /* Reset the DATAMIN/MAX keywords in the output file */
	     if (updminmax (fname, i+1, &(out->group[i])))
		 return (1);
	     if (hstio_err()) {
		 sprintf (MsgText, "Can't update DATAMIN/MAX in %s\n", fname);
		 n_error (MsgText);
		 return (1);
	     }
	}

	/* Successful return */
	return (0);
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
		     return (1);
		 }
	         getKeyF (&(image->sci.hdr),  "DATAMIN", &datamin);
	         getKeyF (&(image->sci.hdr),  "DATAMAX", &datamax);
	     } else if (i==2) {
		 sprintf (&(tmp[strlen(tmp)]), "[err,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     return (1);
		 }
	         getKeyF (&(image->err.hdr),  "DATAMIN", &datamin);
	         getKeyF (&(image->err.hdr),  "DATAMAX", &datamax);
	     } else if (i==3) {
		 sprintf (&(tmp[strlen(tmp)]), "[dq,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     return (1);
		 }
	         getKeyF (&(image->dq.hdr),   "DATAMIN", &datamin);
	         getKeyF (&(image->dq.hdr),   "DATAMAX", &datamax);
	     } else if (i==4) {
		 sprintf (&(tmp[strlen(tmp)]), "[samp,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     return (1);
		 }
	         getKeyF (&(image->smpl.hdr), "DATAMIN", &datamin);
	         getKeyF (&(image->smpl.hdr), "DATAMAX", &datamax);
	     } else if (i==5) {
		 sprintf (&(tmp[strlen(tmp)]), "[time,%d,noinherit]",extver);
		 im = c_immap (tmp, IRAF_READ_WRITE, 0);
		 if (c_iraferr()) {
		     return (1);
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
	return (0);
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
            sprintf (MsgText, "in copyGroup; can't allocate new group\n");
            n_error (MsgText);
            return (1);
        }
 
	/* Copy the filename */
	to->filename = (char *)calloc((strlen(from->filename)+1),sizeof(char));
	strcpy (to->filename, from->filename);

	/* Copy the group number */
	to->group_num = from->group_num;

        /* Copy the global and extension headers */
        if (copyHdr (to->globalhdr,   from->globalhdr))   return(1);
        if (copyHdr (&(to->sci.hdr),  &(from->sci.hdr)))  return(1);
        if (copyHdr (&(to->err.hdr),  &(from->err.hdr)))  return(1);
        if (copyHdr (&(to->dq.hdr),   &(from->dq.hdr)))   return(1);
        if (copyHdr (&(to->smpl.hdr), &(from->smpl.hdr))) return(1);
        if (copyHdr (&(to->intg.hdr), &(from->intg.hdr))) return(1);
 
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
        return (0);
}

/* N_COPYDQ: Copy one DQ array to another.
** Initializes and allocates the structure for the new array.
*/
 
int n_copyDQ (ShortTwoDArray *to, ShortTwoDArray *from) {
 
/* Arguments:
**      to      o: new array
**      from    i: array to be copied
*/
 
        /* Initialize and allocate new array structure */
        initShortData (to);
        if (allocShortData (to, from->nx, from->ny) == -1) {
            sprintf (MsgText, "in copyDQ; can't allocate new array\n");
            n_error (MsgText);
            return (1);
        }
 
        /* Copy the data array */
        memcpy (to->data, from->data, from->nx*from->ny*sizeof(short));
 
        /* Successful return */
        return (0);
}

/* N_REPLACEDQ: Replace one DQ array with another.
** The structure for the array being copied from is freed.
*/
 
int n_replaceDQ (ShortTwoDArray *to, ShortTwoDArray *from) {
 
/* Arguments:
**      to     io: array to be copied into
**      from    i: array to be copied from
*/
 
        /* Copy the data array */
        memcpy (to->data, from->data, from->nx*from->ny*sizeof(short));
 
	freeShortData (from);

        /* Successful return */
        return (0);
}

