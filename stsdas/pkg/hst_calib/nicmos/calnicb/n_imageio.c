# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <ximio.h>	/* defines IRAF imio functions */
# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structure */
# include "calnicb.h"	/* defines CALNICB data structure */

/* N_IMAGEIO: Contains routines for reading and writing image
** data to be calibrated. These are the routines:
**
** n_getPriHdr: Reads the primary header of a file.
**
** n_putPriHdr: Write the primary header of a file.
**
** n_getInputData: Read all assocation member datasets.
**
** n_putMosData: Write an output mosaic image.
**
** n_mkSPT: Make an SPT file to go with a mosaic image.
**
** n_ckImgSize: Checks the size of input images to make sure they're
**		what we expect for NICMOS.
**
** updMinMaxf: Updates DATAMIN/MAX keywords in a float image.
**
** updMinMaxs: Updates DATAMIN/MAX keywords in a short image.
**
** updminmax: Inserts the DATAMIN/MAX keywords in an output image.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	Oct 1996	Added n_mkSPT routine (Version 0.1.4)
** H.Bushouse	30-Jan-1997	Modified n_putPriHdr and n_putMosData to use
**				new HSTIO ckNewFile routine (Version 2.0)
** H.Bushouse	March 1997	Modified n_mkSPT to only issue a warning if
**				input SPT not found, rather than stopping
**				(Version 0.1.5)
** H.Bushouse	28-Apr-1997	Removed mosnum from n_putMosData argument list;
**				removed construction of MOS filename from
**				n_putMosData; modified n_mkSPT to update
**				FILENAME keyword (Version 0.1.6)
** H.Bushouse	29-Apr-1997	Changed MemberInfo.imgtype to mtype
**				(Version 2.0)
** H.Bushouse	06-May-1997	Changed single quotes (') to double (") in all
**				output strings (Version 2.0)
** H.Bushouse	02-Dec-1997	Removed AsnInfo and mosnum from n_putMosData
**                              argument list (Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 2.2.1)
** H.Bushouse	19-May-1999	Added updminmax routine to n_putMosData to
**				update DATAMIN/MAX keywords in output _mos
**				file. Added updMinMaxf and updMinMaxs routines.
**				(Version 2.3)
**
**M. Sosey      06-nov-2008 updated to keep track of some exposure 
**              information for the whole association.
**
**
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
	    n_openerr (file);
	    return (status = 1);
	}

	/* Initialize the header data structure */
	initHdr (PriHdr);

	/* Read the header */
	if (getHeader (im, PriHdr))
	    status = 1;
	if (hstio_err() || status) {
	    n_readerr (file);
	    closeImage (im);
	    freeHdr (PriHdr);
	    return (status = 1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (status = 0);
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
	    return  (status = 1);
	}

	/* Open the image; this also writes the header */
	im = openOutputImage (file, "", 0, PriHdr, 0, 0, FITSBYTE);
	if (hstio_err()) {
	    n_openerr (file);
	    return (status = 1);
	}

	/* Close the image */
	closeImage (im);

	/* Successful return */
	return (status = 0);
}

/* N_GETINPUTDATA: Read image data from all the input files.
** Only the first group (EXTVER=1) is read from each file.
** Also record some exposure information for the set of 
** images to the asn structure.
*/

int n_getInputData (AsnInfo *asn, AsnImages *in) {

/* Arguments:
**	asn	i: association info structure
**	in	o: input image data
*/

	/* Local variables */
	int i;				/* loop index */
	char fname[SZ_NAME+1];		/* file name */
	float exptime;
    double expstart;
    double expend;
    char asnmtyp[SZ_NAME+1];
    Hdr hdr;
    int p;              /* loop index for product members */
    
	/* Function definitions */
	void n_initAsnImages (AsnImages *);
	int  n_allocAsnImages (AsnImages *, int);
	void n_freeAsnImages (AsnImages *);

	/* Initialize the input data structure */
	n_initAsnImages (in);

	/* Allocate the member data structures */
	if (n_allocAsnImages (in, asn->nmembers))
	    return (status = 1);

	/* Read the input members */
	sprintf (MsgText, "Reading input images ...");
	n_message (MsgText);

    /*set some defaults */
    asn->exptime=0.;
    asn->expstart=5e6; /*should be way bigger than anything else*/
    asn->expend=0.;
    exptime=0.0;
    expstart=0.0;
    expend=0.0;
    
    /* Loop over all EXP-TARG/EXP-PROD association members */
	for (i = 0; i < asn->nmembers; i++) {
	     fname[0] = '\0';
	     sprintf (fname, "%s%s", asn->member[i].name, "_cal.fits");
	     if (getSingleNicmosGroup(fname, 1, &(in->member[i])))
		 status = 1;
	     if (hstio_err() || status) {
		 	n_filerr (fname);
		 	n_freeAsnImages (in);
		    return (status = 1);
	     }
         /*grab some header information for the structure as well*/
         if (n_getPriHdr (fname, &hdr)){
            sprintf(MsgText,"Problem getting member header");
            n_error(MsgText);
         }
         /*
         The following collections of exptime related keywords should
         only be obtained for science exposures; specifically, only
         those exposures which do not have ASN_MTYP[7] == 'EXP-BCK'.
         This will insure that association products generated from 
         chop patterns will have the correct end time and total exptime.
         22-Jan-2009 WJH
         */
         asnmtyp[0] = '\0';
         if (getKeyS (&hdr, "ASN_MTYP", asnmtyp)){
                sprintf(MsgText,"Problem getting ASN_MTYP from member header");
                n_error(MsgText);
                return(status);
         }
         if (getKeyF (&hdr,  "EXPTIME", &exptime)){
                sprintf(MsgText,"Problem getting EXPTIME from member header");
                n_error(MsgText);
                return(status);
         }
         if (getKeyD (&hdr,  "EXPSTART", &expstart)){
                sprintf(MsgText,"Problem getting EXPSTART from member header");
                n_error(MsgText);
                return(status);
         }
         if (getKeyD (&hdr,  "EXPEND", &expend)){
                sprintf(MsgText,"Problem getting EXPEND from member header");
                n_error(MsgText);
                return(status);
         }

         /* Compile the start/end and total exptimes for each
         product, prod-targ and all prod-bck products. 
         If we are working with a science exposure...
         */
         if (strncmp(asnmtyp, "EXP-TARG", 8) == 0){          
            /* use the values to update the prod-targ results, as before */
            asn->exptime = asn->exptime + exptime;

            if (expstart < asn->expstart){
                asn->expstart = expstart;
            }
            if(expend > asn->expend){
                asn->expend = expend;
            }
         /* End EXP-TARG case */
         } else { 
         /*... else we are working with a background product.
               We need to match these values with the correct product.
         */
            /* Start by keeping track of each input background exposure's 
              expstart/end/time values.
            */
            asn->member[i].expstart = expstart;
            asn->member[i].expend = expend;
            asn->member[i].exptime = exptime;
            /* Now, update product for this background with values 
               Start by looping over all association entries to find
               the entry for the associated product.
            */   
            for (p = 0; p < asn->tmembers; p++) {
                if (asn->member[p].bkgprod == asn->member[i].bkgid){
                    /*We have found the product associated with this background 
                      exposure, so update the exptime information.
                    */
                    asn->member[p].exptime += exptime;
                    
                    if ((asn->member[p].expstart < 0) || (expstart < asn->member[p].expstart)){
                        asn->member[p].expstart = expstart;
                    }
                    if (expend > asn->member[p].expend){
                        asn->member[p].expend = expend;
                    }
                } /*End of prod-bck expstart/end/time update */
            } /* Finished looping over members looking for prod-bck */

         } /* End EXP-BCK case */
   	}

	/* Successful return */
	return (status = 0);
}

/* N_PUTMOSDATA: Write the image data for a single mosaic file. */

int n_putMosData (SingleNicmosGroup *image) {

/* Arguments:
**	image	i: image to be written
*/

	/* Local variables */
	int file_stat;			/* file status */

	/* Function definitions */
	int updminmax (char *, int, SingleNicmosGroup *);

	/* Check for existence of output file */
	file_stat = ckNewFile(image->filename);
	if (file_stat == -1) {
	    sprintf (MsgText, "Existing output file \"%s\" was deleted",
		     image->filename);
	    n_warn  (MsgText);
	} else if (file_stat > 0) {
	    sprintf (MsgText, "Output file \"%s\" already exists",
		     image->filename);
	    n_error (MsgText);
	    return (status = 1);
	}
	
	/* Write the data to the output file */
	sprintf (MsgText, "Writing image %s", image->filename);
	n_message (MsgText);

	if (putSingleNicmosGroup (image->filename, 1, image, 0))
	    status = 1;
	if (hstio_err() || status) {
	    sprintf (MsgText, "Can't write to output file %s", image->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Reset the DATAMIN/MAX keywords in the output file */
	if (updminmax (image->filename, 1, image))
	    status = 1;
	if (hstio_err() || status) {
	    sprintf (MsgText, "Can't update DATAMIN/MAX in %s",image->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_MKSPT: Make an SPT file to go with a mosaic output product.
** Also update pertinent keywords in the SPT primary header.
*/

int n_mkSPT (AsnInfo *asn, int mosnum) {

/* Arguments:
**	asn	 i: association info structure
**	mosnum	 i: mosaic number we're working on
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int k;			/* mosaic index within ASN list */
	Hdr header;		/* SPT header */
	char fname[SZ_NAME+1];	/* SPT filename */
	char rname[SZ_NAME+1];	/* SPT rootname */
	char obsnum[4];		/* Observation number string */
	FILE *fp;		/* file pointer */

	/* Function definitions */
	int n_getPriHdr (char *, Hdr *);
	int n_putPriHdr (char *, Hdr *);

	/* Find the first input image that belongs to this mosaic */
	for (i = 0; i < asn->nmembers; i++) {
	     if (asn->member[i].mospos == mosnum+1)
		 break;
	}

	/* Construct the SPT file name that goes with this input (CAL) file */
	fname[0] = '\0';
	sprintf (fname, "%s%s", asn->member[i].name, "_spt.fits");

	/* Check for existence of this SPT file */
	if ((fp = fopen (fname, "rb")) == NULL) {
	    sprintf (MsgText, "Can't find input file \"%s;\"", fname);
	    n_warn  (MsgText);
	    sprintf (MsgText, "SPT file for mosaic %d will not be made",
		     mosnum+1);
	    n_warn  (MsgText);
	    return (status = 0);   /* don't abort */
	} else
	    fclose (fp);

	/* Read the primary header of the input SPT file */
	if (n_getPriHdr (fname, &header))
	    return (status);
		 
	/* Figure out the location of this mosaic within
	** the list of files in the ASN table */
	k = asn->nmembers + mosnum;

	/* Construct the rootname for the new SPT file */
	rname[0] = '\0';
	strcpy (rname, asn->member[k].name);

	/* Construct the full file name for the new SPT file */
	fname[0] = '\0';
	sprintf (fname, "%s%s", rname, "_spt.fits");

	/* Update the FILENAME header keyword */
	if (putKeyS (&header, "FILENAME", fname, ""))
	    return (status = 1);

	/* Update the ROOTNAME header keyword */
	for (j = 0; j < strlen(rname); j++)
	     rname[j] = toupper(rname[j]);
	if (putKeyS (&header, "ROOTNAME", rname, ""))
	    return (status = 1);

	/* Update the OBSERVTN header keyword */
	strncpy (obsnum, &rname[6], 3); obsnum[3] = '\0';
	if (putKeyS (&header, "OBSERVTN", obsnum, ""))
	    return (status = 1);
			
	/* Update the NEXTEND header keyword */
	if (putKeyI (&header, "NEXTEND", 0, ""))
	    return (status = 1);

	/* Update the ASN_MTYP header keyword */
	if (putKeyS (&header, "ASN_MTYP", asn->member[k].mtype, ""))
	    return (status = 1);

	/* Write the new SPT file */
	if (n_putPriHdr (fname, &header))
	    return (status);

	/* Successful return */
	return (status = 0);

}

/* N_CKIMGSIZE: Check the size of all images in a group to make sure that
** they're the right size for a NICMOS image */
 
int n_ckImgSize (SingleNicmosGroup *in) {
 
/* Arguments:
**      in      i: input image group
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

void n_initAsnImages (AsnImages *asn) {
	asn->nmembers = 0;
	asn->member = NULL;
}

int n_allocAsnImages (AsnImages *asn, int n) {
	int i;
	void n_freeAsnImages (AsnImages *);

	if (asn->member != NULL)
	    n_freeAsnImages (asn);

	asn->nmembers = n;
	asn->member = (SingleNicmosGroup *)calloc(n,sizeof(SingleNicmosGroup));
	
	if (asn->member == NULL) {
	    asn->nmembers = 0;
	    sprintf (MsgText, "Can't allocate memory for input images");
	    n_error (MsgText);
	    return (status = 1);
	}

	for (i=0; i < asn->nmembers; i++)
	     initSingleNicmosGroup (&(asn->member[i]));

	return (status = 0);
}

void n_freeAsnImages (AsnImages *asn) {
	int i;
	for (i=0; i < asn->nmembers; i++)
	     freeSingleNicmosGroup (&(asn->member[i]));

	free (asn->member);
	n_initAsnImages (asn);
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
**      fname    i: file name
**      extver   i: extension version number
**      image   io: image to be updated
*/
 
        /* Local variables */
        int i;                          /* loop index */
        float datamin, datamax;         /* datamin/max values */
        IRAFPointer im;                 /* image pointer */
        char tmp[60];                   /* full file name */
 
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

int updMinMaxf (FloatHdrData *image) {
 
        /* Local variables */
        float min, max;         /* data min and max */
 
        /* Function definitions */
        void compMinMaxf (FloatTwoDArray *, float *, float *);
 
        /* Compute the min and max of the data array */
        compMinMaxf (&image->data, &min, &max);
 
        /* Update the DATAMIN keyword */
        if (putKeyF (&image->hdr, "DATAMIN", min, ""))
            return (status = 1);
 
        /* Update the DATAMAX keyword */
        if (putKeyF (&image->hdr, "DATAMAX", max, ""))
            return (status = 1);
 
        /* Successful return */
        return (status = 0);
}
 
int updMinMaxs (ShortHdrData *image) {
 
        /* Local variables */
        float min, max;         /* data min and max */
 
        /* Function definitions */
        void compMinMaxs (ShortTwoDArray *, float *, float *);
 
        /* Compute the min and max of the data array */
        compMinMaxs (&image->data, &min, &max);
 
        /* Update the DATAMIN keyword */
        if (putKeyF (&image->hdr, "DATAMIN", min, ""))
            return (status = 1);
 
        /* Update the DATAMAX keyword */
        if (putKeyF (&image->hdr, "DATAMAX", max, ""))
            return (status = 1);
 
        /* Successful return */
        return (status = 0);
}
 
void compMinMaxf (FloatTwoDArray *data, float *min, float *max) {
 
        /* Local variables */
        int i, j;               /* pixel indexes */
        float val;              /* pixel value */
 
        /* Compute the min and max of the data array */
        val = PPix(data,0,0);
        *min = val; *max = val;
        for (j=0; j<data->ny; j++) {
             for (i=0; i<data->nx; i++) {
                  val = PPix(data,i,j);
                  if (val < *min) *min = val;
                  if (val > *max) *max = val;
             }
        }
}
 
void compMinMaxs (ShortTwoDArray *data, float *min, float *max) {
 
        /* Local variables */
        int i, j;               /* pixel indexes */
        float val;              /* pixel value */
 
        /* Compute the min and max of the data array */
        val = PPix(data,0,0);
        *min = val; *max = val;
        for (j=0; j<data->ny; j++) {
             for (i=0; i<data->nx; i++) {
                  val = PPix(data,i,j);
                  if (val < *min) *min = val;
                  if (val > *max) *max = val;
             }
        }
}

