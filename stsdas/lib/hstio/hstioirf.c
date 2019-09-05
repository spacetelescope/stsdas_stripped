/*
** Allen Farris - Original Implementation.
**
** M.D. De La Pena  25 February 1998: Modified use of "long" to "int" to 
** enforce compatibility with SPP/IRAF; declaration of linevector now seven
** elements all set to 1; removed linevector variable where not needed.  
** Changed references from 0L to 0.
**
** M.D. De La Pena  14 April 1998: Fixed a bug in put[Float/Short]Data when
** checking to determine if an image is constant.  PPix indices were reversed
** and non-square images where NAXIS1>NAXIS2 would not be identified constant.
**
** M.D. De La Pena, 15 April 1998: Added the putSectFloatData and 
** putSectShortData functions to support output of subsections of images
** from memory where the subsections are the full size of the new output files.
**
** M.D. De La Pena, 20 April 1998: Modified put[Float/Short]Data, and
** putSect[Float/Short]Data to check if NPIX1/NPIX2/PIXVALUE keywords are in
** the header of non-constant images.  If present, delete these keywords.
**
** M.D. De La Pena, 26 August 1998: Modified putSectFloatData and
** putSectShortData as the parameters xbeg and ybeg are zero-indexed.  
** 
** M.D. De La Pena, 09 November 1998:
** Retired the old get/put[Float/Short]Sect routines; these routines were
** meant to be used with the [Float/Short]TwoDArray data structure, but were
** never implemented as intended.  The putSect[Float/Short]Data routines have
** been renamed put[Float/Short]Sect.  Fixed a bug in checking for constant
** subimages in put[Float/Short]Sect.
**
** M.D. De La Pena, 18 August 1999:
** Modified openInputImage only to allow the opening of 1D images.  This is
** only for access to the header keywords.  Added void to functions which have 
** no parameters.
**
** M.D. De La Pena, 24 February 2000:
** Modified the return variable from strstr() to be compared to NULL instead
** of 0.  The openUpdateImage() function now opens the FITS image extensions
** with NOINHERIT.  The get[Float/Short]Line() and put[Float/Short]Line()
** routines now increment their line arguments.  The line argument passed into
** these routines should be in a zero-based reference system, but the line
** value must be incremented for the IRAF routines which are one-based system.
**
** M.D. De La Pena, 27 April 2000:
** Modified openUpdateImage, get[Float/Short]Data, put[Float/Short]Data, 
** put[Float/Short]Sect, and putHeader to accommodate fully performing I/O on
** one-dimensional images.
**
*/
# if defined(__VMS)
# if defined(__ALPHA)
# pragma extern_model common_block
# else
# pragma extern_model common_block shr
# endif
# endif

static void detect_iraferr(void) {
	sprintf(error_msg,"\nIRAF error %d: %s\n",c_iraferr(),
		c_iraferrmsg());
}

static void ioerr(HSTIOError e, IODescPtr x_) {
	IODesc *x;
	x = (IODesc *)x_;
	sprintf(&error_msg[strlen(error_msg)],
		"Filename %s EXTNAME %s EXTVER %d",
		x->filename, x->extname, x->extver);
	error(e,0);
}

/*
** Make_iodesc takes a filename, extname, and extver and creates and
** initializes an IODesc structure.  In the process, it builds a
** correct filename to be used in the open statement to IRAF.  This
** constructed filename is returned.
*/
static char *make_iodesc(IODesc **x, char *fname, char *ename, int ever) {
	int i, n, flen;
	char *tmp;
	IODesc *iodesc;
	char xname[9];

	iodesc = (IODesc *)calloc(1,sizeof(IODesc));
	if (iodesc == NULL) {
	    error(NOMEM,"Allocating I/O descriptor");
	    return NULL;
	}
	iodesc->fdesc = 0;
	iodesc->filename = NULL;
	iodesc->extname = NULL;
	iodesc->extver = 0;
	iodesc->hflag = 0;
	iodesc->hsize = 0;
	iodesc->hdr = NULL;
	iodesc->naxis1 = 0;
	iodesc->naxis2 = 0;
	iodesc->type = 0;
	if (fname == 0) fname = "";
	if (ename == 0) ename = "";
	iodesc->filename = (char *)calloc(((flen = strlen(fname)) + 1),
		sizeof(char));
	if (iodesc->filename == NULL) {
	    free(iodesc);
	    error(NOMEM,"Allocating I/O descriptor");
	    return NULL;
	}
	n = strlen(ename);
	if (n > 8) { ioerr(BADEXTNAME,iodesc); return NULL; }
	for (i = 0; i < n; ++i)
	    xname[i] = toupper(ename[i]);
	for (--i; i >= 0 && xname[i] == ' '; --i) ;
	++i;
	xname[i] = '\0';
	iodesc->extname = (char *)calloc((strlen(xname) + 1),sizeof(char));
	if (iodesc->extname == NULL) {
	    free(iodesc->filename);
	    free(iodesc);
	    error(NOMEM,"Allocating I/O descriptor");
	    return NULL;
	}
	strcpy(iodesc->filename,fname);
	strcpy(iodesc->extname,xname);
	iodesc->extver = ever;

	/* make up the proper filename */
	/* check for a request for the primary HDU */
	tmp = (char *)calloc((flen + 80),sizeof(char));
	if (tmp == NULL) { error(NOMEM,"Allocating I/O descriptor"); return NULL; }
	strcpy(tmp,fname);
	if (ever == 0 || ename == 0 || ename[0] == '\0' || ename[0] == ' ')
	    strcat(tmp,"[0]");
	else
	    sprintf(&tmp[flen],"[%s,%d]",xname,ever);

	*x = iodesc;
	return tmp;
} 

IODescPtr openInputImage(char *fname, char *ename, int ever) {
	IODesc *iodesc;
	int no_dims, n;
	char *tmp;
	short *iuser;
	char *p;
	extern int cvos_irafinit;

	c_pusherr(detect_iraferr);
	if (!cvos_irafinit) c_irafinit(0,0);

	tmp = make_iodesc(&iodesc,fname,ename,ever);
	if (tmp == NULL) return NULL;
	iodesc->options = ReadOnly;

	p = strstr(tmp,"[0]");
	if (p == NULL) {
	    tmp[strlen(tmp) - 1] = '\0';
	    strcat(tmp,",NOINHERIT]");
	}

	/* open the file using IRAF's imio */
	iodesc->fdesc = c_immap(tmp,IRAF_READ_ONLY,0);
	if (c_iraferr()) {
	    free(tmp);
	    ioerr(BADOPEN,iodesc);
	    free(iodesc->extname);
	    free(iodesc->filename);
	    free(iodesc);
	    return NULL;
        }
	free(tmp);
	
	/* get the dimensions and type */
	no_dims = c_imgndim(iodesc->fdesc);
	iodesc->type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	if (no_dims == 2) {
	    iodesc->naxis1 = c_imglen(iodesc->fdesc,1);
	    iodesc->naxis2 = c_imglen(iodesc->fdesc,2);
	} else if (no_dims == 1) {
	    iodesc->naxis1 = c_imglen(iodesc->fdesc,1);
	    iodesc->naxis2 = 0;
	} else if (no_dims == 0) {
	    iodesc->naxis1 = 0;
	    iodesc->naxis2 = 0;
	} else {
	    ioerr(BADDIMS,iodesc); 
	    return NULL; 
	}
	/* get the size of the user area */
	iuser = c_imguserarea(iodesc->fdesc);
	for (n = 0; iuser[n] != 0; ++n) ;
	iodesc->hsize = n + 1;

	clear_err();
	return iodesc;
}

IODescPtr openOutputImage(char *fname, char *ename, int ever, Hdr *hd,
	int d1, int d2, FitsDataType typ) {
	IODesc *iodesc;
	int n;
	char *tmp;
	int i, j;
	char *source;
	short *target;
	extern int cvos_irafinit;
	char *p;
	FitsKw kw;
	char ename_val[9];
	int ever_val;

	c_pusherr(detect_iraferr);
	if (!cvos_irafinit) c_irafinit(0,0);

	tmp = make_iodesc(&iodesc,fname,ename,ever);
	if (tmp == NULL) return NULL;
	iodesc->options = WriteOnly;
	if (ever == 0 || ename == 0 || ename[0] == '\0' || ename[0] == ' ') {
	    int rtn = ckNewFile(fname);
	    if (rtn == 1) {
		ioerr(BADEXIST,iodesc);
		return NULL;
	    } else if (rtn == 2) {
		ioerr(BADREMOVE,iodesc);
		return NULL;
	    }
	}
	p = strstr(tmp,"[0]");
	if (p == NULL) {
	    tmp[strlen(tmp) - 1] = '\0';
	    strcat(tmp,",INHERIT,APPEND]");
	}
	else {
	    tmp[strlen(tmp) - 3] = '\0'; /* eliminate the "[0]" */ 
	}
	

	/* make sure ename and ever are in the header array */
	kw = findKw(hd,"EXTNAME");
	if (kw == NotFound) {
	    if (ever != 0 && ename != 0 && 
		ename[0] != '\0' && ename[0] != ' ') {
		kw = insertfirst(hd);
		kw = insertStringKw(kw,"EXTNAME",ename,"Name of the extension");
	    }
	} else {
	    /* Make sure it has the right value */
	    getStringKw(kw,ename_val,8);
	    if (strncpy(ename_val,ename,strlen(ename)) != 0)
		putStringKw(kw,ename);
	}
	kw = findKw(hd,"EXTVER");
	if (kw == NotFound) {
	    if (ever != 0 && ename != 0 &&
		ename[0] != '\0' && ename[0] != ' ') {
		kw = findKw(hd,"EXTNAME");
		kw = insertIntKw(kw,"EXTVER",ever,"Extension version");
	    }
	} else {
	    /* Make sure it has the right value */
	    ever_val = getIntKw(kw);
	    if (ever != ever_val)
		putIntKw(kw,ever);
	}

	/* open the file using IRAF's imio */
	n = hd->nlines * HDRSize + 1; /* compute user area size */
	iodesc->hsize = n;
	if (ever == 0 || ename == 0 || ename[0] == '\0' || ename[0] == ' ')
	    iodesc->fdesc = c_immap(fname,IRAF_NEW_IMAGE,n);
	else
	    iodesc->fdesc = c_immap(tmp,IRAF_NEW_IMAGE,n);
	if (c_iraferr()) {
	    free(tmp);
	    ioerr(BADOPEN,iodesc);
	    free(iodesc->extname);
	    free(iodesc->filename);
	    free(iodesc);
	    return NULL;
        }
	free(tmp);

	iodesc->naxis1 = d1;
	iodesc->naxis2 = d2;
	if (typ == FITSBYTE)
	    iodesc->type = IRAF_SHORT;
	else if (typ == FITSSHORT)
	    iodesc->type = IRAF_SHORT;
	else if (typ == FITSLONG)
	    iodesc->type = IRAF_LONG;
	else if (typ == FITSFLOAT)
	    iodesc->type = IRAF_REAL;
	else if (typ == FITSDOUBLE)
	    iodesc->type = IRAF_DOUBLE;
	else if (typ == FITSNOVALUE)
	    iodesc->type = IRAF_SHORT;
	else
	    iodesc->type = IRAF_NOTYPE;
	iodesc->hdr = hd;

	iodesc->hflag = 1; /* mark to write header */
	if (iodesc->naxis1 == 0) {
	    putHeader(iodesc);
	    iodesc->hflag = 0;
	}

	clear_err();
	return iodesc;
}

IODescPtr openUpdateImage(char *fname, char *ename, int ever, Hdr *hd) {
	IODesc *iodesc;
	int no_dims, n;
	short *iuser;
	char *tmp;
	char *p;
	extern int cvos_irafinit;

	c_pusherr(detect_iraferr);
	if (!cvos_irafinit) c_irafinit(0,0);

	tmp = make_iodesc(&iodesc,fname,ename,ever);
	if (tmp == NULL) return NULL;
	iodesc->options = ReadWrite;

	p = strstr(tmp,"[0]");
	if (p == NULL) {
	    tmp[strlen(tmp) - 1] = '\0';
	    strcat(tmp,",NOINHERIT]");
	}

	/* open the file using IRAF's imio */
	iodesc->fdesc = c_immap(tmp,IRAF_READ_WRITE,0);
	if (c_iraferr()) {
	    free(tmp);
	    ioerr(BADOPEN,iodesc);
	    free(iodesc->extname);
	    free(iodesc->filename);
	    free(iodesc);
	    return NULL;
        }
	free(tmp);

	/* get the dimensions and type */
	no_dims = c_imgndim(iodesc->fdesc);
	iodesc->type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	if (no_dims == 2) {
	    iodesc->naxis1 = c_imglen(iodesc->fdesc,1);
	    iodesc->naxis2 = c_imglen(iodesc->fdesc,2);
	} else if (no_dims == 1) {
	    iodesc->naxis1 = c_imglen(iodesc->fdesc,1);
	    iodesc->naxis2 = 0;
	} else if (no_dims == 0) {
	    iodesc->naxis1 = 0;
	    iodesc->naxis2 = 0;
	} else {
	    ioerr(BADDIMS,iodesc); 
	    return NULL; 
	}
	/* get the size of the user area */
	iuser = c_imguserarea(iodesc->fdesc);
	for (n = 0; iuser[n] != 0; ++n) ;
	iodesc->hsize = n + 1;

	/* read the user area into the header array */
	getHeader(iodesc,hd);

	clear_err();
	return iodesc;
}

void closeImage(IODescPtr iodesc_) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int i, j;
	char *source;
	short *target;

	if (iodesc->options != ReadOnly && iodesc->naxis1 != 0)
	    putHeader(iodesc);

	c_imunmap(iodesc->fdesc);
	/* if (there is an IRAF error) */
        /*	ioerr(IRAF_CLOSE,iodesc); */
        free(iodesc->extname);
	free(iodesc->filename);
	free(iodesc);
	c_poperr();
}

int getHeader(IODescPtr iodesc_, Hdr *hd) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int ncards, i, j;
	short *source;
	char *target;

	if (iodesc->options == WriteOnly) {
		ioerr(NOGET,iodesc);
		return -1;
	}
	/* get the number of cards in the header */
	source = c_imguserarea(iodesc->fdesc);
	for (i = 0; source[i] != 0; ++i) ;
	ncards = i / HDRSize;

	/* allocate space for the header cards */
	if (allocHdr(hd,ncards) == -1) return -1;

	/* translate the data */
	for (i = 0, target = hd->array[i]; i < ncards; ++i) {
	    for (j = 0; j < (HDRSize - 1); ++j)
		*target++ = *source++;
	    *target++ = '\0';
	    source++;
	    hd->nlines++;
	}
	iodesc->hdr = hd;
	clear_err();
	return 0;
}

int putHeader(IODescPtr iodesc_) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int ncards, i, j;
	char *source;
	short *target;

	if (iodesc->options == ReadOnly) {
		ioerr(NOPUT,iodesc);
		return -1;
	}

	if (iodesc->hflag) {
            
            /* If the image is actually 1-dimensional, modify the naxis2 
             * value so the output header is written with only NAXIS and
             * NAXIS1 keywords, where NAXIS=1, and NAXIS1=number.
             */
	    if (iodesc->naxis1 != 0 && iodesc->naxis2 == 1)
                iodesc->naxis2 = 0;

	    /* set the pixel type */
	    c_imptypepix(iodesc->fdesc,(int)(iodesc->type));
	    if (iodesc->naxis1 == 0 && iodesc->naxis2 == 0)
	        c_impndim(iodesc->fdesc,0);
	    if (iodesc->naxis1 != 0 && iodesc->naxis2 == 0) {
	        /* set the number of dimensions */
	        c_impndim(iodesc->fdesc,1);
	        /* set dim1 */	
	        c_implen(iodesc->fdesc,1,iodesc->naxis1);
	    } else {
	        /* set the number of dimensions */
	        c_impndim(iodesc->fdesc,2);
	        /* set dim1 and dim2 */	
	        c_implen(iodesc->fdesc,1,iodesc->naxis1);
	        c_implen(iodesc->fdesc,2,iodesc->naxis2);
	    }
	}

	/* get the address of the user area */
	target = c_imguserarea(iodesc->fdesc);
	/* Verify the size of the user area */
	if (iodesc->hsize < iodesc->hdr->nlines) { 
		ioerr(BADHSIZE,iodesc); return -1; }

	/* translate the data */
	for (i = 0, source = iodesc->hdr->array[i]; 
		i < iodesc->hdr->nlines; ++i) {
	    for (j = 0; j < (HDRSize - 1); ++j)
		*target++ = *source++;
	    *target++ = '\n';
	    source++;
	}
	*target = 0;

	clear_err();
        return 0;
}

int getFloatData(IODescPtr iodesc_, FloatTwoDArray *da) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int no_dims, dim1, dim2, i, j;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
	float *x;
	IRAFType type;
	FitsKw kw;
	float val;

	if (iodesc->options == WriteOnly) { ioerr(NOGET,iodesc); return -1; }
	no_dims = c_imgndim(iodesc->fdesc);

        /* 
           If the number  of dimensions of the image is zero, need to 
           determine how many dimensions the image is supposed to have
           according to the NPIX[1/2] keyword(s). 
        */
	if (no_dims == 0) {
	    kw = findKw(iodesc->hdr,"PIXVALUE");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    val = getFloatKw(kw);

	    kw = findKw(iodesc->hdr,"NPIX1");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    dim1 = getIntKw(kw);

            /* If NPIX2 is not found, the image should be 1D; dim2 = 1 and * 
             * not 0 for purposes of memory allocation.                    */
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw == 0)  {
                dim2 = 1;
	        iodesc->naxis2 = 0;
            } else {
	        dim2 = getIntKw(kw);
	        iodesc->naxis2 = dim2;
            }

	    if (allocFloatData(da,dim1,dim2)) return -1;
	    iodesc->naxis1 = dim1;
	    for (j = 0; j < dim2; ++j)
	        for (i = 0; i < dim1; ++i)
        	    PPix(da,i,j) = val;

	} else if (no_dims == 1) {
	    dim1 = c_imglen(iodesc->fdesc,1);
            dim2 = 1;
	    type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	    if (allocFloatData(da,dim1,dim2)) return -1;
/*
	    if (c_imgnlr(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
	    	ioerr(BADREAD,iodesc); return -1; }
*/
	    x = c_imgl1r(iodesc->fdesc);
	    memcpy(&(PPix(da,0,0)),x,dim1*sizeof(float));
        } else if (no_dims == 2) {
	    dim1 = c_imglen(iodesc->fdesc,1);
	    dim2 = c_imglen(iodesc->fdesc,2);
	    type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	    if (allocFloatData(da,dim1,dim2)) return -1;
	    for (i = 0; i < dim2; ++i) {
	        if (c_imgnlr(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
	    	    ioerr(BADREAD,iodesc); return -1; }
	        memcpy(&(PPix(da,0,i)),x,dim1*sizeof(float));
	    }
	} else {
	    ioerr(BADDIMS,iodesc); 
            return -1; 
        }
	clear_err();
	return 0;
}

int putFloatData(IODescPtr iodesc_, FloatTwoDArray *da) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int i, j;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
        float *x;
	float tmp;
	FitsKw kw;
	int is_eq;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }

	/* check for a constant array, if not SCI data */
	if (strcmp(iodesc->extname,"SCI") != 0 
	    && da->tot_nx != 0 && da->tot_ny != 0) {
	    tmp = PPix(da,0,0);
	    for (i = 0, is_eq = 1; (i < da->tot_nx) && is_eq; ++i)
		for (j = 0; (j < da->tot_ny) && is_eq; ++j)
		    if (PPix(da,i,j) != tmp) is_eq = 0;
	    if (is_eq) {
		/* This is a constant array. */
		/* add NPIX1, NPIX2 (if necessary), and PIXVALUE keywords */
		kw = findKw(iodesc->hdr,"PIXVALUE");
		if (kw == 0) /* add it */
		    addFloatKw(iodesc->hdr,"PIXVALUE",tmp,
			"values of pixels in constant array");
		else
		    putFloatKw(kw,tmp);

		kw = findKw(iodesc->hdr,"NPIX1");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"NPIX1",iodesc->naxis1,
			"length of constant array axis 1");
		else
		    putIntKw(kw,iodesc->naxis1);

                /* NPIX2 should only be added if the y-dimension is > 1. */
                if (da->tot_ny > 1) {
		    kw = findKw(iodesc->hdr,"NPIX2");
		    if (kw == 0) /* add it */
		        addIntKw(iodesc->hdr,"NPIX2",iodesc->naxis2,
		    	    "length of constant array axis 2");
	    	    else
		        putIntKw(kw,iodesc->naxis2);
                }

		c_impndim(iodesc->fdesc,0);
		iodesc->naxis1 = 0;
		iodesc->naxis2 = 0;
		/* update the header, etc. */
		if (iodesc->hflag) { 
		    iodesc->type = IRAF_REAL;
		    putHeader(iodesc);
		    iodesc->hflag = 0; 
		}
		c_imflush(iodesc->fdesc);
		clear_err();
		return 0;
	    }
	}

        /* If not a constant array, make sure NPIX1, NPIX2, and PIXVALUE *
         * are NOT present in the header to be written out.              */
	kw = findKw(iodesc->hdr,"NPIX1");
	if (kw != 0) /* remove it */
            delKw(kw);

        if (da->tot_ny > 1) {
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw != 0) /* remove it */
                delKw(kw);
        }

	kw = findKw(iodesc->hdr,"PIXVALUE");
	if (kw != 0) /* remove it */
            delKw(kw);

	/* update the header area */
	if (iodesc->hflag) { 
	    iodesc->type = IRAF_REAL;
	    putHeader(iodesc);
	    iodesc->hflag = 0; 
	}
	for (i = 0; i < da->tot_ny; ++i) {
	    if (c_impnlr(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
		ioerr(BADWRITE,iodesc); return -1; }
	    memcpy(x,&(PPix(da,0,i)),da->tot_nx*sizeof(float));
	}
	c_imflush(iodesc->fdesc);
	clear_err();
	return 0;
}

/*                                                                     **
** Write output a subsection of an image in memory to a file where the **
** subsection is the full size of the output data.                     **
**                                                                     */
int putFloatSect(IODescPtr iodesc_, FloatTwoDArray *da, int xbeg, 
                     int ybeg, int xsize, int ysize) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int i, j, xend, yend;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
        float *x;
	float tmp;
	FitsKw kw;
	int is_eq;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }

        xend = xbeg + xsize;
        yend = ybeg + ysize;
	/* check for a constant array, if not SCI data */
	if (strcmp(iodesc->extname,"SCI") != 0 
	    && da->tot_nx != 0 && da->tot_ny != 0) {
	    tmp = PPix(da,0,0);
            for (i = xbeg, is_eq = 1; (i < xend) && is_eq; ++i)
                for (j = ybeg; (j < yend) && is_eq; ++j)
	            if (PPix(da,i,j) != tmp) is_eq = 0;
	    if (is_eq) {
		/* This is a constant array. */
		/* add NPIX1, NPIX2 (if necessary), and PIXVALUE keywords */
		kw = findKw(iodesc->hdr,"PIXVALUE");
		if (kw == 0) /* add it */
		    addFloatKw(iodesc->hdr,"PIXVALUE",tmp,
			"values of pixels in constant array");
		else
		    putFloatKw(kw,tmp);

		kw = findKw(iodesc->hdr,"NPIX1");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"NPIX1",iodesc->naxis1,
			"length of constant array axis 1");
		else
		    putIntKw(kw,iodesc->naxis1);

                /* NPIX2 should only be added if the y-dimension is > 1. */
                if (da->tot_ny > 1) {
		    kw = findKw(iodesc->hdr,"NPIX2");
		    if (kw == 0) /* add it */
		        addIntKw(iodesc->hdr,"NPIX2",iodesc->naxis2,
		    	    "length of constant array axis 2");
	    	    else
		        putIntKw(kw,iodesc->naxis2);
                }

		c_impndim(iodesc->fdesc,0);
		iodesc->naxis1 = 0;
		iodesc->naxis2 = 0;
		/* update the header, etc. */
		if (iodesc->hflag) { 
		    iodesc->type = IRAF_REAL;
		    putHeader(iodesc);
		    iodesc->hflag = 0; 
		}
		c_imflush(iodesc->fdesc);
		clear_err();
		return 0;
	    }
	}

        /* If not a constant array, make sure NPIX1, NPIX2, and PIXVALUE *
         * are NOT present in the header to be written out.              */
	kw = findKw(iodesc->hdr,"PIXVALUE");
	if (kw != 0) /* remove it */
            delKw(kw);
  
	kw = findKw(iodesc->hdr,"NPIX1");
	if (kw != 0) /* remove it */
            delKw(kw);

        if (da->tot_ny > 1) {
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw != 0) /* remove it */
                delKw(kw);
        }

	/* update the header area */
	if (iodesc->hflag) { 
	    iodesc->type = IRAF_REAL;
	    putHeader(iodesc);
	    iodesc->hflag = 0; 
	}

        for (i = ybeg; i < yend; ++i) {
            if (c_impnlr(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
		ioerr(BADWRITE,iodesc); return -1; }
            memcpy(x,&(PPix(da,xbeg,i)),xsize*sizeof(float));
	}
	c_imflush(iodesc->fdesc);
	clear_err();
	return 0;
}

int getShortData(IODescPtr iodesc_, ShortTwoDArray *da) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int no_dims, dim1, dim2, i, j;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
	short *x;
	IRAFType type;
	FitsKw kw;
	short val;

	if (iodesc->options == WriteOnly) { ioerr(NOGET,iodesc); return -1; }
	no_dims = c_imgndim(iodesc->fdesc);

        /* 
           If the number  of dimensions of the image is zero, need to 
           determine how many dimensions the image is supposed to have
           according to the NPIX[1/2] keyword(s). 
        */
	if (no_dims == 0) {
	    kw = findKw(iodesc->hdr,"PIXVALUE");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    val = getIntKw(kw);

	    kw = findKw(iodesc->hdr,"NPIX1");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    dim1 = getIntKw(kw);

            /* If NPIX2 is not found, the image should be 1D; dim2 = 1 and * 
             * not 0 for purposes of memory allocation.                    */
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw == 0)  {
                dim2 = 1;
	        iodesc->naxis2 = 0;
            } else {
	        dim2 = getIntKw(kw);
	        iodesc->naxis2 = dim2;
            }

	    if (allocShortData(da,dim1,dim2)) return -1;
	    iodesc->naxis1 = dim1;
	    for (j = 0; j < dim2; ++j)
	        for (i = 0; i < dim1; ++i)
        	    PPix(da,i,j) = val;

	} else if (no_dims == 1) {
	    dim1 = c_imglen(iodesc->fdesc,1);
	    dim2 = 1; 
	    type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	    if (allocShortData(da,dim1,dim2)) return -1;
/*
	    if (c_imgnls(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
	    	ioerr(BADREAD,iodesc); return -1; }
*/
	    x = c_imgl1s(iodesc->fdesc);
	    memcpy(&(PPix(da,0,0)),x,dim1*sizeof(short));
        } else if (no_dims == 2) {
	    dim1 = c_imglen(iodesc->fdesc,1);
	    dim2 = c_imglen(iodesc->fdesc,2);
	    type = (IRAFType)c_imgtypepix(iodesc->fdesc);
	    if (allocShortData(da,dim1,dim2)) return -1;
	    for (i = 0; i < dim2; ++i) {
	        if (c_imgnls(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
	    	    ioerr(BADREAD,iodesc); return -1; }
	        memcpy(&(PPix(da,0,i)),x,dim1*sizeof(short));
	    }
	} else {
	    ioerr(BADDIMS,iodesc); 
            return -1; 
        }
	clear_err();
	return 0;
}

int putShortData(IODescPtr iodesc_, ShortTwoDArray *da) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int i, j;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
        short *x;
	short tmp;
	FitsKw kw;
	int is_eq;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }

	/* check for a constant array, if not SCI data */
	if (strcmp(iodesc->extname,"SCI") != 0 
	    && da->tot_nx != 0 && da->tot_ny != 0) {
	    tmp = PPix(da,0,0);
	    for (i = 0, is_eq = 1; (i < da->tot_nx) && is_eq; ++i)
		for (j = 0; (j < da->tot_ny) && is_eq; ++j)
		    if (PPix(da,i,j) != tmp) is_eq = 0;
	    if (is_eq) {
		/* This is a constant array. */
		/* add NPIX1, NPIX2 (if necessary), and PIXVALUE keywords */
		kw = findKw(iodesc->hdr,"PIXVALUE");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"PIXVALUE",(int)tmp,
			"values of pixels in constant array");
		else
		    putIntKw(kw,(int)tmp);

		kw = findKw(iodesc->hdr,"NPIX1");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"NPIX1",iodesc->naxis1,
			"length of constant array axis 1");
		else
		    putIntKw(kw,iodesc->naxis1);

                /* NPIX2 should only be added if the y-dimension is > 1. */
                if (da->tot_ny > 1) {
		    kw = findKw(iodesc->hdr,"NPIX2");
		    if (kw == 0) /* add it */
		        addIntKw(iodesc->hdr,"NPIX2",iodesc->naxis2,
		    	    "length of constant array axis 2");
	    	    else
		        putIntKw(kw,iodesc->naxis2);
                }

		c_impndim(iodesc->fdesc,0);
		iodesc->naxis1 = 0;
		iodesc->naxis2 = 0;
		/* update the header, etc. */
		if (iodesc->hflag) { 
		    iodesc->type = IRAF_SHORT;
		    putHeader(iodesc);
		    iodesc->hflag = 0; 
		}
		c_imflush(iodesc->fdesc);
		clear_err();
		return 0;
	    }
	}

        /* If not a constant array, make sure NPIX1, NPIX2, and PIXVALUE *
         * are NOT present in the header to be written out.              */
	kw = findKw(iodesc->hdr,"NPIX1");
	if (kw != 0) /* remove it */
            delKw(kw);

        if (da->tot_ny > 1) {
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw != 0) /* remove it */
                delKw(kw);
        }

	kw = findKw(iodesc->hdr,"PIXVALUE");
	if (kw != 0) /* remove it */
            delKw(kw);

	/* update the header area */
	if (iodesc->hflag) { 
	    iodesc->type = IRAF_SHORT;
	    putHeader(iodesc);
	    iodesc->hflag = 0; 
	}

	for (i = 0; i < da->tot_ny; ++i) {
	    if (c_impnls(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
		ioerr(BADWRITE,iodesc); return -1; }
	    memcpy(x,&(PPix(da,0,i)),da->tot_nx*sizeof(short));
	}
	c_imflush(iodesc->fdesc);
	clear_err();
	return 0;
}

/*                                                                     **
** Write output a subsection of an image in memory to a file where the **
** subsection is the full size of the output data.                     **
**                                                                     */
int putShortSect(IODescPtr iodesc_, ShortTwoDArray *da, int xbeg, int ybeg,
                     int xsize, int ysize) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int i, j, xend, yend;
	int linevector[] = {1, 1, 1, 1, 1, 1, 1};
        short *x;
	short tmp;
	FitsKw kw;
	int is_eq;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }

        xend = xbeg + xsize;
        yend = ybeg + ysize;
	/* check for a constant array, if not SCI data */
	if (strcmp(iodesc->extname,"SCI") != 0 
	    && da->tot_nx != 0 && da->tot_ny != 0) {
	    tmp = PPix(da,0,0);
            for (i = xbeg, is_eq = 1; (i < xend) && is_eq; ++i)
                for (j = ybeg; (j < yend) && is_eq; ++j)
		    if (PPix(da,i,j) != tmp) is_eq = 0;
	    if (is_eq) {
		/* This is a constant array. */
		/* add NPIX1, NPIX2 (if necessary), and PIXVALUE keywords */
		kw = findKw(iodesc->hdr,"PIXVALUE");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"PIXVALUE",(int)tmp,
			"values of pixels in constant array");
		else
		    putIntKw(kw,(int)tmp);

		kw = findKw(iodesc->hdr,"NPIX1");
		if (kw == 0) /* add it */
		    addIntKw(iodesc->hdr,"NPIX1",iodesc->naxis1,
			"length of constant array axis 1");
		else
		    putIntKw(kw,iodesc->naxis1);

                /* NPIX2 should only be added if the y-dimension is > 1. */
                if (da->tot_ny > 1) {
		    kw = findKw(iodesc->hdr,"NPIX2");
		    if (kw == 0) /* add it */
		        addIntKw(iodesc->hdr,"NPIX2",iodesc->naxis2,
		    	    "length of constant array axis 2");
	    	    else
		        putIntKw(kw,iodesc->naxis2);
                }

		c_impndim(iodesc->fdesc,0);
		iodesc->naxis1 = 0;
		iodesc->naxis2 = 0;
		/* update the header, etc. */
		if (iodesc->hflag) { 
		    iodesc->type = IRAF_SHORT;
		    putHeader(iodesc);
		    iodesc->hflag = 0; 
		}
		c_imflush(iodesc->fdesc);
		clear_err();
		return 0;
	    }
	}

        /* If not a constant array, make sure NPIX1, NPIX2, and PIXVALUE *
         * are NOT present in the header to be written out.              */
	kw = findKw(iodesc->hdr,"PIXVALUE");
	if (kw != 0) /* remove it */
            delKw(kw);

	kw = findKw(iodesc->hdr,"NPIX1");
	if (kw != 0) /* remove it */
            delKw(kw);

        if (da->tot_ny > 1) {
	    kw = findKw(iodesc->hdr,"NPIX2");
	    if (kw != 0) /* remove it */
                delKw(kw);
        }

	/* update the header area */
	if (iodesc->hflag) { 
	    iodesc->type = IRAF_SHORT;
	    putHeader(iodesc);
	    iodesc->hflag = 0; 
	}

        for (i = ybeg; i < yend; ++i) {
	    if (c_impnls(iodesc->fdesc,&x,linevector) == IRAF_EOF) {
		ioerr(BADWRITE,iodesc); return -1; }
            memcpy(x,&(PPix(da,xbeg,i)),xsize*sizeof(short));
	}
	c_imflush(iodesc->fdesc);
	clear_err();
	return 0;
}

int getFloatLine(IODescPtr iodesc_, int line, float *ptr) {
	IODesc *iodesc = (IODesc *)iodesc_;
	float *x;
	int no_dims, dim1, i;
	FitsKw kw;
	float val;

        /* Increment the line number which is in a zero-based reference
         * system since IRAF expects a one-based reference system value.
         */
        line++;

	if (iodesc->options == WriteOnly) { ioerr(NOGET,iodesc); return -1; }
	no_dims = c_imgndim(iodesc->fdesc);
	if (no_dims == 0) {
	    kw = findKw(iodesc->hdr,"NPIX1");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    dim1 = getIntKw(kw);
	    kw = findKw(iodesc->hdr,"PIXVALUE");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    val = getFloatKw(kw);
	    for (i = 0; i < dim1; ++i)
                ptr[i] = val;
	} else {

            dim1 = c_imglen (iodesc->fdesc,1);
	    x = c_imgl2r(iodesc->fdesc,line);
	    memcpy(ptr,x,dim1*sizeof(float));
	}
	clear_err();
	return 0;
}

int putFloatLine(IODescPtr iodesc_, int line, float *ptr) {
	IODesc *iodesc = (IODesc *)iodesc_;
        float *x;

        /* Increment the line number which is in a zero-based reference
         * system since IRAF expects a one-based reference system value.
         */
        line++;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }
	if (iodesc->hflag) { iodesc->hflag = 0; putHeader(iodesc); }
	x = c_impl2r(iodesc->fdesc,line);
	memcpy(x,ptr,(iodesc->naxis1)*sizeof(float));
	clear_err();
	return 0;
}

int getShortLine(IODescPtr iodesc_, int line, short *ptr) {
	IODesc *iodesc = (IODesc *)iodesc_;
	int no_dims, dim1, i;
	short *x;
	FitsKw kw;
	short val;

        /* Increment the line number which is in a zero-based reference
         * system since IRAF expects a one-based reference system value.
         */
        line++;

	if (iodesc->options == WriteOnly) { ioerr(NOGET,iodesc); return -1; }
	no_dims = c_imgndim(iodesc->fdesc);
	if (no_dims == 0) {
	    kw = findKw(iodesc->hdr,"NPIX1");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    dim1 = getIntKw(kw);
	    kw = findKw(iodesc->hdr,"PIXVALUE");
	    if (kw == 0) { ioerr(BADSCIDIMS,iodesc); return -1; }
	    val = getIntKw(kw);
	    for (i = 0; i < dim1; ++i)
                ptr[i] = val;
	} else {

            dim1 = c_imglen (iodesc->fdesc,1);
	    x = c_imgl2s(iodesc->fdesc,line);
	    memcpy(ptr,x,dim1*sizeof(short));
	}
	clear_err();
	return 0;
}

int putShortLine(IODescPtr iodesc_, int line, short *ptr) {
	IODesc *iodesc = (IODesc *)iodesc_;
        short *x;

        /* Increment the line number which is in a zero-based reference
         * system since IRAF expects a one-based reference system value.
         */
        line++;

	if (iodesc->options == ReadOnly) { ioerr(NOPUT,iodesc); return -1; }
	if (iodesc->hflag) { iodesc->hflag = 0; putHeader(iodesc); }
	x = c_impl2s(iodesc->fdesc,line);
	memcpy(x,ptr,(iodesc->naxis1)*sizeof(short));
	clear_err();
	return 0;
}
