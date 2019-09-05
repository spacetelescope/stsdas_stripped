# include <stdio.h>
# include <string.h>
# include <c_iraf.h>
# include <ximio.h>
# include "estreak.h"

# define  NKW    4        /* Maximum number of keywords */


/*  G_CHECKINPUT  --  Checks input images.
 *
 *  The first valid file in the list sets the parameter values which the 
 *  remaining files will be checked against. Errors result in warning message 
 *  and skipping to next file in list.
 *
 *  The NICMOS time and sample accumulators must be allocated by the caller.
 *
 *
 *   Revision history:
 *   ---------------
 *   01 May 96  -  Implementation  (IB)
 *   21 May 96  -  WFPC support (IB)
 *   07 Oct 96  -  Revised after code review (IB)
 *
 */


int g_checkInput (IOControl *ioc, IRAFPointer fin, Bool verbose,
                  floatArray *accumTime, floatArray *accumSample) {

	int            pri,inst,i;
	long                    l;
	char    filename[SZ_NAME]; /* Current file's name                    */
	char    spt_name[SZ_NAME]; /* Current support file's name            */
	char    dqf_name[SZ_NAME]; /* Current DQ file's name (WFPC)          */
	char    rootname[SZ_NAME]; /* Current file's name root               */
	char    suffix[SZ_SUFFIX]; /* Current file's name suffix             */
	char extension[SZ_EXTENSION]; /* Current file's name extension       */
	char  keyword[SZ_KEYWORD]; /* Current keyword                        */
	char   camera[SZ_KEYWORD]; /* WFPC1 camera type (WF, PC)             */
	int                 image; /* Current image in input list            */
	int           image_index; /* Current image in I/O control vector    */
	int                gcount; /* GCOUNT value from  first valid image   */
	float pixValue[MAX_ARRAYS];/* Constant pixel value in compressed mode*/
	Bool          first_valid; /* First valid image already read         */
	Bool           error_flag; /* Error flag                             */
	short       *dqf, *sample; /* IMIO buffers for NICMOS-specific arrays*/
	float               *time;

	char ref_keyword[NKW][SZ_KEYWORD]; 
	/* This supports 4 instruments with NKW keywords per instrument,
         * ordered according to the ordering defined in estreak.h 
         */
	char *base_keyword[4][NKW] = 
        {{"INSTRUME", "CAMERA",   "FILTER1", "FILTER2"},
         {"INSTRUME", "DETECTOR", "FILTER1", "FILTER2"},
         {"INSTRUME", "CAMERA",   "FILTER",  "OBSMODE"},
         {"", "", "",  ""}};

	int g_streakAngle (char *, float *);
	void g_supportName (IOControl *, char *, char *) ;
	int g_checkInputName (IOControl *, char *, char *, char *, char *);

	if (verbose)
	    g_message ("Checking input images...\n");

	/* Force opening of all existing arrays in each image. */
	ioc->open_all = True;

	/* Loop over all input files. */
	first_valid = False;
	image_index = 0;
	inst = (int)ioc->instrument - 1;
	for (image = 0; image< ioc->nimage; image++) {

	    /* Read next file name in input list. */
	    c_imtgetim (fin, filename, SZ_NAME);

	    if (verbose) {
	        sprintf (ErrText, " %s\n", filename);
	        g_message (ErrText);
	    }

	    /* Parse file name into root, suffix and extension; check
	    its validity per se. */
	    if (g_checkInputName (ioc, filename, rootname, suffix, extension))
	        continue;

	    /* Now check validity of suffix and extension against reference. */
	    if (first_valid) {
	        if (strcmp (extension, ioc->inputExten) != 0) {
	            sprintf (ErrText, "Invalid extension in %s", filename);
	            g_warn2 (ErrText);
	            continue;
	        }
	        if (strcmp (suffix, ioc->inputSuffix) != 0) {
	            sprintf (ErrText, "Invalid extension in %s", filename);
	            g_warn2 (ErrText);
	            continue;
	        }
	    }

	    /* Name checked OK, store name in I/O structure so its is
             * available for I/O routines. 
             */
	    if (ioc->image[image_index].filename != NULL) 
	        free (ioc->image[image_index].filename);
	    ioc->image[image_index].filename = (char *) malloc ((size_t) 
                                            strlen(rootname)+1 * sizeof(char)); 
	    strcpy (ioc->image[image_index].filename, rootname);
	    if (!first_valid) {
	        strcpy (ioc->inputExten,  extension);
	        strcpy (ioc->inputSuffix, suffix);
	    }

	    /* Check existence of associated DQ file (WFPC only) */
	    if ((ioc-> instrument == WFPC) ||
	        (ioc-> instrument == WFPC2)) {
	        strcpy (dqf_name, ioc->image[image_index].filename);
	        strcat (dqf_name, ioc->inputSuffix);
	        strcat (dqf_name, ioc->dqfExten);
	        if (!c_ximaccess (dqf_name, IRAF_READ_ONLY)) {
	            sprintf (ErrText, "No Data Quality File for %s", 
                                       filename);
	            g_warn2 (ErrText);
	            continue;
	        }
	    }

	    /* Generate associated support file name and checks its existence.*/
	    g_supportName (ioc, rootname, spt_name);
	    if (c_ximaccess (spt_name, IRAF_READ_ONLY)) {

	        /* Exists, compute streak angle. */
	        if (g_streakAngle (spt_name, 
                                   &(ioc->image[image_index].streakAngle))) {
	            sprintf (ErrText, "Missing keyword in support file for %s", 
                                       filename);
	            g_warn2 (ErrText);
	            continue;
	        }
	    } else {
	        sprintf (ErrText, "No support file for %s", filename);
	        g_warn2 (ErrText);
	        continue;
	    }

	    /* Open all existing arrays. */
	    g_openImage (ioc, image_index, False);

	    /* Check keywords in (primary) header. */
	    if ((ioc-> instrument == WFPC) ||
	        (ioc-> instrument == WFPC2))
	        pri = 1;   /* this is a pointer to the primary header */
	    else
	        pri = 0;
	    error_flag = False;
	    for (i = 0; i < NKW; i++) {

	        /* Test keyword existence. */
	        if (!c_imaccf (ioc->im[pri], base_keyword[inst][i])) {
	            sprintf (ErrText,"No %s keyword in %s",
                             base_keyword[inst][i], filename);
	            g_warn2 (ErrText);
	            error_flag = True;
	            continue;
	        }

	        /* Found, read and compare with reference. */
	        c_imgstr (ioc->im[pri], base_keyword[inst][i], keyword, 
                          SZ_KEYWORD);
	        if (first_valid) {
	            if (strcmp (keyword, ref_keyword[i]) != 0) {
	                sprintf (ErrText, "Invalid %s keyword in %s.", 
                                base_keyword[inst][i], filename);
	                g_warn2 (ErrText);
	                error_flag = True;
	                continue;
	            }
	        } else
	            strcpy (ref_keyword[i], keyword);
	    }

	    /* Check GCOUNT keyword (WFPC only) */
	    if ((ioc-> instrument == WFPC) ||
	        (ioc-> instrument == WFPC2)) {
	        if ((gcount = c_imgeti (ioc->im[pri], "GCOUNT")) != 
                    ioc->ngroups) {
	            if (first_valid) {
	                sprintf (ErrText, "Invalid number of groups in %s", 
                                 filename);
	                g_warn2 (ErrText);
	                continue;
	            } else {
	                sprintf (ErrText, 
                       "File has only %d group(s). Continuing anyway.",gcount);
	                g_warn (ErrText);
	                g_warn ("Remaining output groups will be empty.");
	                ioc->ngroups = gcount;
	            }
	        }
	    }

	    if (error_flag) {             /* skip to next image */
	        g_closeImage (ioc);
	        continue;
	    }

	    /* Check axis sizes in each array. Skip primary header ! */
	    error_flag = False;
	    for (i = 1; i < MAX_ARRAYS; i++) {
	        pixValue[i] = NO_PIXVAL;
	        if (ioc->im[i] != (IRAFPointer)NULL) {

	            /* NAXIS == 0  signals compressed format. */
	            if (c_imgndim (ioc->im[i]) == 0) {
	                c_pusherr (0);
	                pixValue[i] = c_imgetr (ioc->im[i], "PIXVALUE");
	                if (c_iraferr()) {
	                    sprintf (ErrText, "No PIXVALUE keyword in %s.", 
                                     filename);
	                    g_warn2 (ErrText);
	                    error_flag = True;
	                    continue;
	                }
	                c_poperr();
	            } else if (c_imgndim (ioc->im[i]) != 2) {
	                sprintf (ErrText, "Non-2D array found in %s.", 
                                 filename);
	                g_warn2 (ErrText);
	                error_flag = True;
	                continue;
	            } else if ((c_imglen(ioc->im[i], 1) != ioc->x_size) ||
                               (c_imglen(ioc->im[i], 2) != ioc->y_size)) {
	                sprintf (ErrText, "Wrong array size found in %s.", 
                                 filename);
	                g_warn2 (ErrText);
	                error_flag = True;
	                continue;
	            }
	        }
	    }
	    if (error_flag) {               /* skip to next image */
	        g_closeImage (ioc);
	        continue;
	    }

	    /* Everything checked OK, signal first successful file was 
             * read. Also store instrument and camera-dependent position angle 
             * offsets. Notice that after setting the appropriate parameters, 
             * all WFPC types fold down into one single type. 
             */
	    if (!first_valid) {
	        first_valid = True;
	        switch (ioc->instrument) {
	        case NICMOS:
	            i = c_imgeti (ioc->im[pri], "CAMERA");
	            switch (i) {
	                case 1: ioc->angleOffset[0] = NIC_CAM1; break;
	                case 2: ioc->angleOffset[0] = NIC_CAM2; break;
	                case 3: ioc->angleOffset[0] = NIC_CAM3; break;
	            }
	            break;
	        case WFPC:
	            c_imgstr (ioc->im[pri], "CAMERA", camera, 20);
	            if (!strncmp (camera, "WF", 2)) {
	                ioc->angleOffset[0] = WF1_CHIP1;
	                ioc->angleOffset[1] = WF1_CHIP2;
	                ioc->angleOffset[2] = WF1_CHIP3;
	                ioc->angleOffset[3] = WF1_CHIP4;
	            } else {
	                ioc->angleOffset[0] = PC_CHIP1;
	                ioc->angleOffset[1] = PC_CHIP2;
	                ioc->angleOffset[2] = PC_CHIP3;
	                ioc->angleOffset[3] = PC_CHIP4;
	            }
	            break;
	        case WFPC2:
	            ioc->instrument     = WFPC;
	            ioc->angleOffset[0] = WF2_CHIP1;
	            ioc->angleOffset[1] = WF2_CHIP2;
	            ioc->angleOffset[2] = WF2_CHIP3;
	            ioc->angleOffset[3] = WF2_CHIP4;
	            break;
	        }
	    }

	    /* Update time and samples accumulators. */
	    if (ioc->instrument == NICMOS) {

	        /* The im[] index values are defined at file name 
                 * building time by routine g_buildNames 
                 */

	        /* First read the DQF. */
	        dqf = c_imgs2s (ioc->im[SDQF], 1,ioc->x_size,1,ioc->y_size); 

	        /* Now read and accumulate the times... */
	        if (pixValue[SINT] == NO_PIXVAL)
	            time = c_imgs2r (ioc->im[SINT], 1, ioc->x_size,
                                     1, ioc->y_size); 
	        for (l = 0L; l < accumTime->bufsize; l++) {
	            if (!(dqf[l] & ioc->mask)) {
	                if (pixValue[SINT] == NO_PIXVAL)
	                    accumTime->data[l] += time[l];
	                else
	                    accumTime->data[l] += pixValue[SINT];
	            }
	        }

	        /* ...and samples. */
	        if (pixValue[SSMP] == NO_PIXVAL)
	            sample = c_imgs2s (ioc->im[SSMP], 1, ioc->x_size, 
                             1, ioc->y_size); 
	        for (l = 0L; l < accumSample->bufsize; l++) {
	            if (!(dqf[l] & ioc->mask)) {
	                if (pixValue[SSMP] == NO_PIXVAL)
	                    accumSample->data[l] += (float)sample[l];
	                else
	                    accumSample->data[l] += pixValue[SSMP];
	            }
	        }
	    }

	    /* Close current image and bump image index. */
	    g_closeImage (ioc);
	    image_index++;
	}

	/* Update number of good images. */
	ioc->nimage = image_index;

	/* Restore opening of only sci and dqf arrays in each image. */
	ioc->open_all = False;

	return (0);
}
