# include <stdio.h>
# include <string.h>
# include <ximio.h>
# include "estreak.h"


/*  G_IMIO:  Functions to move data between images and 2-d arrays.
 *
 *
 *
 *  Revision history:
 *  ---------------
 *  23 Apr 96  -  Implementation  (IB)
 *  07 Oct 96  -  Revised after code review (IB)
 *  16 Oct 96  -  Added protection against immap failure (IB)
 */



/*  Opens image. 
 *
 *  Image name is taken from the I/O control structure, using index 'fi'
 *  (zero-indexed). Switch 'tmp' selects between input or temporary images.
 *
 *  The 'open_all' switch in I/O control structure tells the routine how to
 *  interpret the open request. If 'open_all' is True, it means that all 
 *  arrays up to MAX_ARRAYS have to be opened at once. Routine stores a set 
 *  of MAX_ARRAYS IMIO IRAPointers in I/O control structure. The non-existing
 *  arrays will generate (IRAFPointer)NULL pointers. This mode is used when
 *  checking input images at task startup and to read the extra arrays beyond 
 *  the standard science and dqf. If 'open_all' is False, only the science 
 *  and dqf arrays will be opened. In this case the following applies:
 *
 *     If image resides on disk, the open operation stores a set of two 
 *     IMIO IRAPointers in I/O control structure. First pointer is for the 
 *     science image, second pointer is for the DQ image. Only input images 
 *     have associated DQ files, temporary images carry only the science 
 *     array an in this case the DQ pointer is set to (IRAFPointer)NULL.
 *
 *     If image resides on memory, sets the sci array in I/O control
 *     structure to point to appropriate memory-resident array.
 */

int g_openImage (IOControl *ioc, int fi, Bool tmp) {

	char    arrayname[MAX_ARRAYS][SZ_NAME+SZ_STR];
	int     i;

	void g_buildNames (IOControl *, Bool, 
                           char arrayname[][SZ_NAME+SZ_STR]);
	int g_openError (IRAFPointer, IOControl *);

	/* Set current image index. */
	ioc->current = fi;

	/* Build the extension/group names. */
	g_buildNames (ioc, tmp, arrayname);

	if (ioc->open_all) {

	    /* Open all arrays. The im[] vector will have its elements 
             * pointing to respectively the primary header, sci, dq, err,  
             * int and sampl arrays. This ordering is defined by routine 
             * g_buildNames. Non-existent arrays have a NULL pointer.
             */
	    for (i = 0; i < MAX_ARRAYS; i++) {
	        if (strlen (arrayname[i]) > 0) {
	            if (c_ximaccess (arrayname[i], IRAF_READ_ONLY))  {
	                ioc->im[i] = c_immap (arrayname[i],IRAF_READ_ONLY,
                                              (IRAFPointer)0);
	                if (g_openError (ioc->im[i], ioc))
	                    return (1);
	            } else
	                ioc->im[i] = (IRAFPointer)NULL;
	        } else 
	            ioc->im[i] = (IRAFPointer)NULL;
	    }
	} else {

	    /* Open only the SCI and DQF arrays. If in Ddisk mode, the 
             * im[] vector will have its first two elements pointing 
             * to the SCI and DQF arrays, and all others set to NULL. 
             */
	    if (ioc->image[fi].accessMode == Ddisk) {

	        /* Image resides in disk, handle IMIO pointers. */
	        if (tmp) {
	            if (c_ximaccess (arrayname[SSCI], IRAF_READ_WRITE)) {

	                /* If tmp file already exist, just open it. */
	                ioc->im[SSCI] = c_immap (arrayname[SSCI],
                                                 IRAF_READ_WRITE,
                                                 (IRAFPointer)0); 
	                if (g_openError (ioc->im[SSCI], ioc))
	                    return (1);
	                ioc->im[SHDR] = (IRAFPointer)NULL;
	                ioc->im[SDQF] = (IRAFPointer)NULL;
	                ioc->im[SERR] = (IRAFPointer)NULL;
	                ioc->im[SINT] = (IRAFPointer)NULL;
	                ioc->im[SSMP] = (IRAFPointer)NULL;

	            } else { 

	                /* If tmp file does not exist, open it as NEW_IMAGE. */
	                ioc->im[SSCI] = c_immap (arrayname[SSCI],IRAF_NEW_IMAGE,
                                                 (IRAFPointer)0);
	                if (g_openError (ioc->im[SSCI], ioc))
	                    return (1);
	                c_impndim (ioc->im[SSCI], 2);
	                c_implen  (ioc->im[SSCI], 1, (long)ioc->x_size);
	                c_implen  (ioc->im[SSCI], 2, (long)ioc->y_size);
	                ioc->im[SHDR] = (IRAFPointer)NULL;
	                ioc->im[SDQF] = (IRAFPointer)NULL;
	                ioc->im[SERR] = (IRAFPointer)NULL;
	                ioc->im[SINT] = (IRAFPointer)NULL;
	                ioc->im[SSMP] = (IRAFPointer)NULL;
	            }
	        } else {

                    /* Open input image arrays. */
	            ioc->im[SSCI] = c_immap (arrayname[SSCI], IRAF_READ_ONLY, 
                                          (IRAFPointer)0); 
	            if (g_openError (ioc->im[SSCI], ioc))
	                return (1);
	            ioc->im[SDQF] = c_immap (arrayname[SDQF], IRAF_READ_ONLY, 
                                          (IRAFPointer)0); 
	            if (g_openError (ioc->im[SDQF], ioc))
	                return (1);
	            ioc->im[SHDR] = (IRAFPointer)NULL;
	            ioc->im[SERR] = (IRAFPointer)NULL;
	            ioc->im[SINT] = (IRAFPointer)NULL;
	            ioc->im[SSMP] = (IRAFPointer)NULL;
	        }
	    } else {

	        /* Image resides in memory, handle memory array pointers. */
	        if (tmp)
	            ioc->sci = ioc->image[fi].tmp;
	        else
	            ioc->sci = ioc->image[fi].inp;
	    }
	}
	return (0);
}




/*  Processes c_immap open error. */

int g_openError (IRAFPointer im, IOControl *ioc) {

	void g_closeImage (IOControl *);

	if (im == (IRAFPointer)NULL) {
	    g_error ("Cannot open image.");
	    g_closeImage (ioc);
	    return (1);
	} else
	    return (0);
}



/*  Closes image. */

void g_closeImage (IOControl *ioc) {

	int   i;

	if (ioc->image[ioc->current].accessMode == Ddisk) {
	    for (i = 0; i < MAX_ARRAYS; i++) {
	        if (ioc->im[i] != (IRAFPointer)NULL) {
	            c_imunmap (ioc->im[i]);
	            ioc->im[i] = (IRAFPointer)NULL;
	        }
	    }
	} else
	    ioc->sci = NULL;
}



/*  Read-only block of image lines into array. 
 *
 *  If image resides on disk, calls IMIO to get the appropriate image
 *  section. Output array will be updated with a pointer to the IMIO 
 *  buffer, so there is no need to alloc space for it.
 *  If image resides on memory, just sets the appropriate pointer.
 *
 *  BEWARE ! The buffer returned by this routine must be used read-only,
 *  otherwise the tmp image may be destroyed when in memory mode.
 */

void g_getBlock (IOControl *ioc, int line, int blkSize, floatArray *array) {

	unsigned long          l;
	short           *sbuffer;
	float           *fbuffer;

	/* Set array axis sizes. */
	array->nx      = ioc->x_size;
	array->ny      = blkSize;
	array->bufsize = ioc->x_size * blkSize;

	if (ioc->image[ioc->current].accessMode == Ddisk) {

	    /* Disk mode, begin by reading section. */
	    fbuffer = c_imgs2r (ioc->im[SSCI], 1, ioc->x_size, 
                                            line, line+blkSize-1);

	    /* If DQF is open, read it too. */
	    if (ioc->im[SDQF] != (IRAFPointer)NULL) {
	        sbuffer = c_imgs2s (ioc->im[SDQF], 1, ioc->x_size, 
                                                line, line+blkSize-1);
	        /* Flag masked pixels. */
	        for (l=0; l < array->bufsize; l++) {
	            if (sbuffer[l] & ioc->mask)
	                fbuffer[l] = BADVAL;
	        }
	    }

	    /* Set output to point to IMIO buffer. */
	    array->data = fbuffer;

	} else

	    /* Memory mode, just point to appropriate place. */
	    array->data = (ioc->sci->data + (line-1)*ioc->x_size);

}


/*  Write full array into temporary image.  
 *
 *  If image resides on disk, use IMIO. Otherwise, physically copy array
 *  into memory-resident image.
 */

void g_putImage (IOControl *ioc, floatArray *array) {

	float      *fbuffer;

	if (ioc->image[ioc->current].accessMode == Ddisk) {

	    /* Disk mode, begin by opening buffer. */
	    fbuffer = c_imps2r (ioc->im[SSCI], 1,ioc->x_size, 1,ioc->y_size);

	    /* Copy array into buffer. */
	    memcpy (fbuffer, array->data, array->bufsize*sizeof(float));

	} else {

	    /* Memory mode, copy into indexed temp array in ioc structure. */
	    memcpy (ioc->image[ioc->current].tmp->data, array->data, 
                    array->bufsize*sizeof(float));
	}
}



