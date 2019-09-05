# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include <ximio.h>
# include <c_iraf.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "estreak.h"

/*  G_INPUTPAR  --   Reads task parameters, do some checking and 
 *                   initializes data structures.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   01 May 96  -  Implementation (IB)
 *   07 May 96  -  IRAF CL support (IB).
 *   20 May 96  -  WFPC support (IB).
 *   07 Oct 96  -  Revised after code review (IB)
 *   03 Jun 97  -  Default badmask value changed to zero because INDEFS
 *                 constant does not exist in standard C include files (IB).
 *
 */


int g_inputPar (IOControl *ioc, IRAFPointer *fin, char *output, int *niter,
                int width[], int *ngood, short *badmask, Bool *verbose) {

/*
 *  IOControl            ioc  o: I/O control structure
 *  IRAFPointer          fin  o: input file list/template pointer
 *  char    output[SZ_NAME]   o: output file name
 *  int                niter  o: number of iterations
 *  int      width[MAX_ITER]  o: fwhm of smoothing boxcar at each iter.
 *  int                ngood  o: min. # of pixels for median computation.
 *  short            badmask  o: flag value for output bad pixels
 *  Bool             verbose  o: verbose ?
 */

	/* Local variables. */
	IRAFPointer            im; /* IMIO pointer                  */
	IRAFPointer            ps; /* pset pointer                  */
	char      input[SZ_NAME];  /* Input template/list           */
	char  firstfile[SZ_NAME];  /* First file name in input list */
	char     widths[SZ_NAME];  /* Boxcar filter halfwidths      */
	char instrument[SZ_KEYWORD+1]; /* INSTRUME keyword value    */
	char  dummy[SZ_KEYWORD+1];
	int             i, nwidth;

	/* Function declarations. */
	int g_parseIntString (char *, int[], int *);
	void g_getPixelMask (IOControl *);
	int g_strdic (char *, char *, int, char *);
	int g_checkOutput (IOControl *, char *);

	/* First thing to do is to detect which instrument
         * we are dealing with, so go and open input file list.
         */
	c_clgstr ("input", input, SZ_NAME);
	if ((*fin = c_imtopen (input)) == (IRAFPointer)NULL) {
	    g_error ("Error in input file name/list.");
	    return (1);
	}

	/* Decision is based on INSTRUME header keyword. */
	c_imtgetim (*fin, firstfile, SZ_NAME);
	c_imtrew (*fin);
	strcat (firstfile, "[0]"); /* this avoids problems with INHERIT=F */
	if (!c_ximaccess (firstfile, IRAF_READ_ONLY)) {
	    sprintf (ErrText, "Cannot find first file (%s) in input list.",
                     firstfile);
	    g_error (ErrText);
	    return (1);
	}
	im = c_immap (firstfile, IRAF_READ_ONLY, (IRAFPointer)0);
	if (im == (IRAFPointer)NULL) {
	    g_error ("Cannot open first file in input list.");
	    return (1);
	}
	if (!c_imaccf (im, "INSTRUME")) {
	    sprintf (ErrText, "No INSTRUME keyword in file %s.", firstfile);
	    g_error (ErrText);
	    c_imunmap (im);
	    return (1);
	}
	c_imgstr (im, "INSTRUME", instrument, SZ_KEYWORD);
	c_imunmap (im);
	ioc->instrument = g_strdic (instrument,dummy,SZ_KEYWORD,INSTRUMENTS);
	if (!ioc->instrument) {
	    sprintf (ErrText, "Instrument: %s not supported yet.",instrument);
	    g_error (ErrText);
	    return (1);
	}

	/* Store instrument-dependent image format info. */
	switch (ioc->instrument) {
	case NICMOS:
	    ioc->ngroups = NIC_NGROUPS;
	    ioc->x_size  = NIC_XSIZE;
	    ioc->y_size  = NIC_YSIZE;
	    break;
	case WFPC:
	case WFPC2:
	    ioc->ngroups = WFPC_NGROUPS;
	    ioc->x_size  = WFPC_XSIZE;
	    ioc->y_size  = WFPC_YSIZE;
	    break;
	case STIS:
	    g_error ("STIS not supported yet.");
	    return (1);
	}

	/* Read more CL parameters. */
	c_clgstr ("output", output, SZ_NAME);
	switch (ioc->instrument) {
	    case NICMOS: ps = c_clopset ("nstreakpar"); break;
	    case WFPC:   ps = c_clopset ("wstreakpar"); break;
	    case WFPC2:  ps = c_clopset ("wstreakpar"); break;
	}
	*niter = c_clgpseti (ps, "niter");
	c_clgpseta (ps, "widths", widths, SZ_NAME);
	*badmask = c_clgpsets (ps, "badmask");
	g_getPixelMask (ioc);
	c_clcpset (ps);
	*ngood = c_clgeti ("ngood");
	ioc->availMemory = c_clgeti ("memory");
	*verbose = c_clgetb ("verbose");
	/* Set bad pixel bit for output pixels */
# if defined(NATIVE_IRAF)
	if (*badmask == 0) {
#endif
	    switch (ioc->instrument) {
	        case NICMOS: *badmask = N_BADPIX; break;
	        case WFPC:   *badmask = W_BADPIX; break;
	        case WFPC2:  *badmask = W_BADPIX; break;
	    }
# if defined(NATIVE_IRAF)
	}
#endif

	/* Check validity of output file name extension. */
	if (g_checkOutput (ioc, output))
	    return (1);

	/* Abort right now if output file already exists, 
         * do not wait until algorithm completes. 
         */
	if (c_ximaccess (output, IRAF_READ_WRITE)) {
	    sprintf (ErrText, "Output file %s already exists.", output);
	    g_error (ErrText);
	    return (1);
	}

	/* Initialize main I/O control structure. */
        ioc->nimage  = c_imtlen (*fin);
	ioc->current = -1;
	for (i = 0; i < MAX_ARRAYS; ioc->im[i++] = (IRAFPointer)NULL);
	ioc->sci = NULL;
	if ((ioc->image = (IOImage *) malloc ((size_t) (ioc->nimage) * 
            sizeof(IOImage))) == NULL) {
	    g_error ("Cannot allocate main I/O control structure.");
	    return (1);
	}
	for (i = 0; i < ioc->nimage; i++) {
	    ioc->image[i].filename   = NULL;
	    ioc->image[i].accessMode = Ddisk;
	    ioc->image[i].inp = (floatArray *) malloc ((unsigned int) 
                                sizeof(floatArray));
	    ioc->image[i].tmp = (floatArray *) malloc ((unsigned int) 
                                sizeof(floatArray));
	}
	ioc->blkSize = ioc->y_size;
	ioc->group   = 0;

	/* Limit iterations to a maximum. */
	*niter = (*niter > MAX_ITER) ? MAX_ITER : *niter;

	/* Parse the widths string into numbers. */
	if (g_parseIntString (widths, width, &nwidth)) {
	    g_error ("Cannot parse widths string.");
	    return (1);
	}
	if (nwidth < *niter) {
	    g_error ("Missing filter width(s).");
	    return (1);
	} else if (nwidth > *niter) {
	    sprintf (ErrText, "Using only first %d filter widths.", *niter);
	    g_warn (ErrText);
	}

	if (*verbose) {
	    sprintf (ErrText, 
            "Mask for input pixels (hexadecimal) = %04x\n", ioc->mask);
	    g_message (ErrText);
	    sprintf (ErrText, "Output bad pixel flag = %d\n", *badmask);
	    g_message (ErrText);
	    sprintf (ErrText, "Number of iterations = %d\n", *niter);
	    g_message (ErrText);
	    sprintf (ErrText, "Boxcar filter widths:  ");
	    g_message (ErrText);
	    for (i = 0; i < *niter; i++) {
	        sprintf (ErrText, "%d  ", width[i]);
	        g_message (ErrText);
	    }
	    g_message ("\n");
	    sprintf (ErrText, "Usable memory = %d Mb\n", ioc->availMemory);
	    g_message (ErrText);
	}

	return (0);
}

