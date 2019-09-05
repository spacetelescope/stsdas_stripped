# include <stdio.h>
# include <string.h>
# include <hstio.h>
# include "estreak.h"


/*  G_DOIT:   Checks input images, generates the flat field and anciliary
 *            arrays, writes output image.
 *
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   03 May 96  -  Implementation  (IB)
 *   23 May 96  -  WFPC support  (IB)
 *   30 May 96  -  Virtual file support  (IB)
 *
 */

int g_doIt (IOControl *ioc, IRAFPointer *fin, char output[], int niter, 
            int width[], int ngood, short badmask, char time_begin[],
            Bool verbose) {

/*  Parameters:
 *
 *  IOControl            ioc  i: I/O control structure
 *  IRAFPointer         *fin  i: input file list/template pointer
 *  char    output[SZ_FNAME]  i: output file name
 *  int                niter  i: number of iterations
 *  float    width[MAX_ITER]  i: fwhm of smoothing boxcar at each iter.
 *  int                ngood  i: min. # of pixels for median computation.
 *  short            badmask  i: flag value for output bad pixels
 *  char  time_begin[SZ_STR]  i: time at which task begun
 *  Bool             verbose  i: verbose ?
 */

/*  Local variables                                                       */

	char                             filename[SZ_NAME];
	char         arrayname[MAX_ARRAYS][SZ_NAME+SZ_STR];
	char                              sci_out[SZ_NAME];
	char                              dqf_out[SZ_NAME];
	char                        time_end[SZ_TIMESTAMP];
	char                                          *dot;
	IRAFPointer                       sci_imo, dqf_imo;
	floatArray                            sample, time;
	floatArray                             flat, error;
	float                              angle, avg, std;
	int                                       cg, i, n;

        char *malloc_err = "Cannot allocate memory.";

/*  Function declarations.                                               */

	void g_timeStamp (char *);
	int g_checkInput (IOControl *, IRAFPointer, Bool,
                  floatArray *, floatArray *);
	int g_doFlat (IOControl *, int, int [], int, Bool, floatArray *);
	int g_doError (IOControl *, floatArray *, floatArray *, Bool);
	int g_writeResult (IOControl *, char *, int [], int, short, 
                           floatArray *, floatArray *, floatArray *, 
                           floatArray *, char *);
	int g_updateHeader (IOControl *, SingleNicmosGroup *, 
                            IRAFPointer, IRAFPointer, 
                            char *, int *,int, char *, char *);
	void g_buildNames (IOControl *, Bool, char arrayname[][SZ_NAME+50]);
	int g_initMemory (IOControl *, Bool);
	void g_refreshMemory (IOControl *);
	void g_freeMemory (IOControl *);
        void g_stat (floatArray *, int, int, int, int, Bool, float, float,
                     float *, float *, int *, Bool);

	/* Calloc time and samples accumulator arrays (NICMOS only). */
	if (ioc->instrument == NICMOS) {
	    if (allocArray (&time, ioc->x_size, ioc->y_size))  {
	        g_error (malloc_err);
	        return (1);
	    }
	    if (allocArray (&sample, ioc->x_size, ioc->y_size))  {
	        g_error (malloc_err);
	        return (1);
	    }
	}

	/* Scan input files: check them and compute streak angles.
         * For NICMOS files only, accumulate samples and times. 
         */
	if (g_checkInput (ioc, *fin, verbose, &time, &sample)) {
	    g_error ("Error when checking input images.");
	    if (ioc->instrument == NICMOS) {
	        freeArray (&sample);
	        freeArray (&time);
	    }
	    c_imtclose (*fin);
	    return (1);
	}

	/* Close input file list. */
	c_imtclose (*fin);

	/* Abort if not enough images. */
	if (ioc->nimage < ngood) {
	    g_error ("Insufficient number of good images.");
	    if (ioc->instrument == NICMOS) {
	        freeArray (&sample);
	        freeArray (&time);
	    }
	    return (1);
	}

	/* Alloc flatfield array. */
	if (allocArray (&flat, ioc->x_size, ioc->y_size))  {
	    g_error (malloc_err);
	    return (1);
	}

	/* Do some memory management. */
	if (g_initMemory (ioc, verbose))  {
	    g_error (malloc_err);
	    return (1);
	}

	if (verbose) {
	    sprintf (ErrText, 
                    "Block size for median computation = %d lines.\n", 
                     ioc->blkSize);
	    g_message (ErrText);
	}

	/* Loop over GEIS groups. */
	for (cg = 0; cg < ioc->ngroups; cg++) {

	    ioc->group = cg;

	    /* Report streak angles for this group. */
	    if (verbose) {
	        if (ioc->instrument == WFPC)
	            sprintf (ErrText, 
                    "Starting streakflat algorithm for group %d.\n",
                     ioc->group+1);
	        else
	            sprintf (ErrText, "Starting streakflat algorithm.\n");
	        g_message (ErrText);
	        g_message ("  Streak angles:\n");
	        for (i = 0; i < ioc->nimage; i++) {
	            angle = ioc->image[i].streakAngle - 
                            ioc->angleOffset[ioc->group];
	            while (angle >   90.0F) angle -= 180.0F;
	            while (angle <= -90.0F) angle += 180.0F;
	            strcpy (filename, ioc->image[i].filename);
	            strcat (filename, ioc->inputSuffix);
	            strcat (filename, ioc->inputExten);
	            sprintf (ErrText, "  %s   %8.3f\n", filename, angle);
	            g_message (ErrText);
	        }
	    }

	    /* Fill up memory-resident data arrays with current group data. */
	    g_refreshMemory (ioc);

	    /* Compute the flat field. */
	    if (g_doFlat (ioc, niter, width, ngood, verbose, &flat))
	        return (1);

	    /* Output stats. */
	    if (verbose) {
	        g_message ("Flat field stats:      ");
	        g_stat (&flat, 0,ioc->x_size,0,ioc->y_size, False,0.0F,0.0F,
                        &avg,&std,&n, True);
	    }

	    /* Write WFPC group. */
	    if (ioc->instrument == WFPC) {
	        if (g_writeResult (ioc, output, width, niter, badmask, &flat, 
                                   &error, &time, &sample, time_begin))
	            return (1);
	    }
	}

	/* Write HISTORY of ending time in WFPC 4-group output files. */
	if (ioc->instrument == WFPC) {
	    g_timeStamp (time_end);
	    strcpy (sci_out, output);
	    strcpy (dqf_out, output);
	    dot = strrchr (dqf_out, '.'); /* replace extension */
	    *dot = '\0';
	    strcat (dqf_out, W_DQOUT);
	    sci_imo = c_immap (sci_out, IRAF_READ_WRITE, (IRAFPointer)0);
	    if (sci_imo == (IRAFPointer)NULL) {
	        g_error ("Cannot open output image.");
	        return (1);
	    }
	    dqf_imo = c_immap (dqf_out, IRAF_READ_WRITE, (IRAFPointer)0);
	    if (dqf_imo == (IRAFPointer)NULL) {
	        g_error ("Cannot open output DQF.");
	        c_imunmap (sci_imo);
	        return (1);
	    }
	    if (g_updateHeader (ioc, NULL, sci_imo, dqf_imo, output, width, 
                                niter, time_begin, time_end)) {
	        c_imunmap (dqf_imo);
	        c_imunmap (sci_imo);
	        return (1);
	    }
	   c_imunmap (dqf_imo);
	   c_imunmap (sci_imo);
	}

	/* Compute error array. */
	if ((ioc->instrument == NICMOS) ||
            (ioc->instrument == STIS))  {
	    if (allocArray (&error, ioc->x_size, ioc->y_size))  {
	        g_error (malloc_err);
	        return (1);
	    }
	    if (g_doError (ioc, &flat, &error, verbose))
	        return (1);
	}

	/* Reset the group counter. */
	ioc->group = 0;

	/* Write NICMOS/STIS output. */
	if ((ioc->instrument == STIS) ||
            (ioc->instrument == NICMOS)) {
	    if (g_writeResult (ioc, output, width, niter, badmask, &flat, 
                               &error, &time, &sample, time_begin))
	       return (1);
	}

	/* More stats. */
	if (verbose) {
	    if ((ioc->instrument == NICMOS) ||
                (ioc->instrument == STIS))  {
	        g_message ("Error array stats:     ");
                g_stat (&error, 0,ioc->x_size,0,ioc->y_size, False, 0.0F,0.0F, 
                        &avg,&std,&n, True);
	    }
	    if (ioc->instrument == NICMOS) {
	        g_message ("Time array stats:      ");
                g_stat (&time, 0,ioc->x_size,0,ioc->y_size, False, 0.0F,0.0F, 
                        &avg,&std,&n, True);
	        g_message ("Samples array stats:   ");
                g_stat (&sample, 0,ioc->x_size,0,ioc->y_size, False,0.0F,0.0F, 
                        &avg,&std,&n, True);
	    }
	}

	/* Delete temporary files. */
	for (i = 0; i < ioc->nimage; i++) {
	    ioc->current = i;
	    if (ioc->image[i].accessMode == Ddisk) {
	        g_buildNames (ioc, True, arrayname);
	        c_imdelete (arrayname[0]); 
	    }
	}

	/* Free memory. */
	g_freeMemory (ioc);
	if (ioc->instrument == NICMOS) {
	    freeArray (&sample);
	    freeArray (&time);
	}
	if ((ioc->instrument == NICMOS) ||
            (ioc->instrument == STIS))
	    freeArray (&error);
	free (ioc->image);

	return (0);
}
