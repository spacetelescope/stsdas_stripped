# include <stdio.h>
# include <string.h>
# include "msstat.h"


/*  E_SINGLEFILE  -  Computes and prints statistics for one single file
 *                   in input list.
 *
 *  This is mostly a housekeeping routine that takes care of file
 *  opening/closing, checking image sizes, memory allocation/dealocation 
 *  and such. It also controls the main loops over image groups.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   07 Jun 96  -  Implementation (IB)
 *   01 Jul 96  -  DQF name list/template support (IB).
 *   02 Jul 96  -  Simultaneous input from all HDUs (IB).
 *   08 Jul 96  -  Lower and upper cutoffs (IB).
 *   15 Jul 96  -  Histogram computation (IB).
 *   21 Oct 96  -  Revised after code review (IB)
 *   12 Nov 96  -  Support for compressed HDUs (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs (IB)
 *
 */

int e_singleFile (Control *con, char *filename, char *maskname,
                  char *dqfname, float lower, float upper, int ncol) {

	char   root[SZ_FNAME];
	char    ext[SZ_FNAME];
	char   sect[SZ_FNAME];
	int        group_init;
	int         group_end;
	int        group_step;
	int         groupSpec;
	int           i, type;
	Bool             hist;

	int  e_parseName (char *, char *, char *, int *, char *);
	int  e_describeFile (Control *, char *);
	int  e_accum (Control *, float, float);
	int  e_hist (Control *, float, float);
	int  e_openDQMask (Control *);
	int  e_compSizes (Control *, int);
	void e_clearAccum (Control *);
	void e_closeDQMask (Control *);
	void e_report (Control *, int, int, Bool);
	void e_imunmap (IRAFPointer);
	IRAFPointer e_openHDU (Control *, HDUType);

	/* Parse file name into root, extension, group and section. */
	if (e_parseName (filename, root, ext, &groupSpec, sect)) {
	    sprintf (MsgText, "Cannot parse file name %s", filename);
	    e_warn2 (MsgText);
	    return (1);
	}
	strcpy (con->filename,   root);
	strcpy (con->inputExten, ext);
	strcpy (con->section,    sect);
	con->groupSpec = groupSpec;

	/* Reset file descriptors and flags */
	for (i = 0; i < MAX_HDUS; con->im[i++] = (IRAFPointer)NULL);
	con->msk     = (IRAFPointer)NULL;
	con->dqf     = (IRAFPointer)NULL;
	con->err     = (IRAFPointer)NULL;
	con->mskOK   = False;
	con->dqfOK   = False;
	con->errOK   = False;
	con->ngroups = 1;

	/* Detect file type details. */
	if (e_describeFile (con, dqfname)) {
	    sprintf (MsgText, "Cannot detect file type in %s ", filename);
	    e_warn2 (MsgText);
	    return (1);
	}

	/* Open mask. */
	if (strlen (maskname) > 0) {
            con->msk = c_immap (maskname, IRAF_READ_ONLY, (IRAFPointer)0);
	    if (c_iraferr())  {
	        e_IRAFerror();
	        return (1);
	    }
	    con->mskOK = True;
	} else {
	    con->msk = (IRAFPointer)NULL;
	    con->mskOK = False;
	}

	/* Replace task parameters by group spec in file name. This is
         * needed in order to allow a group spec explictly included in a
         * file name to supersede the task group range list without 
         * destroying it. 
         */
	if (con->groupSpec > 0) {
	    group_init = con->groupSpec;
	    group_end  = con->groupSpec;
	    group_step = 1;
	} else {
	    if (con->allGroups ) {
	        group_init = 1;
	        group_end  = con->ngroups;
	        group_step = 1;
	    } else {
	        group_init = con->group_init;
	        group_end  = con->group_end;
	        group_step = con->group_step;
	    }
	}

	/* See if histogram-based stats were asked by the user. */
	hist = False;
	for (i = 0; i < con->nstats; i++) {
	    if ((con->stats[i] == ALL) ||
	        (con->stats[i] == MIDPT) ||
	        (con->stats[i] == MODE))
	        hist = True;
	}

	/* Clear accumulators. */
	e_clearAccum (con);

	/* Loop over image groups to be processed. */
	for (con->group = group_init; 
             con->group <= group_end; 
             con->group += group_step) { 

	    /* Test if current group exists in current file. */
	    if (con->group <= con->ngroups) {

	        /* Reset accumulators. */
	        if (!con->globalAccum) e_clearAccum (con);

	        /* Open DQF and error. These are handled as special HDUs
                 * because they might be used in the stats computataion for
                 * all the other HDUs in the group. 
                 */
	        if (e_openDQMask (con)) {
	            e_warn ("Found non-2D array.");
	            e_closeDQMask (con);
	            continue;
	        }

	        /* Loop over the HDU types the user asked for. */
	        for (type = 0; type < con->nhdu; type++) {

	            /* Open this HDU */
	            con->im[type] = e_openHDU (con, con->hdu[type]);

	            if (con->im[type] != (IRAFPointer)NULL) {

	                /* Compare this HDU axis sizes with other arrays. */
	                if (e_compSizes (con, type)) {
	                    e_warn ("Image arrays not of same size.");
	                    continue;
	                }
	            }
	        }

	        /* Read pixel, mask, DQF and error, and accumulate. */
	        if (e_accum (con, lower, upper)) {
	            e_closeDQMask (con);
	            if (con->mskOK)
	                e_imunmap (con->msk);
	            return(1);
	        }

	        /* Accumulate histograms, but only if working separately
                 * on each group. If in global accumulation mode, histogram
                 * accumulation has to be done after all groups were scanned
                 * to locate global maximum and minimum. 
                 */
	        if (hist && !con->globalAccum) {
	            if (e_hist (con, lower, upper)) {
	                e_closeDQMask (con);
	                if (con->mskOK)
	                    e_imunmap (con->msk);
	                return(1);
	            }
	        }

	        /* Close HDUs and compute stats. */
	        for (type = 0; type < con->nhdu; type++) {
	            if (con->im[type] != (IRAFPointer)NULL) {
	                e_imunmap (con->im[type]);

	                /* Compute and report stats, store in CL parameters. */
	                if (!con->globalAccum) 
	                    e_report (con, type, ncol, hist);

	            }
	        }

	        /* Close DQF and ERR */
	        e_closeDQMask (con);
	    }
	}

	/* The following loop processes histograms when in global 
         * accumulate mode. This must be done after the first loop
         * over groups, since only now the full data range over all
         * groups becomes available. 
         */
	if (hist && con->globalAccum) {

	    /* Loop over image groups to be processed. */
	    for (con->group = group_init; 
                 con->group <= group_end; 
                 con->group += group_step) { 

	        /* Test if current group exists in current file. */
	        if (con->group <= con->ngroups) {

	            /* Open DQF and error. */
	            if (e_openDQMask (con)) {
	                e_warn ("Found non-2D array.");
	                e_closeDQMask (con);
	                continue;
	            }

	            /* Loop over the HDU types the user asked for. */
	            for (type = 0; type < con->nhdu; type++) {
 
	                /* Open this HDU. Image sizes have to be
                         * re-tested because current HDU might be skipped
                         * in the first loop. 
                         */
	                con->im[type] = e_openHDU (con, con->hdu[type]);
	                if (con->im[type] != (IRAFPointer)NULL) {
	                    if (e_compSizes (con, type)) {
	                        e_warn ("Image arrays not of same size.");
	                        continue;
	                    }
	                }
	            }

	            /* Accumulate histograms. */
	            if (e_hist (con, lower, upper)) {
	                e_closeDQMask (con);
	                if (con->mskOK)
	                    e_imunmap (con->msk);
	                return(1);
	            }

	            /* Close HDUs. */
	            for (type = 0; type < con->nhdu; type++) {
	                if (con->im[type] != (IRAFPointer)NULL)
	                    e_imunmap (con->im[type]);
	            }

	            /* Close DQF and ERR */
	            e_closeDQMask (con);
	        }
	    }
	}

	/* Global accumulation reporting. */
	if (con->globalAccum)
	    for (type = 0; type < con->nhdu; e_report (con,type++,ncol,hist));

	/* This is just to free histogram memory. */
	if (hist)
	    e_clearAccum (con);

	/* Close mask image. */
	if (con->mskOK)
	    e_imunmap (con->msk);

	return (0);
}
