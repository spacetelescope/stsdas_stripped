# include <stdio.h>
# include <float.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "msstat.h"


/*   E_GSTAT  -  Extended gstatistics.
 *
 *   This is the actual main routine, it is called from another module
 *   just to avoid exit() calls.
 *
 *   This routine reads and parses CL parameters, initializes data structures
 *   and expand the input file list.
 *
 *
 * 
 *                                                          Author: I. Busko
 *
 *   Revision history:
 *   ----------------
 *   06 Jun 96  -  Implementation as IRAF native task.
 *   01 Jul 96  -  DQF as input template/list (IB).
 *   18 Oct 96  -  Revised after code review - version 1.1 (IB).
 *   12 Nov 96  -  Support for compressed HDUs - version 1.2 (IB)
 *   06 Mar 97  -  Modified definition of weighted variance (IB)
 *   06 Mar 97  -  Warns when invalid tokens are found in control strings (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs - v 1.3 (IB)
 *   16 Apr 97  -  Fixed getLine call in e_hist (IB)
 *   30 May 97  -  Task renamed "msstatistics" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 * 
 */

int e_gstat (int argc, char **argv) {

	Control                   con; /* Main control structure      */
	IRAFPointer               fin; /* Input list/template pointer */
	IRAFPointer               min; /* Mask list/template pointer  */
	IRAFPointer               qin; /* DQF list/template pointer   */
	IRAFPointer                pp; /* pset pointer                */
	char       cl_input[SZ_FNAME]; /* Input template/list         */
	char       cl_masks[SZ_FNAME]; /* Mask template/list          */
	char         cl_dqf[SZ_FNAME]; /* DQF template/list           */
	char      cl_groups[SZ_FNAME]; /* Groups range list           */
	char       cl_stats[SZ_FNAME]; /* Stats list                  */
	char      cl_arrays[SZ_FNAME]; /* NICMOS/STIS/ACS HDUs        */
	char     cl_clarray[SZ_FNAME];
	char       filename[SZ_FNAME];
	char       maskname[SZ_FNAME];
	char        dqfname[SZ_FNAME];
	IRAFPointer                ps; /* pset pointer                */
	Bool                    doall;
	Bool                  errflag;
	Bool                     dqon;
	int                 namecol,i;
	float            lower, upper;

	void e_getMask (Control *, Bool);
	void e_parseString (Control *, char *, int);
	void e_parseGroups (Control *, char *);
	void e_clearAccum (Control *);
	int  e_singleFile(Control *, char *, char *, char *, float, float, int);
	void e_headerLine (Control *, int);

	/* Temporary replacements for failing cvos bindings */
	float l_clgetf (char *);  /* Locally defined C biding. */
	void l_clppsetf(IRAFPointer, char *, float);

	/* Begin by reading CL parameters. All existing psets must be
         * read at once since we do not know a priori which file types are
         * included in the input list. 
         */

	/* Read global parameters. */
	c_clgstr ("input", cl_input,   SZ_FNAME);
	c_clgstr ("masks",  cl_masks,  SZ_FNAME);
	c_clgstr ("dqf",    cl_dqf,    SZ_FNAME);
	c_clgstr ("groups", cl_groups, SZ_FNAME);
	c_clgstr ("stats",  cl_stats,  SZ_FNAME);

	/* This is a locally-defined C binding that
         * avoids an unexplained bug in the CL/cvos.
         */
	lower = l_clgetf ("lower");
	upper = l_clgetf ("upper");

	/* We need an IS_INDEF macro ! */
	if (lower > 1.E30)
	    lower = -FLT_MAX;
	if (upper > 1.E30)
	    upper =  FLT_MAX;

	namecol = c_clgeti ("namecol");
	con.globalAccum = c_clgetb ("gaccum");
	dqon = c_clgetb ("dqon");

	/* Read STIS/ACS/NICMOS/COS parameters. */
	ps = c_clopset ("nsstatpar");
	c_clgpseta (ps, "arrays",  cl_arrays,  SZ_FNAME);
	c_clgpseta (ps, "clarray", cl_clarray, SZ_FNAME);
	c_clcpset (ps);

	/* Read WFPC parameters. */
	ps = c_clopset ("wfdqpar");
	c_clgpseta (ps, "dqfext", con.dqfExten, SZ_FNAME);
	c_clcpset (ps);

	/* Set the bit masks. */
	e_getMask (&con, dqon);

	/* Parse string parameters. */
	e_parseString (&con, cl_stats,   TYPE_STAT);
	e_parseString (&con, cl_arrays,  TYPE_HDU);
	e_parseString (&con, cl_clarray, TYPE_CLHDU);

	if ((con.nhdu * con.nstats) == 0) {
	    e_error ("No selected stats or arrays.");
	    return (1);
	}

	/* Special case of 'doall' stats. */
	doall = False;
	for (i = 0; i < con.nstats; i++) {
	    if (con.stats[i] == ALL)
	        doall = True;
	}
	if (doall) {
	    con.nstats = MAX_STATS - 1;
	    for (i = 0; i < con.nstats; i++) /* see the definition for the */
	        con.stats[i] = i;            /* statType_ enumerated type. */
	}

	/* Parse range string with selected groups. */
	e_parseGroups (&con, cl_groups);
	if (!con.allGroups) {
	    errflag = False;
	    if (con.group_init <= 0)             errflag = True;
	    if (con.group_init >  con.group_end) errflag = True;
	    if (con.group_step <= 0)             con.group_step = 1;
	    if (errflag) {
	        e_error ("No valid group specification.");
	        return (1);
	    }
	}

	/* Initalize (cleanse) output pset */
	pp = c_clopset (OUT_PSET);
	c_clppseti (pp, "npix",      0);
	l_clppsetf (pp, "min",       E_INDEF);
	l_clppsetf (pp, "max",       E_INDEF);
	l_clppsetf (pp, "sum",       E_INDEF);
	l_clppsetf (pp, "mean",      E_INDEF);
	l_clppsetf (pp, "stddev",    E_INDEF);          
	l_clppsetf (pp, "histmidpt", E_INDEF);    
	l_clppsetf (pp, "histpeak",  E_INDEF);     
	l_clppsetf (pp, "skew",      E_INDEF);
	l_clppsetf (pp, "kurt",      E_INDEF);
	l_clppsetf (pp, "wmean",     E_INDEF);
	l_clppsetf (pp, "wvar",      E_INDEF);
	c_clcpset (pp);

	/* Initialize histogram pointers. */
	for (i = 0; i < MAX_HDUS; con.accum[i++].histogram = NULL);

	/* Open input lists. */
	if ((fin = c_imtopen (cl_input)) == (IRAFPointer)NULL) {
	    e_error ("Error in input file name/list.");
	    return (1);
	}
	min = c_imtopen (cl_masks);		/* Masks */
	if (min != (IRAFPointer)NULL) {
	    if ((c_imtlen (fin) != c_imtlen (min)) &&
	        (c_imtlen (min) >  1)) {
	        e_error ("Image and mask lists not of same lenght.");
	        c_imtclose (fin);
	        c_imtclose (min);
	        return (1);
	    }
	}
	qin = c_imtopen (cl_dqf);		/* DQF */
	if (qin != (IRAFPointer)NULL) {
	    if ((c_imtlen (fin) != c_imtlen (qin)) &&
	        (c_imtlen (qin) >  1)) {
	        e_error ("Image and DQF lists not of same lenght.");
	        c_imtclose (fin);
	        c_imtclose (qin);
	        if (min != (IRAFPointer)NULL)
	            c_imtclose (min);
	        return (1);
	    }
	}

	/* Print header line at STDOUT */
	e_headerLine (&con, namecol);

	/* Loop over files in the input list. */
	for (i = 0; i < c_imtlen (fin); i++) {

	    /* Read next file name in input list. */
	    c_imtgetim (fin, filename, SZ_FNAME);

	    /* Read appropriate mask name. */
	    if (min != (IRAFPointer)NULL) {
	        if ((i == 0) || (c_imtlen (fin) == c_imtlen (min)))
	            c_imtgetim (min, maskname, SZ_FNAME);
	    } else
	        *maskname = '\0'; 

	    /* Read appropriate DQF name. */
	    if (qin != (IRAFPointer)NULL) {
	        if ((i == 0) || (c_imtlen (fin) == c_imtlen (qin)))
	            c_imtgetim (qin, dqfname, SZ_FNAME);
	    } else
	        *dqfname = '\0'; 

	    /* Compute and print stats for all groups in this file. 
             * Ignore any errors and always go to next file in input list.
             */ 
	    if (e_singleFile(&con, filename, maskname, dqfname, lower, upper,
	                     namecol))
	        continue;
	}

	/* Close input file lists. */
	c_imtclose (fin);
	if (min != (IRAFPointer)NULL) c_imtclose (min);
	if (qin != (IRAFPointer)NULL) c_imtclose (qin);

	return (0);
}
