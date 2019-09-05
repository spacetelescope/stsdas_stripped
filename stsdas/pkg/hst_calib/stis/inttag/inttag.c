/* INTTAG   Integrate time tag event data into an image.

   This standalone program will read a FITS file with event data
   where the X coordinate has already been corrected for the Doppler
   effect.

   The command line arguments are as follows:

   required:
       input           name of input timetag file (EVENTS & GTI tables)
       output          name of output FITS file for integrated image

   optional:
       start time      time to begin accumulating counts into image,
		       in units of seconds since the shutter opened;
		       use 'first' for the first start time in GTI table
       time increment  integrate each output imset for this many seconds
       repeat count    number of output imsets to create (number of intervals)
       -a              include all events in table, regardless of time
       -h              create high-res output image; default is low-res
       -v              verbose printed output

* Nelson Zarate October 1995.
* S.Hulbert April 1997 Change to new header keywords
* Phil Hodge, May 1997
* Phil Hodge, 9 September 1997:
	Modify to allow multiple EVENTS extensions.
* Phil Hodge, 8 October 1997:
	Print version number and date (INTTAG_VERSION) if verbose.
* Phil Hodge, 26 January 2000 (version 1.1):
	If the output will be low-res, set LORSCORR to COMPLETE,
	and update the CD matrix and CRPIX;
	delete the PutKey[] functions (use putKey[] instead).
* Phil Hodge, 8 September 2000 (version 1.2):
	For the case that the output image is to be low-res, rows with
	AXIS1 = 0 were being included in the first image column.  Zero
	was converted to xindex = -1 (zero indexed), but when this was
	divided by two to convert from high-res to low-res, -1 / 2 = 0
	(integer arithmetic), which was counted as being within the output
	image.  A check that xindex and yindex are non-negative has been
	added, prior to converting to low-res.
* Phil Hodge, 13 October 2000 (version 1.3):
	Append ".fits" to the output file name if no FITS-type extension
	was specified by the user.
* Phil Hodge, 26 April 2002 (version 1.4):
	Rewrite computation of exposure time.  Remove function get_gti_indexes,
	and do not use the variables start_index or end_index.  In the previous
	version, when an end_time fell between good-time-intervals (GTIs),
	the end_index was for the next GTI, and the exp_range function included
	that extra GTI in the computation of exposure time and the time when
	the exposure ended.  If the end_time was after the stop time in the
	last GTI, the exposure time could also be calculated incorrectly.
* Phil Hodge, 24 April 2003 (version 1.5):
	Modify tbhdr2image to call getAxes and wcsRename, and to improve
	(slightly) the handling of blank keywords.
	Add getAxes and wcsRename; the latter renames TCRVLn, etc.
*/

# include <stdio.h>
# include <time.h>
# include <string.h>
# include <ctype.h>

# include <math.h>		/* sqrt */
# include <hstio.h>
# include <ximio.h>
# include <xtables.h>
# include <xclio.h>             /* c_clgstr */
# include <c_iraf_priv.h>	/* char2iraf */
# include <c_iraf.h>

/* This string is the version number and modification date for inttag. */
# define INTTAG_VERSION "1.5 (11-April-2003)"

# define ERROR     -1
# define ERROR_RETURN	2	/* consistent with stxtools/errxit.x */
# define OK         0
# define SZ_CARD   81
# define SZ_KEYW    8
# define SZ_LINE  164
# define EXT_PER_IMSET 3
# define SEC_PER_DAY  86400.

# define max(A,B)	((A) > (B) ? (A) : (B))
# define min(A,B)	((A) < (B) ? (A) : (B))
# define NINT(x)  ((x >= 0.) ? (int) (x + 0.5) : (int) (x - 0.5))

static void get_input_parameters (int, char **, char *, char *,
		double *, Bool *, double *, int *,
		Bool *, Bool *, Bool *, int *);
static void append_extension (char *);
static void p_syntax (void);
static void p_info (Bool, Bool, double, double, double, int);
static void get_primary_header_par (char *, int*,  int*,  int*,  int*);
static void read_gti_table (char *, Bool, double **, double **, int *, Bool *);
static void get_expstart (char *, double *);
static IRAFPointer open_events_table (char *, int, int *,
		IRAFPointer *, IRAFPointer *, IRAFPointer *);
static IRAFPointer open_table (char *, int);
static void close_table (IRAFPointer);
static void full_time_range (char *, double *, double *);
static void exp_range (double *, double *, int,
		double, double, double *,
		double, double *, double *);
static int copy_primary (char *, Hdr *);
static int update_primary (char *, int, Bool, double, double, double, int);
static void UFilename (char *, Hdr *);
static int putgrpkey (char *, SingleGroup *, int, Bool,
		double *, double *, double, double, double);
static void BinCD (double *, double *);
static void tbhdr2image (IRAFPointer, Hdr *);
static void getAxes (IRAFPointer tp, int axes[]);
static void wcsRename (char *keyword, int axes[]);
static int c_tbfres (char *);
static void crash (char *);

static int status;		/* zero is OK */

void main (int argc, char **argv) {


	char input[SZ_LINE], output[SZ_LINE];
	double start_time;
	Bool start_first_gti;	/* set start_time to first GTI start time? */
	double time_interval;
	int nintervals;
	Bool all_time;		/* accept any time in events table? */
	Bool hires;		/* create high-res output? */
	Bool verbose;
	int nargs;		/* a count of command line arguments */

	SingleGroup ttag;
	IRAFPointer tp;
	IRAFPointer cp_xindex;
	IRAFPointer cp_yindex;
	IRAFPointer cp_time;
	int	    xcenter, ycenter;
	int	    nx, ny, xcorner, ycorner, x ,y;
	int	    row, nrows, xindex, yindex, imset;
	int	    i_extver;	/* extver for input EVENTS table */
	int	    o_extver;	/* imset number, extver for output file */
	int	    option = 0;
	int	    ngti, i;
	Bool        good_time;
	Bool	    output_created = False;
	Bool	    got_gti;		/* does the GTI table exist? */
	Bool	    all_done;		/* no more EVENTS tables */
	Bool	    done;		/* done with current imset */
	double      *start, *stop, time;
	double	    end_time;
	double	    exp_time;
	double	    bin, ltm[2], ltv[2];
	/* zero point for time, from events table header EXPSTART (MJD) */
	double	    tzero_mjd;
	double	    expstart, expend;		/* MJD */
	double	    texpstart, texpend;		/* MJD */
	double	    texptime = 0.;		/* seconds */


	status = OK;

	c_irafinit (argc, argv);

	initSingleGroup (&ttag);

	get_input_parameters (argc, argv, input, output,
		&start_time, &start_first_gti,
		&time_interval, &nintervals,
		&all_time, &hires, &verbose, &nargs);

	if (verbose)
	    printf ("inttag version %s\n", INTTAG_VERSION);

	bin = hires ? 1. : 2.;		/* output bin size, high-res pixels */

	/* Get info from "GTI" (Good Time Intervals) table. */
	read_gti_table (input, all_time, &start, &stop, &ngti, &got_gti);
	if (status)
	    crash ("reading GTI table");

	/* Read first EVENTS table to get expstart. */
	get_expstart (input, &tzero_mjd);
	if (status)
	    crash ("opening EVENTS table");

	/* If there's no GTI table, get the start time from the first
	   row of the first EVENTS table, and get the stop time from the
	   last row of the EVENTS table.
	*/
	if (!got_gti) {
	    full_time_range (input, start, stop);
	    if (status)
		crash ("reading last row of last EVENTS table");
	}

	/* Now assign values to parameters that were not specified. */

	if (nargs < 3)			/* third argument is start_time */
	    start_time = start[0];

	if (start_first_gti)		/* start_time = "first"? */
	    start_time = start[0];	/* first GTI start time */

	if (nargs < 4)		/* fourth argument is time_interval */
	    end_time = stop[ngti-1];
	else
	    end_time = start_time + time_interval;

	if (verbose) {
	    p_info (hires, all_time,
		start_time, end_time, time_interval, nintervals);
	}

	/* Get info from primary header of timetag file. */
	get_primary_header_par (input, &xcenter, &ycenter, &nx, &ny);

	/* zero indexed, and always in units of high-res pixels */
	xcorner = ((xcenter - nx / 2) - 1) * 2;
	ycorner = ((ycenter - ny / 2) - 1) * 2;

	if (hires) {
	    nx *= 2;
	    ny *= 2;
	}
	ltv[0] = ((bin - 2.) / 2. - xcorner) / bin;
	ltv[1] = ((bin - 2.) / 2. - ycorner) / bin;
	ltm[0] = 2. / bin;
	ltm[1] = 2. / bin;

	tp = 0;
	nrows = 0;  row = 0;
	i_extver = 0;
	o_extver = 0;
	all_done = False;

	for (imset = 0; imset < nintervals; imset++) {

	    /* Get exposure time and exposure start & stop times. */
	    exp_range (start, stop, ngti,
			start_time, end_time, &exp_time,
			tzero_mjd, &expstart, &expend);
	    if (exp_time <= 0.) {
		if (verbose) {
		    printf (
"           start: %.10g, end: %.10g (skipping, due to no overlap with GTI)\n",
			start_time, end_time);
		    fflush (stdout);
		}
		start_time = end_time;
		end_time = start_time + time_interval;
		continue;
	    }

	    o_extver++;

	    if (o_extver == 1)
		texpstart = expstart;
	    texpend = expend;		/* so far */

	    if (verbose) {
		printf (
		"imset:  %d, start: %.10g, end: %.10g, exposure time: %.10g\n",
			o_extver, start_time, end_time, exp_time);
		fflush (stdout);
	    }

	    /* Allocate space for output image */
	    allocSingleGroup (&ttag, nx, ny);

	    /* Read X, Y and TIME for the current imset. */

	    /* Loop is actually terminated with break or all_done. */
	    done = False;
	    while (!done) {

		if (tp > 0) {
		    if (row > nrows) {
			close_table (tp);
			tp = 0;
		    }
		}

		if (tp == 0) {		/* EVENTS table not open yet */

		    i_extver++;

		    /* Open table with EXTNAME=EVENTS, EXTVER=i_extver. */
		    tp = open_events_table (input, i_extver, &nrows,
				&cp_xindex, &cp_yindex, &cp_time);
		    if (status)
			crash ("opening EVENTS table");
		    if (tp == 0) {
			all_done = True;	/* no EVENTS tables left */
			break;
		    }
		    if (nrows < 1) {
			close_table (tp);
			tp = 0;
			continue;
		    }
		    if (verbose)
			printf ("EVENTS table opened, EXTVER %d\n", i_extver);

		    row = 1;
		}

		if (all_time) {
		    good_time = True;

		} else {

		    c_tbegtd (tp, cp_time, row, &time);
		    if (c_iraferr())
			crash ("getting time from table");

		    /* See if event time is in the current interval */
		    if (time < start_time) {
			row++;		/* continue with next row */
			continue;
		    } else if (time > end_time) {
			break;		/* done with current interval */
		    }

		    /* Check that time is within a GTI. */
		    good_time = False;
		    for (i = 0; i < ngti; i++) {
			if (time >= start[i] && time <= stop[i]) {
			    good_time = True;
			    break;
			}
	 	    }
		}

		if (good_time) {
		    c_tbegti (tp, cp_xindex, row, &xindex);
		    c_tbegti (tp, cp_yindex, row, &yindex);
		    if (c_iraferr())
			crash ("getting position from table");
		    /* Subtract corner and convert to zero indexed. */
		    xindex -= (xcorner + 1);
		    yindex -= (ycorner + 1);
		    /* Check for a negative value before dividing by two,
			because -1 / 2 = 0 would appear to be within the image.
		    */
		    if (xindex >= 0 && yindex >= 0) {
			if (!hires) {
			    xindex = xindex / 2;
			    yindex = yindex / 2;
			}
			/* Increment the indexed pixel. */
			if (xindex >= 0 && xindex < nx &&
			    yindex >= 0 && yindex < ny) {
			    Pix (ttag.sci.data, xindex, yindex) += 1.;
			}
		    }
		}

		row++;		/* for next iteration */
	    }

	    /* Now put values in the error array as sqrt(number_of_events).
	    */
	    for (y = 0; y < ny; y++)
		for (x = 0; x < nx; x++)
		    Pix (ttag.err.data, x, y) =
				sqrt (Pix (ttag.sci.data, x, y));

	    if (o_extver == 1) {
		/* Copy primary header from input to output. */
		copy_primary (input, ttag.globalhdr);
		if (status)
		    crash ("copying primary header");
	    }

	    /* Copy the EVENTS extension header to SCI, ERR, DQ extensions,
		and update some extension keywords.
	    */
	    putgrpkey (input, &ttag, o_extver, hires, ltm, ltv,
			expstart, expend, exp_time);
	    if (status)
		crash ("updating extension headers");

	    putSingleGroup (output, o_extver, &ttag, option);
	    if (hstio_err())
		crash ("writing output");
	    freeSingleGroup (&ttag);
	    output_created = True;

	    /* Set the interval for next image */
	    start_time = end_time;
	    end_time = start_time + time_interval;
	    texptime += exp_time;

	    if (all_done)
		break;

	}	/* end loop over time intervals */

	if (output_created) {
	    update_primary (output, o_extver, hires,
			texpstart, texpend, texptime, NINT(bin));
	    if (status)
		crash ("updating primary header");
	} else {
	    printf ("Warning:  No output created.\n");
	}

	free (start);
	free (stop);
	exit (0);
}

/* routine to get input parameters from the command line or through
 * prompts to the user.
 */

static void get_input_parameters (int argc, char **argv,
		char *input, char *output,
		double *start_time, Bool *start_first_gti,
		double *time_interval, int *nintervals,
		Bool *all_time, Bool *hires, Bool *verbose, int *nargs) {

	int i, j;

	/* default values */
	input[0] = '\0';
	output[0] = '\0';
	*start_time = 0.;
	*start_first_gti = False;
	*time_interval = 1.;
	*nintervals = 1;
	*all_time = False;
	*hires = False;
	*verbose = False;

	*nargs = 0;

	for (i = 1;  i < argc;  i++) {

	    if (argv[i][0] == '-') {

		for (j = 1;  argv[i][j] != '\0';  j++) {
		    if (argv[i][j] == 'a') {
			*all_time = True;
		    } else if (argv[i][j] == 'h') {
			*hires = True;
		    } else if (argv[i][j] == 'v') {
			*verbose = True;
		    } else {
			printf ("Unrecognized option %s\n", argv[i]);
			p_syntax();
			exit (ERROR_RETURN);
		    }
		}

	    } else if (*nargs == 0) {
		strcpy (input, argv[i]);
		*nargs = 1;

	    } else if (*nargs == 1) {
		strcpy (output, argv[i]);
		append_extension (output);	/* append .fits if absent */
		*nargs = 2;

	    } else if (*nargs == 2) {
		if (strcmp (argv[i], "first") == 0) {
		    *start_first_gti = True;
		    *start_time = -1.;
		} else {
		    *start_time = atof (argv[i]);
		}
		*nargs = 3;

	    } else if (*nargs == 3) {
		*time_interval = atof (argv[i]);
		if (*time_interval <= 0.) {
		    printf ("Time interval must be positive:  %.6g\n",
			*time_interval);
		    exit (ERROR_RETURN);
		}
		*nargs = 4;

	    } else if (*nargs == 4) {
		*nintervals = atoi (argv[i]);
		if (*nintervals < 1)
		    *nintervals = 1;
		*nargs = 5;

	    } else {
		printf ("Too many command line arguments;");
		printf (" `%s' not recognized.\n", argv[i]);
		p_syntax();
		exit (ERROR_RETURN);
	    }
	}

	if (*nargs < 2) {
	    printf ("Must specify at least input and output file names.\n");
	    p_syntax();
	    exit (ERROR_RETURN);
	}

	if (*nargs > 2 && *all_time) {
	    printf (
	"Too many command line arguments; since -a was specified,\n");
	    printf ("time interval and repeat count may not be given.\n");
	    p_syntax();
	    exit (ERROR_RETURN);
	}
}

/* This routine checks to make sure the output file name includes the
   extension ".fits" or ".fit".  If no such an extension is found,
   ".fits" will be appended to the name.
*/

static void append_extension (char *output) {

/* arguments:
char *output     io: name of output file; ".fits" may be appended
*/

	int len;	/* length of 'output' string, before modification */
	int ok_as_is;	/* true if the file name already includes extension */

	len = strlen (output);

	ok_as_is = 0;		/* initial value */

	if (len >= 5) {
	    if (strcmp (&output[len-5], ".fits") == 0)
		ok_as_is = 1;
	}

	if (!ok_as_is && len >= 4) {
	    if (strcmp (&output[len-4], ".fit") == 0)
		ok_as_is = 1;
	}

	if (!ok_as_is)
	    strcat (output, ".fits");
}

static void p_syntax (void) {

	printf ("syntax:  inttag.e input output\n");
	printf ("  optional arguments:\n");
	printf ("    start time      (sec); use 'first' for first GTI time\n");
	printf ("    time increment  (sec)\n");
	printf ("    repeat count    (number of time intervals)\n");
	printf ("   -h   high-res output\n");
	printf ("   -v   print verbose info\n");
	printf ("   -a   include all events, regardless of time\n");
}

static void p_info (Bool hires, Bool all_time,
		double start_time, double end_time,
		double time_interval, int nintervals) {

	printf ("Output will consist of");

	if (nintervals == 1) {

	    printf (" one");
	    if (hires)
		printf (" high-res");
	    else
		printf (" low-res");
	    printf (" imset,\n");

	    if (all_time) {
		printf ("  ignoring values in the TIME column");
		printf (" (i.e. all events will be included).\n");
	    } else {
		printf ("  covering the time interval %.10g to %.10g\n",
			start_time, end_time);
	    }

	} else {

	    printf (" %d", nintervals);
	    if (hires)
		printf (" high-res");
	    else
		printf (" low-res");
	    printf (" imsets,\n");

	    printf ("  start time %.10g, increment %.10g\n",
			start_time, time_interval);
	}
	fflush (stdout);
}

static void get_primary_header_par (char *input,
		int *xcenter, int *ycenter, int *nx, int *ny) {

	IRAFPointer im;
	char name[SZ_LINE];

	strcpy (name, input);
	strcat (name, "[0]");

	im = c_immap (name, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	   printf ("Cannot open Primary FITS header in file %s\n", name);
	   status = ERROR;
	   return;
	}
	*xcenter = c_imgeti (im, "CENTERA1");
	if (c_iraferr()) {
	   printf ("Keyword CENTERA1 not found in Primary header\n");
	   status = ERROR;
	   return;
	}
	*ycenter = c_imgeti (im, "CENTERA2");
	if (c_iraferr()) {
	   printf ("Keyword CENTERA2 not found in Primary header\n");
	   status = ERROR;
	   return;
	}
	*nx = c_imgeti (im, "SIZAXIS1");
	if (c_iraferr()) {
	   printf ("Keyword SIZAXIS1 not found in Primary header\n");
	   status = ERROR;
	   return;
	}
	*ny = c_imgeti (im, "SIZAXIS2");
	if (c_iraferr()) {
	   printf ("Keyword SIZAXIS2 not found in Primary header\n");
	   status = ERROR;
	   return;
	}

	c_imunmap(im);
}

/* This routine opens an EVENTS table. */

static IRAFPointer open_table (char *input, int i_extver) {

	IRAFPointer tp;
	char *name;
	int len;

	status = OK;

	len = strlen (input) + SZ_LINE;

	if ((name = calloc (len, sizeof(char))) == NULL)
	    crash ("can't allocate memory for table name");

	sprintf (name, "%s[EVENTS,%d]", input, i_extver);
	tp = c_tbtopn (name, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    status = ERROR;
	    tp = 0;
	}

	free (name);
	return (tp);
}

static void close_table (IRAFPointer tp) {

	c_tbtclo (tp);
}

/* Open the first EVENTS table to get EXPSTART from header. */

static void get_expstart (char *input, double *tzero_mjd) {

	IRAFPointer tp;

	tp = open_table (input, 1);
	if (status) {
	    printf ("Cannot open EVENTS table in file %s\n", input);
	    return;
	}

	/* Get the zero point for time. */
	*tzero_mjd = c_tbhgtd (tp, "EXPSTART");
	if (c_iraferr()) {
	    printf ("Warning:  Cannot get EXPSTART from EVENTS table header\n");
	    *tzero_mjd = 0.;
	}

	close_table (tp);
}

/* Open an input EVENTS table. */

static IRAFPointer open_events_table (char *input, int i_extver,
		int *nrows,
		IRAFPointer *cp_xindex, IRAFPointer *cp_yindex,
		IRAFPointer *cp_time) {

	IRAFPointer tp;

	tp = open_table (input, i_extver);
	if (status) {
	    /* Assume this means there are no more EVENTS tables. */
	    clear_cvoserr();
	    status = 0;
	    return 0;
	}

	/* The number of events in the table */
	*nrows = c_tbpsta (tp, TBL_NROWS);
	if (*nrows < 1)
	    printf ("Warning:  no rows in EVENTS table, EXTVER = %d\n",
			i_extver);

	/* Find the X, Y, TIME columns */
	c_tbcfnd1 (tp, "AXIS1", cp_xindex);
	if (*cp_xindex == 0) {
	    printf ("No AXIS1 column found in [EVENTS,%d] table\n", i_extver);
	    status = ERROR;
	    return 0;
	}
	c_tbcfnd1 (tp, "AXIS2", cp_yindex);
	if (*cp_yindex == 0) {
	    printf ("No AXIS2 column found in [EVENTS,%d] table\n", i_extver);
	    status = ERROR;
	    return 0;
	}
	c_tbcfnd1 (tp, "TIME", cp_time);
	if (*cp_time == 0) {
	    printf ("No TIME column found in [EVENTS,%d] table\n", i_extver);
	    status = ERROR;
	    return 0;
	}

	return tp;
}

/* This routine opens the GTI table, allocates memory for the arrays of
   start and stop times (good time intervals), and reads the GTI table
   into these arrays.

   If all_time is true, or if the GTI table doesn't exist, or if the
   number of rows is zero, the flag got_gti will be set to false, the
   number (ngti) of intervals will be set to one, and only one element
   will be allocated for the start and stop times.
*/

static void read_gti_table (char *input, Bool all_time,
		double **addr_start, double **addr_stop,
		int *ngti, Bool *got_gti) {

	IRAFPointer tp, cp_start, cp_stop;
	char 	name[SZ_LINE];
	int	i;
	double  *start, *stop;		/* local copies of pointers */

	if (all_time)
	    *got_gti = False;		/* ignore GTI table */
	else
	    *got_gti = True;

	*ngti = 1;			/* default */

	if (*got_gti) {

	    /* Append [gti] to the input name */
	    strcpy (name, input);
	    strcat (name, "[gti]");
	    tp = c_tbtopn(name, IRAF_READ_ONLY, 0);
	    if (c_iraferr()) {

		printf ("Warning:  No GTI extension in %s\n", input);
		printf ("IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
		clear_cvoserr();		/* not a fatal error */
		*got_gti = False;
		*ngti = 1;
	    }
	}

	if (*got_gti) {

	    /* The number of good time intervals in the table */
	    *ngti = c_tbpsta (tp, TBL_NROWS);
	    if (*ngti < 1) {
		close_table (tp);
		printf ("Warning:  GTI extension contains no data.\n");
		*got_gti = False;
		*ngti = 1;
	    } else {
		*got_gti = True;
	    }
	}

	*addr_start = (double *) malloc (*ngti * sizeof(double));
	*addr_stop  = (double *) malloc (*ngti * sizeof(double));

	if (*got_gti) {

	    start = *addr_start;
	    stop = *addr_stop;

	    c_tbcfnd1 (tp, "START", &cp_start);
	    if (cp_start == 0 || c_iraferr()) {
		printf ("Error finding GTI START column\n");
		status = ERROR;
		return;
	    }
	    c_tbcfnd1 (tp, "STOP", &cp_stop);
	    if (cp_stop == 0 || c_iraferr()) {
		printf ("Error finding GTI STOP column\n");
		status = ERROR;
		return;
	    }

	    /* Get data from table. */
	    for (i = 0;  i < *ngti;  i++) {
		/* row number is one indexed */
		c_tbegtd (tp, cp_start, i+1, &start[i]);
		c_tbegtd (tp, cp_stop, i+1, &stop[i]);
		if (c_iraferr()) {
		    printf ("Error reading GTI data\n");
		    status = ERROR;
		    return;
		}
	    }
	    close_table (tp);
	}
}

/* If the GTI table doesn't exist, this routine is called to get the
   start and stop times from the values in the time column for the first
   row of the first EVENTS table and the last row of the last EVENTS table
   respectively.
*/

static void full_time_range (char *input, double *start, double *stop) {

	IRAFPointer tp, cp_time;
	double first_time;	/* time read from first row of first table */
	double last_time;	/* time read from last row of current table */
	int nrows;		/* number of rows in current table */
	int i_extver;		/* extver for current table */
	Bool got_first;		/* have we read the first time yet? */

	first_time = 0.;
	last_time = 0.;
	got_first = False;

	i_extver = 1;

	tp = open_table (input, i_extver);

	/* Loop through the file, opening each EVENTS table, with
	   increasing EXTVER, until there are no more.
	*/
	while (tp > 0) {

	    nrows = c_tbpsta (tp, TBL_NROWS);

	    if (nrows > 0) {

		c_tbcfnd1 (tp, "TIME", &cp_time);
		if (cp_time == 0) {
		    printf ("No TIME column found in [EVENTS,%d] table\n",
			i_extver);
		    status = ERROR;
		    return;
		}

		if (!got_first) {
		    c_tbegtd (tp, cp_time, 1, &first_time);
		    if (c_iraferr()) {
			status = ERROR;
			return;
		    }
		    got_first = True;
		}

		c_tbegtd (tp, cp_time, nrows, &last_time);
		if (c_iraferr()) {
		    status = ERROR;
		    return;
		}
	    }

	    close_table (tp);

	    /* See if there's another EVENTS table. */
	    i_extver++;
	    tp = open_table (input, i_extver);
	}

	/* The last attempt to open the table presumably failed,
	   so clear the error flags.
	*/
	clear_cvoserr();
	status = OK;

	start[0] = first_time;
	stop[0] = last_time;
}

/* Calculate exposure time, expstart, and expend. */

static void exp_range (double *start, double *stop, int ngti,
		double start_time, double end_time, double *exp_time,
		double tzero_mjd, double *expstart, double *expend) {

/* arguments:
double start[]      i: array of GTI start times
double stop[]       i: array of GTI stop times
int ngti            i: number of GTI intervals
double start_time   i: start time for current loop
double end_time     i: end time for current loop
double exp_time     o: exposure time, seconds
double tzero_mjd    i: zero point for time, MJD
double expstart     o: current exposure start time, MJD
double expend       o: current exposure end time, MJD
*/

	double start_expt, end_expt;
	int i;

	if (end_time < start[0] || start_time > stop[ngti-1]) {
	    *exp_time = 0.;
	    *expstart = tzero_mjd;
	    *expend = tzero_mjd;
	    return;
	}

	*exp_time = 0.;
	for (i = 0;  i < ngti;  i++) {
	    if (end_time < start[i] || start_time > stop[i])
		continue;
	    start_expt = max (start[i], start_time);
	    end_expt = min (stop[i], end_time);
	    *exp_time += (end_expt - start_expt);
	}

	if (*exp_time <= 0.) {
	    *expstart = tzero_mjd;
	    *expend = tzero_mjd;
	    return;
	}

	/* The following assumes the GTI are sorted by time. */

	for (i = 0;  i < ngti;  i++) {
	    if (start_time >= start[i] && start_time <= stop[i]) {
		start_expt = start_time;
		break;
	    } else if (start_time <= start[i]) {
		start_expt = start[i];
		break;
	    }
	}
	/* Convert to days and add zero point. */
	*expstart = tzero_mjd + start_expt / SEC_PER_DAY;

	for (i = 0;  i < ngti;  i++) {
	    if (end_time >= start[i] && end_time <= stop[i]) {
		end_expt = end_time;
		break;
	    } else if (end_time < start[i]) {
		end_expt = stop[i-1];
		break;
	    }
	}
	*expend = tzero_mjd + end_expt / SEC_PER_DAY;
}

/* This routine copies the primary header from the input file input
   to the output primary header globalhdr.
*/

static int copy_primary (char *input, Hdr *globalhdr) {

	IODescPtr im;		/* descriptor for input image */
	Hdr phdr;		/* primary header for input file */

	initHdr (&phdr);

	/* Open input image in order to read its primary header. */
	im = openInputImage (input, "", 0);
	if (hstio_err())
	    return (status = 114);
	getHeader (im, &phdr);		/* get primary header */
	if (hstio_err())
	    return (status = 114);
	closeImage (im);

	copyHdr (globalhdr, &phdr);

	freeHdr (&phdr);

	return (status = 0);
}

/* This routine updates keywords in the output primary header. */

static int update_primary (char *output, int o_extver, Bool hires,
	double texpstart, double texpend, double texptime, int bin) {

	IODescPtr im;
	Hdr phdr;

	initHdr (&phdr);

	im = openUpdateImage (output, "", 0, &phdr);
	if (hstio_err())
	    return (status = 114);

	putKeyI (&phdr, "NEXTEND",
			o_extver * EXT_PER_IMSET, "Number of extensions");

	putKeyI (&phdr, "NRPTEXP", o_extver,
			"Number of repeat exposures in this file");

	putKeyD (&phdr, "TEXPSTRT", texpstart,
			"Total exposure start time (MJD)");
	putKeyD (&phdr, "TEXPEND", texpend,
			"Total exposure end time (MJD)");
	putKeyD (&phdr, "TEXPTIME", texptime,
			"Total exposure time for this file");

	putKeyI (&phdr, "BINAXIS1", bin, "");
	putKeyI (&phdr, "BINAXIS2", bin, "");

	/* If inttag is doing this step, reset the calibration switch. */
	if (!hires)
	    putKeyS (&phdr, "LORSCORR", "COMPLETE", "");

	UFilename (output, &phdr);	/* update FILENAME */

	putHeader (im);
	if (hstio_err())
	    return (status = 114);
	closeImage (im);

	freeHdr (&phdr);

	return (status = 0);
}

/* This routine updates the FILENAME primary header keyword, or adds it
   to the header if it's not already present.  If the input file name
   begins with an environment variable or an explicit directory, that will
   be skipped over before writing the name to the header.
   The section for finding a directory prefix is based on the iraf zfnbrk
   function.

   This was copied from the calstis lib/ufilename.c.
*/

static void UFilename (char *filename, Hdr *phdr) {

/* arguments:
char *filename  i: file name, but may begin with directory specification
Hdr *phdr       io: pointer to header; FILENAME will be updated
*/

	int ch;			/* one character in filename */
	int namelen;		/* length of filename */
	int start = 0;		/* start of file name */
	int i;

	namelen = strlen (filename);

	/* If there's a directory prefix, skip over it. */
	for (i = 0;  i < namelen;  i++) {
	    ch = filename[i];
	    if (isalnum(ch) || ch == '_' || ch == '.')
		continue;		/* ordinary character */
	    else if (ch == '\\' && i+1 < namelen)
		i++;			/* skip \ and next character */
	    else
		start = i + 1;	/* not ordinary, so part of directory prefix */
	}

	/* Were we unable to find the filename root? */
	if (start >= namelen - 1)
	    start = 0;

	/* Update the FILENAME keyword, or add it if it doesn't exist. */
	putKeyS (phdr, "FILENAME", &filename[start], "name of file");
}

/* This routine copies the EVENTS extension header to the SCI, ERR, and DQ
   extensions, and it updates some extension keywords.
*/

static int putgrpkey (char *input, SingleGroup *ttag, int o_extver,
		Bool hires, double *ltm, double *ltv,
		double expstart, double expend, double exp_time) {

/* arguments:
char *input         i: name of input file
SingleGroup *ttag   i: pointer to output data (headers updated)
int o_extver        i: image set number
Bool hires          i: true if output will be high-res
double ltm[2]       i: diagonal of LTM matrix
double ltv[2]       i: linear transformation, vector part
double tzero_mjd    i: zero point for time, MJD
double expstart     i: current exposure start time, MJD
double expend       i: current exposure end time, MJD (same as total so far)
double exp_time     i: exposure time, seconds
*/

	IRAFPointer tp;

	/* These keywords will be modified if the output is being
	   binned to low-res.
	   The CD matrix is mapped to the 1-D array cd in the following
	   order:  CD1_1, CD1_2, CD2_1, CD2_2
	*/
	double cd[4], crpix[2];

	tp = open_table (input, 1);
	if (status || tp == 0)
	    crash ("in putgrpkey, opening first EVENTS extension");

	/* Copy the primary header from input to the output extensions. */
	tbhdr2image (tp, &ttag->sci.hdr);
	tbhdr2image (tp, &ttag->err.hdr);
	tbhdr2image (tp, &ttag->dq.hdr);
	if (status)
	    return (status);

	/* If the output is low-res, we need to modify the CD matrix
	   and CRPIX keywords.
	*/
	if (!hires) {
	    if (getKeyD (&ttag->sci.hdr, "CD1_1", &cd[0]))
		cd[0] = 1.;
	    if (getKeyD (&ttag->sci.hdr, "CD1_2", &cd[1]))
		cd[1] = 0.;
	    if (getKeyD (&ttag->sci.hdr, "CD2_1", &cd[2]))
		cd[2] = 0.;
	    if (getKeyD (&ttag->sci.hdr, "CD2_2", &cd[3]))
		cd[3] = 1.;
	    if (getKeyD (&ttag->sci.hdr, "CRPIX1", &crpix[0]))
		crpix[0] = 1.;
	    if (getKeyD (&ttag->sci.hdr, "CRPIX2", &crpix[1]))
		crpix[1] = 1.;
	    BinCD (cd, crpix);
	}

	/* SCI extension */

	putKeyS (&ttag->sci.hdr, "EXTNAME", "SCI", "Extension name");
	putKeyI (&ttag->sci.hdr, "EXTVER", o_extver, "Extension version");

	putKeyD (&ttag->sci.hdr, "LTM1_1", ltm[0], "");
	putKeyD (&ttag->sci.hdr, "LTM2_2", ltm[1], "");
	putKeyD (&ttag->sci.hdr, "LTV1", ltv[0], "");
	putKeyD (&ttag->sci.hdr, "LTV2", ltv[1], "");

	if (!hires) {
	    putKeyD (&ttag->sci.hdr, "CD1_1", cd[0], "");
	    putKeyD (&ttag->sci.hdr, "CD1_2", cd[1], "");
	    putKeyD (&ttag->sci.hdr, "CD2_1", cd[2], "");
	    putKeyD (&ttag->sci.hdr, "CD2_2", cd[3], "");
	    putKeyD (&ttag->sci.hdr, "CRPIX1", crpix[0], "");
	    putKeyD (&ttag->sci.hdr, "CRPIX2", crpix[1], "");
	}

	putKeyD (&ttag->sci.hdr, "EXPSTART", expstart,
			"Start time of exposure");
	putKeyD (&ttag->sci.hdr, "EXPEND", expend, "End time of exposure");
	putKeyD (&ttag->sci.hdr, "EXPTIME", exp_time, "Exposure time");

	/* ERR extension */

	putKeyS (&ttag->err.hdr, "EXTNAME", "ERR", "Extension name");
	putKeyI (&ttag->err.hdr, "EXTVER", o_extver, "Extension version");

	putKeyD (&ttag->err.hdr, "LTM1_1", ltm[0], "");
	putKeyD (&ttag->err.hdr, "LTM2_2", ltm[1], "");
	putKeyD (&ttag->err.hdr, "LTV1", ltv[0], "");
	putKeyD (&ttag->err.hdr, "LTV2", ltv[1], "");

	if (!hires) {
	    putKeyD (&ttag->err.hdr, "CD1_1", cd[0], "");
	    putKeyD (&ttag->err.hdr, "CD1_2", cd[1], "");
	    putKeyD (&ttag->err.hdr, "CD2_1", cd[2], "");
	    putKeyD (&ttag->err.hdr, "CD2_2", cd[3], "");
	    putKeyD (&ttag->err.hdr, "CRPIX1", crpix[0], "");
	    putKeyD (&ttag->err.hdr, "CRPIX2", crpix[1], "");
	}

	putKeyD (&ttag->err.hdr, "EXPSTART", expstart,
			"Start time of exposure");
	putKeyD (&ttag->err.hdr, "EXPEND", expend, "End time of exposure");
	putKeyD (&ttag->err.hdr, "EXPTIME", exp_time, "Exposure time");

	/* DQ extension */

	putKeyS (&ttag->dq.hdr, "EXTNAME", "DQ", "Extension name");
	putKeyI (&ttag->dq.hdr, "EXTVER", o_extver, "Extension version");

	putKeyD (&ttag->dq.hdr, "LTM1_1", ltm[0], "");
	putKeyD (&ttag->dq.hdr, "LTM2_2", ltm[1], "");
	putKeyD (&ttag->dq.hdr, "LTV1", ltv[0], "");
	putKeyD (&ttag->dq.hdr, "LTV2", ltv[1], "");

	if (!hires) {
	    putKeyD (&ttag->dq.hdr, "CD1_1", cd[0], "");
	    putKeyD (&ttag->dq.hdr, "CD1_2", cd[1], "");
	    putKeyD (&ttag->dq.hdr, "CD2_1", cd[2], "");
	    putKeyD (&ttag->dq.hdr, "CD2_2", cd[3], "");
	    putKeyD (&ttag->dq.hdr, "CRPIX1", crpix[0], "");
	    putKeyD (&ttag->dq.hdr, "CRPIX2", crpix[1], "");
	}

	putKeyD (&ttag->dq.hdr, "EXPSTART", expstart,
			"Start time of exposure");
	putKeyD (&ttag->dq.hdr, "EXPEND", expend, "End time of exposure");
	putKeyD (&ttag->dq.hdr, "EXPTIME", exp_time, "Exposure time");
	if (status)
	    return (status);

	close_table (tp);

	return (status = 0);
}

/* This routine converts the CD matrix and the CRPIX keywords from
   high-res pixels to low-res pixels.
*/

static void BinCD (double *cd, double *crpix) {

/* arguments:
double cd[4]            io: CD matrix
double crpix[2]         io: reference pixel
*/

	cd[0] *= 2.;			/* CD1_1 */
	cd[1] *= 2.;			/* CD1_2 */
	cd[2] *= 2.;			/* CD2_1 */
	cd[3] *= 2.;			/* CD2_2 */

	crpix[0] = (crpix[0] + 0.5) / 2.;
	crpix[1] = (crpix[1] + 0.5) / 2.;
}

/* routine to copy table header parameters into an image extension header.
   The original STIS time-tag tables had image-specific WCS keywords, such
   as CTYPE1 & CTYPE2.  If the input is this type of file, we will just copy
   those WCS keywords to the output file; however, we will also delete any
   pixel-list WCS keywords that we find, such as TCTYP2 & TCTYP3.
   More recent time-tag tables have only the pixel list keywords; in this
   case we will rename these keywords to the corresponding image-specific
   keywords, e.g. TCTYP2 --> CTYPE1.
*/

static void tbhdr2image (IRAFPointer tp, Hdr *h) {

	char card[SZ_CARD], keyword[SZ_KEYW+1];
	char comment[SZ_CARD];
	int  i, nlines, dtype;
	int axes[2];		/* column numbers for AXIS1 & AXIS2 */
	/* true if input file has image-type WCS keywords CTYPEi, etc */
	int image_wcs = 0;

	getAxes (tp, axes);	/* get column numbers of spatial axes */

	nlines = c_tbpsta (tp, TBL_NPAR);
	for (i=1; i <= nlines; i++) {

	    /* get the Ith parameter and its comment */
	    c_tbhgnp (tp, i, keyword, &dtype, card);
	    c_tbhgcm (tp, keyword, comment, SZ_CARD-1);

	    /* Do we already have image-specific WCS keywords?  (Setting
		this flag based on CTYPE assumes that the image-specific
		keywords precede the pixel list type keywords.)
	    */
	    if (strncmp (keyword, "CTYPE", 5) == 0)
		image_wcs = 1;

	    /* rename the keyword if it is a pixel-list WCS keyword */
	    if (!image_wcs)
		wcsRename (keyword, axes);

	    if (c_tbfres(keyword))	/* don't copy reserved keywords */
		continue;

	    switch(dtype) {
	    case IRAF_INT:
		addIntKw (h, keyword, atoi(card), comment);
		break;
	    case IRAF_REAL:
		addFloatKw (h, keyword, atof(card), comment);
		break;
	    case IRAF_DOUBLE:
		addDoubleKw (h, keyword, atof(card), comment);
		break;
	    case IRAF_BOOL:
		addBoolKw (h, keyword, atoi(card)==True, comment);
		break;
	    case IRAF_CHAR:
		if (keyword[0] == '\0') {
		    if (strlen (card) > 0)
			addSpacesKw (h, card);
		    else
			addSpacesKw (h, "        /");
		} else {
		    addStringKw (h, keyword, card, comment);
		}
		break;
	    default:
		printf ("TimeTag:  Table parameter type not defined: %d\n",
			dtype);
		status = ERROR;
		return;
	    }
	}
	putKeyI (h, "WCSAXES", 2, "coordinate dimensionality");
}

/* This routine finds columns AXIS1 and AXIS2, and it assigns the numbers
   of those columns to the elements of the axes argument.  axes is a
   two-element array, and on output its values should be the column numbers
   of the AXIS1 and AXIS2 columns.  Column numbers in FITS are one-indexed,
   so axes will be initialized to zero to allow checking that the columns
   do exist, although this check has already been done previously.
*/

static void getAxes (IRAFPointer tp, int axes[]) {

	char keyword[SZ_KEYW+1], colname[SZ_CARD];
	int ncols;		/* number of columns in table */
	int n;			/* column number */

	ncols = c_tbpsta (tp, TBL_NCOLS);

	axes[0] = 0;
	axes[1] = 0;
	for (n = 1;  n <= ncols;  n++) {
	    sprintf (keyword, "TTYPE%d", n);
	    c_tbhgtt (tp, keyword, colname, SZ_CARD-1);
	    if (strcmp (colname, "AXIS1") == 0)
		axes[0] = n;
	    else if (strcmp (colname, "AXIS2") == 0)
		axes[1] = n;
	}
}

/* This routine renames some WCS keywords from "pixel list" style to
   image style.  Assuming AXIS1 and AXIS2 are in columns 2 and 3
   respectively, the input and modified keywords are as follows:

	TCTYP2 --> CTYPE1
	TCTYP3 --> CTYPE2
	TCRPX2 --> CRPIX1
	TCRPX3 --> CRPIX2
	TCRVL2 --> CRVAL1
	TCRVL3 --> CRVAL2
	TCUNI2 --> CUNIT1
	TCUNI3 --> CUNIT2
	TC2_2  --> CD1_1
	TC2_3  --> CD1_2
	TC3_2  --> CD2_1
	TC3_3  --> CD2_2
*/

static void wcsRename (char *keyword, int axes[]) {

	char wcs_key[SZ_KEYW+1];
	int n, k;		/* column numbers, one indexed */
	int i;			/* image axis number, one indexed */

	for (i = 1;  i <= 2;  i++) {
	    n = axes[i-1];

	    sprintf (wcs_key, "TCTYP%d", n);		/* TCTYPn --> CTYPEi */
	    if (strcmp (keyword, wcs_key) == 0)
		sprintf (keyword, "CTYPE%d", i);

	    sprintf (wcs_key, "TCRPX%d", n);		/* TCRPXn --> CRPIXi */
	    if (strcmp (keyword, wcs_key) == 0)
		sprintf (keyword, "CRPIX%d", i);

	    sprintf (wcs_key, "TCRVL%d", n);		/* TCRVLn --> CRVALi */
	    if (strcmp (keyword, wcs_key) == 0)
		sprintf (keyword, "CRVAL%d", i);

	    sprintf (wcs_key, "TCUNI%d", n);		/* TCUNIn --> CUNITi */
	    if (strcmp (keyword, wcs_key) == 0)
		sprintf (keyword, "CUNIT%d", i);
	}

	n = axes[0];		/* column number of column AXIS1 */
	k = axes[1];		/* column number of column AXIS2 */

	sprintf (wcs_key, "TC%d_%d", n, n);		/* TCn_n --> CD1_1 */
	if (strcmp (keyword, wcs_key) == 0)
	    strcpy (keyword, "CD1_1");

	sprintf (wcs_key, "TC%d_%d", n, k);		/* TCn_k --> CD1_2 */
	if (strcmp (keyword, wcs_key) == 0)
	    strcpy (keyword, "CD1_2");

	sprintf (wcs_key, "TC%d_%d", k, n);		/* TCk_n --> CD2_1 */
	if (strcmp (keyword, wcs_key) == 0)
	    strcpy (keyword, "CD2_1");

	sprintf (wcs_key, "TC%d_%d", k, k);		/* TCk_k --> CD2_2 */
	if (strcmp (keyword, wcs_key) == 0)
	    strcpy (keyword, "CD2_2");
}

# if defined(NO_UNDERSCORE)
# define TBFRES tbfres
# else
# define TBFRES tbfres_
# endif

extern int TBFRES (short *);

/* This function returns 1 if the input keyword is a reserved keyword
   for tables, i.e. if it should not be copied to output.
*/

static int c_tbfres (char *keyword) {

	int len;

	if (TBFRES ((short *)char2iraf(keyword,0))) {

	    return (1);

	} else {

	    len = strlen (keyword);
	    if (!isdigit (keyword[len-1]))
		return (0);

	    len--;
	    while (len > 0 && isdigit (keyword[len-1]))
		len--;

	    if (len < 1)
		return (0);

	    if (strncmp (keyword, "TCTYPE", len) == 0)
		return (1);
	    if (strncmp (keyword, "TCRVL", len) == 0)
		return (1);
	    if (strncmp (keyword, "TCDLT", len) == 0)
		return (1);
	    if (strncmp (keyword, "TCRPX", len) == 0)
		return (1);
	}

	return (0);
}

static void crash (char *message) {

	if (message[0] != '\0')
	    printf ("ERROR %s\n", message);

	if (status)
	    printf ("status = %d\n", status);

	if (hstio_err())
	    printf ("HSTIO error:  %s\n", hstio_errmsg());
	else if (c_iraferr())
	    printf ("IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());

	exit (ERROR_RETURN);
}
