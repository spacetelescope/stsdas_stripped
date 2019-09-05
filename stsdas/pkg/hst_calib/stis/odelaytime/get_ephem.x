include	<tbset.h>
include	"delaytime.h"

define	NCOLS	4	# time, x, y, z

#  GET_EPHEM -- read an ephemeris of state vectors
#
#  Description:
#  ------------
#  Read state vectors from one or more input tables.  The times and
#  rectangular coordinates of the state vectors will be read from the
#  input table from columns "TIME" (or "JD"), "X", "Y", and "Z".  Memory
#  will be allocated for these four columns.  The times will be converted
#  to units of Modified Julian Date, and the positions will be converted
#  to astronomical units.
#
#  The number of points npts will be zero if the list of input tables
#  is empty.  A table is required for the earth ephemeris, but none is
#  required for the observer ephemeris.
#
#  Input table column names
#  ------------------------
#  JD or TIME		times corresponding to x,y,z:
#			    in earth_ephem tables:
#			        JD = Julian Day Number
#			    in obs_ephem tables:
#			        TIME = offset of time from FIRSTMJD
#  X, Y, Z		state vector components
#
#  Input table parameters
#  ----------------------
#  "FIRSTMJD"		MJD of the first row in the table (only for obs_ephem)
#
#  Date		Author		Description
#  ----		------		-----------
#  11-Mar-1990	J.-C. Hsu	design and coding
#  10-Jun-2003  Phil Hodge	allow multiple tables; allocate memory here;
#				get the array of times from a table column
#  15-Jan-2007  Phil Hodge	if keyword firstmjd is not found, read
#				ephmstrt from primary header instead
#------------------------------------------------------------------------------

procedure get_ephem (intables, eph_type, time, x, y, z, npts)

char	intables[ARB]	# i: names of the ephemeris tables
int	eph_type	# i: type of ephemeris table
pointer time		# o: pointer to the times corresponding to x,y,z (MJD)
pointer x, y, z		# o: pointers to the state vector (in AU)
int	npts		# o: total number of rows in the ephemeris tables

char	table[SZ_FNAME]		# the name of one ephemeris table
char	gridunit[SZ_COLUNITS]	# units for the TIME column
double	firstmjd		# value of FIRSTMJD header keyword
double	previous_mjd		# value of FIRSTMJD in previous table
double	timescale		# factor for converting time to days
char	colunit[SZ_COLUNITS]	# units for the X, Y and Z columns
int	nrows			# number of rows in one input table
int	k			# offset to start of data for each table
int	i, j			# loop indexes
pointer	sp, nullflag, ok_flag
bool	out_of_order		# true if any element of time is out of order

pointer tnt, tbnopen(), tp, tbtopn()
pointer	cp[NCOLS]
int	tbnlen(), tbnget()
int	ntables			# number of input tables

double	g_firstmjd()
int	tbpsta()
bool	streq()
int	strncmp()
#==============================================================================
begin
	# Open the file name template string for the list of ephemeris tables.
	tnt = tbnopen (intables)
	ntables = tbnlen (tnt)
	if (ntables == 0) {
	    call tbnclose (tnt)
	    npts = 0
	    return
	}

	# Count the total number of rows in all the input tables.
	npts = 0
	while (tbnget (tnt, table, SZ_FNAME) != EOF) {
	    tp = tbtopn (table, READ_ONLY, 0)
	    nrows = tbpsta (tp, TBL_NROWS)
	    npts = npts + nrows
	    call tbtclo (tp)
	}
	call tbnrew (tnt)

	if (npts == 0) {
	    call tbnclose (tnt)
	    call eprintf ("Tables %s are empty.\n")
		call pargstr (intables)
	    call error (1, "")
	}

	# Allocate memory.  These will be filled with data locally, and
	# the pointer values will be returned in the calling sequence.
	call calloc (time, npts, TY_DOUBLE)
	call calloc (x, npts, TY_DOUBLE)
	call calloc (y, npts, TY_DOUBLE)
	call calloc (z, npts, TY_DOUBLE)

	# Now loop over the list of input tables and read the data.
	k = 0
	previous_mjd = 0.d0
	while (tbnget (tnt, table, SZ_FNAME) != EOF) {
	    tp = tbtopn (table, READ_ONLY, 0)

	    call smark (sp)
	    call salloc (nullflag, nrows, TY_BOOL)

	    if (eph_type == EARTH_EPHEMERIS)
		call tbcfnd1 (tp, "JD", cp[1])
	    else
		call tbcfnd1 (tp, "TIME", cp[1])
	    call tbcfnd1 (tp, "X", cp[2])
	    call tbcfnd1 (tp, "Y", cp[3])
	    call tbcfnd1 (tp, "Z", cp[4])
	    do j = 1, NCOLS {
		if (cp[j] == NULL) {
		    call eprintf ("Error:  column(s) not found in %s\n")
			call pargstr (table)
		    if (eph_type == EARTH_EPHEMERIS)
			call error (1, "columns JD, X, Y, Z must exist")
		    else
			call error (1, "columns TIME, X, Y, Z must exist")
		}
	    }

	    if (eph_type == OBS_EPHEMERIS) {
		# read header parameter, the zero-point epoch
		firstmjd = g_firstmjd (table, tp)
		if (firstmjd < previous_mjd) {
		    call eprintf ("The obs_ephem tables are out of order;\n")
		    call eprintf (
"  use the FIRSTMJD table header keyword to determine the correct order.\n")
		    call error (1, "times must be monotonically increasing")
		}
		previous_mjd = firstmjd

		call tbcigt (cp[1], TBL_COL_UNITS, gridunit, SZ_COLUNITS)
		call strupr (gridunit)

		# get the scale factor for converting the times to days
		if (streq (gridunit, "DAY"))
		    timescale = 1.d0
		else if (streq (gridunit, "HR") || streq (gridunit, "HOUR"))
		    timescale = 1.d0 / HRPERDAY
		else if (strncmp (gridunit, "MIN", 3) == 0)
		    timescale = 1.d0 / MINPERDAY
		else if (gridunit[1] == 'S')
		    timescale = 1.d0 / SECPERDAY
		else
		    call error (1, "unrecognized TIME unit in ephemeris table")
	    }

	    # read the data
	    call tbcgtd (tp, cp[1], Memd[time+k], Memb[nullflag], 1, nrows)
	    call tbcgtd (tp, cp[2], Memd[x+k], Memb[nullflag], 1, nrows)
	    call tbcgtd (tp, cp[3], Memd[y+k], Memb[nullflag], 1, nrows)
	    call tbcgtd (tp, cp[4], Memd[z+k], Memb[nullflag], 1, nrows)

	    if (eph_type == EARTH_EPHEMERIS) {
		# convert Julian Day Number to MJD
		do j = k, nrows+k-1
		    Memd[time+j] = Memd[time+j] - JD_TO_MJD
	    } else {
		# scale the times, and add the zero point
		do j = k, nrows+k-1
		    Memd[time+j] = firstmjd + timescale * Memd[time+j]
	    }

	    # convert the state vectors to AU
	    call tbcigt (cp[2], TBL_COL_UNITS, colunit, SZ_COLUNITS)
	    call strupr (colunit)
	    if (streq (colunit, "AU"))
		;
	    else if (streq (colunit, "KM"))
		call adivkd (Memd[x+k], KMPERAU, Memd[x+k], nrows)
	    else
		call error (1, "unrecognized X unit in ephemeris table")

	    call tbcigt (cp[3], TBL_COL_UNITS, colunit, SZ_COLUNITS)
	    call strupr (colunit)
	    if (streq (colunit, "AU"))
		;
	    else if (streq (colunit, "KM"))
		call adivkd (Memd[y+k], KMPERAU, Memd[y+k], nrows)
	    else
		call error (1, "unrecognized Y unit in ephemeris table")

	    call tbcigt (cp[4], TBL_COL_UNITS, colunit, SZ_COLUNITS)
	    call strupr (colunit)
	    if (streq (colunit, "AU"))
		;
	    else if (streq (colunit, "KM"))
		call adivkd (Memd[z+k], KMPERAU, Memd[z+k], nrows)
	    else
		call error (1, "unrecognized Z unit in ephemeris table")

	    call tbtclo (tp)
	    call sfree (sp)

	    k = k + nrows
	}

	if (npts > 1) {
	    if (Memd[time] > Memd[time+1]) {
		call eprintf (
		"Error:  times in %s must be monotonically increasing.\n")
		    call pargstr (intables)
		call error (1, "")
	    }
	}

	if (ntables > 1) {
	    # Remove any overlapping regions.
	    call smark (sp)
	    # in this array of flags, true means the point is in chrono. order
	    call salloc (ok_flag, npts, TY_BOOL)
	    Memb[ok_flag] = true		# first element is always OK
	    out_of_order = false		# initial value
	    i = 0				# i and j are zero-indexed
	    do j = 1, npts-1 {
		if (Memd[time+i] < Memd[time+j]) {
		    Memb[ok_flag+j] = true	# OK
		    i = j			# but j will be incremented
		} else {
		    Memb[ok_flag+j] = false
		    out_of_order = true
		}
	    }
	    if (out_of_order) {
		# i and j are zero-indexed indices
		i = 1				# skip zero element, always OK
		do j = 1, npts-1 {
		    if (Memb[ok_flag+j]) {
			Memd[time+i] = Memd[time+j]
			Memd[x+i] = Memd[x+j]
			Memd[y+i] = Memd[y+j]
			Memd[z+i] = Memd[z+j]
			i = i + 1
		    }
		}
		npts = i
	    }
	    call sfree (sp)
	}

	call tbnclose (tnt)
end

# g_firstmjd -- read firstmjd keyword, or use ephmstrt
#
# Normally the FIRSTMJD keyword will be found in the table of the ORB table.
# For data taken shortly after the ORB tables began to be written as a FITS
# table (rather than as VAX binary-format ORX files), however, FIRSTMJD was
# not yet being written into the table header.  For these files, we will
# read the EPHMSTRT keyword from the primary header and convert that string
# from SMS format to MJD.

double procedure g_firstmjd (tablename, tp)

char	tablename[ARB]	# i: name as specified by user
pointer tp		# i: pointer to table
#--
double	firstmjd
pointer tp_primary	# tp for primary header
# for getting EPHMSTRT, if FIRSTMJD is not found:
char	fname[SZ_FNAME]		# name of file containing table; pri hdr name
char	extname[SZ_FNAME]	# returned by tbparse and ignored
int	hdu			# returned by tbparse and ignored
char	ephmstrt[SZ_FNAME]	# primary header keyword
int	year, doy		# year, day of year
int	month			# set to 1
double	day			# doy + time/24
int	hour, min
double	sec
int	k
int	ip, ctoi(), ctod()
pointer tbtopn()
double	tbhgtd()

begin
	iferr {
	    firstmjd = tbhgtd (tp, "FIRSTMJD")
	} then {
	    # Get the name of the file (i.e. without extension specification)
	    call tbparse (tablename, fname, extname, SZ_FNAME, hdu)
	    call strcat ("[0]", fname, SZ_FNAME)	# primary header
	    tp_primary = tbtopn (fname, READ_ONLY, NULL)
	    call tbhgtt (tp_primary, "EPHMSTRT", ephmstrt, SZ_FNAME)
	    call tbtclo (tp_primary)

	    # Interpret ephmstrt:  year, day of year, hour, minute, second
	    ip = 1
	    if (ctoi (ephmstrt, ip, year) < 1) {
		call error (1,
			"FIRSTMJD not found, and can't interpret EPHMSTRT")
	    }
	    if (ephmstrt[ip] == '.')
		ip = ip + 1
	    if (ctoi (ephmstrt, ip, doy) < 1) {
		call error (1,
			"FIRSTMJD not found, and can't interpret EPHMSTRT")
	    }
	    if (ephmstrt[ip] == ':')
		ip = ip + 1
	    hour = 0
	    min = 0
	    sec = 0.
	    day = double (doy)			# initial value
	    if (ctoi (ephmstrt, ip, hour) > 0) {
		day = day + double (hour) / 24.
		if (ephmstrt[ip] == ':')
		    ip = ip + 1
		if (ctoi (ephmstrt, ip, min) > 0) {
		    day = day + double (min) / 1440.
		    if (ephmstrt[ip] == ':')
			ip = ip + 1
		    if (ctod (ephmstrt, ip, sec) > 0) {
			day = day + sec / 86400.
		    }
		}
	    }

	    # Evaluate this expression with month = 1 because here 'day' is
	    # the day of the year rather than day of the month.
	    # reference:  Pulkinnen & VanFlandern, ApJ Suppl.
	    month = 1
	    k = 275*month/9 - 7 * (year + (month+9)/12) / 4 -
			3 * ((year + (month-9)/7) / 100 + 1) / 4
	    firstmjd = 367. * year + k + day - 678972.
	}

	return (firstmjd)
end
