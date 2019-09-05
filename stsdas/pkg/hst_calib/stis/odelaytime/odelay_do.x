include	<time.h>
include	<tbset.h>
include "delaytime.h"
define	NLAN_EARTH	10
define	NLAN_OBS	10
define	INTERVAL	0.1			# in second

# These are the file types that are supported.
define	EVENTS_TABLE	1
define	X1D_TABLE	2
define	SCI_IMAGE	3

#  ODELAY_DO -- Apply correction for light delay time for STIS data
#
#  Description:
#  ------------
#  Compute the times of observation relative to the solar-system barycenter
#  of a distant object (outside the solar system).  It is assumed that
#  the set of observations covers a short enough time that the object does
#  not move appreciably during the observations.  Note that a one-arcsecond
#  error in the position of the object can result in a time-delay error
#  of ~2.4 millisec.  It is also assumed that the set of observations covers
#  a short enough time interval that the relativistic correction, if required,
#  remains constant during the observations.
#
#  Date		Author		Description
#  ----		------		-----------
#  10-Nov-1984	C. D. Biemesderfer	Original module
#  18-Apr-1990  J.-C. Hsu	rewrite in SPP
#  19-Jun-1992  J.-C. Hsu	read RA_TARG and DEC_TARG from header
#  19-Aug-1997  J.-C. Hsu	modify for STIS data
#  23-Aug-2000  Phil Hodge	remove out_col & add verbose arguments;
#				modify times in-place; update GTI table and
#				header keywords as well; change INTERVAL
#				from 1. to 0.1; add DELAYCOR and HISTORY
#  10-Jun-2003  Phil Hodge	modify to work with SCI extensions;
#				change calling sequence (ephem table names
#				instead of table pointers)
#------------------------------------------------------------------------------

procedure odelay_do (fin, nfin, parallax, earth_ephem, obs_ephem,
		in_col, verbose)

pointer	fin			# i: file template pointer
int	nfin 			# i: number of files in the input template
double	parallax		# i: parallax of the target (in arc sec)
char	earth_ephem[ARB]	# i: name of the earth_ephem table
char	obs_ephem[ARB]		# i: names of the obs_ephem tables
char	in_col[ARB]		# i: time column name
bool	verbose			# i: print timing info?
#--
pointer sp
pointer history		# for constructing a history record
pointer extname		# from first extension, for checking file type
double	ra, dec		# RA and Dec of the target at J2000 (in deg.)
double	objvec[3]	# unit geocentric state vector of the target
double	mjd1, mjd2 	# MJD of the observation start/end time
double	epoch		# MJD of an event
double	delta_sec	# correction (sec) to be added to TIME col
double	t0_delay	# correction at TEXPSTRT
char	ifile[SZ_FNAME]
char	ifilen[SZ_FNAME]
int	nchar, i, j, k
pointer	time_e, x_e, y_e, z_e, time_o, x_o, y_o, z_o
pointer	tp		# pointer of the input table
pointer	cptr		# for the TIME column in EVENTS tables
pointer cp1, cp2	# for START and STOP columns in GTI table
int	nrows		# number of rows in a table
int	nextend		# keyword from primary header
int	nevents_tab	# number of EVENTS tables (one less than NEXTEND, or 0)
int	nsci_ext	# number of x1d or image extensions (0 or NEXTEND)
int	npts_earth, npts_obs
double	tm		# a time from the TIME column
double	tm_prev		# the last tm for which all_delay was called
char	text[SZ_TIME]	# for printing timing info
char	delaycorr[SZ_FNAME]
int	frac		# for printing percentage done
int	filetype	# events table, x1d table, image set
bool	modified	# true if an x1d or image extension was updated
	
pointer	tbtopn()
int	tbpsta()
double	tbhgtd()
int	tbhgti()
int	imtgetim()
long	clktime()
int	strlen()
bool	streq()
#==============================================================================
begin
	call smark (sp)
	call salloc (history, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	# allocate memory for the ephemerides, and read in the data
	call get_ephem (earth_ephem, EARTH_EPHEMERIS,
		time_e, x_e, y_e, z_e, npts_earth)
	call get_ephem (obs_ephem, OBS_EPHEMERIS,
		time_o, x_o, y_o, z_o, npts_obs)

	# loop over all input files
	do j = 1, nfin {

	    # read the next file name in the template list
	    nchar = imtgetim (fin, ifile, SZ_FNAME)
	    if (strlen (ifile) <= 0) next

	    if (verbose) {
		call printf ("odelaytime:  processing %s ...\n")
		    call pargstr (ifile)
		call flush(STDOUT)
	    }

	    # determine the file type, based on the first extension
	    call strcpy (ifile, ifilen, SZ_FNAME)
	    call strcat ("[1]", ifilen, SZ_FNAME)
	    iferr (tp = tbtopn (ifilen, READ_ONLY, 0)) {
                call eprintf ("Cannot open the input file '%s' (skipping)\n")
                    call pargstr (ifilen)
            	next
	    }
	    call tbhgtt (tp, "EXTNAME", Memc[extname], SZ_FNAME)
	    call strupr (Memc[extname])
	    if (tbpsta (tp, TBL_SUBTYPE) == TBL_SUBTYPE_IMAGE) {
		filetype = SCI_IMAGE
	    } else if (streq (Memc[extname], "EVENTS")) {
		filetype = EVENTS_TABLE
	    } else if (streq (Memc[extname], "SCI")) {
		filetype = X1D_TABLE
	    } else {
		call tbtclo (tp)
                call eprintf ("Unknown file type for '%s' (skipping)\n")
                    call pargstr (ifile)
		next
	    }
	    call tbtclo (tp)

	    # open the primary header
	    call strcpy (ifile, ifilen, SZ_FNAME)
	    call strcat ("[0]", ifilen, SZ_FNAME)
	    iferr (tp = tbtopn (ifilen, READ_ONLY, 0)) {
                call eprintf (
		"Cannot open the primary header of '%s' (skipping)\n")
                    call pargstr (ifile)
            	next
	    }

	    # check whether this table has already been corrected
	    iferr {
		call tbhgtt (tp, "DELAYCOR", delaycorr, SZ_FNAME)
	    } then {
		call strcpy ("PERFORM", delaycorr, SZ_FNAME)
	    }
	    if (streq (delaycorr, "COMPLETE")) {
		call eprintf ("%s has already been corrected,\n")
		    call pargstr (ifile)
		call eprintf (
		"  so no further processing will be applied to this file.\n")
		call tbtclo (tp)
		next
	    }

	    # get these for checking the range and for updating the GTI table
	    mjd1 = tbhgtd (tp, "TEXPSTRT")
	    mjd2 = tbhgtd (tp, "TEXPEND")

	    ra = tbhgtd (tp, "RA_TARG")
	    dec = tbhgtd (tp, "DEC_TARG")
	    iferr {
		nextend = tbhgti (tp, "NEXTEND")
	    } then {
		nextend = 1
	    }
	    call tbtclo (tp)

	    if (filetype == EVENTS_TABLE) {
		# assume (for now) that the last extension is a GTI table
		nevents_tab = max (nextend - 1, 1)
		nsci_ext = 0
	    } else if (filetype == X1D_TABLE) {
		nevents_tab = 0
		nsci_ext = nextend
	    } else {				# file contains image sets
		nevents_tab = 0
		nsci_ext = nextend
	    }

	    # check the range
	    if (mjd1 < Memd[time_e] || mjd2 > Memd[time_e+npts_earth-1] ) {
		call eprintf ("Epoch is outside the Earth ephemeris range,\n")
		call eprintf ("so %s will be skipped\n")
		    call pargstr (ifile)
		next
	    }
	    if (npts_obs > 0) {
		if (mjd1 < Memd[time_o] || mjd2 > Memd[time_o+npts_obs-1] ) {
		    call eprintf ("Epoch is outside the obs ephemeris range\n")
		    call eprintf ("so %s will be skipped\n")
			call pargstr (ifile)
		    next
		}
	    }

	    # Check that all expected extensions and the time column are
	    # present.  We do this here before making any change, to avoid
	    # the possibility of correcting only part of the input file.
	    iferr {
		call ext_exist (ifile, in_col, nevents_tab, nsci_ext)
	    } then {
		next
	    }

	    # calculate the target's positional vector
	    call object_pos (ra, dec, objvec)

	    # Get the delay at the exposure start time.  We'll subtract this
	    # delay from each time that we update, if that time is relative
	    # to EXPSTART (which must be the same as TEXPSTRT).
	    call all_delay (mjd1, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, t0_delay)

	    if (filetype == EVENTS_TABLE) {
		# update times in GTI table
		call strcpy (ifile, ifilen, SZ_FNAME)
		call strcat ("[GTI]", ifilen, SZ_FNAME)
		iferr (tp = tbtopn (ifilen, READ_WRITE, 0)) {
		    call eprintf ("Warning:  %s has no GTI extension\n")
			call pargstr (ifile)
		    # if there's no GTI, last extension is an EVENTS table
		    nevents_tab = nevents_tab + 1
		} else {
		    call tbcfnd1 (tp, "START", cp1)
		    call tbcfnd1 (tp, "STOP", cp2)
		    if (cp1 == NULL || cp2 == NULL) {
			call tbtclo (tp)
			call error (1,
		"either START or STOP column was not found in GTI extension")
		    }
		    nrows = tbpsta (tp, TBL_NROWS)
		    do i = 1, nrows {
			call tbegtd (tp, cp1, i, tm)
			epoch = mjd1 + tm / SECPERDAY
			call all_delay (epoch, parallax, objvec,
			    Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			    npts_earth,
			    Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			    npts_obs, delta_sec)
			call tbeptd (tp, cp1, i, tm + (delta_sec - t0_delay))
			call tbegtd (tp, cp2, i, tm)
			epoch = mjd1 + tm / SECPERDAY
			call all_delay (epoch, parallax, objvec,
			    Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			    npts_earth,
			    Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			    npts_obs, delta_sec)
			call tbeptd (tp, cp2, i, tm + (delta_sec - t0_delay))
		    }
		    if (verbose) {
			call printf ("  GTI extension has been updated\n")
			call flush(STDOUT)
		    }
		}
	    }

	    # loop through all EVENTS extensions (there will be none if
	    # the input is an x1d or image file)
	    do k = 1, nevents_tab {
		call sprintf (ifilen, SZ_FNAME, "%s[EVENTS,%d]")
		    call pargstr (ifile)
		    call pargi (k)
		tp = tbtopn (ifilen, READ_WRITE, 0)

	        mjd1 = tbhgtd (tp, "EXPSTART")
	        mjd2 = tbhgtd (tp, "EXPEND")

                # find the time (since EXPSTART) column in the table
                call tbcfnd1 (tp, in_col, cptr)
		if (cptr == NULL)	# shouldn't happen (already checked)
		    call error (1, "time column not found")

                # find how many rows are there
                nrows = tbpsta (tp, TBL_NROWS)

	        tm_prev = 0.		# just to have a definite starting value

	        # print out timing info
		if (verbose) {
	            call cnvtime (clktime(0), text, SZ_TIME)
	            call printf ("  start time:  %s\n")
	                call pargstr(text)
	            call flush (STDOUT)

	            frac = nrows / 10
	            call printf ("    Percentage done:  ")
	            call flush(STDOUT)
		}

	        # go through each row
	        do i = 1, nrows {

		    if (verbose) {
    		        if (mod(i,frac) == 0) {
	                    call printf (" %d")
	                        call pargi(i/frac*10)
	                    call flush(STDOUT)
                        }
		    }

		    call tbegtd (tp, cptr, i, tm)

		    # if the time is within INTERVAL of the previous time,
		    # apply the same delaytime
		    if (i == 1 || (tm - tm_prev) > INTERVAL) {
		        epoch = mjd1 + tm / SECPERDAY

			call all_delay (epoch, parallax, objvec,
				Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
				npts_earth,
				Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
				npts_obs, delta_sec)
			tm_prev = tm
	 	    }

		    # write the corrected time back to the time column
		    call tbeptd (tp, cptr, i, tm + (delta_sec - t0_delay))
	        }

		# add delaytime to EXPSTART and EXPEND, and update header
		call all_delay (mjd1, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
		call tbhptd (tp, "EXPSTART", mjd1 + delta_sec/SECPERDAY)
		call all_delay (mjd2, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
		call tbhptd (tp, "EXPEND", mjd2 + delta_sec/SECPERDAY)

	        # close input table
	        call tbtclo (tp)

	        if (verbose) {
		    call printf ("\n")		# finish the "Percentage done"
		    call printf ("    [EVENTS,%d] extension has been updated\n")
			call pargi (k)
		    call cnvtime (clktime(0), text, SZ_TIME)
		    call printf ("  finish time:  %s\n")
			call pargstr(text)
		    call flush(STDOUT)
		}
	    }

	    # Loop through all x1d or image extensions (there will be none if
	    # the input is an events file).  Note that we only expect EXPSTART
	    # and EXPEND to be present in SCI extensions; however, they
	    # could be in ERR and DQ as well (e.g. in output from inttag),
	    # and if they are present they must be updated.
	    do k = 1, nsci_ext {
		call sprintf (ifilen, SZ_FNAME, "%s[%d]")
		    call pargstr (ifile)
		    call pargi (k)
		tp = tbtopn (ifilen, READ_WRITE, 0)

		modified = false		# initial value

		# add delaytime to EXPSTART and EXPEND, and update header
		call tbhfkw (tp, "EXPSTART", i)
		if (i > 0) {			# was keyword found?
		    mjd1 = tbhgtd (tp, "EXPSTART")
		    call all_delay (mjd1, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
		    call tbhptd (tp, "EXPSTART", mjd1 + delta_sec/SECPERDAY)
		    modified = true
		}

		call tbhfkw (tp, "EXPEND", i)
		if (i > 0) {
		    mjd2 = tbhgtd (tp, "EXPEND")
		    call all_delay (mjd2, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
		    call tbhptd (tp, "EXPEND", mjd2 + delta_sec/SECPERDAY)
		    modified = true
		}

	        # close input extension
	        call tbtclo (tp)

	        if (verbose && modified) {
		    call printf ("    extension %d has been updated\n")
			call pargi (k)
		    call flush(STDOUT)
		}
	    }

	    # add delaytime to TEXPSTRT and TEXPEND, and update primary header
	    call strcpy (ifile, ifilen, SZ_FNAME)
	    call strcat ("[0]", ifilen, SZ_FNAME)
	    tp = tbtopn (ifilen, READ_WRITE, 0)

	    mjd1 = tbhgtd (tp, "TEXPSTRT")
	    call all_delay (mjd1, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
	    call tbhptd (tp, "TEXPSTRT", mjd1 + delta_sec/SECPERDAY)

	    mjd2 = tbhgtd (tp, "TEXPEND")
	    call all_delay (mjd2, parallax, objvec,
			Memd[time_e], Memd[x_e], Memd[y_e], Memd[z_e],
			npts_earth,
			Memd[time_o], Memd[x_o], Memd[y_o], Memd[z_o],
			npts_obs, delta_sec)
	    call tbhptd (tp, "TEXPEND", mjd2 + delta_sec/SECPERDAY)

	    # add keyword to flag the fact that the times have been corrected
	    call tbhadt (tp, "DELAYCOR", "COMPLETE")
	    call tbhpcm (tp, "DELAYCOR", "delaytime has been applied")
	    call tbhadt (tp, "HISTORY",
		"Times corrected to solar system barycenter;")
	    if (npts_obs > 0) {
		call sprintf (Memc[history], SZ_FNAME, "ORX table %s")
		    call pargstr (obs_ephem)
	    } else {
		call strcpy ("no ORX table was used.", Memc[history], SZ_FNAME)
	    }
	    call tbhadt (tp, "HISTORY", Memc[history])

	    call tbtclo (tp)

	    if (verbose) {
		call printf ("... done\n")
		call flush(STDOUT)
	    }
	}

	# free the memory
	call mfree (time_e, TY_DOUBLE)
	call mfree (x_e, TY_DOUBLE)
	call mfree (y_e, TY_DOUBLE)
	call mfree (z_e, TY_DOUBLE)
	if (npts_obs > 0) {
	    call mfree (time_o, TY_DOUBLE)
	    call mfree (x_o, TY_DOUBLE)
	    call mfree (y_o, TY_DOUBLE)
	    call mfree (z_o, TY_DOUBLE)
	}

	call sfree (sp)
end

# Check that all expected extensions are present (except GTI).
# Also check that the time column exists, if the input is a time-tag file.

procedure ext_exist (ifile, in_col, nevents_tab, nsci_ext)

char	ifile[ARB]	# i: name of input file
char	in_col[ARB]	# i: time column name
int	nevents_tab	# i: number of EVENTS tables
int	nsci_ext	# i: number of SCI (or SCI+ERR+DQ) extensions
#--
char	ifilen[SZ_FNAME]
int	k
pointer tp, cptr
pointer tbtopn()

begin
	# If the input is a time-tag file, look for events extensions.
	do k = 1, nevents_tab {
	    call sprintf (ifilen, SZ_FNAME, "%s[EVENTS,%d]")
		call pargstr (ifile)
		call pargi (k)
	    iferr {
		tp = tbtopn (ifilen, READ_ONLY, 0)
	    } then {
		call eprintf ("Cannot open '%s' (skipping this file)\n")
		    call pargstr (ifilen)
		call error (1, "")
	    }
	    # check that the time column exists
	    call tbcfnd1 (tp, in_col, cptr)
	    if (cptr == NULL) {
		call eprintf (
		"Column '%s' not found in %s (skipping this file)\n")
		    call pargstr (in_col)
		    call pargstr (ifilen)
		call tbtclo (tp)
		call error (1, "")
	    }
	    call tbtclo (tp)
	}
	# If the input is a 1-D extracted spectrum or a collection of
	# image sets, open all the extensions (specified by NEXTEND).
	do k = 1, nsci_ext {
	    call sprintf (ifilen, SZ_FNAME, "%s[%d]")
		call pargstr (ifile)
		call pargi (k)
	    iferr {
		tp = tbtopn (ifilen, READ_ONLY, 0)
	    } then {
		call eprintf ("Cannot open '%s' (skipping this file)\n")
		    call pargstr (ifilen)
		call error (1, "")
	    }
	    call tbtclo (tp)
	}
end

# This routine computes the delaytime in seconds.

procedure all_delay (epoch, parallax, objvec,
		time_e, x_e, y_e, z_e, npts_earth,
		time_o, x_o, y_o, z_o, npts_obs,
		delta_sec)

double	epoch		# i: time (MJD) of event
double	parallax	# i: parallax of target
double	objvec[3]	# i: unit geocentric state vector of the target
double	time_e[ARB], x_e[ARB], y_e[ARB], z_e[ARB]
int	npts_earth
double	time_o[ARB], x_o[ARB], y_o[ARB], z_o[ARB]
int	npts_obs
double	delta_sec	# o: correction (sec) to be added to TIME col
#--
char	mess[SZ_FNAME]	# for error message
double	geomdelt, reldelt	# time corrections (sec)
double	xyz_obs[3]	# geocentric state vector of the observer
double	xyz_earth[3]	# baycentric state vector of the earth
double	telvec[3]	# baycentric state vector of the observer

begin
	# calculate delaytime due to relativistic effects
	call relativ (epoch, reldelt)

	# calculate the geometric delay time
	iferr (call intrp_state (epoch, time_e, x_e, y_e, z_e,
		xyz_earth, npts_earth, NLAN_EARTH)) {
	    call sprintf (mess, SZ_FNAME,
			"epoch = MJD %f, outside the earth_ephem range")
		call pargd (epoch)
	    call error (1, mess)
	}
	if (npts_obs > 0) {
	    iferr (call intrp_state (epoch, time_o, x_o, y_o, z_o,
			xyz_obs, npts_obs, NLAN_OBS)) {
		call sprintf (mess, SZ_FNAME,
			"epoch = MJD %f, outside the obs_ephem range")
		    call pargd (epoch)
		call error (1, mess)
	    }
	} else {
	    xyz_obs[1] = 0.d0
	    xyz_obs[2] = 0.d0
	    xyz_obs[3] = 0.d0
	}

	call aaddd (xyz_obs, xyz_earth, telvec, 3)
	call geo_delay (telvec, objvec, parallax, geomdelt)

	delta_sec = geomdelt + reldelt
end
