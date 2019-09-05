include <math.h>		# for RADTODEG
include "north.h"

# nor_orient -- get orientations from header keywords
# This routine gets various keyword values from the science file and/or
# the shp header and computes the orientation.
# Nominally the values of orientat, pa_aper, cd_one, cd_two, and pa_v3
# should all be the same.  In practice, they won't be, so the parameter
# value is a single number intended to be the "best" value for the
# orientation angles, while range is the maximum minus minimum of all
# the individual values.
#
# Phil Hodge,  4-May-1992  Subroutine created.
# Phil Hodge, 30-Jul-1992  Check APEROBJ if zoom mode.
# Phil Hodge,  2-Feb-1993  In subroutine nor_orient, swap off-diagonal
#		elements of CD matrix in atan2 expressions for orientat.
# Phil Hodge,  2-Aug-1993  Call nor_non_foc to handle non-FOC data.
# Phil Hodge, 17-Feb-1994  Get KXDEPLOY, and if true add 180 to pa_v3;
#		in SHP, also look for SS_OPRLY and SS_PXFMT.
# Phil Hodge, 17-Jun-1994  New value of theta starting 26-Dec-1993.

procedure nor_orient (im1, im2, angle, range,
	orientat, pa_aper, cd_one, cd_two, pa_v3, files, parity, maxch,
	comment, max_comment)

pointer im1			# i: imhdr pointer for science file or NULL
pointer im2			# i: imhdr pointer for shp or NULL
double	angle			# o: one number representing orientation angle
double	range			# o: range of orientation values
double	orientat		# o: value of keyword from science file
double	cd_one, cd_two		# o: two values of orientation from CD matrix
double	pa_aper			# o: orientation based on pa_aper from .shh
double	pa_v3			# o: orientation based on pa_v3 from .shh
char	files[ARB]		# o: "sci" or "shp" or "sci,shp"
char	parity[ARB]		# o: normal or reversed (or unknown)
int	maxch			# i: max lengths of the strings files & parity
char	comment[ARB]		# o: comments about corrections to angles
int	max_comment		# i: max length of comment
#--
pointer sp
pointer scr			# scratch for appending to comment
char	instrume[SZ_KEYVAL]	# instrument name
char	pxformt[SZ_KEYVAL]	# zoom or normal
char	pxlcorr[SZ_KEYVAL]	# has the image been dezoomed?
char	optcrly[SZ_KEYVAL]	# f/48 or f/96
char	aperobj[SZ_KEYVAL]	# name of aperture (e.g. x96n512)
double	cd[2,2]			# CD matrix
double	ra_v1, dec_v1		# ra & dec at V1 axis
double	ra_aper, dec_aper	# ra & dec at the aperture (=crval1 & crval2)
double	delta_pa		# add to orientation at V1
double	theta			# angle from sample direction to V2
double	determinant		# determinant of the CD matrix
double	minval, maxval		# min & max of all orientation angles
int	date			# date the file was written
bool	is_foc			# true if instrume = foc
bool	is_foc_shp		# true of instrume in shp file = foc
bool	zoom			# zoom mode image?
bool	dezoomed		# if zoom mode, has the image been dezoomed?
bool	kxdeploy		# is costar deployed?
bool	got_instrume		# instrume keyword found in shp header?
bool	got_date		# have we got a value for the date?
bool	got_pxformt		# have we got a value for pxformt?
bool	got_optcrly		# have we got a value for optcrly?
bool	got_kxdeploy		# have we got a value for kxdeploy?
bool	got_orientat		# have we got a value for orientat?
bool	got_cd			# have we got values for the CD matrix?
bool	got_aper		# have we got ra & dec at aperture?
bool	got_v1			# have we got the ra & dec at V1 axis?
bool	got_pa_aper		# have we got a value for pa_aper?
bool	got_pa_v3		# have we got a value for pa_v3?
bool	got_theta		# have we got a value for theta?
double	imgetd()
int	imaccf()
int	nor_date()
bool	streq(), strne()
bool	imgetb()

begin
	call smark (sp)
	call salloc (scr, SZ_FNAME, TY_CHAR)

	# Initial values.
	range = INDEFD
	orientat = INDEFD
	cd_one = INDEFD
	cd_two = INDEFD
	pa_aper = INDEFD
	pa_v3 = INDEFD
	call strcpy ("unknown", parity, maxch)
	comment[1] = '#'
	comment[2] = EOS

	cd[1,1] = INDEFD
	cd[1,2] = INDEFD
	cd[2,1] = INDEFD
	cd[2,2] = INDEFD
	date = INDEFI
	theta = 0.d0
	delta_pa = 0.d0
	zoom = false
	instrume[1] = EOS
	pxformt[1] = EOS
	pxlcorr[1] = EOS
	optcrly[1] = EOS
	aperobj[1] = EOS

	got_instrume = false
	got_date = false
	got_pxformt = false
	got_optcrly = false
	got_kxdeploy = false
	got_orientat = false
	got_cd = false
	got_aper = false
	got_v1 = false
	got_pa_aper = false
	got_pa_v3 = false
	got_theta = false

	if (im1 != NULL && im2 != NULL)
	    call strcpy ("sci,shp", files, maxch)
	else if (im1 != NULL)
	    call strcpy ("sci    ", files, maxch)
	else if (im2 != NULL)
	    call strcpy ("    shp", files, maxch)

	# Is this an FOC observation?  Look for INSTRUME keyword.
	# Note that the got_instrume flag is only for shp header.
	is_foc = false				# initial value
	if (im1 != NULL) {
	    if (imaccf (im1, "instrume") == YES) {
		call imgstr (im1, "instrume", instrume, SZ_KEYVAL)
		call strlwr (instrume)
		is_foc = streq (instrume, "foc")
	    }
	}
	if (im2 != NULL) {
	    if (imaccf (im2, "instrume") == YES) {
		got_instrume = true
		call imgstr (im2, "instrume", instrume, SZ_KEYVAL)
		call strlwr (instrume)
		is_foc_shp = streq (instrume, "foc")

		if (im1 == NULL) {
		    is_foc = is_foc_shp
		} else if ((is_foc && !is_foc_shp) ||
			  (!is_foc && is_foc_shp)) {
		    call eprintf ("warning:  inconsistent values of INSTRUME\n")
		    is_foc = false
		}
	    }
	}

	# If this is not an FOC observation, get orientation from
	# ORIENTAT and/or CD matrix alone.
	if (!is_foc) {
	    call nor_non_foc (im1, angle, range,
			orientat, cd_one, cd_two, parity, maxch,
			comment, max_comment)
	    if (im1 == NULL)
		call strcpy ("<none> ", files, maxch)
	    else
		# Can't use shp for non-foc image.
		call strcpy ("sci    ", files, maxch)
	    return
	}

	# Get values from the science header.
	if (im1 != NULL) {

	    date = nor_date (im1)
	    got_date = !IS_INDEFI(date)

	    if (imaccf (im1, "pxformt") == YES) {
		call imgstr (im1, "pxformt", pxformt, SZ_KEYVAL)
		got_pxformt = true
		call strupr (pxformt)
		zoom = streq (pxformt, "ZOOM")
	    }

	    if (imaccf (im1, "kxdeploy") == YES) {
		kxdeploy = imgetb (im1, "kxdeploy")
		got_kxdeploy = true
	    }

	    dezoomed = true			# initial value
	    if (zoom) {
		if (imaccf (im1, "pxlcorr") == YES) {
		    call imgstr (im1, "pxlcorr", pxlcorr, SZ_KEYVAL)
		    call strupr (pxlcorr)
		    dezoomed = streq (pxlcorr, "COMPLETE")
		}
	    }

	    # Get the angle from the sample direction to the V2 direction.
	    # To do this we need the relay and the date the file was written.
	    if (imaccf (im1, "optcrly") == YES) {
		call imgstr (im1, "optcrly", optcrly, SZ_KEYVAL)
		got_optcrly = true
		call strupr (optcrly)
		if (got_date) {
		    if (streq (optcrly, "F48")) {
			if (date < FLIP_DATE)
			    theta = OLD_THETA_48
			else if (date < COSTAR_DATE)
			    theta = NEW_THETA_48
			else
			    theta = THETA_48_COSTAR
		    } else if (streq (optcrly, "F96")) {
			if (date < FLIP_DATE)
			    theta = OLD_THETA_96
			else if (date < COSTAR_DATE)
			    theta = NEW_THETA_96
			else
			    theta = THETA_96_COSTAR
		    }
		    got_theta = true
		}
	    }

	    if (imaccf (im1, "orientat") == YES) {
		orientat = imgetd (im1, "orientat")
		got_orientat = true
	    }

	    if (imaccf (im1, "cd1_1") == YES) {
		cd[1,1] = imgetd (im1, "cd1_1")
		got_cd = true
		if (imaccf (im1, "cd2_2") == YES)
		    cd[2,2] = imgetd (im1, "cd2_2")
		else
		    got_cd = false		# must have both cd1_1 & cd2_2
		if (imaccf (im1, "cd1_2") == YES)
		    cd[1,2] = imgetd (im1, "cd1_2")
		else
		    cd[1,2] = 0.d0
		if (imaccf (im1, "cd2_1") == YES)
		    cd[2,1] = imgetd (im1, "cd2_1")
		else
		    cd[2,1] = 0.d0
		if (!dezoomed) {
		    # "Dezoom" the CD matrix.
		    cd[1,1] = cd[1,1] / 2.d0
		    cd[2,1] = cd[2,1] / 2.d0
		    call strcat (" dezoom CD;", comment, max_comment)
		}
	    }

	    if (imaccf (im1, "crval1") == YES &&
		imaccf (im1, "crval2") == YES) {
		ra_aper = imgetd (im1, "crval1")
		dec_aper = imgetd (im1, "crval2")
		got_aper = true
		# Check for dummy coordinates.
		if (ra_aper == 0.d0 && dec_aper == 0.d0)
		    got_aper = false
	    }
	}

	# Get values from the SHP header.
	if (im2 != NULL) {

	    if (!got_date) {
		date = nor_date (im2)
		got_date = !IS_INDEFI(date)
	    }

	    if (imaccf (im2, "pa_aper") == YES) {
		pa_aper = imgetd (im2, "pa_aper")
		got_pa_aper = true
	    } else if (imaccf (im2, "pangaper") == YES) {
		pa_aper = imgetd (im2, "pangaper")
		got_pa_aper = true
	    }

	    if (imaccf (im2, "pa_v3") == YES) {
		pa_v3 = imgetd (im2, "pa_v3")
		got_pa_v3 = true
	    } else if (imaccf (im2, "psanglv3") == YES) {
		pa_v3 = imgetd (im2, "psanglv3")
		got_pa_v3 = true
	    }

	    if (imaccf (im2, "ra_v1") == YES) {
		ra_v1 = imgetd (im2, "ra_v1")
		dec_v1 = imgetd (im2, "dec_v1")
		got_v1 = true
	    } else if (imaccf (im2, "rtascnv1") == YES) {
		ra_v1 = imgetd (im2, "rtascnv1")
		dec_v1 = imgetd (im2, "declnv1")
		got_v1 = true
	    }

	    # If we didn't get pxformt from the science header, get it now.
	    if (!got_pxformt) {
		if (imaccf (im2, "ss_pxfmt") == YES) {
		    call imgstr (im2, "ss_pxfmt", pxformt, SZ_KEYVAL)
		    got_pxformt = true
		} else if (imaccf (im2, "pxformt") == YES) {
		    call imgstr (im2, "pxformt", pxformt, SZ_KEYVAL)
		    got_pxformt = true
		}
		if (got_pxformt) {
		    call strupr (pxformt)
		    zoom = streq (pxformt, "ZOOM")
		}
	    }

	    # If we didn't get kxdeploy from the science header, get it now.
	    if (!got_kxdeploy) {
		if (imaccf (im2, "kxdeploy") == YES) {
		    kxdeploy = imgetb (im2, "kxdeploy")
		    got_kxdeploy = true
		}
	    }

	    # Get the aperture name.  For zoomed images there are a few
	    # formats for which we should NOT subtract 90 deg from pa_aper.
	    if (zoom) {
		if (imaccf (im2, "aperobj") == YES) {
		    call imgstr (im2, "aperobj", aperobj, SZ_KEYVAL)
		    call strlwr (aperobj)
		}
	    }

	    # If we didn't get optcrly from the science header, get it now.
	    # Actually, it's theta we need, but we need optcrly to get theta.
	    if (!got_theta) {
		if (imaccf (im2, "ss_oprly") == YES) {
		    call imgstr (im2, "ss_oprly", optcrly, SZ_KEYVAL)
		    got_optcrly = true
		} else if (imaccf (im2, "optcrly") == YES) {
		    call imgstr (im2, "optcrly", optcrly, SZ_KEYVAL)
		    got_optcrly = true
		}
		if (got_optcrly && got_date) {
		    call strupr (optcrly)
		    if (streq (optcrly, "F48")) {
			if (date < FLIP_DATE)
			    theta = OLD_THETA_48
			else
			    theta = NEW_THETA_48
		    } else if (streq (optcrly, "F96")) {
			if (date < FLIP_DATE)
			    theta = OLD_THETA_96
			else
			    theta = NEW_THETA_96
		    }
		    got_theta = true
		}
	    }
	}

	# Compute orientation in various ways.

	# First get the correction from V1 orientation to aperture orientation.
	if (got_aper && got_v1)			# else delta_pa = 0
	    call nor_offset (ra_v1, dec_v1, ra_aper, dec_aper, delta_pa)

	if (got_cd) {
	    # Calculate the orientation from the CD matrix.
	    # cd_one is from a vector pointing in the north direction;
	    # cd_two is from a vector pointing in the east direction.

	    determinant = cd[1,1] * cd[2,2] - cd[1,2] * cd[2,1]
	    # bug fix 1993 Feb 2
	    if (determinant < 0) {
		call strcpy ("normal", parity, maxch)
		cd_one = RADTODEG (atan2 (cd[2,1], -cd[1,1]))
		cd_two = RADTODEG (atan2 (cd[1,2],  cd[2,2]))
	    } else if (determinant > 0) {
		call strcpy ("reversed", parity, maxch)
		cd_one = RADTODEG (atan2 (-cd[2,1], cd[1,1]))
		cd_two = RADTODEG (atan2 ( cd[1,2], cd[2,2]))
	    } else {
		got_cd = false
		call strcpy ("|CD| = 0", parity, maxch)
		call strcat (" |CD| = 0;", comment, max_comment)
	    }
	}

	# At this point pa_v3 is the position angle of the V3 axis;
	# replace it by the value of orientation that it implies.
	# Note that we include the rotation due to the displacement
	# between V1 and the aperture.
	if (got_pa_v3) {
	    if (got_aper && got_v1) {
		pa_v3 = pa_v3 + delta_pa
		call sprintf (Memc[scr], SZ_FNAME, " pa_v3 += %0.2f;")
		    call pargd (delta_pa)
		call strcat (Memc[scr], comment, max_comment)
	    }
	    if (got_theta) {
		pa_v3 = pa_v3 - theta
		call sprintf (Memc[scr], SZ_FNAME, " pa_v3 -= %0.2f;")
		    call pargd (theta)
		call strcat (Memc[scr], comment, max_comment)
	    }
	    if (got_kxdeploy && kxdeploy) {		# costar deployed?
		pa_v3 = pa_v3 + 180.d0
		call sprintf (Memc[scr], SZ_FNAME, " pa_v3 += 180;")
		call strcat (Memc[scr], comment, max_comment)
	    }
	    if (pa_v3 < 0.d0)
		pa_v3 = pa_v3 + 360.d0
	    else if (pa_v3 >= 360.d0)
		pa_v3 = pa_v3 - 360.d0
	}

	# Correct pa_aper for zoom mode, if appropriate.
	if (got_pa_aper) {
	    if (zoom) {
		if (strne (aperobj, "x96zrec") &&
		    strne (aperobj, "x48zrec") && 
		    strne (aperobj, "x48zrecs")) {
		    pa_aper = pa_aper - 90.
		    call strcat (" pa_aper -= 90;", comment, max_comment)
		}
	    }
	    if (pa_aper < 0.d0)
		pa_aper = pa_aper + 360.d0
	}

	# Before the flip, orientat was off by theta; after the flip,
	# orientat was off because it was based on V1 orientation.
	if (got_orientat) {
	    if (got_date) {
		if (date < FLIP_DATE) {
		    if (got_theta) {
			orientat = orientat + theta
			call sprintf (Memc[scr], SZ_FNAME,
				" orientat += %0.2f;")
			    call pargd (theta)
			call strcat (Memc[scr], comment, max_comment)
		    }
		} else if (date >= FLIP_DATE && date < OK_DATE) {
		    if (got_aper && got_v1) {
			orientat = orientat + delta_pa
			call sprintf (Memc[scr], SZ_FNAME,
				" orientat += %0.2f;")
			    call pargd (delta_pa)
			call strcat (Memc[scr], comment, max_comment)
		    }
		}
	    }
	    if (orientat < 0.d0)
		orientat = orientat + 360.d0
	    else if (orientat >= 360.d0)
		orientat = orientat - 360.d0
	}
	if (got_cd) {
	    if (got_date) {
		if (date >= FLIP_DATE && date < OK_DATE) {
		    # Based on V1 orientation, so apply the offset.
		    if (got_aper && got_v1) {
			cd_one = cd_one + delta_pa
			cd_two = cd_two + delta_pa
			call sprintf (Memc[scr], SZ_FNAME, " CD += %0.2f;")
			    call pargd (delta_pa)
			call strcat (Memc[scr], comment, max_comment)
		    }
		}
	    }
	    if (cd_one < 0.d0)
		cd_one = cd_one + 360.d0
	    if (cd_one >= 360.d0)
		cd_one = cd_one - 360.d0
	    if (cd_two < 0.d0)
		cd_two = cd_two + 360.d0
	    if (cd_two >= 360.d0)
		cd_two = cd_two - 360.d0
	}

	# Get one number to represent the best value for the image orientation.
	if (got_pa_v3 && got_aper && got_v1 && got_theta)
	    angle = pa_v3		# corrected pa_v3 is most reliable
	else if (got_pa_aper)
	    angle = pa_aper
	else if (got_orientat)
	    angle = orientat
	else if (got_cd)
	    angle = (cd_one + cd_two) / 2.d0
	else if (got_pa_v3 && got_theta)
	    angle = pa_v3
	else
	    angle = INDEFD

	# Find the range of values.
	if (IS_INDEFD(angle)) {
	    range = INDEFD
	} else {
	    minval = angle
	    maxval = angle
	    if (got_pa_aper) {
		minval = min (minval, pa_aper)
		maxval = max (maxval, pa_aper)
	    }
	    if (got_orientat) {
		minval = min (minval, orientat)
		maxval = max (maxval, orientat)
	    }
	    if (got_cd) {
		minval = min (minval, cd_one)
		maxval = max (maxval, cd_one)
		minval = min (minval, cd_two)
		maxval = max (maxval, cd_two)
	    }
	    if (got_pa_v3) {
		minval = min (minval, pa_v3)
		maxval = max (maxval, pa_v3)
	    }
	    range = maxval - minval
	    if (range > 180.d0)		# e.g. maxval = 359, minval = 1
		range = minval + 360.d0 - maxval
	}
	if (!got_instrume || !got_date || !got_pxformt ||
		!got_orientat || !got_cd || !got_aper || !got_v1 ||
		!got_pa_aper || !got_pa_v3 || !got_optcrly) {
	    call strcat ("\n# Missing:  ", comment, max_comment)
	    if (!got_instrume)
		call strcat ("INSTRUME (from SHP); ", comment, max_comment)
	    if (!got_date)
		call strcat ("DATE; ", comment, max_comment)
	    if (!got_pxformt)
		call strcat ("PXFORMT; ", comment, max_comment)
	    if (!got_orientat)
		call strcat ("ORIENTAT; ", comment, max_comment)
	    if (!got_cd)
		call strcat ("CD matrix; ", comment, max_comment)
	    if (!got_aper)
		call strcat ("RA & Dec at aperture; ", comment, max_comment)
	    if (!got_v1)
		call strcat ("RA & Dec at V1; ", comment, max_comment)
	    if (!got_pa_aper)
		call strcat ("PA_APER; ", comment, max_comment)
	    if (!got_pa_v3)
		call strcat ("PA_V3; ", comment, max_comment)
	    if (!got_optcrly)
		call strcat ("OPTCRLY", comment, max_comment)
	}

	call sfree (sp)
end

# nor_date -- get the date the file was written
# This routine gets the value of the DATE keyword, the date the file
# was written, not the date of observation.  This value is converted
# to an integer with digits YYYYMMDD so it can be compared with other
# dates using <, =, or >.  The function value is this number, or
# INDEFI if DATE was not found in the header.

int procedure nor_date (im)

pointer im			# i: imhdr pointer for image
#--
char	date[SZ_KEYVAL]		# date the files were written
int	ip, junk		# for ctoi
int	year, month, day	# date file was written
int	imaccf(), ctoi()

begin
	if (imaccf (im, "date") == YES) {
	    call imgstr (im, "date", date, SZ_KEYVAL)
	    date[3] = ' '
	    date[6] = ' '
	    ip = 1
	    junk = ctoi (date, ip, day)
	    junk = ctoi (date, ip, month)
	    junk = ctoi (date, ip, year)
	    if (year < 90)		# after 1999 Dec 31
		year = year + 100
	    year = year + 1900
	    return (year*10000 + month*100 + day)
	} else {
	    return (INDEFI)
	}
end

# nor_offset -- correct for offset from V1
# The orientation differs depending on the location of the aperture with
# respect to the V1 axis.  This routine computes a (good) approximation
# to the correction to be added to the orientation at the V1 axis to give
# the orientation at the aperture.  This is based on an algorithm
# provided by John Baum.

procedure nor_offset (ra_v1, dec_v1, ra_aper, dec_aper, delta_pa)

double	ra_v1		# i: right ascension at the V1 axis (degrees)
double	dec_v1		# i: declination at the V1 axis (degrees)
double	ra_aper		# i: right ascension at the aperture (degrees)
double	dec_aper	# i: declination at the aperture (degrees)
double	delta_pa	# o: add this to the orientation at V1 (degrees)
#--
double	ra_v1_x, dec_v1_x, ra_aper_x, dec_aper_x	# angles in radians

begin
	ra_v1_x = DEGTORAD(ra_v1)
	dec_v1_x = DEGTORAD(dec_v1)
	ra_aper_x = DEGTORAD(ra_aper)
	dec_aper_x = DEGTORAD(dec_aper)

	delta_pa = 2.d0 * asin (sin ((ra_aper_x - ra_v1_x) / 2.d0) *
			      sin ((dec_aper_x + dec_v1_x) / 2.d0))
	delta_pa = RADTODEG(delta_pa)
end
