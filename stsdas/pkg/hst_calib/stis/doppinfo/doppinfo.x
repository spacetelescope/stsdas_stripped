task	doppinfo

include <ctype.h>	# IS_WHITE
include <math.h>	# TWOPI
include "orbdat.h"

define	SZ_KW_BUFFER	24		# size of string for keyword value
			# multiply by DEG_RAD to convert from degrees to radians
define	DEG_RAD		(3.1415926535897932384626433d0 / 180.d0)
define	EXT_PER_IMSET	3		# number of extensions per image set
define	SPEED_OF_LIGHT	(2.99792458d5)	# km / s
define	SEC_PER_DAY	(86400.d0)

# number of points for computing circular-orbit parameters
define	NPTS		 64
define	NPTS_D		(64.d0)

# This task computes and prints Doppler shift information for each imset
# of an observation set, using keywords from the science file and support
# file.
#
# DOPPZERO is the time when the Doppler shift is zero.  There are two
# such times per orbit, and the particular one used for DOPPZERO is when
# the radial velocity is starting to increase, i.e. when HST is closer
# to the target.
#
# DOPPMAG is the magnitude of the Doppler shift, in high-res pixels.
# This task computes DOPPMAG for the CCD as well as for the MAMAs for
# the sake of providing the information, even though on-board Doppler
# correction is not performed for the CCD.  For the CCD, DOPPMAG is
# in units of unbinned pixels.
#
# This is based on Steve Hulbert's code for computing the position and
# velocity of HST from the orbital elements.  The orbital elements are
# gotten from the primary header of the support file.  The primary header
# of the science file is read to get the target position from the keywords
# RA_TARG and DEC_TARG.  The number of imsets in the science file is
# assumed to be NEXTEND / 3.  DETECTOR and CENWAVE are also read, in
# order to compute DOPPMAG.
#
# For each imset, the SCI extension header is opened to read the keywords
# EXPSTART, EXPEND, CD1_1 and LTM1_1.  The position and velocity of HST
# are computed at intervals throughout one orbital period centered on the
# midpoint of the exposure, (EXPSTART + EXPEND) / 2.
#
# The magnitude of the Doppler shift is computed from the maximum radial
# velocity using CENWAVE as the wavelength.  The high-res pixel spacing
# is computed from CD1_1 and LTM1_1:
#
#	DOPPMAG = (maximum_radial_velocity * wavelength) /
#		(speed_of_light * pixel_spacing)
# where
#	wavelength = CENWAVE
#	pixel_spacing = CD1_1 * LTM1_1 / highres_factor
#	highres_factor = 2 for the MAMA detectors, 1 for the CCD
#
# Phil Hodge,  5-Aug-1999  Task created.
# Phil Hodge, 27-Sep-1999  Add update parameter, to update DOPPZERO keyword;
#		allow input and spt to be lists of names.
# Phil Hodge, 20-Feb-2003  Remove update parameter; add increment,
#		verbose, and output parameters doppzero, doppmag, radvel.
#		Compute the output parameters based on a circular orbit.
#		Change the printed output contents and format.
# Phil Hodge, 17-Jul-2003  Compare initial times of min and max radial
#		velocities with start and end of exposure time before trying
#		to improve the times with quadratic fits.

procedure doppinfo()

pointer imt_raw		# imt pointer for list of input (science) files
pointer imt_spt		# imt pointer for list of spt (support) files
double	increment	# print info at these intervals
bool	verbose		# true if info should be printed
#--
char	input[SZ_FNAME]		# name of one science file
char	spt[SZ_FNAME]		# name of one support file
int	nraw, nspt	# number of files in input and spt lists
int	junk
pointer imtopenp()
int	imtlen(), imtgetim()
double	clgetd()
bool	clgetb()

begin
	imt_raw = imtopenp ("input")
	imt_spt = imtopenp ("spt")
	increment = clgetd ("increment") / SEC_PER_DAY	# convert to days
	verbose = clgetb ("verbose")

	nraw = imtlen (imt_raw)
	nspt = imtlen (imt_spt)

	# If spt is empty, we'll construct the support file name (in
	# dopp_1_file) based on the input file name.
	# If spt contains just one file name, get it now, and we'll
	# use it for all the science files.
	# Otherwise, the number of names in input and spt must be the same.
	if (nspt == 0) {
	    spt[1] = EOS
	} else if (nspt == 1) {
	    junk = imtgetim (imt_spt, spt, SZ_FNAME)
	} else if (nspt != nraw) {
	    call error (1, "Number of input and spt files are not the same.")
	}

	while (imtgetim (imt_raw, input, SZ_FNAME) != EOF) {

	    if (nspt > 1)
		junk = imtgetim (imt_spt, spt, SZ_FNAME)

	    call dopp_1_file (input, spt, increment, verbose)
	}

	call imtclose (imt_raw)
	call imtclose (imt_spt)
end

procedure dopp_1_file (input, sptfile, increment, verbose)

char	input[SZ_FNAME]		# i: name of science file
char	sptfile[SZ_FNAME]	# i: name of support file
double	increment		# i: print info every 'increment' days
bool	verbose			# i: true if info should be printed
#--
char	raw[SZ_FNAME]		# <rootname>_raw.fits[0], [1]
char	spt[SZ_FNAME]		# <rootname>_spt.fits[0]
char	raw_rootname[SZ_KW_BUFFER]
char	spt_rootname[SZ_KW_BUFFER]
char	detector[SZ_KW_BUFFER]

pointer orb			# orbital elements
pointer im
pointer immap()

double	cenwave			# central wavelength
double	highres_factor		# normally 2, but 1 if detector is CCD
double	cd1_1, ltm1_1		# to account for binning of data
double	expmiddle		# MJD at midpoint of exposure
double	orbit_period		# 2 * HSTHORB, converted from seconds to days
double	expstart, expend	# exposure start and stop times
double	t_origin		# origin for time for finding circular orbit
double	time, dt		# time in MJD; time increment in days
double	doppzero		# time when Doppler shift is zero, increasing
double	radvel			# radial velocity of target with respect to HST
# rv_ampl and dopp_ampl are computed for a circular orbit
double	rv_ampl			# amplitude of radial velocity, km/s
double	dopp_ampl		# ampl of rad vel, in high-res pixels
double	doppmag			# maximum radial velocity, in high-res pixels
double	mid_radvel		# radial velocity at midpoint in exposure
double	min_radvel		# minimum radial velocity during exposure
double	max_radvel		# maximum radial velocity over some time range
double	avg_radvel		# radial velocity averaged over the exposure
double	mid_dopp, min_dopp, max_dopp, avg_dopp	# Doppler shift in pixels
double	ra_targ, dec_targ	# target RA & Dec
double	targ[3]			# unit vector toward target
int	imset, nimsets		# for looping over imsets
double	sum_sin, sum_cos	# for finding doppzero, doppmag
double	acoeff, bcoeff		# coeff of sin, -cos
double	t_min, t_max		# approximate times of min & max radial vel
double	rv[3]			# radial velocity at three times, dt apart
int	i
bool	min_at_end, max_at_end	# min or max radial vel at end of interval?
double	imgetd()
int	imgeti()
int	imaccf()
double	peak_quadratic()
bool	streq(), strne()

begin
	call strcpy (input, raw, SZ_FNAME)
	call strcpy (sptfile, spt, SZ_FNAME)

	# If spt wasn't specified, assign a value based on input.
	call get_spt_name (input, spt, SZ_FNAME)

	# Get keywords from primary header of _raw.fits file.

	call strcat ("[0]", raw, SZ_FNAME)
	im = immap (raw, READ_ONLY, NULL)
	call imgstr (im, "rootname", raw_rootname, SZ_KW_BUFFER)
	ra_targ = imgetd (im, "ra_targ") * DEG_RAD
	dec_targ = imgetd (im, "dec_targ") * DEG_RAD
	nimsets = imgeti (im, "nextend") / EXT_PER_IMSET
	call imgstr (im, "detector", detector, SZ_KW_BUFFER)
	cenwave = imgetd (im, "cenwave")
	call imunmap (im)

	# DOPPMAG is in units of high-res pixels.
	if (streq (detector, "CCD"))
	    highres_factor = 1.d0
	else
	    highres_factor = 2.d0		# either MAMA detector

	# Convert target RA & Dec to rectangular coordinates.
	call sph_rec (ra_targ, dec_targ, targ)

	# Get orbital elements from primary header of spt file.

	call strcat ("[0]", spt, SZ_FNAME)
	im = immap (spt, READ_ONLY, NULL)
	call imgstr (im, "rootname", spt_rootname, SZ_KW_BUFFER)
	call getorb (im, orb)
	call imunmap (im)
	orbit_period = 2.d0 * HSTHORB(orb) / SEC_PER_DAY	# days

	# Sanity check (but just a warning).
	if (strne (raw_rootname, spt_rootname)) {
	    call eprintf (
	"Warning:  ROOTNAMEs in input and spt files are not the same:\n")
	    call eprintf ("    ROOTNAME = %s in %s,\n")
		call pargstr (raw_rootname)
		call pargstr (raw)
	    call eprintf ("    ROOTNAME = %s in %s.\n")
		call pargstr (spt_rootname)
		call pargstr (spt)
	}

	do imset = 1, nimsets {

	    call sprintf (raw, SZ_FNAME, "%s[sci,%d]")
		call pargstr (input)
		call pargi (imset)

	    im = immap (raw, READ_ONLY, NULL)

	    expstart = imgetd (im, "expstart")
	    expend = imgetd (im, "expend")
	    expmiddle = (expstart + expend) / 2.d0
	    if (imaccf (im, "cd1_1") == YES)
		cd1_1 = imgetd (im, "cd1_1")
	    else
		cd1_1 = 1.d0
	    if (imaccf (im, "ltm1_1") == YES)
		ltm1_1 = imgetd (im, "ltm1_1")
	    else
		ltm1_1 = 1.d0

	    # Find doppzero and doppmag, based on the assumption of a
	    # circular orbit for HST:
	    #   radvel = acoeff * sin (phase) - bcoeff * cos (phase)
	    #   acoeff = rv_ampl * cos (2*pi*t0/P)
	    #   bcoeff = rv_ampl * sin (2*pi*t0/P)
	    #   t0 = doppzero - expmiddle

	    sum_sin = 0.d0
	    sum_cos = 0.d0

	    # Compute Fourier coefficients based on NPTS points in an orbit,
	    # centered on the middle of the exposure.
	    t_origin = expmiddle - orbit_period / 2.d0
	    do i = 0, NPTS-1 {
		dt = i * (orbit_period / NPTS_D)
		time = t_origin + dt
		call get_rv (orb, time, targ, radvel)
		sum_sin = sum_sin + radvel * sin (TWOPI * dt / orbit_period)
		sum_cos = sum_cos + radvel * cos (TWOPI * dt / orbit_period)
	    }
	    # Normalize by dividing by sum (sin**2) at NPTS equally spaced
	    # times in one orbit.
	    acoeff = sum_sin / (NPTS_D/2.d0)
	    bcoeff = sum_cos / (NPTS_D/2.d0)

	    # rv_ampl and dopp_ampl will not be modified again after being
	    # assigned here.  max_radvel and doppmag, in contrast, may be
	    # reused in several places.
	    rv_ampl = sqrt (acoeff**2 + bcoeff**2)
	    doppzero = atan2 (-bcoeff, acoeff) * orbit_period / TWOPI +
			t_origin

	    dopp_ampl = rv_ampl * cenwave * highres_factor /
			(SPEED_OF_LIGHT * cd1_1 * ltm1_1)

	    # Copy the values to the par file.
	    call clputd ("doppzero", doppzero)
	    call clputd ("doppmag", dopp_ampl)
	    call clputd ("radvel", rv_ampl)

	    if (verbose) {
		if (increment > 0.) {
		    call printf ("# time (MJD)   shift   radvel  %s\n")
			call pargstr (raw)

		    # Add 1.d-4 to expend to include end of interval, in case
		    # increment divides exposure time evenly.
		    do time = expstart, expend+1.d-4, increment {
			call get_rv (orb, time, targ, radvel)
			doppmag = radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
			call printf ("%12.6f %7.2f %8.3f\n")
			    call pargd (time)
			    call pargd (doppmag)
			    call pargd (radvel)
			call flush (STDOUT)
		    }
		} else {
		    # Use the radial velocity at the middle of the exposure
		    # as an initial value for finding min & max.
		    call get_rv (orb, expmiddle, targ, mid_radvel)
		    min_radvel = mid_radvel
		    max_radvel = mid_radvel
		    t_min = expmiddle
		    t_max = expmiddle
		    dt = 1.d-4
		    # loop index is relative to expstart
		    do time = dt, expend-expstart, dt {
			call get_rv (orb, time+expstart, targ, radvel)
			if (radvel < min_radvel) {
			    min_radvel = radvel
			    t_min = time + expstart
			}
			if (radvel > max_radvel) {
			    max_radvel = radvel
			    t_max = time + expstart
			}
		    }
		    # Explicitly check the radial velocities at the endpoints.
		    min_at_end = false			# initial values
		    max_at_end = false
		    call get_rv (orb, expstart, targ, radvel)
		    if (radvel < min_radvel) {
			min_radvel = radvel
			t_min = expstart
			min_at_end = true
		    }
		    if (radvel > max_radvel) {
			max_radvel = radvel
			t_max = expstart
			max_at_end = true
		    }
		    call get_rv (orb, expend, targ, radvel)
		    if (radvel < min_radvel) {
			min_radvel = radvel
			t_min = expend
			min_at_end = true
		    }
		    if (radvel > max_radvel) {
			max_radvel = radvel
			t_max = expend
			max_at_end = true
		    }
		    # Improve the values of min and max radial velocity.
		    if (!min_at_end) {
			call get_rv (orb, t_min-dt, targ, rv[1])
			call get_rv (orb, t_min,    targ, rv[2])
			call get_rv (orb, t_min+dt, targ, rv[3])
			time = peak_quadratic (rv, t_min, dt)
			time = max (time, expstart)
			time = min (time, expend)
			call get_rv (orb, time, targ, min_radvel)
		    }

		    if (!max_at_end) {
			call get_rv (orb, t_max-dt, targ, rv[1])
			call get_rv (orb, t_max,    targ, rv[2])
			call get_rv (orb, t_max+dt, targ, rv[3])
			time = peak_quadratic (rv, t_max, dt)
			time = max (time, expstart)
			time = min (time, expend)
			call get_rv (orb, time, targ, max_radvel)
		    }

		    # Compute the average radial velocity.  Note that this
		    # assumes a circular orbit.
		    if (expend == expstart) {
			call get_rv (orb, expmiddle, targ, avg_radvel)
		    	avg_dopp = avg_radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
		    } else {
			avg_dopp = dopp_ampl *
			(cos (TWOPI * (expstart - doppzero) / orbit_period) -
			 cos (TWOPI * (expend - doppzero) / orbit_period)) *
				orbit_period / TWOPI / (expend - expstart)
			avg_radvel = avg_dopp *
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1) /
				(cenwave * highres_factor)
		    }
		    mid_dopp = mid_radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
		    avg_dopp = avg_radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
		    min_dopp = min_radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
		    max_dopp = max_radvel * cenwave * highres_factor /
				(SPEED_OF_LIGHT * cd1_1 * ltm1_1)
		    call printf (
"# midpoint   midpoint Doppler  average Doppler  minimum Doppler  maximum Doppler\n")
		    call printf (
"#   MJD        pixels   km/s    pixels   km/s    pixels   km/s    pixels   km/s\n")
		    call printf (
	"%12.6f %8.2f %6.3f  %8.2f %6.3f  %8.2f %6.3f  %8.2f %6.3f  %s\n")
			call pargd (expmiddle)
			call pargd (mid_dopp)
			call pargd (mid_radvel)
			call pargd (avg_dopp)
			call pargd (avg_radvel)
			call pargd (min_dopp)
			call pargd (min_radvel)
			call pargd (max_dopp)
			call pargd (max_radvel)
			call pargstr (raw)
		    call flush (STDOUT)
		}
	    }

	    call imunmap (im)
	}

	call mfree (orb, TY_DOUBLE)
end

# If spt was specified (not blank or null), the name will not be modified
# (other than to trim trailing blanks).
#
# If spt is blank or null, a name will be created based on the input name.
# The input is expected to be of the form rootname_suffix.fits, where suffix
# is "raw", "flt", "crj", "x2d", "sx2" or "sfl".
# The _suffix.fits will then be replaced by "_spt.fits" to create the
# support file name.

procedure get_spt_name (input, spt, maxch)

char	input[ARB]	# i: name of input file
char	spt[ARB]	# io: name of support file
int	maxch		# i: size of spt string
#--
char	underscore
int	i
bool	fail
int	strlen(), strldx()
bool	strne()

begin
	# Check whether a support file name was specified.
	do i = strlen (spt), 1, -1 {
	    if (IS_WHITE(spt[i]))
		spt[i] = EOS
	    else
		break
	}
	if (spt[1] == EOS) {
	    # Construct a support file name from the science file name.
	    fail = false		# initial value
	    underscore = '_'
	    i = strldx (underscore, input)
	    if (i > 0) {
		if (strne (input[i], "_raw.fits") &&
		    strne (input[i], "_flt.fits") &&
		    strne (input[i], "_crj.fits") &&
		    strne (input[i], "_x2d.fits") &&
		    strne (input[i], "_sx2.fits") &&
		    strne (input[i], "_sfl.fits")) {
		    fail = true
		} else {
		    call strcpy (input, spt, maxch)
		    spt[i] = EOS		# chop off "_raw.fits"
		    call strcat ("_spt.fits", spt, maxch)
		}
	    } else {
		fail = true
	    }
	    if (fail) {
		call eprintf (
"The input file name is not standard format (e.g. rootname_raw.fits),\n")
		call eprintf (
"so you must specify the spt file name explicitly.\n")
		call error (1, "Don't know how to construct spt file name.")
	    }
	}
end

# Convert from RA & Dec to rectangular coordinates.

procedure sph_rec (ra_targ, dec_targ, targ)

double	ra_targ, dec_targ	# i: position of target
double	targ[3]			# i: unit vector in direction toward target
#--

begin
	targ[1] = cos (dec_targ) * cos (ra_targ)
	targ[2] = cos (dec_targ) * sin (ra_targ)
	targ[3] = sin (dec_targ)
end

# This computes the velocity of HST in the direction away from the target
# (i.e. the radial velocity).

procedure get_rv (orb, time, targ, radvel)

pointer	orb		# i: pointer to orbital data structure
double	time		# i: time (MJD)
double	targ[3]		# i: unit vector toward the target
double	radvel		# o: radial velocity, km/s
#--
double	x_hst[3]	# position of HST (km)
double	v_hst[3]	# velocity of HST (km/s)
double	dot_product()

begin
	call getpos (orb, time, x_hst, v_hst)

	radvel = -dot_product (targ, v_hst)
end

# This returns the value of the dot product of two vectors.

double procedure dot_product (a, b)

double	a[3], b[3]

begin
	return (a[1] * b[1] + a[2] * b[2] + a[3] * b[3])
end

# This function returns the location of the maximum (or minimum) of a
# quadratic that passes through three uniformly spaced points.

double procedure peak_quadratic (y, x_middle, spacing)

double	y[3]		# i: values at three uniformly spaced points
double	x_middle	# i: location of the middle point
double	spacing		# i: spacing between elements of y
#--
double	denominator
double	dx

begin
	denominator = y[1] - 2.d0 * y[2] + y[3]
	if (denominator == 0.d0)
	    return (x_middle)

	dx = (y[1] - y[3]) / (2.d0 * denominator)

	return (dx * spacing + x_middle)
end

# GETPOS -- Get position at a given time
#
#
# S. Hulbert, Oct 91    Original

procedure getpos (orb, mjd, x_hst, v_hst)

pointer	orb		#I: pointer to orbital data structure
double	mjd		#I: mjd
double	x_hst[3]	# o: position of HST
double	v_hst[3]	# o: velocity of HST
#--
double	sec85, deltim, m, v, r, wsmall, wbig, f
double	x1, x2, x3
double	a0, a1
int	i

begin
	# convert MJD to seconds since 1jan85
        sec85 = (mjd - 46066.0d0) * SEC_PER_DAY

	# calculate time difference between observation and epoch time
        deltim = sec85 - EPCHTIME(orb)

	# mean anomaly
        x1 = MEANANOM(orb)
	x2 = FDMEANAN(orb) * deltim
	x3 = 0.5d0 * SDMEANAN(orb) * deltim**2
	m = x1 + TWOPI * (x2 + x3)

	# true anomaly (equation of the center)
        v = m + sin(m) * (ECCENTX2(orb) + ECBDX3(orb) * cos(m)**2 -
		ECBDX4D3(orb) * sin(m)**2 + ESQDX5D2(orb) * cos(m))

	# distance
        r = SEMILREC(orb) / (1.0d0 + ECCENTRY(orb) * cos(v))

	# argument of perigee
        wsmall = TWOPI * (ARGPERIG(orb) + RCARGPER(orb) * deltim)

	# longitude of the ascending node
        wbig = TWOPI * (RASCASCN(orb) + RCASCNRV(orb) * deltim)

	# calculate the rectangular coordinates
	# (see Smart, Spherical Astronomy, section 75, page 122-124
        f = wsmall + v
        x_hst[1] = r * (cos(wbig) * cos(f) - COSINCLI(orb) * sin(wbig) * sin(f))
        x_hst[2] = r * (sin(wbig) * cos(f) + COSINCLI(orb) * cos(wbig) * sin(f))
        x_hst[3] = r * SINEINCL(orb) * sin(f)

	# calculate orbital velocities
        a0 = CIRVELOC(orb) * ECCENTRY(orb) * sin(v) / r
        a1 = CIRVELOC(orb) *
		(1.0d0 + ECCENTRY(orb) * cos(v)) +
		TWOPI * RCARGPER(orb) * r
        v_hst[1] = a0 * x_hst[1] -
		a1 * (cos(wbig) * sin(f) + COSINCLI(orb) * sin(wbig) * cos(f)) -
		TWOPI * RCASCNRV(orb) * x_hst[2]
        v_hst[2] = a0 * x_hst[2] -
		a1 * (sin(wbig) * sin(f) - COSINCLI(orb) * cos(wbig) * cos(f)) +
		TWOPI * RCASCNRV(orb) * x_hst[1]
        v_hst[3] = a0 * x_hst[3] + a1 * SINEINCL(orb) * cos(f)

	# Convert from meters to kilometers.
	do i = 1, 3 {
	    x_hst[i] = x_hst[i] / 1000.d0
	    v_hst[i] = v_hst[i] / 1000.d0
	}
end
