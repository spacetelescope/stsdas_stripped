include <tbset.h>
include <math.h>
include <mach.h>

# elemental precession routines.
# The code was derived/translated from the precession routines in the 
# original SDAS package.  Some of the comments from that code have been
# copied into this file.
#
# Andrew Cseko, Jr.   May-1990    Original

#----------------------------------------------------------------------------
# The value of the date could be "b1950.0", for example.
# On return, the value of start is incremented so that it points to the
# next character after the word read from the date string.  The number of
# characters read is returned as the function value, or zero if a number
# could not be read.

int procedure interpret_date (date, start, day)

char    date[ARB]   # i: user input date
int     start       # io: starting character in input string; updated at end
double  day         # o: julian day number
#--
char    prefix
double  year
int     index
int     count, newcount

double  jday_of()
int     gctod()

begin
	prefix = 'j'

	count = 0
	index = start
	while (date[index] == ' ' || date[index] == '\t') {
	    if (date[index] == EOS || date[index] == '\n')
		return (0)
	    index = index + 1
	    count = count + 1
	}

	if ( (date[index] == 'B') || (date[index] == 'b') ) {
	    prefix = 'b'
	    index = index + 1
	    count = count + 1
	} else if ( (date[index] == 'J') || (date[index] == 'j') ) {
	    index = index + 1
	    count = count + 1
	}

	newcount = gctod (date, index, year)
	if (newcount < 1)
	    return (0)

	day = jday_of (year, prefix)
	start = index			# udpate the index within the string
	count = count + newcount

	return (count)
end

#----------------------------------------------------------------------------

# jday_of -- compute the julian day number given a julian year.
#            if the input date is GT 3000 then presume it is a day number
#
# Andrew Cseko, Jr.   May-1990    Original

double procedure jday_of (date, prefix)

double date
char   prefix

#--
double jday

define  J2000_DAYN      2451545.d0      # J2000 day number

begin
	if ( date > 3000.d0 )
	    # presume date input was a julian date number
	    jday = date
	else if ( (prefix == 'b') || (prefix == 'B') ||
	         (prefix == ' ') && (date   == 1950.0d0) )
	    jday = (date - 1900.d0) * 365.242198781d0 + 2415020.31352d0
	else
	    # 2000 = reference year; 365.25 = number of days in julian year
	    jday = (date - 2000.d0) * 365.25d0 + J2000_DAYN

	return (jday)
end

#----------------------------------------------------------------------------

# precess -- precess given point through the given precession angles.
# the precession angles must first computed with a call to prec_angles()
#
# Andrew Cseko, Jr.    May-1990       Original

procedure precess (ra, dec, zetaO, inZ, theta)

double ra, dec           # i/o : precesion applied to and returned in
double zetaO, inZ, theta # i   : precesion angles in radians
#--
double x, y, z, tmp

begin
	# first rotation implemented in spherical coordinate system
	# - implements as simple addition

	ra = ra + zetaO

	# prepare to rotate in XZ plane - convert ra/dec to cartesian coords

	x = cos (ra) * cos (dec)
	y = sin (ra) * cos (dec)
	z = sin (dec)

	# rotate

	tmp = x
	x = tmp * cos (theta) - z * sin (theta)
	z = tmp * sin (theta) + z * cos (theta)

	# convert back to spherical coordinates 
	# (note: aproximate that if xy component < (?) then force ra to 0)

	tmp = sqrt (x**2 + y**2)

	if (tmp < 10*EPSILOND)
	    ra = 0.0
	else
	    ra = atan2 (y, x)

	dec = atan2 (z, tmp)

	# last rotation also done in sperical coordinates

	ra = ra + inZ

	# normalize to 0-2pi

	if (ra < 0.0)  ra = ra + TWOPI
	if (ra > TWOPI) ra = ra - TWOPI
end

#----------------------------------------------------------------------------

# prec_angles -- determine precession-rotation angles given julian dates
#
#   References:
#        IAU 1976:  Lieske, et al. 1976, Astron & Astrophys vol 58, p 1.;
#		     J.H. Lieske, 1979, Astron & Astrophys vol 73, 282-284.
#
#   the following are the original equations
#
#       Assign DT = (Final_JDN - Initial_JDN) / 36525.
#       Assign DRT = (Initial_JDN - reference date) / 36525.
#  .
#       Zeta0 = (2306.2181 + 1.39656*DRT - 0.000139*DRT**2) * DT
#               + (0.30188 - 0.000344*DRT) * DT**2 + 0.017998 * DT**3
#  .
#       Z     = (2306.2181 + 1.39656*DRT - 0.000139*DRT**2) * DT
#               + (1.09468 + 0.000066*DRT) * DT**2 + 0.018203 * DT**3
#  .
#       Theta = (2004.3109 - 0.85330*DRT - 0.000217*DRT**2) * DT
#               - (0.42665 + 0.000217*DRT) * DT**2 - 0.041833 * DT**3
#  .
# Andrew Cseko, Jr.      May-1990      Original

procedure prec_angles (ijday, fjday, zetaO, Z, theta)

double ijday, fjday           # i : initial and final julian day 
double zetaO, Z, theta        # o : precesion angles in radians
#--
double a, b
double a_sqrd, a_cubed

begin
	a = (fjday - ijday) / 36525
	b = (ijday - J2000_DAYN) / 36525

	a_sqrd = a**2
	a_cubed = a**3

	zetaO = (2306.2181 + 1.39656d0 * b - 0.000139d0 * b**2) * a
	Z = zetaO

	zetaO = zetaO + (0.30188 - 0.000344 * b) * a_sqrd + 0.017998 * a_cubed

	Z = Z + (1.09468 + 0.000066 * b) * a_sqrd + 0.018203 * a_cubed

	theta = (2004.3109 - 0.85330 * b - 0.000217 * b**2) * a -
	         (0.42665 + 0.000217 * b) * a_sqrd - 0.041833 * a_cubed
 
	# convert from arc seconds to radians

	zetaO = DEGTORAD(zetaO/3600.0d0)
	Z     = DEGTORAD(    Z/3600.0d0)
	theta = DEGTORAD(theta/3600.0d0)
end
