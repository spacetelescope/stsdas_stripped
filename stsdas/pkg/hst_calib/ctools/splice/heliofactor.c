/* This file contains:
	HelioFactor
	EarthVel
	Mod2pi
	Precess
*/

# include <math.h>		/* sin, cos */

# include "pweight.h"

# define PI    3.1415926535897932384626433
# define DEG_RAD     (PI / 180.)		/* degrees to radians */
# define ARCSEC_RAD  (PI / (180.*3600.))	/* arcseconds to radians */

# define SPEED_OF_LIGHT 299792.458		/* c in km/sec */
# define REFDATE (51544.5)		/* MJD for 2000 Jan 1, 12h UT */
# define KM_AU   (1.4959787e8)		/* kilometers per astronomical unit */
# define SEC_DAY (86400.)		/* seconds per day */

static void EarthVel (double, double *);
static double mod2pi (double);
static void Precess (double, double *);

/* This routine computes the heliocentric correction factor.  An
   observed wavelength should be multiplied by this factor to correct
   for the earth's orbital velocity around the sun:

   heliocentric wavelength = [1 + V/c * cos (theta)] * observed wavelength,

   where V is the speed of the earth in its orbit around the Sun,
   c is the speed of light, and theta is the angle between the earth's
   velocity vector and the direction toward the target.  The expression
   in brackets is returned as the factor.

   Note that the radial velocity is -V * cos (theta).

   Phil Hodge, 2000 Jan 31:
	Copied from cs7/heliofactor.c; calling sequence modified.
*/

void HelioFactor (WgtInfo *sts) {

	double ra, dec;		/* coords of target */
	double velocity[3];	/* velocity vector of earth around sun */
	double target[3];	/* unit vector pointing toward target */
	double vel_r;		/* component of velocity toward target */
	int i;

	/* Convert target position to rectangular coord unit vector. */
	ra = sts->ra_targ * DEG_RAD;
	dec = sts->dec_targ * DEG_RAD;
	target[0] = cos (dec) * cos (ra);
	target[1] = cos (dec) * sin (ra);
	target[2] = sin (dec);

	/* Precess the target coordinates from J2000 to the current date. */
	Precess (sts->time_of_exp, target);

	/* Get the earth's velocity vector (km/sec). */
	EarthVel (sts->time_of_exp, velocity);

	/* Dot product. */
	vel_r = 0.;
	for (i = 0;  i < 3;  i++)
	    vel_r += (velocity[i] * target[i]);

	sts->hfactor = 1. + vel_r / SPEED_OF_LIGHT;
}

/* This routine computes the earth's orbital velocity around the Sun
   in celestial rectangular coordinates.  The expressions are from the
   Astronomical Almanac, p C24, which gives low precision formulas for
   the Sun's coordinates.  We'll apply these formulas directly to get
   the velocity of the Sun relative to the earth, then we'll convert to
   km per sec and change the sign to get the velocity of the earth.
*/

static void EarthVel (double mjd, double *velocity) {

/* arguments:
double mjd           i: time, modified Julian Day Number
double velocity[3]   o: velocity vector of earth around sun, celestial coords
*/

	double tdays;		/* time in days since JD 2451545.0 */

	/* all angular values are in radians */
	double g;		/* mean anomaly */
	double l;		/* mean longitude, corrected for aberration */
	double elong;		/* ecliptic longitude */
	double radius;		/* distance to sun in AU */
	double eps;		/* obliquity of ecliptic */

	/* derivatives are in radians per day or AU per day */
	double g_dot, l_dot, elong_dot, radius_dot;
	double x_dot, y_dot, z_dot;	/* velocity of sun wrt earth */

	tdays = mjd - REFDATE;

	g_dot = 0.9856003 * DEG_RAD;
	l_dot = 0.9856474 * DEG_RAD;

	eps = (23.439 - 0.0000004 * tdays) * DEG_RAD;

	g = mod2pi ((357.528 + 0.9856003 * tdays) * DEG_RAD);
	l = mod2pi ((280.461 + 0.9856474 * tdays) * DEG_RAD);

	/*      L   1.915 deg            0.02 deg */
	elong = l + 0.033423 * sin (g) + 0.000349 * sin (2.*g);
	elong_dot = l_dot +
	            0.033423 * cos (g) * g_dot +
	                                 0.000349 * cos (2.*g) * 2.*g_dot;

	radius = 1.00014 - 0.01671 * cos (g) - 0.00014 * cos (2.*g);
	radius_dot =       0.01671 * sin (g) * g_dot +
	                                       0.00014 * sin (2.*g) * 2.*g_dot;

	x_dot = radius_dot * cos (elong) -
	            radius * sin (elong) * elong_dot;

	y_dot = radius_dot * cos (eps) * sin (elong) +
	            radius * cos (eps) * cos (elong) * elong_dot;

	z_dot = radius_dot * sin (eps) * sin (elong) +
	            radius * sin (eps) * cos (elong) * elong_dot;

	/* Convert to km/sec with Sun as origin. */
	velocity[0] = -x_dot * KM_AU / SEC_DAY;
	velocity[1] = -y_dot * KM_AU / SEC_DAY;
	velocity[2] = -z_dot * KM_AU / SEC_DAY;
}

/* This routine returns the argument modulo two pi. */

static double mod2pi (double x) {

	double quotient;
	long iq;

	quotient = x / (2. * PI);
	iq = (long) quotient;
	if (quotient < 0.)
	    iq -= 1;

	return (x - (double)iq * 2. * PI);
}

/* Precess from J2000 to the date mjd.

   References:
        IAU 1976:  Lieske, et al. 1976, Astron & Astrophys vol 58, p 1.;
		J.H. Lieske, 1979, Astron & Astrophys vol 73, 282-284.

   Target will be modified in-place.  On input, target is a unit vector
   pointing toward the target in J2000 coordinates.  On output target
   will have been precessed to the date given by modified Julian date mjd.
*/

static void Precess (double mjd, double *target) {

/* arguments:
double mjd          i: current date, MJD
double target[3]    io: target position, J2000 --> mjd
*/

	double dt;		/* time interval in Julian centuries */
	double dt2, dt3;
	double zeta, z, theta;	/* precession constants for date MJD */
	double a[9];		/* precession matrix */
	double targ[3];		/* local copy of input target */

	targ[0] = target[0];
	targ[1] = target[1];
	targ[2] = target[2];

	dt = (mjd - REFDATE) / 36525.;
	dt2 = dt * dt;
	dt3 = dt * dt * dt;

	zeta = 2306.2181 * dt + 0.30188 * dt2 + 0.017998 * dt3;

	z = 2306.2181 * dt + 1.09468 * dt2 + 0.018203 * dt3;

	theta = 2004.3109 * dt - 0.42665 * dt2 - 0.041833 * dt3;
 
	/* convert from arc seconds to radians */
	zeta *= ARCSEC_RAD;
	z *= ARCSEC_RAD;
	theta *= ARCSEC_RAD;

	/* convert to a matrix */
	/* first row */
	a[0] =  cos (z) * cos (theta) * cos (zeta) - sin (z) * sin (zeta);
	a[1] = -cos (z) * cos (theta) * sin (zeta) - sin (z) * cos (zeta);
	a[2] = -cos (z) * sin (theta);

	/* second row */
	a[3] =  sin (z) * cos (theta) * cos (zeta) + cos (z) * sin (zeta);
	a[4] = -sin (z) * cos (theta) * sin (zeta) + cos (z) * cos (zeta);
	a[5] = -sin (z) * sin (theta);

	/* third row */
	a[6] =            sin (theta) * cos (zeta);
	a[7] =           -sin (theta) * sin (zeta);
	a[8] =            cos (theta);

	/* Multiply:  matrix * target position */
	target[0] = a[0] * targ[0] + a[1] * targ[1] + a[2] * targ[2];
	target[1] = a[3] * targ[0] + a[4] * targ[1] + a[5] * targ[2];
	target[2] = a[6] * targ[0] + a[7] * targ[1] + a[8] * targ[2];
}
