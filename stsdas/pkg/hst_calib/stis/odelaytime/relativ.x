#  RELATIV -- Compute relativistic light time delay
#
#  Description:
#  ------------
#  Any clock moving with the earth is subject to an annual variation caused
#  by two relativistic effects : (1) the time dilation of special relativity,
#  which varies with the earth's annual variation in orbital velocity, and
#  (2) the gravitational redshift of general relativity, according to which
#  a clock runs more slowly in a stronger gravitational field.
#
#  RELATIV performs the transformation from proper time on earth to coordinate
#  time in space-time frame of reference in which the solar system barycenter
#  is at rest.  The algorithm is from Moyer, Cel. Mech., 23, 33, 1981, but
#  with the changing value of the Earth orbit eccentricity taken into account
#  together with a more accurate value for the coefficient of the main annual
#  term, and with two extra terms included to allow for the eccentricities of
#  the orbits of Jupiter and Saturn.  It is known that Moyer's equation is
#  accurate to about a 7 microsecond level.
#
#  In practice, to better than sufficient accuracy it is enough to use the
#  proper time on earth (ephemeris time, terrestrial time (TT) or atomic
#  time (TAI); the small offset of 32.184 seconds doesn't matter.)
#  The output time difference
#
#	DELTA_T = T - TAI ,
#
#  where T is the coordinate time in the solar system barycenter frame of
#  reference, and TAI is the International Atomic Time from the clock on
#  earth.  DELTA_T has a main (annual) term of amplitude approximately
#  1.7 milliseconds.
#
#  References:
#  Escobal, P.  1967, Methods of Astrodynamics, Wiley, p. 8.
#  Moyer, T.    1981, Celestial Mechanics, vol. 23, pp. 33 & 57.
#
#  Date		Author			Description
#  ----		------			-----------
#  11-Dec-1984	C. D. Biemesderfer	Modified Starlink RCC2 function
#  11-Mar-1990	J.-C. Hsu		rewrite in SPP
#   6-Sep-2005	Phil Hodge		Remove "/ RADIAN" factors;
#					don't include <math.h>
#------------------------------------------------------------------------------

procedure relativ (epoch, delta_t)

double	epoch		# input: MJD of the observation
double	delta_t		# output: Relativistic time delay (sec)

# Assorted orbital parameters
double	ecc_e		# Eccentricity of Earth orbit
double	e_anom_e	# Eccentric anomaly of Earth
double	m_anom_e	# Mean anomaly of Earth
double	m_elon_m	# Mean elongation of Moon from Sun

double	m_anom_j	# Mean anomaly of Jupiter
double	l_m_lj		# Mean longitude of Jupiter from E-M barycenter
double	omega_j		# Related to argument of perihelion for Jupiter

double	m_anom_s	# Mean anomaly of Saturn
double	l_m_ls		# Mean longitude of Saturn  from E-M barycenter
double	omega_s		# Related to argument of perihelion for Saturn

double	time		# Obs time in secs past 1950 Jan 1.0
#==============================================================================
begin

	# Convert input time to seconds past 1950 Jan 1.0
	time     = (epoch - 33282.d0) * 86400.d0

	# Calculate orbital parameters at time of observation
	ecc_e    = 1.673 014D-2 - 1.325D-14 * time		# Escobal (1.5)

	m_anom_e = 6.248 291D0  + 1.990 968 71  D-07 * time	# Moyer II (44)
	m_elon_m = 2.518 411D0  + 2.462 600 818 D-06 * time	# Moyer II (45)
	l_m_lj   = 5.652 593D0  + 1.823 136 37  D-07 * time	# Moyer II (46)
	l_m_ls   = 2.125 474D0  + 1.923 399 23  D-07 * time	# Moyer II (47)
	m_anom_j = 5.286 877D0  + 1.678 506 3   D-08 * time	# Moyer II (48)
	m_anom_s = 1.165 341D0  + 0.675 855 8   D-08 * time	# Moyer II (49)

	e_anom_e = m_anom_e + ecc_e * sin (m_anom_e)		# Moyer II (40)

	omega_j  = l_m_lj - m_anom_j			# Added by Starlink
	omega_s  = l_m_ls - m_anom_s			# Added by Starlink

	# Correction from proper time to coordinate time, using Moyer II (38)

	# Daily motion of Earth around Sun correction has amplitude of millisecs
	delta_t  = (9.9153 D-2 * ecc_e) * sin (e_anom_e) +

	# Remaining corrections have amplitudes of microsec
     		   1.548 D-6 * sin (m_elon_m) +
     		   5.21  D-6 * sin (m_anom_j) +
     		   2.45  D-6 * sin (m_anom_s) +
     		  20.73  D-6 * sin (l_m_lj) +
     		   4.58  D-6 * sin (l_m_ls) +
     		   1.00  D-6 * sin (omega_j) +		# Added by Starlink
     		   0.26  D-6 * sin (omega_s)		# Added by Starlink

end
