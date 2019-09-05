include	"delaytime.h"

#  GEO_DELAY -- Get time delay due to displacement from solar-system barycenter
#
#  Description:
#  ------------
#  Find the difference between actual time of observation and the time that
#  would have been observed at the solar-system barycenter.  The object which
#  was observed is assumed to be very distant (outside the solar system).
#  
#  This module applies the geometric correction due to displacement, not
#  the correction due to gravitational redshift.  Geometric effects include
#  the corrections for Earth orbital motion (amplitude of 499 sec) and for
#  ST orbital motion (amplitude of 23 millisec)
#  
#  A comment on accuracy required:  one arcsec difference in the position of
#  the object can make a difference in timing of 2.5 millisec.
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Nov-1984	C. D. Biemesderfer	Original module
#  11-Apr-1990	J.-C. Hsu		rewrite in SPP
#------------------------------------------------------------------------------

procedure geo_delay (stvec, objvec, parallax, geomdelt)

double	stvec[3]	# input: telescope state vector (AU)
double	objvec[3]	# input: unit state vector of the target (unitless)
double	parallax	# input: Trigonometric parallax (")
double	geomdelt	# output: Geometric time correction (sec)

double	ob_dot_st	# inner product of OBJVEC and STVEC
double	st_dot_st	# Squared modulus of STVEC
double	pxau		# Parallax expressed in AU 
double	rsqrterm	# 2nd order term in (r_cos_theta - amplitude of 499 s)

double	adotd()
#==============================================================================
begin

	# Compute inner product of object unit state vector and telescope state
	# vector
	ob_dot_st = adotd (objvec, stvec, 3)			# in AU 
	st_dot_st = adotd (stvec, stvec, 3)			# in AU**2 

	# Compute parallax over 2 coefficient in AU and term involving
	# the square of the telescope's barycentric distance
	pxau = parallax / AUPERPC
	rsqrterm = st_dot_st - ob_dot_st * ob_dot_st

	# Low precision correction includes first two terms in series expansion.
	# Amplitude of second term is on the order of 1 millisec.
	geomdelt = ob_dot_st - (pxau / 2.d0) * rsqrterm
 
	#if (precision .eq. high) 

	# Include third term (third order in r_cosine_theta - amplitude of order
	# nanosec or smaller for extra-solar-system objects although much larger
	# for solar system observation)
	    #geomdelt = geomdelt - (pxau * pxau / 2.d0) * ob_dot_st * rsqrterm

	# Correct result in AU to sec
	geomdelt = geomdelt * CLIGHT
end
