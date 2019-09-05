include <error.h>
include <mach.h>
include <syserr.h>

#################################################################################
#										#
# EVALMODEL --	Calculate the square-root of the variance expected at the 	#
#		given DN level, based upon the noise model parameters.  	#
#										#
#	6/91	RAShaw:	Initial code						#
#	8/91	RAShaw:	Revised for chisqr calculation				#

procedure evalmodel (dn, sig, hisig, losig, xmin, xmax, nfnpts) 

include	"nmcom.h"

#  Calling arguments:
real	dn[nfnpts]		# Value of pixel in Data Numbers
real	sig[nfnpts]		# Sigma at pixel in DN from noise model
real	hisig[nfnpts]		# Max variation in sigma from noise model
real	losig[nfnpts]		# Min variation in sigma from noise model
real	xmin			# Minimum ordinate value
real	xmax			# Maximum ordinate value
int	nfnpts			# Number of points for noise function

#  Local variables:
int	dpts			# Number of points in each decade of Xrange
int	i, j			# Loop index
real	lstep			# Log interval for function evaluation
real	step			# Interval for function evaluation
real	temp			# Temporary variable
real	xhi			# Revised maximum X value

# Functions called:
real	noisefn()		# Return sigma from model for input DN

begin

#  Set the stepsize such that the number of data points in each decade of 
#  X range is the same, then fill the DN and SIGMA arrays with values from 
#  each decade.
	if (xmax <= 0. || xmin >= xmax)
	    call error (0, "Xmax must exceed zero and Xmin")
	if (real (int (log10 (xmax))) < log10 (xmax)) 
	    xhi = 10 ** (int (log10 (xmax)) + 1)
	if (xmin >= 10.) {
	    lstep = (log10 (xhi) - int (log10 (xmin))) / real (nfnpts-1)
	    do i = 1, nfnpts {
		dn[i]  = 10 ** (real(i-1) * lstep) * xmin
		sig[i] = noisefn(dn[i])
	    }
	    return
	} else if (xmin >= 0.) {
	    j = 1
	} else {
	    j = 2
	    dn[1]  = xmin
	    sig[1] = noisefn(dn[1])
	}
	dpts  = real (nfnpts-j) / log10 (xhi)
	step  = 10. / real (dpts)
	do i = j, dpts+j-1 {
	    dn[i]  = real(i-j) * step 
	    sig[i] = noisefn(dn[i])
	}
	lstep = (log10 (xhi) - 1.) / real (nfnpts-dpts-j)
	do i = dpts+j, nfnpts {
	    dn[i]  = 10 ** (real(i-dpts-j) * lstep + 1.)
	    sig[i] = noisefn(dn[i])
	}

#  Compute upper bound to function based upon chi-clipping limits
	temp = sqrt (2. / (xypts - 1.))
	if (!IS_INDEFR (cliphi) && xypts > 1) {
	    do i = 1, nfnpts 
		hisig[i] = sig[i] * (1. + cliphi * temp)
	} else {
	    do i = 1, nfnpts 
		hisig[i] = INDEFR
	}

#  Compute lower bound to function based upon chi-clipping limits, impose floor
	if (!IS_INDEFR (cliplo) && xypts > 1) {
	    do i = 1, nfnpts 
		losig[i] = sig[i] * (1. - cliplo * temp)
	    call arltr (losig, nfnpts, 1./MAX_REAL, INDEFR)
	} else {
	    do i = 1, nfnpts 
		losig[i] = INDEFR
	}

end


#################################################################################
#										#
# NOISEFUNC --	Calculate the square-root of the variance expected at the 	#
#		given DN level, based upon the noise model parameters.  	#
#										#
#	Initial code:	7/91 by RAShaw						#

real procedure noisefn(dn) 

include	"nmcom.h"

#  Calling argument
real	dn		# Value of pixel in Data Numbers

# Local variables
real	tmp
real	noise		# Function value

begin
	tmp = scalen / 100.
	if (dn <= 0.) 
	    noise = readn 
	else
	    noise = sqrt (readn * readn + dn / gain + dn * dn * tmp * tmp) 
	return (noise)
end


#################################################################################
#										#
# CHIS --	Calculate the normalized chi-square of the noisemodel given  	#
#		the noise model parameters.  					#
#										#
#	8/91	Initial code:					RAShaw		#

procedure chis (mean, sigma, nbins, chisqr, npts) 

include	"nmcom.h"

# Calling arguments:
real	mean[nbins]	# Array of mean values
real	sigma[nbins]	# Array of standard deviations about mean 
int	nbins		# Size of above arrays
double	chisqr		# Value of chi-squared
int	npts		# Number of non-rejected bins in chi-square, fit

# Local variables:
real	clip		# Chi-square rejection threshold
int	i		# Loop index
real	nrmdev		# Normalized deviation
real	temp		# Normalization

# Functions used:
real	noisefn()	# Return sigma from model for input DN

begin
	chisqr = 0.0d0
	npts   = 0
	temp   = sqrt ((xypts - 1.) / 2.)
	do i = 1, nbins {
	    if (!IS_INDEFR (sigma[i])) {
		nrmdev = ((sigma[i] / noisefn(mean[i])) - 1.) * temp
		if (nrmdev < 0.)
		    clip = cliplo 
		else
		    clip = cliphi 
		if (abs (nrmdev) <= clip || IS_INDEFR (clip)) {
		    iferr (chisqr = chisqr + nrmdev ** 2) {
			call error (1, 
			    "Deviations too large: decrease CLIP[hi|lo]")
			chisqr = INDEFD
			break
		    }
		    npts = npts + 1
		}
	    }
	}
	if (npts > 3)
	    chisqr = chisqr / (npts - 3.)
	else
	    chisqr = INDEFR
end
