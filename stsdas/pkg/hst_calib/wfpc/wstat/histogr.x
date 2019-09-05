include	<mach.h>
include "wstat.h"

#################################################################################
#										#
# WHIST --	Procedure to accumulate the histogram of the image pixels.	#
#										#
#	9/91	RAShaw	Initial SPP implementation				#

procedure whist (data, npix, hgm, nbins, hmin, hmax, hwidth)

#  Calling arguments:
real	data[ARB]	# Non-rejected pixels
int	npix		# Number of non-rejected pixels
int	hgm[ARB]	# pointer to the histogram
int	nbins		# number of histogram bins
real	hmin, hmax	# minimum, maximum histogram values
real	hwidth		# Bin width

#  Local variables:
int	bin		# bin index
real	dz		# inverse bin width
int	i		# loop index

begin
	dz     = real (nbins - 1) / (hmax - hmin)
	hwidth = (hmax - hmin) / real (nbins - 1)
	if (abs (dz - 1.0) < (EPSILONR * 2.0)) {
	    hwidth = 1.0
	    do i = 1, npix {
		bin = int (data[i] - hmin) + 1
		hgm[bin] = hgm[bin] + 1
	    }
	} else {
	    do i = 1, npix {
		bin = int ((data[i] - hmin) * dz) + 1
		hgm[bin] = hgm[bin] + 1
	    }
	}
end


#################################################################################
#										#
# HMEDIAN --	Procedure to compute the median of the values from histogram.	#
#										#
#	9/91	RAShaw	Initial SPP implementation				#

procedure hmedian (wst, hgm, nbins, hmin, hmax, hwidth)

#  Calling arguments:
pointer	wst		# pointer to the statistics strucuture
int	hgm[ARB]	# histogram of the pixels
int	nbins		# number of bins in the histogram
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value
real	hwidth		# resolution of the histogram

#  Local variables:
int	i		# Loop index
int	lo, hi
pointer	sp, ihgm
real	h1, hdiff, hnorm

#  Function used:
bool	fp_equalr()	# Test for floating-point equality

begin
	# take care of the zero point case - JC Hsu 10/20/95
	if (WS_NPIX(wst) <= 0) {
	    WS_MEDIAN(wst) = INDEF
	    return
	}
	call smark (sp)
	call salloc (ihgm, nbins, TY_REAL)

# Integrate the histogram and normalize.
	Memr[ihgm] = hgm[1]
	do i = 2, nbins
	    Memr[ihgm+i-1] = hgm[i] + Memr[ihgm+i-2]
	hnorm = Memr[ihgm+nbins-1]
	if (abs(hnorm) > EPSILONR)
	    call adivkr (Memr[ihgm], hnorm, Memr[ihgm], nbins)
	else
	    call amovkr (0., Memr[ihgm], nbins)

# Initialize the low and high bin numbers.
	lo = 0
	hi = 1

# Search for the point which divides the integral in half.
	do i = 1, nbins {
	    if (Memr[ihgm+i-1] > 0.5)
		break
	    lo = i
	}
	hi = lo + 1

# Approximate the histogram.
	h1 = hmin + lo * hwidth
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]

	if (fp_equalr (hdiff, 0.0))
	    WS_MEDIAN(wst) = h1
	else if (lo == 0)
	    WS_MEDIAN(wst) = h1 + 0.5 / hdiff * hwidth
	else
	    WS_MEDIAN(wst) = h1 + (0.5 - Memr[ihgm+lo-1]) / hdiff * hwidth

	call sfree (sp)
end


#################################################################################
#										#
# HMODE --	Procedure to compute the mode from data histogram.  		#
#										#
#	9/91	RAShaw	Initial SPP implementation				#

procedure hmode (wst, hgm, nbins, hmin, hmax, hwidth)

#  Calling arguments:
pointer	wst		# pointer to the statistics strucuture
int	hgm[ARB]	# histogram of the pixels
int	nbins		# number of bins in the histogram
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value
real	hwidth		# resolution of the histogram

#  Local variables:
int	i		# Loop index
int	bpeak		# Bin index containing peak
real	hpeak		# Histogram peak value
real	dh1, dh2, denom

#  Function used:
bool	fp_equalr()	# Test for floating-point equality

begin

# Find the three points surrounding the histogram max.
	hpeak = hgm[1]
	bpeak = 1
	do i = 2, nbins {
	    if (hgm[i] > hpeak) {
		hpeak = hgm[i]
		bpeak = i
	    }
	}
	bpeak = max (1, bpeak-1)

# Do a parabolic interpolation to find the peak.
	dh1 = hgm[bpeak+1] - hgm[bpeak]
	dh2 = hgm[bpeak+1] - hgm[bpeak+2]
	denom = dh1 + dh2
	if (fp_equalr (denom, 0.0)) {
	    WS_MODE(wst) = hmin + (bpeak + 0.5) * hwidth
	} else {
	    WS_MODE(wst) = bpeak + 1 + 0.5 * (dh2 - dh1) / denom
	    WS_MODE(wst) = hmin + .5 * hwidth + (WS_MODE(wst) - 1.0) * hwidth
	}
end
