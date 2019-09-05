include	<mach.h>
include "wstat.h"

################################################################################
#										
# WINITIALIZE -- Initialize the statistics structure.				
#										
#										
#	9/91	RAShaw	Initial SPP implementation.				

procedure winitialize (wst, lower, upper)

pointer	wst		# pointer to the statistics structure
real	lower		# lower data limit
real	upper		# upper data limit

begin
	if (IS_INDEFR(lower))
	    WS_LO(wst) = -MAX_REAL
	else
	    WS_LO(wst) = lower

	if (IS_INDEFR(upper))
	    WS_HI(wst) = MAX_REAL
	else
	    WS_HI(wst) = upper

	WS_NPIX(wst)   = 0
	WS_MIN(wst)    =  MAX_REAL
	WS_MAX(wst)    = -MAX_REAL
	WS_SUMX(wst)   = 0.d0
	WS_MEAN(wst)   = INDEFR
	WS_MEDIAN(wst) = INDEFR
	WS_MODE(wst)   = INDEFR
	WS_STDDEV(wst) = INDEFR
	WS_SKEW(wst)   = INDEFR
	WS_KURT(wst)   = INDEFR
end


################################################################################
#										
# WMEAN --	Determine min, max, and mean of data values between lower 	
#		and upper threshold.  						
#										
#	9/91	RAShaw	Initial SPP implementation				

procedure wmean (wst, x, npts, work, lower, upper)

pointer	wst		# pointer to the statistics structure
real	x[ARB]		# Data array
int	npts		# the number of data points
real	work[ARB]	# Data array excluding flagged pixels
real	lower		# lower data boundary
real	upper		# upper data boundary

int	i		# Loop index
int	gpts		# Running total of non-rejected pixels
real	lo, hi		# Current extreme pixel values
real	xx		# Current pixel value 
real	xmin, xmax	# Min/max of non-rejected pixels
double	sumx		# Running sum of non-rejected pixels

begin
	lo   = WS_LO(wst)
	hi   = WS_HI(wst)
	gpts = WS_NPIX(wst)
	sumx = 0.d0
	xmin = WS_MIN(wst)
	xmax = WS_MAX(wst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    do i = 1, npts {
		xx   = x[i]
		gpts = gpts + 1
		work[gpts] = xx
		if (xx < xmin)
		    xmin = xx
		if (xx > xmax)
		    xmax = xx
		sumx = sumx + xx
	    }
	} else {
	    do i = 1, npts {
		xx = x[i]
		if (xx < lo || xx > hi)
		    next
		gpts = gpts + 1
		work[gpts] = xx
		if (xx < xmin)
		    xmin = xx
		if (xx > xmax)
		    xmax = xx
		sumx = sumx + xx
	    }
	}

	WS_NPIX(wst) = gpts
	WS_SUMX(wst) = WS_SUMX(wst) + sumx
	if (gpts > 0)
	    WS_MEAN(wst) = WS_SUMX(wst) / gpts
	if (WS_NPIX(wst) > 0) {
	    WS_MIN(wst)  = xmin
	    WS_MAX(wst)  = xmax

	# comment the following 3 lines out JCH 10/19/95.
	# otherwise it will give INDEF for MAX if the first line has 0 valid
	# points
	#} else {
	    #WS_MIN(wst)  = INDEFR
	    #WS_MAX(wst)  = INDEFR
	}
end


#################################################################################
#										#
# DQWMEAN --	Determine min, max, and mean of data values between lower 	#
#		and upper threshold.  						#
#										#
#	9/91	RAShaw	Initial SPP implementation.				#

procedure dqwmean (wst, x, dqf, npts, work, lower, upper, badbits)

pointer	wst		# pointer to the statistics structure
real	x[ARB]		# the data array
int	dqf[ARB]	# the data quality array
int	npts		# the number of data points
real	work[ARB]	# Data array excluding flagged pixels
real	lower		# lower data boundary
real	upper		# upper data boundary
int	badbits		# Flagged DQF bits

int	bflag		# DQF flag @pixel
int	i		# Loop index
int	gpts		# Running total of non-rejected pixels
real	lo, hi		# Current extreme pixel values
real	xx		# Current pixel value
real	xmin, xmax	# Min/max of non-rejected pixels
double	sumx		# Running sum of non-rejected pixels

begin
	lo   = WS_LO(wst)
	hi   = WS_HI(wst)
	gpts = WS_NPIX(wst)
	sumx = 0.d0
	xmin = WS_MIN(wst)
	xmax = WS_MAX(wst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    do i = 1, npts {
		xx    = x[i]
                bflag = dqf[i]
                call aandki (bflag, badbits, bflag, 1)
                if (bflag == 0) {
		    gpts = gpts + 1
		    work[gpts] = xx
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    }
	} else {
	    do i = 1, npts {
		xx    = x[i]
                bflag = dqf[i]
                call aandki (bflag, badbits, bflag, 1)
		if (xx < lo || xx > hi || bflag != 0)
		    next
		else {
		    gpts = gpts + 1
		    work[gpts] = xx
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    }
	}

	WS_NPIX(wst) = gpts
	WS_SUMX(wst) = WS_SUMX(wst) + sumx
	if (gpts > 0)
	    WS_MEAN(wst) = WS_SUMX(wst) / gpts
	WS_MIN(wst)  = xmin
	WS_MAX(wst)  = xmax
end


#################################################################################
#										#
# MOMENT --	Procedure to compute the first four central moments of the 	#
#		distribution.							#
#										#
#	9/91	RAShaw	Initial SPP implementation.				#

procedure moment (wst, x)

pointer	wst			# statistics structure
real	x[ARB]			# data array

double  dev			# Deviation of pixel value from image mean
real	mean			# Mean of data
int	npts			# Length of array
int     i			# Loop index
double	p			# 
double  var, stdev              # First and second moments
double  skew, kurt              # Third and fourth moments 
real    xx                      # Image value @pixel[i]

begin
	npts = WS_NPIX(wst)
        mean = WS_MEAN(wst)
	if (npts < 2 || IS_INDEFI (npts) || IS_INDEFR (mean))
	    return
	dev  = 0.d0
        var  = 0.d0
        skew = 0.d0
        kurt = 0.d0

# Accumulate sums
	do i = 1, npts {
	    xx   = x[i]
	    dev  = xx - mean
	    p    = dev * dev
	    var  = var + p
	    p    = p * dev
	    skew = skew + p
	    p    = p * dev
	    kurt = kurt + p
	}

# Normalize sums to derive moments
	var   = var / real (npts - 1)
	stdev = sqrt (var)
	WS_STDDEV(wst) = stdev
	if (var == 0.d0)
	    return
	WS_SKEW(wst) = skew / (npts * stdev * var)
	WS_KURT(wst) = kurt / (npts * var ** 2) - 3.
end
