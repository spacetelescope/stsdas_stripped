include <imhdr.h>

define	PADDING	1.e-6
define	PAD2	1.e-3

#  get_hist -- construct the accumulated histogram for an image
#
#  Input CL parameters:
#  -----------------
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  09-Jan-1997  J.-C. Hsu		Modified from get_hist in sdisplay
#					Make the histogram array's size flexible
#  31-Jul-1997  J.-C. Hsu		Rewrite, use the select routine so it
#					is more robust when there are spikes
#					in the data values
#------------------------------------------------------------------------------

procedure get_hist (ipin, upper, lower, hist, nbins, hmin, hmax)

pointer	ipin		# input image pointer
real	lower		# lower limit of the usable histogram
			# if lower = 0.01, the bottom 1% of the 
			# original data points will be excluded
real	upper		# upper limit of the usable histogram
			# if upper = 0.99, the top 1% of the 
			# original data points will be excluded
real	hist[nbins]	# histogram 
int	nbins		# number of bins
real	hmin, hmax	# minimum and maximum of the "usable" histogram

pointer	pic2
int	i, j, k
int	dim_x, dim_y, npix
real	width
real	ratio, base
	
pointer	imgs2r()
real	select()
#==============================================================================
begin

	dim_x = IM_LEN(ipin, 1)
	dim_y = IM_LEN(ipin, 2)
	npix = dim_x * dim_y

	# get min and max value of the input image
	pic2 = imgs2r (ipin, 1, dim_x, 1, dim_y)
	call alimr (Memr[pic2], npix, hmin, hmax)

	if (hmin == hmax) return

	# find the proper range 
	k = nint (real(npix) * lower)
	k = min (k, npix)
	k = max (k, 1)
	hmin = select (Memr[pic2], npix, k)
	k = nint (real(npix) * upper)
	k = min (k, npix)
	k = max (k, 1)
	hmax = select (Memr[pic2], npix, k)

	if (hmin == hmax) return

	# construct the histogram
	width = (hmax - hmin) / real(nbins)

	# initialize the histogram 
	do j = 1, nbins
	    hist[j] = 0

	do i = 1, npix {
	    ratio = (Memr[pic2+i-1] - hmin) / width
	    if (ratio < real(nbins+2) && ratio > -1.) {
	        j = int(ratio)+ 1
	        if (j <= nbins && j > 0)
	            hist[j] = hist[j] + 1.
	    }
	}

	# construct the accumulated histogram
	do j = 2, nbins
	    hist[j] = hist[j] + hist[j-1]

	# if hmin and hmax are too close, make an artificial separation
	base = max (abs(hmin), abs(hmax))
	if ((hmax-hmin)/base <= PAD2/2.) {
	    hmax = hmax + base * PAD2
	    hmin = hmin - base * PAD2
	}
end
