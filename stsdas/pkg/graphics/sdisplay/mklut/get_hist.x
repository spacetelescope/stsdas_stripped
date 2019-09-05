include <imhdr.h>
define	SZ_HIST	4000

#  get_hist -- construct the accumulated histogram for an image
#
#  Input CL parameters:
#  -----------------
#
#  Description:
#  ------------
#  Setting an upper and lower limit for the accumulated histogram and iterate.
#  
#  Date		Author			Description
#  ----		------			-----------
#  30-Sep-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure get_hist (ipin, upper, lower, hist, immin, immax)

pointer	ipin				# input image pointer
real	upper, lower
real	hist[SZ_HIST]			# histogram 
real	immin, immax

pointer	pic2
int	i, j
int	jmin, jmax
int	iter, max_iter
int	dim_x, dim_y, npix
real	top, bottom
real	area, xmin0
real	width
	
pointer	imgs2r()
#==============================================================================
begin

	max_iter = 10

	dim_x = IM_LEN(ipin, 1)
	dim_y = IM_LEN(ipin, 2)
	npix = dim_x * dim_y

	# get min and max value of the input image
	pic2 = imgs2r (ipin, 1, dim_x, 1, dim_y)
	call alimr (Memr[pic2], npix, immin, immax)

	do iter = 1, max_iter {
	    width = (immax - immin) / real(SZ_HIST)
	    xmin0 = immin

	    # initialize the histogram 
	    do j = 1, SZ_HIST
	        hist[j] = 0

	    do i = 1, npix {

	        # construct the histogram
	        j = int((Memr[pic2+i-1] - immin) / width) + 1
	        if (j <= SZ_HIST && j > 0)
	            hist[j] = hist[j] + 1.
	    }

	    # construct the accumulated histogram
	    do j = 2, SZ_HIST
	        hist[j] = hist[j] + hist[j-1]

	    if (iter == 1) 
		area = hist[SZ_HIST]

	    # determine the new bounds of the histogram
	    top = upper * area
	    bottom = lower * area

	    jmin = 1
	    jmax = SZ_HIST

	    do j = 1, SZ_HIST-1 {
	    	if (hist[j] <= top && hist[j+1] > top) {
		    immax = xmin0 + width * float(j)
	 	    jmax = j
		}
	    
	    	if (hist[j] <= bottom && hist[j+1] > bottom) {
		    immin = xmin0 + width * float(j-1)
	 	    jmin = j
		}
	    }

	    # bounds area reached 
	    if (jmin == 1 && jmax == SZ_HIST)
		break
	}
end
