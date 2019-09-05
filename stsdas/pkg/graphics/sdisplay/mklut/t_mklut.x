include <imhdr.h>
define	SZ_LUT	200
define	FACTOR	20
define	SZ_HIST	4000

#  t_mklut -- Generate a look up table for hardware device display
#
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input data file template name
#  "lower"		Lower limit of the usable histogram
#  "upper"		Upper limit of the usable histogram
#  "contrast"		parameter which affects the display contrast
#  
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  24-Sep-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure t_mklut()

pointer	ipin
char	infile[SZ_FNAME]
real	hist[SZ_HIST]			# histogram 
int	lut[SZ_LUT]			# look up table
real	rgarr[SZ_HIST]
int	tarr[SZ_HIST], garr[SZ_HIST], ginvarr[SZ_HIST]
int	j, k
real	chnorm, rg2g
real	immin, immax
real	lutmean, lutsigsq
real	lower, upper
real	sigma
real	width
	
pointer	immap()
real	clgetr()
#==============================================================================
begin

	# read CL parameters
	call clgstr ("infile", infile, SZ_FNAME)
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	sigma = clgetr ("contrast")

	# open input file
	iferr (ipin = immap (infile, READ_ONLY, 0)) {
	    call error (1, "input file does not exist")
	}

	# input file must be 2-D data 
	if (IM_NDIM(ipin) != 2) {
	    call error (1, "data file is not 2-D")
	}

	# get min and max value of the input image
	call get_hist (ipin, upper, lower, hist, immin, immax)

	# close input file
	call imunmap (ipin)

	if (hist[SZ_HIST] > 0)
	   chnorm = float(SZ_HIST)/hist[SZ_HIST]
	else
	   call error (1, "cumulative histogram is zero")

	# normalize the accumulated histogram
	do j = 1, SZ_HIST
	    tarr[j] = nint(hist[j] * chnorm)

	# mean and sigma of desired output histogram
	lutsigsq = (0.5*float(SZ_HIST) / sigma) ** 2
	lutmean = 0.5*float(SZ_HIST)

	# Compute integral (cumulative histogram) of desired output histogram.
	rgarr[1] = exp(-0.5*lutmean**2/lutsigsq)
	do j = 2, SZ_HIST
	    rgarr[j] = rgarr[j-1] + exp(-0.5*(lutmean - j + 1)**2/lutsigsq)

	# Convert to integers
	rg2g = float(SZ_HIST) / (rgarr[SZ_HIST] - rgarr[1])
	do j = 1, SZ_HIST
	    garr[j] = nint(rg2g * rgarr[j])

	# Zero the inverse function array of G, the cumulative histogram of the
	# desired output histogram.
	do j = 1, SZ_HIST
	    ginvarr[j] = 0

	# Do inverse lookup to fill values of the inverse of G.  Where zero,
	# give it the previous value.
	do j = 1, SZ_HIST {
	    k = garr[j]
	    ginvarr[k] = j
	}
	do j = 1, SZ_HIST {
	    if (ginvarr[j] == 0) 
		ginvarr[j] = ginvarr[j-1]
	}

	#                                -1
	# Do a double lookup to perform G  ( T( input ) ) = LUT.
	# The dimensions and value ranges of the arrays are as follows:
	#
	#            X(len)      Y(range)
	#    Ginv  lut_size  x  lut_range
	#    G     lut_range x  lut_size
	#    T     lut_size  x  lut_size
	#
	# all of which follows from Ginv( T(i) ) = LUT(i).
	do j = 1, SZ_LUT {
	    k = j*FACTOR - FACTOR/2
	    lut[j] = nint(ginvarr[tarr[k]]/float(FACTOR))
	}
	
	# reverse the look up table
	#do j = 1, SZ_LUT
	    #lut[j] = SZ_LUT - lut[j]

	#lut[SZ_LUT] = 0
	lut[SZ_LUT] = SZ_LUT

	width = (immax-immin) / float(SZ_LUT)
	call printf ("# final look up table\n")
	call printf ("# input file is %s\n")
	    call pargstr (infile)
	call printf ("# lower = %g upper = %g contrast = %g\n")
	    call pargr (lower)
	    call pargr (upper)
	    call pargr (sigma)
	do j = 1, SZ_LUT {
	    call printf ("%6.2f  %d\n")
		call pargr (immin+width*(j-1))
		call pargi (lut[j])
	}
end
