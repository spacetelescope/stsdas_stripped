#Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
include <mach.h>
include <imhdr.h>
include "gstat.h"

#--------------------------------------------------------------------15 Dec 95--
.help gmidmod.x Dec95 imgtools$gstatistics
.ih
NAME
  hgmaccum -- Accumulate histogram for non-rejected pixels
 do_midmod -- Compute median and mode
gst_goodpt -- Accumulate non-rejected pixels from a line vector
 gst_ihist -- Initilaize the histogram of the image pixels.
   hmedian -- Compute the median of the values.
     hmode -- Refine the mode by re-binning
 gst_hmode -- Compute mode from histogram
.endhelp
#-------------------------------------------------------------------------------
# hgmaccum -- Accumulate histogram for non-rejected pixels
# Revision History:
#	   Aug 93  CYZhang	Initial implementation
#	 1 Dec 95 by RAShaw	Use macros for dereferencing arrays; moved 
#				test for histogram min < max to begining of 
#				routine.
#	 1 Sep 98 EKK 		Changed done == true to done			
#
#
procedure hgmaccum (gst, im, msk, msktype, lower, upper)

# Calling arguments
pointer	im			# Pointer to input image descriptor
pointer	msk			# Pointer to DQF image descriptor
pointer	gst			# Pointer to stat structure
int	msktype			# Pixel lists or image type mask
real	lower, upper		# Lower and upper data boundary

# Local variables
pointer buf			# Data from one line of the image
pointer mskbuf			# Data from one line of mask image
pointer	wkbuf			# Non-rejected pixels in one line
int	ndim			# Dimensionality of image
long	v1[IM_MAXDIM]		# Index of next line in image
long	v2[IM_MAXDIM]		# Index of next line in mask
long	vs[IM_MAXDIM]		# Beginning of range
long	ve[IM_MAXDIM]		# Ending of range
long	v[IM_MAXDIM]		# Positions of segments
int	npts			# Number of points in one line
int	mval			# Mask value for the current segment
int 	gpix			# Number of non-rejected pixels
long	one_l			# A constant of long
int	i			# Loop variable

# Functions called
int	imgnlr()		# Fetch next line independent of dim
int	imgnli()		# Type int for mask images
int	mio_glsegr()		# Get segments through mask

# Memory management:
define	Buf		Memr[buf]
define	Workbuf		Memr[wkbuf]
define	Maskbuf		Memi[mskbuf]

begin
	if (GS_HMAX(gst) <= GS_HMIN(gst))
	    return

	one_l = 1

	# Construct histogram out of non-rejected pixels
	if (msktype == MSK_IMG && msk != NULL) {

	    # Initialize position vectors to line 1, column 1, band 1, .......
	    call amovkl (one_l, v1, IM_MAXDIM)
	    call amovkl (one_l, v2, IM_MAXDIM)
	    npts = int(IM_LEN (im, 1))

	    call malloc (wkbuf, npts, TY_REAL)
	    while (imgnlr (im, buf, v1) != EOF && 
		imgnli (msk, mskbuf, v2) != EOF) {
		call gst_goodpt (Buf, Maskbuf, npts, Workbuf, gpix, 
		    lower, upper)

		# If a line contains no good pixels or all pixel values in the
		# entire image are the same, skip the next line (hwidth would 
		# be zero).

		if (gpix > 0)
		    call ahgmr (Workbuf, gpix, Memi[GS_HGM(gst)],
		        GS_NBINS(gst), GS_HMIN(gst), GS_HMAX(gst))
	    }
	    call mfree (wkbuf, TY_REAL)

	} else if (msktype == MSK_LST && msk != NULL) {
	    ndim = IM_NDIM(im)
	    call amovkl (one_l, vs, ndim)
	    do i = 1, ndim
		ve[i] = IM_LEN(im,i)
	    call mio_setrange (msk, vs, ve, ndim)
	    while (mio_glsegr (msk, buf, mval, v, gpix) != EOF){

	    	# If a line contains no good pixels or all pixel values in the
		# entire image are the same, skip the next line (hwidth would 
		# be zero).

		if (gpix > 0)
		    call ahgmr (Buf, gpix, Memi[GS_HGM(gst)],
		        GS_NBINS(gst), GS_HMIN(gst), GS_HMAX(gst))
	    }

	# No masking operation needed
	} else if (msktype == MSK_NONE || msk == NULL) {

	    call amovkl (one_l, v1, IM_MAXDIM)
	    npts = int(IM_LEN (im, 1))
	    while (imgnlr (im, buf, v1) != EOF) {

		# If a line contains no pixels or all pixel values in the
		# entire image are the same, skip to the next line (hwidth 
		# would be zero)

		if (npts > 0)
		    call ahgmr (Buf, npts, Memi[GS_HGM(gst)],
		        GS_NBINS(gst), GS_HMIN(gst), GS_HMAX(gst))
	    } 
	}

end

#-------------------------------------------------------------------------------
#  DO_MIDMOD -- Procedure to compute median and mode

procedure do_midmod (gst, gsw)

# Calling arguments
pointer	gst			# Pointer to statistics structure
pointer	gsw			# Pointer to switch structure

begin

	if (SW_MIDPT(gsw) == YES)
	    call hmedian (gst)
	if (SW_MODE(gsw) == YES)
	    call hmode (gst)
end


#-------------------------------------------------------------------------------
#  GST_GOODPT -- Procedure to accumulate non-rejected pixels from a line vector
#
#	 1 Dec 95 by RAShaw	Fixed for case where either "lower" or "upper" 
#				is INDEF, but not both.  

procedure gst_goodpt (x, msk, npts, work, gpix, lower, upper)

# Calling arguments
real	x[ARB]		# input image line
int	msk[ARB]	# image mask line
real	work[ARB]	# Array of non-rejected pixels
int	npts		# Number of data points
int	gpix		# Number of good points
real	lower, upper	# Lower and Upper data boundary

# Local variables
int	i		# Loop variable
bool	lower_defined	# has a minimum pixel threshold been established?
bool	upper_defined	# has a maximum pixel threshold been established?
real	xx		

define	GOOD_PIXEL	0

begin
	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = 0
	    do i = 1, npts {
		if (msk[i] == GOOD_PIXEL) {
		    gpix = gpix + 1
		    work[gpix] = x[i]
		}
	    }

	} else {	
	    lower_defined = !IS_INDEFR(lower)
	    upper_defined = !IS_INDEFR(upper)
	    gpix = 0
	    do i = 1, npts {
		xx = x[i]
		if (lower_defined && xx < lower)
		    next
		if (upper_defined && xx > upper)
		    next
		if (msk[i] == GOOD_PIXEL) {
		    gpix = gpix + 1
		    work[gpix] = x[i]
		}
	    }
	}

end
 		
#-------------------------------------------------------------------------------
# GST_IHIST --	Procedure to initilaize the histogram of the image pixels.
#		Returns a status of successful (YES) or not (NO).

int procedure gst_ihist (gst)

pointer	gst		# pointer to the statistics structure

# Local variables
int 	npts		# number of non-rejected pixels in image

begin
	GS_HGM(gst) = NULL
	GS_HMIN(gst) = GS_MIN(gst)
	GS_HMAX(gst) = GS_MAX(gst)
	npts = GS_NPIX(gst)
	GS_NBINS(gst) = min (npts/4, GS_HLIM)
	GS_NBINS(gst) = max (GS_NBINS(gst), 2)
	GS_HWIDTH(gst) = (GS_HMAX(gst) - GS_HMIN(gst)) / 
	    real (GS_NBINS(gst) - 1)
	if (GS_NBINS(gst) < 3 || GS_HMAX(gst) <= GS_HMIN(gst) ||
	    GS_HWIDTH(gst) <= 0.0) {
	    GS_MIDPT(gst) = INDEFR
	    GS_MODE(gst) = INDEFR
	    return (NO)
	}
	call realloc (GS_HGM(gst), GS_NBINS(gst), TY_INT)
	call aclri (Memi[GS_HGM(gst)], GS_NBINS(gst))
	return (YES)
end


#-------------------------------------------------------------------------------
# HMEDIAN -- Procedure to compute the median of the values.
#
#  Revision history:
#	 1-Dec-95 by RAShaw	Introduced a macro to dereference array ptrs; 
#				deallocated memory upon premature return.

procedure hmedian (gst)

# Calling arguments
pointer	gst		# pointer to the statistics strucuture

# Local variables
int	i, lo, hi
pointer	ihgm
real	h1, hdiff, hnorm
bool	fp_equalr()

# Memory management:
define	Ihgm	Memr[ihgm+$1-1]

define	MIN_NBINS	3	# min bins from which midpt estimation will work

begin

	# If all pixels are zero, don't do anything

	if (GS_NBINS(gst) < MIN_NBINS || GS_HMAX(gst) < GS_HMIN(gst) ||
	    GS_HWIDTH(gst) <= 0.0) {
	    GS_MIDPT(gst) = INDEFR
	    return
	}

	call malloc (ihgm, GS_NBINS(gst), TY_REAL)

	# Integrate the histogram and normalize.
	Ihgm(1) = Memi[GS_HGM(gst)]
	do i = 2, GS_NBINS(gst)
	    Ihgm(i) = Memi[GS_HGM(gst)+i-1] + Ihgm(i-1)
	hnorm = Ihgm(GS_NBINS(gst))

	# hnorm should not be zero
	if (fp_equalr (hnorm, 0.0)) {
	    GS_MIDPT(gst) = INDEFR
	    call mfree (ihgm, TY_REAL)
	    return
	}
	call adivkr (Ihgm(1), hnorm, Ihgm(1), GS_NBINS(gst))

	# Initialize the low and high bin numbers.
	lo = 0
	hi = 1

	# Search for the point which divides the integral in half.
	do i = 1, GS_NBINS(gst) {
	    if (Ihgm(i) > 0.5)
		break
	    lo = i
	}
	hi = lo + 1

	# Approximate the histogram.
	h1 = GS_HMIN(gst) + lo * GS_HWIDTH(gst)
	if (lo == 0)
	    hdiff = Ihgm(hi)
	else
	    hdiff = Ihgm(hi) - Ihgm(lo)

	if (fp_equalr (hdiff, 0.0))
	    GS_MIDPT(gst) = h1
	else if (lo == 0)
	    GS_MIDPT(gst) = h1 + 0.5 / hdiff * GS_HWIDTH(gst) 
	else
	    GS_MIDPT(gst) = h1 + (0.5 - Ihgm(lo)) / hdiff * GS_HWIDTH(gst)

	call mfree (ihgm, TY_REAL)
end

#-------------------------------------------------------------------------------
# HMODE --  Procedure to refine the mode by re-binning
#
#  Revision history:
#	 9-Nov-95 by RAShaw	Revised the loop control structure at the 
#				end from a GOTO to a repeat...until block. 
#				Also added a termination condition so that 
#				the iteration count does not exceed the 
#				size of the "mmode" array.  
#	15-Dec-95 by RAShaw	Protected against iter_count being out of 
#				range in last accululation loop; removed 
#				unnecessary initialization of hgm array; 
#				introduced macros to dereference array ptrs.

procedure hmode (gst)

# Calling arguments
pointer	gst			# Pointer to statistics structure

# Local variables
bool	done			# termination flag
pointer hgm			# Pointer to work array of rebinned histogram
int	min_nbins		# min acceptable number of bins
real	mmode[GS_MITER]		# Array holding modes
int	i, j			# Loop indexes
int	iter_count		# iteration count
real	rhmin			# Minimum histogram value	
int	rbins			# Number of bins in rebinned histogram
real	rhwidth 		# Rebinned resolution
real	sum			# generic accumulator
int	temp1, temp2		#

# Memory management:
define	Hgm	Memi[hgm+$1-1]

define	MIN_NBINS	3	# min bins from which midpt estimation will work

begin

	# If all pixels are zero, abort the calculation.
	# NB: this is not necessarily the correct thing to do -- RAShaw

	if (GS_NBINS(gst) < MIN_NBINS || GS_HMAX(gst) <= GS_HMIN(gst) ||
	    GS_HWIDTH(gst) <= 0.0)	{
	    GS_MODE(gst) = INDEFR
	    return
	}
	# Estimate mode with iterations

	rhmin   = GS_HMIN(gst)
	rbins   = GS_NBINS(gst)
	rhwidth = GS_HWIDTH(gst)
	call malloc (hgm, rbins, TY_INT)
	do i = 1, rbins
	    Hgm(i) = Memi[GS_HGM(gst)+i-1]

        call gst_hmode (Hgm(1), rhmin, rbins, rhwidth, mmode[1])
	iter_count = 2

	temp1     = 2 * nint ( real (GS_NPIX(gst)-1) ** (0.4) )
	temp2     =     nint ( real (GS_NPIX(gst)) / 10.0 )
	min_nbins = max (temp1, temp2)

	done = false
	repeat {
	    rbins = rbins / 2
	    rhwidth = rhwidth * 2
	    do i = 1, rbins {
	    	j = 2 * i - 1
	    	Hgm(i) =  Hgm(j) + Hgm(j+1)
	    }
	    call gst_hmode (Hgm(1), rhmin, rbins, rhwidth, mmode[iter_count])

	    if (rhwidth < 0.01 * GS_STDDEV(gst) || rbins > min_nbins ) {
	    	iter_count = iter_count + 1

	    } else 
		done = true
	
	} until (done || iter_count > GS_MITER)

	sum        = 0.0
	iter_count = min (iter_count, GS_MITER)
	do j = 1, iter_count
	    sum = sum + mmode[j]
	GS_MODE(gst) = sum / real(iter_count)

	call mfree (hgm, TY_INT)

end

#-------------------------------------------------------------------------------
# GST_HMODE -- Compute mode from histogram

procedure gst_hmode (x, rhmin, rbins, rhwidth, mode)

#  Calling arguments:
int	x[ARB]		# histogram of the pixels
real	rhmin		# minimum histogram value
int	rbins		# number of bins in the histogram
real	rhwidth		# resolution of the histogram
real	mode		# mode of the pixel distribution

#  Local variables:
int	i		# Loop index
int	bpeak		# Bin index containing peak
real	hpeak		# Histogram peak value
real	dh1, dh2, denom

#  Function used:
bool	fp_equalr()	# Test for floating-point equality

begin

# Find the three points surrounding the histogram max.
	hpeak = x[1]
	bpeak = 1
	do i = 2, rbins {
	    if (x[i] > hpeak) {
		hpeak = x[i]
		bpeak = i
	    }
	}
	bpeak = max (1, bpeak-1)


	# If the maximum is in the first bin return the midpoint of the bin.
	if (bpeak == 1) {
	    mode = rhmin + 0.5 * rhwidth
	    return
	}

	# If the maximum is in the last bin return the midpoint of the bin.
	if (bpeak == rbins) {
	    mode = rhmin + (rbins - 0.5) * rhwidth
	    return
	}

# Do a parabolic interpolation to find the peak.
	dh1 = x[bpeak+1] - x[bpeak]
	dh2 = x[bpeak+1] - x[bpeak+2]
	denom = dh1 + dh2
	if (fp_equalr (denom, 0.0)) {
	    mode = rhmin + (real(bpeak) + 0.5) * rhwidth
	} else {
	    mode = bpeak + 1.0 + 0.5 * (dh2 - dh1) / denom
	    mode = rhmin + 0.5 * rhwidth + (mode - 1.0) * rhwidth
	}

end


