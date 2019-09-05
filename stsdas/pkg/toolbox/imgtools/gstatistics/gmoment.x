#Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
include <mach.h>
include <imhdr.h>
include "gstat.h"

define	GOODPIXEL	0

#---------------------------------------------------------------------4 Dec 95--
.help gmoment.x Dec95 imgtools$gstatistics
.ih
NAME
      MNTACCUM - Accumulate the central moments
GST_INITIALIZE - Initialize the sum arrays to zero.
    GST_ACCUM4 - Accum. sums up to 4th power for data between lower & upper
 GST_MSKACCUM4 - Accum. sums up to 4th power for good data between lower & upper
    GST_ACCUM3 - Accum. sums up to 3rd power for data between lower & upper
 GST_MSKACCUM3 - Accum. sums up to 3rd power for good data between lower & upper
    GST_ACCUM2 - Accum. sums up to 2nd power for data between lower & upper
 GST_MSKACCUM2 - Accum. sums up to 2nd power for good data between lower & upper
    GST_ACCUM1 - Accum. sums up to 1st power for data between lower & upper
 GST_MSKACCUM1 - Accum. sums up to 1st power for good data between lower & upper
    GST_ACCUM0 - Accum. sums up to 0th power for data between lower & upper
 GST_MSKACCUM0 - Accum. sums up to 0th power for good data between lower & upper
      DO_STATS - Compute the first four central moments of the distribution
.endhelp
#-------------------------------------------------------------------------------
#  MNTACCUM -- Accumulate the central moments.
#
#  Revision History:
#  	   Aug 93 C.Y. Zhang	Initial implementation
#	 4 Dec 95 RAShaw	Use macros to dereference arrays.

procedure mntaccum (gst, im, msk, msktype, lower, upper, gsw)

# Calling arguments
pointer	gst			# Pointer to stat structure
pointer	im			# Pointer to input image descriptor
pointer	msk			# Pointer to DQF image descriptor
int	msktype			# Pixel lists or image type mask
real	lower, upper		# Floor & ceiling of data
pointer	gsw			# Pointer to switch structure

# Local variables
pointer buf			# Data from one line of the image
int	i			# Loop index
pointer mskbuf			# Data from one line of mask image
int	mval			# Mask value for the current segment
int	ndim			# Dimensionalty of image
int	npts			# Number of points in one line
long	one_l			# A constant of long 1
long	v1[IM_MAXDIM]		# Index of next line in image
long	v2[IM_MAXDIM]		# Index of next line in mask
long	vs[IM_MAXDIM]		# Starting of the pixel list range
long	ve[IM_MAXDIM]		# Ending of the pixel list range
long	v[IM_MAXDIM]		# Array holding the segment positions

# Functions called
int	imgnlr()		# Fetch next line independent of dim
int	imgnli()		# Type int for mask images
int	mio_glsegr()		# Get segments through mask

# Memory management
define	Buf		Memr[buf]
define	Maskbuf		Memi[mskbuf]

begin
	one_l = 1
	if (msktype == MSK_IMG && msk != NULL) {

	    # Initialize position vectors to line 1, column 1, band 1, .......
	    call amovkl (one_l, v1, IM_MAXDIM)
	    call amovkl (one_l, v2, IM_MAXDIM)
	    npts = int (IM_LEN (im, 1))
	    if (SW_KURT(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF && 
		    imgnli (msk, mskbuf, v2) != EOF) # mask pixtype int
	 	    call gst_mskaccum4 (gst, Maskbuf, Buf, 
		        npts, lower, upper, gsw)

	    } else if (SW_SKEW(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF && 
		    imgnli (msk, mskbuf, v2) != EOF) # mask pixtype int
	 	    call gst_mskaccum3 (gst, Maskbuf, Buf, 
		        npts, lower, upper, gsw)

	    } else if (SW_STDDEV(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF && 
		    imgnli (msk, mskbuf, v2) != EOF) # mask pixtype int
	 	    call gst_mskaccum2 (gst, Maskbuf, Buf, 
		        npts, lower, upper, gsw)

	    } else if (SW_MEAN(gsw) == YES || SW_SUM(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF && 
		    imgnli (msk, mskbuf, v2) != EOF) # mask pixtype int
	 	    call gst_mskaccum1 (gst, Maskbuf, Buf, 
		        npts, lower, upper, gsw)

	    } else if (SW_NPIX(gsw) == YES || SW_MNMX(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF && 
		    imgnli (msk, mskbuf, v2) != EOF) # mask pixtype int
	 	    call gst_mskaccum0 (gst, Maskbuf, Buf, 
		        npts, lower, upper, gsw)
	    }

	# Use MIO system call to get only those line segments of image visible 
	# through the mask ("good" pixels only); note in "mio_glsegr", "r"
	# refers to real pixel values of the image.
 
	} else if (msktype == MSK_LST && msk != NULL) {

	    # Initialize beginning of range vector to line 1, column 1, 
	    # band 1, ..., and the end of the range vector to lines IM_LEN.
	    ndim = IM_NDIM(im)
	    call amovkl (one_l, vs, ndim)
	    do i = 1, ndim
		ve[i] = IM_LEN(im,i)
	    call mio_setrange (msk, vs, ve, ndim)

	    if (SW_KURT(gsw) == YES) {

		# npts from mio_glsegT is number of non-rejected pixels
		while (mio_glsegr (msk, buf, mval, v, npts) != EOF) 
	 	    call gst_accum4 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_SKEW(gsw) == YES) {
		while (mio_glsegr (msk, buf, mval, v, npts) != EOF) 
	 	    call gst_accum3 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_STDDEV(gsw) == YES) {
		while (mio_glsegr (msk, buf, mval, v, npts) != EOF) 
	 	    call gst_accum2 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_MEAN(gsw) == YES || SW_SUM(gsw) == YES) {
		while (mio_glsegr (msk, buf, mval, v, npts) != EOF) 
	 	    call gst_accum1 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_NPIX(gsw) == YES || SW_MNMX(gsw) == YES) {
		while (mio_glsegr (msk, buf, mval, v, npts) != EOF) 
	 	    call gst_accum0 (gst, Buf, npts, lower, upper, gsw)
	    }

	# No masking operation needed
	} else if (msktype == MSK_NONE || msk == NULL) {

	    call amovkl (one_l, v1, IM_MAXDIM)
	    npts = int (IM_LEN (im, 1))
	    if (SW_KURT(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF)
	 	    call gst_accum4 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_SKEW(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF)
	 	    call gst_accum3 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_STDDEV(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF)
	 	    call gst_accum2 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_MEAN(gsw) == YES || SW_SUM(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF)
	 	    call gst_accum1 (gst, Buf, npts, lower, upper, gsw)

	    } else if (SW_NPIX(gsw) == YES || SW_MNMX(gsw) == YES) {
		while (imgnlr (im, buf, v1) != EOF)
	 	    call gst_accum0 (gst, Buf, npts, lower, upper, gsw)
	    }
	}
end

#-------------------------------------------------------------------------------
# GST_INITIALIZE -- Initialize the sum/accumulator arrays.

procedure gst_initialize (gst, lower, upper)

pointer	gst		# pointer to the statistics structure
real	lower		# lower limit of data for calculation
real	upper		# upper limit of data for calculation

begin
	if (IS_INDEFR(lower))
	    GS_LO(gst) = -MAX_REAL
	else
	    GS_LO(gst) = lower
	if (IS_INDEFR(upper))
	    GS_HI(gst) = MAX_REAL
	else
	    GS_HI(gst) = upper

	GS_NPIX(gst)   = 0
	GS_SUMX(gst)   = 0.0d0
	GS_SUMX2(gst)  = 0.0d0
	GS_SUMX3(gst)  = 0.0d0
	GS_SUMX4(gst)  = 0.0d0

	GS_MIN(gst)    = MAX_REAL
	GS_MAX(gst)    = -MAX_REAL
	GS_MEAN(gst)   = INDEFR
	GS_MIDPT(gst)  = INDEFR
	GS_MODE(gst)   = INDEFR
	GS_STDDEV(gst) = INDEFR
	GS_SKEW(gst)   = INDEFR
	GS_KURT(gst)   = INDEFR
end


#-------------------------------------------------------------------------------
# GST_ACCUM4 -- Accumulate sums up to the fourth power of the data for
# 		data values between lower and upper.

procedure gst_accum4 (gst, x, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix
real	lo, hi, xmin, xmax
double	xx, xx2, sumx, sumx2, sumx3, sumx4

begin
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	npix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	sumx4 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	}

	GS_NPIX(gst) = npix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_SUMX3(gst) = GS_SUMX3(gst) + sumx3
	GS_SUMX4(gst) = GS_SUMX4(gst) + sumx4
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
end

#-------------------------------------------------------------------------------
# GST_MSKACCUM4 -- Accumulate sums up to the fourth power of the "good"
# pixels for data values between lower and upper.

procedure gst_mskaccum4 (gst, msk, work, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	work[ARB]	# the data array
int	msk[ARB]	# the mask array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix, gpix
pointer	xpt		# pointer to non-rejected pixels
real	lo, hi, xmin, xmax
double	xx, xx2, sumx, sumx2, sumx3, sumx4

# Memory management
define	Good_data	Memr[xpt+$1-1]

begin
	call malloc (xpt, npts, TY_REAL)
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	gpix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	sumx4 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)
	
	npix = 0
	do i = 1, npts {
	    if (msk[i] == GOODPIXEL) {
	 	npix = npix + 1
		Memr[xpt+npix-1] = work[i]
	    }
	}

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = gpix + npix
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npts {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	}

	GS_NPIX(gst) = gpix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_SUMX3(gst) = GS_SUMX3(gst) + sumx3
	GS_SUMX4(gst) = GS_SUMX4(gst) + sumx4
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax

	call mfree (xpt, TY_REAL)
end


#-------------------------------------------------------------------------------
# GST_ACCUM3 -- Accumulate sums up to the third power of the data for
# data values between lower and upper.

procedure gst_accum3 (gst, x, npts, lower, upper, gsw)

pointer	gst		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix
real	lo, hi, xmin, xmax
double 	xx, xx2, sumx, sumx2, sumx3

begin
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	npix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	}

	GS_NPIX(gst) = npix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_SUMX3(gst) = GS_SUMX3(gst) + sumx3
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
end

#-------------------------------------------------------------------------------
# GST_MSKACCUM3 -- Accumulate sums up to the third power of the 
# good pixels for data values between lower and upper.

procedure gst_mskaccum3 (gst, msk, work, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	work[ARB]	# the data array
int	msk[ARB]	# the mask array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix, gpix
pointer	xpt		# pointer to non-rejected pixels
real	lo, hi, xmin, xmax
double	xx, xx2, sumx, sumx2, sumx3

begin
	call malloc (xpt, npts, TY_REAL)
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	gpix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	npix = 0
	do i = 1, npts {
	    if (msk[i] == GOODPIXEL) {
	 	npix = npix + 1
		Memr[xpt+npix-1] = work[i]
	    }
	}

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = gpix + npix
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npts {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	}

	GS_NPIX(gst) = gpix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_SUMX3(gst) = GS_SUMX3(gst) + sumx3
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax

	call mfree (xpt, TY_REAL)	
end


#-------------------------------------------------------------------------------
# GST_ACCUM2 -- Accumulate sums up to the second power of the data for
# data values between lower and upper.

procedure gst_accum2 (gst, x, npts, lower, upper, gsw)

pointer	gst		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix
real	lo, hi, xmin, xmax
double	xx, sumx, sumx2

begin
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	npix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    }
	}

	GS_NPIX(gst) = npix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
end


#-------------------------------------------------------------------------------
# GST_MSKACCUM2 -- Accumulate sums up to the second power of the 
# good pixels for data values between lower and upper.

procedure gst_mskaccum2 (gst, msk, work, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	work[ARB]	# the data array
int	msk[ARB]	# the mask array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix, gpix
pointer	xpt		# pointer to non-rejected pixels
real	lo, hi, xmin, xmax
double	xx, xx2, sumx, sumx2

begin
	call malloc (xpt, npts, TY_REAL)
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	gpix = GS_NPIX(gst)
	sumx = 0.0
	sumx2 = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	npix = 0
	do i = 1, npts {
	    if (msk[i] == GOODPIXEL) {
	 	npix = npix + 1
		Memr[xpt+npix-1] = work[i]
	    }
	}

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = gpix + npix
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		}
	    } else {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		}
	    } else {
		do i = 1, npts {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    gpix = gpix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		}
	    }
	}

	GS_NPIX(gst) = gpix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_SUMX2(gst) = GS_SUMX2(gst) + sumx2
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
	call mfree (xpt, TY_REAL)
end

#-------------------------------------------------------------------------------
# GST_ACCUM1 -- Accumulate sums up to the first power of the data for
# data values between lower and upper.

procedure gst_accum1 (gst, x, npts, lower, upper, gsw)

pointer	gst		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# poiner to switch structure

int	i, npix
real	lo, hi, xx, xmin, xmax
double	sumx

begin
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	npix = GS_NPIX(gst)
	sumx = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    } else {
		do i = 1, npts
		    sumx = sumx + x[i]
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
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
		    npix = npix + 1
		    sumx = sumx + xx
		}
	    }
	}

	GS_NPIX(gst) = npix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
end

#-------------------------------------------------------------------------------
# GST_MSKACCUM1 -- Accumulate sums up to the first power of the 
# 		good pixels for data values between lower and upper.

procedure gst_mskaccum1 (gst, msk, work, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	work[ARB]	# the data array
int	msk[ARB]	# the mask array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix, gpix
pointer	xpt		# pointer to non-rejected pixels
real	lo, hi, xmin, xmax
double	xx, sumx

begin
	call malloc (xpt, npts, TY_REAL)
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	gpix = GS_NPIX(gst)
	sumx = 0.0
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	npix = 0
	do i = 1, npts {
	    if (msk[i] == GOODPIXEL) {
	 	npix = npix + 1
		Memr[xpt+npix-1] = work[i]
	    }
	}

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = gpix + npix
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    } else {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    sumx = sumx + xx
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    gpix = gpix + 1
		    sumx = sumx + xx
		}
	    } else {
		do i = 1, npts {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    gpix = gpix + 1
		    sumx = sumx + xx
		}
	    }
	}

	GS_NPIX(gst) = gpix
	GS_SUMX(gst) = GS_SUMX(gst) + sumx
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
	call mfree (xpt, TY_REAL)
end

#-------------------------------------------------------------------------------
# GST_ACCUM0 -- Accumulate sums up to the 0th power of the data for
# data values between lower and upper.

procedure gst_accum0 (gst, x, npts, lower, upper, gsw)

pointer	gst		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix
real	lo, hi, xx, xmin, xmax

begin
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	npix = GS_NPIX(gst)
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (SW_MNMX(gsw) == YES) {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
		        xmin = xx
		    if (xx > xmax)
		        xmax = xx
	        }
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
		        next
		    npix = npix + 1
		    if (xx < xmin)
		        xmin = xx
		    if (xx > xmax)
		        xmax = xx
	        }
	    } else {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
		        next
		    npix = npix + 1
		}
	    }
	}

	GS_NPIX(gst) = npix
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
end


#-------------------------------------------------------------------------------
# GST_MSKACCUM0 -- Accumulate sums up to the zero'th power of the 
# good pixels for data values between lower and upper.

procedure gst_mskaccum0 (gst, msk, work, npts, lower, upper, gsw)

pointer	gst		# pointer to the statgstics structure
real	work[ARB]	# the data array
int	msk[ARB]	# the mask array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
pointer	gsw		# pointer to switch structure

int	i, npix, gpix
pointer	xpt		# pointer to non-rejected pixels
real	lo, hi, xmin, xmax, xx

begin
	call malloc (xpt, npts, TY_REAL)
	lo = GS_LO(gst)
	hi = GS_HI(gst)
	gpix = GS_NPIX(gst)
	xmin = GS_MIN(gst)
	xmax = GS_MAX(gst)

	npix = 0
	do i = 1, npts {
	    if (msk[i] == GOODPIXEL) {
	 	npix = npix + 1
		Memr[xpt+npix-1] = work[i]
	    }
	}

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    gpix = gpix + npix
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		}
	    } else {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		}
	    }
	} else {
	    if (SW_MNMX(gsw) == YES) {
		do i = 1, npix {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    gpix = gpix + 1
		}
	    } else {
		do i = 1, npts {
		    xx = Memr[xpt+i-1]
		    if (xx < lo || xx > hi)
			next
		    gpix = gpix + 1
		}
	    }
	}

	GS_NPIX(gst) = gpix
	GS_MIN(gst) = xmin
	GS_MAX(gst) = xmax
	call mfree (xpt, TY_REAL)
end

#-------------------------------------------------------------------------------
# DO_STATS -- Procedure to compute the first four central moments of the
# distribution.

procedure do_stats (gst, gsw)

pointer	gst			# statistics structure
pointer	gsw			# pointer to switch structure

double	mean, var, stdev
bool	fp_equalr()

begin
	if (fp_equalr (GS_MIN(gst), MAX_REAL))
	    GS_MIN(gst) = INDEFR
	if (fp_equalr (GS_MAX(gst), -MAX_REAL))
	    GS_MAX(gst) = INDEFR

	if (GS_NPIX(gst) <= 0)
	    return
	mean = GS_SUMX(gst) / GS_NPIX(gst)
	GS_MEAN(gst) = mean

	if (GS_NPIX(gst) < 2)
	    return
	var = (GS_SUMX2(gst) - GS_SUMX(gst) * mean) /
	    (GS_NPIX(gst) - 1)
	if (var <= 0.0) {
	    GS_STDDEV(gst) = 0.0
	    return
	} else {
	    stdev = sqrt (var)
	    GS_STDDEV(gst) = stdev
	}

	if (SW_SKEW(gsw) == YES)
	    GS_SKEW(gst) = (GS_SUMX3(gst) - 3.0d0 * mean *
	        GS_SUMX2(gst) + 3.0d0 * mean * mean *
		GS_SUMX(gst) - GS_NPIX(gst) * mean ** 3) /
		GS_NPIX(gst) / stdev / stdev / stdev
	    
	if (SW_KURT(gsw) == YES)
	    GS_KURT(gst) = (GS_SUMX4(gst) - 4.0d0 * mean *
	        GS_SUMX3(gst) + 6.0d0 * mean * mean *
	        GS_SUMX2(gst) - 4.0d0 * mean ** 3 * GS_SUMX(gst) +
	        GS_NPIX(gst) * mean ** 4) / GS_NPIX(gst) /
	        stdev / stdev / stdev / stdev - 3.0d0
end

