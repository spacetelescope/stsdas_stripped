#				File:	u_sec_stat.x

include <imhdr.h>    	# IM_LEN
include <mach.h>	# MAX_REAL
include "u_incl.h"
include "u_data.h"

#################################################################################
#                                                                               #
#  U_SEC_STAT --   Compute statistics related to various sections               #	
#		   (see OPR 25162)                                              #
#  Last modified:                                                               #
#  	29 Aug 1993  by CYZhang		Initial implementation                  #
#  	29 Sep 1993  by CYZhang		Change the warning messages for		#
#					constant image section                  #
#	17 Nov 1993 by CYZhang		Test the case when there is no		#
#					good pixel at all			#
#	18 Jan 1994 by CYZhang		Read entire chip instead of sections	#

procedure u_sec_stat (img, dqf, stat, element, full)

#  Calling arguments
pointer	img, dqf 		# Pointers to image and DQF descriptors
pointer	stat			# Pointers to section statistics
int	element			# Camera number
bool	full			# FULL mode?

#  Local variables
#pointer	sp
pointer	imgs, dqfs		# Pointers to sections
int	npts, nrows
#pointer	imgarr, dqfarr
pointer	rbuf, sbuf
char	text[SZ_LINE]
int	x1, x2, y1, y2		# Margins of pyramid shadow area
int	xc			# Center pixel position 
int	sec_x1, sec_x2		# Starting and ending section
pointer work		   	# Pointers to work array
int	npix, gpix		# Number of pixels

pointer	imgs2s(), imgs2r()

begin

	npts  = IM_LEN(img, 1)
	nrows = IM_LEN(img, 2)
	npix = npts * nrows

	rbuf = imgs2r (img, 1, npts, 1, nrows)
	sbuf = imgs2s (dqf, 1, npts, 1, nrows)

#  Image center position in FULL mode
	xc = 400
	
#  AREA mode
	if ( !full )
	    xc = xc / 2

#  No cealing and floor needed for section means
	S_LO(stat)   = INDEFR
	S_HI(stat)   = INDEFR

#  Get section for MEANC10
	sec_x1 = xc - 4
	sec_x2 = xc + 5
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)
#  Initialize statistics
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the first center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC10(stat) = -MAX_REAL
	} else {
	    S_MEANC10(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Get section for MEANC25
	sec_x1 = xc - 11
	sec_x2 = xc + 13
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)

#  Initialize statistics
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the 2nd center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC25(stat) = -MAX_REAL
	} else {
	    S_MEANC25(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Get section for MEANC50
	sec_x1 = xc - 24
	sec_x2 = xc + 25
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)

#  Initialize statistics
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the 3rd center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC50(stat) = -MAX_REAL
	} else {
	    S_MEANC50(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Get section for MEANC100
	sec_x1 = xc - 49
	sec_x2 = xc + 50
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)

#  Initialize statistics
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the 4th center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC100(stat) = -MAX_REAL
	} else {
	    S_MEANC100(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Get section for MEANC200
	sec_x1 = xc - 99
	sec_x2 = xc + 100
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)

#  Initialize statistics
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the 5th center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC200(stat) = -MAX_REAL
	} else {
	    S_MEANC200(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Get section for MEANC300
	sec_x1 = xc - 149
	sec_x2 = xc + 150
	call u_getsect (Memr[rbuf], Mems[sbuf], npts, nrows,
			sec_x1, sec_x2, npix, work, imgs, dqfs)

#  Initialize sum and mean
	S_SUMX(stat) = 0.0D0
	S_AVG(stat)  = 0.0

#  Do statistics on the 6th center section
	call u_smean (Memr[imgs], Mems[dqfs], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_MEANC300(stat) = -MAX_REAL
	} else {
	    S_MEANC300(stat) = S_AVG(stat)
	}
	call mfree (work, TY_REAL)
	call mfree (imgs, TY_REAL)
	call mfree (dqfs, TY_SHORT)

#  Initialize other statistics quantities
	S_SUMX(stat)     = 0.0D0
	S_AVG(stat)      = 0.0
	S_GMEDIAN(stat)  = 0.0
	S_BCKGRD(stat)   = 0.0
	S_SKEW(stat)     = 0.0
	S_HISTWIDE(stat) = 0.0
	S_NBINS(stat)    = 32768

#  AREA mode
	if ( !full ) 
	    S_NBINS(stat) = S_NBINS(stat) / 2

	#  Get good pixels for the entire chip
	#  There is no need to call u_getsec to get the entire chip
	npix = npts * nrows
	call malloc (work, npix, TY_REAL)
	call u_smean (Memr[rbuf], Mems[sbuf], npix, Memr[work], gpix, stat)
	# Guard against gpix = 0  -- CYZhang 17/11/93
	if (gpix <= 0) {
	    S_GMEDIAN(stat)  = -MAX_REAL
	    S_BCKGRD(stat)   = -MAX_REAL
	    S_SKEW(stat)     = -MAX_REAL
	    S_HISTWIDE(stat) = -MAX_REAL
	} else {

#  Initialize histogram of non-rejected pixels
	    call u_ghist (Memr[work], gpix, stat)

#  Compute median of non-rejected pixels, DNs at 10%, 25%, 75%, and HISTWIDE
	    call u_gmedian (stat)

#  Compute BACKGRND and SKEWNESS
	    call u_modskw (stat)
	    call mfree (S_HGM(stat), TY_INT)
	}
	call mfree (work, TY_REAL)

#  The pyramid shadow areas start from
	x1 = 1
	y1 = 1

#  Margin ends of the pyramid shadow areas in FULL mode
	switch ( element ) {
	    case 1:
	        x2 = -4; y2 = 5
	    case 2:
	        x2 = 27; y2 = 3
	    case 3:
	        x2 = 12; y2 = 25
	    case 4:
	        x2 = 22; y2 = 23
	    default:
	        call sprintf (text, SZ_LINE, "Chip No. %d illegal!")
	        call pargi (element)
	        call u_error (text)
	}
	
#  AREA mode

	if ( !full ) {
	    if ( x2 <= 0 && y2 > 0) {
		x2 = x2 / 2 - 1
		y2 = y2 / 2 + 1
	    } else if ( x2 > 0 && y2 <= 0) {
		x2 = x2 / 2 + 1
		y2 = y2 / 2 - 1
	    } else if ( x2 <= 0 && y2 <= 0) {
		x2 = x2 / 2 - 1
		y2 = y2 / 2 - 1
	    } else {
		x2 = x2 / 2 + 1
		y2 = y2 / 2 + 1
	    }
	}

#  Get the pyramid shadow area

	call u_get2sect (Memr[rbuf], npts, nrows, x1, x2, y1, y2, npix, work)

#  If Shad_ends in both X and Y directions are less than or equal to zero, 
#  set S_SMEDIAN = 0.0
	if (npix <= 0)  {
	    S_SMEDIAN(stat)  = 0.0
	} else {
#  Initialize statistical quantities

	    S_SMEDIAN(stat)  = 0.0
	    S_NBINS(stat)    = 4096

#  Compute the median of the pixels in the pyramid shadow area
	    call u_shad_med (Memr[work], npix, stat)
	    call mfree (S_HGM(stat), TY_INT)
#  Free memory
	}
	call mfree (work, TY_REAL)

end

#################################################################################
#                                                                               #
#  U_GETSECT --   Get the squared image section and allocate memory for       	#
#                 a work array                                                  #
#                                                                               #
#  Last revised:                                                                #
#	24 Aug 93  by CYZhang	Initial implementation                          #

procedure u_getsect (imgarr, dqfarr, npts, nrows,
		     x1, x2, npix, work, imgs, dqfs)

#  Calling arguments
int	npts, nrows		# Size of the arrays
real	imgarr[npts,nrows]	# the image array
short	dqfarr[npts,nrows]	# the DQF array
pointer	work			# Pointer to work array
pointer	imgs			# Pointer to image section
pointer	dqfs			# Pointer to DQF section
int	x1, x2			# Section bounds
int	npix			# Number of pixels
pointer	u_imgtsec(), u_dqftsec()      


begin

	# Get size of specified section and allocate memory to work array
	# Work array will end up holding only good pixels
	npix = (x2 - x1 + 1) * (x2 - x1 + 1)
	call malloc (work, npix, TY_REAL)

#  Get the sections needed
	imgs = u_imgtsec (imgarr, npts, nrows, x1, x2, x1, x2)
	dqfs = u_dqftsec (dqfarr, npts, nrows, x1, x2, x1, x2)

end


#################################################################################
#                                                                               #
#  U_SMEAN --   Compute mean for good pixels of an image section.               #
#		The non-rejected pixels are placed in a work array              #
#                                                                               #
#  Last revised:                                                                #
#	24 Aug 93  by CYZhang	Initial implementation                          #

procedure u_smean (imgs, dqfs, npix, work, gpix, stat)

#  Calling arguments
real	imgs[ARB]		# Array of image section
short	dqfs[ARB]		# Array of DQF section
int	npix			# Number of pixels in the section
real	work[ARB]		# Work array holding non-rejected pixels
int	gpix			# Number of non-rejected pixels
pointer	stat			# Pointer to stat structure

#  Local variables
int	i 			# Loop variable

begin

#  Accumulate good pixels and get mean out of it
	gpix = 0
	S_SUMX(stat) = 0.0D0
	if (IS_INDEFR(S_LO(stat)) && IS_INDEFR(S_HI(stat))) {
#  First time called, no ceiling and floor will be set.
	    do i = 1, npix {
		if ( dqfs[i] == short(GOODPIXEL) ) {	
		    gpix = gpix + 1
		    work[gpix] = imgs[i]
		    S_SUMX(stat) = S_SUMX(stat) + imgs[i]
	        }
	    }
	} else {

#  To be used in case finite ceiling and floor could be set up

	    do i = 1, npix {
	        if ( imgs[i] <= S_HI(stat) && imgs[i] >= S_LO(stat) ) {
		    if ( dqfs[i] == short(GOODPIXEL) ) {	
		        gpix = gpix + 1
		        work[gpix] = imgs[i]
		        S_SUMX(stat) = S_SUMX(stat) + imgs[i]
		    }
	        }
	    }
	}
	# Guard against gpix = 0 -- CYZhang 16/11/93
	if (gpix <= 0) {
	    return
	} else {
	    S_AVG(stat) = S_SUMX(stat) / real(gpix)		    
	}

end

#################################################################################
#                                                                               #
#  U_GHIST --   Construct a histogram of good pixels of an image section.       #
#                                                                               #
#  Last revised:                                                                #
#	24 Aug 93  by CYZhang	Initial implementation                          #
#	 8 Dec 93  by CYZhang	Impose a minimum bin width 			#


procedure u_ghist (work, gpix, stat)

#  Calling arguments
pointer	stat			# Pointer to statistics structure
real	work[ARB]		# Work array of non-rejected pixels
int	gpix			# Number of good pixels

#  Local variables
real	gmin, gmax		# Min and Max of non-rejected pixels
int	i			# Loop index
real	xx			# Variable holding pixel value

errchk	ahgmr, aclri

begin

# Initialize the histogram	
	gmin = MAX_REAL
	gmax = -MAX_REAL
	if (gpix >= 1) {
	    do i = 1, gpix {
	        xx = work[i]
		if (xx < gmin)
		    gmin = xx
		if (xx > gmax)
		    gmax = xx
	    }
	}
	S_HMIN(stat) = gmin
	S_HMAX(stat) = gmax
#  S_NBINS(stat) is initially set at 32768 and 16384 for 
#  the entire chip in FULL and AREA modes, while at 4096 for the shadow area
	S_HWIDTH(stat) = (S_HMAX(stat) - S_HMIN(stat)) / real (S_NBINS(stat))
	# Adopt a minimum bin width, reset the S_NBINS
	S_HWIDTH(stat) = max (S_HWIDTH(stat), MIN_BW)
	S_NBINS(stat) = int((S_HMAX(stat) - S_HMIN(stat)) / S_HWIDTH(stat))+1
	S_HGM(stat) = NULL
	call malloc (S_HGM(stat), S_NBINS(stat), TY_INT)
	call aclri (Memi[S_HGM(stat)], S_NBINS(stat))

#  Construct the histogram.
	if (S_HWIDTH(stat) > 0.0 && S_HMAX(stat) > S_HMIN(stat))
	    call ahgmr (work, gpix, Memi[S_HGM(stat)],
		S_NBINS(stat), S_HMIN(stat), S_HMAX(stat))

end

#################################################################################
#                                                                               #
#  U_GMEDIAN --   Compute the median, the DN and the bin number at 10% points   #
#		of histogram, and the width from 25% to 75% points of histogram #
#		for non-rejected pixels                                         #
#                                                                               #
#  Last revised:                                                                #
#	24 Aug 93  by CYZhang	Initial implementation                          #

procedure u_gmedian (stat)

# Calling arguments
pointer	stat			# Pointer to the statistics structure

# Local variables
int	i, lo, hi
pointer	ihgm			# Array for accumulation
real	h1, hdiff, hnorm
bool	fp_equalr()

errchk	fp_equalr, adivkr

begin
	
#  If all pixels are zero, don't do anything

	if (S_HMAX(stat) <= S_HMIN(stat) || S_HWIDTH(stat) <= 0.0) {
	    S_GMEDIAN(stat) = S_HMAX(stat)
	    S_HISTWIDE(stat) = 0.0
	    call u_warn ("Constant image section involved!")
	    return
	}

	call malloc (ihgm, S_NBINS(stat), TY_REAL)

#  Integrate the histogram and normalize.
	Memr[ihgm] = real (Memi[S_HGM(stat)])
	do i = 2, S_NBINS(stat)
	    Memr[ihgm+i-1] = real (Memi[S_HGM(stat)+i-1]) + Memr[ihgm+i-2]
	hnorm = Memr[ihgm+S_NBINS(stat)-1]

#  hnorm should not be zero
	if (fp_equalr (hnorm, 0.0)) {
	    S_GMEDIAN(stat) = -MAX_REAL
	    S_HISTWIDE(stat) = -MAX_REAL
	    call u_warn ("Sum of histogram is zero!")
	    call mfree (ihgm, TY_REAL)
	    return
	}
	call adivkr (Memr[ihgm], hnorm, Memr[ihgm], S_NBINS(stat))

#  Initialize the low and high bin numbers
	lo = 0
	hi = 1

#  Search for the point dividing the integral in half
	do i = 1, S_NBINS(stat) {
	    if (Memr[ihgm+i-1] > 0.5)
		break
	    lo = i
	}
	hi = lo + 1

#  Approximate the histogram
	h1 = S_HMIN(stat) + lo * S_HWIDTH(stat)
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0))
	    S_GMEDIAN(stat) = h1
	else if (lo == 0)
	    S_GMEDIAN(stat) = h1 + 0.5 / hdiff * S_HWIDTH(stat)
	else 
	    S_GMEDIAN(stat) = h1 + (0.5 - Memr[ihgm+lo-1]) /
		 hdiff * S_HWIDTH(stat)

#  Search for pixel values at 10% of good pixels in histogram

	lo = 0
	hi = 1
	do i = 1, S_NBINS(stat) {
	    if (Memr[ihgm+i-1] > 0.1)
		break
	    lo = i
	}
	hi = lo + 1

#  Approximate the histogram
	h1 = S_HMIN(stat) + lo * S_HWIDTH(stat)
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0)) 
	    S_DN10(stat) = h1
	else if (lo == 0)
	    S_DN10(stat) = h1 + 0.1 / hdiff * S_HWIDTH(stat)
	else 
	    S_DN10(stat) = h1 + (0.1 - Memr[ihgm+lo-1]) /
			   hdiff * S_HWIDTH(stat)
	# S_NBIN10 should be set to hi rather than lo -- CYZhang 8/12/93
	S_NBIN10(stat) = hi

#  Search for pixel values at 25% of good pixels in histogram

	lo = 0
	hi = 1
	do i = 1, S_NBINS(stat) {
	    if (Memr[ihgm+i-1] > 0.25)
		break
	    lo = i
	}
	hi = lo + 1

#  Approximate the histogram
	h1 = S_HMIN(stat) + lo * S_HWIDTH(stat)
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0))
	    S_DN25(stat) = h1
	else if (lo == 0)
	    S_DN25(stat) = h1 + 0.25 / hdiff * S_HWIDTH(stat)
	else 
	    S_DN25(stat) = h1 + (0.25 - Memr[ihgm+lo-1]) /
		 hdiff * S_HWIDTH(stat)

#  Search for pixel values at 75% of good pixels in histogram

	lo = 0
	hi = 1
	do i = 1, S_NBINS(stat) {
	    if (Memr[ihgm+i-1] > 0.75)
		break
	    lo = i
	}
	hi = lo + 1

#  Approximate the histogram
	h1 = S_HMIN(stat) + lo * S_HWIDTH(stat)
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0))
	    S_DN75(stat) = h1
	else if (lo == 0)
	    S_DN75(stat) = h1 + 0.75 / hdiff * S_HWIDTH(stat)
	else 
	    S_DN75(stat) = h1 + (0.75 - Memr[ihgm+lo-1]) /
		 hdiff * S_HWIDTH(stat)

#  Find the HISTWIDE
	S_HISTWIDE(stat) = S_DN75(stat) - S_DN25(stat)	

#  Free memory
	call mfree (ihgm, TY_REAL)

end

#################################################################################
#                                                                               #
#  U_MODSKW --  Compute the SKEWNESS and BACKGRND from histogram                #
#                                                                               #
#  Last revised: 								#
#	24 Aug 93  by CYZhang	Initial implementation				#
#	 7 Oct 93  by CYZhang	Bug fixing					#
#	 8 Dec 93  by CYZhang	Fixing indeces in the loop of smoothing		#

procedure u_modskw (stat)

#  Calling arguments
pointer	stat			# Pointer to the statistics structure

#  Local variables
int	i			# Loop index
real	hpeak			# Histogram peak value
int	bpeak			# Bin index containing peak
real	dh1, dh2, demon
int	j, wbins
pointer	whgm			# Work array for boxcar smoothing
bool	fp_equalr()

errchk	fp_equalr

begin

#  Compute skewness
	if ( fp_equalr (S_GMEDIAN(stat), 0.0) || S_GMEDIAN(stat) == -MAX_REAL ) {
	    S_SKEW(stat) = -MAX_REAL
	    S_BCKGRD(stat) = S_GMEDIAN(stat)
	    call u_warn ("Median is zero!")
	    return
	} else if (S_HMAX(stat) <= S_HMIN(stat)) {
	    S_SKEW(stat) = 0.0
	    S_BCKGRD(stat) = S_GMEDIAN(stat)
	    call u_warn ("Constant image section involved!")
	    return
	} 

	S_SKEW(stat) =  ((S_DN25(stat) + S_DN75(stat)) / 2.0 - 
	        S_GMEDIAN(stat)) / S_GMEDIAN(stat)

#  Calculate mode
#  Refine the mode by skipping the lowest 10% points and running average
#  over the histogram
	
	wbins = S_NBINS(stat) - S_NBIN10(stat) + 11

	call malloc (whgm, wbins, TY_REAL)

#  Packing the two ends of the whgm with zeros
	do i = 1, 5
	    Memr[whgm+i-1] = 0.0
	do i = wbins-4, wbins
	    Memr[whgm+i-1] = 0.0
	do j = S_NBIN10(stat)-1, S_NBINS(stat)-1 {
	    i = j - S_NBIN10(stat) + 6
	    Memr[whgm+i] = real (Memi[S_HGM(stat)+j])
	}

#  Do a moving average over adjacent 10 points
	do j = S_NBIN10(stat)-1, S_NBINS(stat)-1 {
	    i = j - S_NBIN10(stat) + 6
	    # Index for S_HGM must not be mixed with that for whgm!
	    # 8/12/93 -- CYZhang
	    # Memi[S_HGM(stat)+i] = int ((Memr[whgm+i-5] +
	    Memi[S_HGM(stat)+j] = int ((Memr[whgm+i-5] +
		Memr[whgm+i-4] + Memr[whgm+i-3] + Memr[whgm+i-2] +
		Memr[whgm+i-1] + Memr[whgm+i] + Memr[whgm+i+1] +
		Memr[whgm+i+2] + Memr[whgm+i+3] + Memr[whgm+i+4] +
		Memr[whgm+i+5]) / 11.0)
	}

#  Free memory
	call mfree (whgm, TY_REAL)
		
#  Find three points around the maximum
#  
	hpeak = real (Memi[S_HGM(stat)+S_NBIN10(stat)-1])
	bpeak = S_NBIN10(stat)
	do i = S_NBIN10(stat)+1, S_NBINS(stat) {
	    if (real(Memi[S_HGM(stat)+i-1]) > hpeak) {
		hpeak = real(Memi[S_HGM(stat)+i-1])
		bpeak = i
	    }
	}
	bpeak = max (S_NBIN10(stat), bpeak-1)

#  If the max in the last bin return the midpoint of the bin
	if (bpeak == S_NBINS(stat)) {
	    S_BCKGRD(stat) = S_HMIN(stat) + (real(S_NBINS(stat)) - 0.5) * 
		S_HWIDTH(stat)
	    return
	}

#  Do a parabolic interpolation to find the peak
	dh1 = real (Memi[S_HGM(stat)+bpeak] - Memi[S_HGM(stat)+bpeak-1])
	dh2 = real (Memi[S_HGM(stat)+bpeak] - Memi[S_HGM(stat)+bpeak+1])
	demon = dh1 + dh2
	if (fp_equalr (demon, 0.0)) 
	    S_BCKGRD(stat) = S_HMIN(stat) + (real(bpeak) + 0.5) *
		S_HWIDTH(stat)
	else {
	    S_BCKGRD(stat) = real(bpeak) + 1.0 + 0.5 * (dh2 - dh1) / demon
	    S_BCKGRD(stat) = S_HMIN(stat) + 0.5 * S_HWIDTH(stat) +
		(S_BCKGRD(stat) - 1.0) * S_HWIDTH(stat)
	}

end


#################################################################################
#                                                                               #
#  U_GET2SECT --   Get the 2 specified image sections at the corner defined as  #
#		   the pyramid shadow area and allocate memory for a work array #
#                                                                               #
#  Last revised:                                                                #
#	29 Aug 93  by CYZhang	Initial implementation                          #

procedure u_get2sect (imgarr, npts, nrows, x1, x2, y1, y2, npix, shad)

#  Calling arguments
int	npts, nrows		# Size of the image array
real	imgarr[npts, nrows]	# Pointer to the image
pointer	shad			# Pointer to work array
int	x1, x2, y1, y2		# Section bounds
int	npix			# Number of pixels

#  Local variables
pointer	imgs			# Pointer to image section
int	npts1, npts2		# Number of pixels
int	i, j			# Loop index
pointer	u_imgtsec()


begin

#  x1, x2, y1, and y2 are specified margins (see OPR 25162)
#  Get size of specified section and allocate memory to work array
	if (x2 <= 0 && y2 > 0) {
	    npts1 = (y2 - y1 + 1) * (npts - x1 + 1)
	    npts2 = 0
	} else if (y2 <= 0 && x2 > 0) {
	    npts2 = (x2 - x1 + 1) * (nrows - y1 + 1)
	    npts1 = 0
	} else if (x2 <= 0 && y2 <= 0) {
	    npix = 0
	    return
	} else {
	    npts1 = (y2 - y1 + 1) * (npts - x1 + 1)
	    npts2 = (x2 - x1 + 1) * (nrows - y2 + 1)
	}
	npix  = npts1 + npts2

	call malloc (shad, npix, TY_REAL)

#  Get the sections needed
	if (x2 <= 0) {
	    imgs = u_imgtsec (imgarr, npts, nrows, x1, npts, y1, y2)
	    do i = 1, npts1
		Memr[shad+i-1] = Memr[imgs+i-1]
	    call mfree (imgs, TY_REAL)
	} else if (y2 <= 0) {
	    imgs = u_imgtsec (imgarr, npts, nrows, x1, x2, y1, nrows)
	    do i = 1, npts2
		Memr[shad+i-1] = Memr[imgs+i-1]
	    call mfree (imgs, TY_REAL)
	} else {
	    imgs = u_imgtsec (imgarr, npts, nrows, x1, npts, y1, y2)
	    do i = 1, npts1
	        Memr[shad+i-1] = Memr[imgs+i-1]
	    call mfree (imgs, TY_REAL)
	    imgs = u_imgtsec (imgarr, npts, nrows, x1, x2, y2, nrows)
	    do i = 1, npts2 {
		j = i + npts1
	        Memr[shad+j-1] = Memr[imgs+i-1]
	    }
	    call mfree (imgs, TY_REAL)
	}
	
end

#################################################################################
#                                                                               #
#  U_SHAD_MED --   To calculate median for the pixels in the pyramid            #
#		   shadow area                                                  #
#  Last modified:                                                               #
#  	29 Aug 1993  by CYZhang		Initial implementation                  #

procedure u_shad_med (shad, npix, stat)

#  Calling arguments
real	shad[ARB]		# Work array for shadow area pixels
int	npix			# Number of pixels in shadow area
pointer	stat			# Pointer to statistics structure

#  Local variables
int	i, lo, hi
pointer	ihgm			# Array for accumulation
real	h1, hdiff, hnorm
bool	fp_equalr ()

errchk	fp_equalr, u_ghist, adivkr

begin

#  Construct histogram for the shadowed pixels	
	call u_ghist (shad, npix, stat)

#  If all pixels are zero, don't do anything

	if (S_HMAX(stat) <= S_HMIN(stat) || S_HWIDTH(stat) <= 0.0) {
	    S_SMEDIAN(stat) = S_HMAX(stat)
	    call u_warn ("Constant image section involved!")
	    return
	}

	call malloc (ihgm, S_NBINS(stat), TY_REAL)

#  Integrate the histogram and normalize.
	Memr[ihgm] = real (Memi[S_HGM(stat)])
	do i = 2, S_NBINS(stat)
	    Memr[ihgm+i-1] = real (Memi[S_HGM(stat)+i-1]) + Memr[ihgm+i-2]
	hnorm = Memr[ihgm+S_NBINS(stat)-1]

#  hnorm should not be zero
	if (fp_equalr (hnorm, 0.0)) {
	    S_SMEDIAN(stat) = -MAX_REAL
	    call u_warn ("Sum of histogram is zero!")
	    return
	}
	call adivkr (Memr[ihgm], hnorm, Memr[ihgm], S_NBINS(stat))

#  Initialize the low and high bin numbers
	lo = 0
	hi = 1

#  Search for the point dividing the integral in half
	do i = 1, S_NBINS(stat) {
	    if (Memr[ihgm+i-1] > 0.5)
		break
	    lo = i
	}
	hi = lo + 1

#  Approximate the histogram
	h1 = S_HMIN(stat) + lo * S_HWIDTH(stat)
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0))
	    S_SMEDIAN(stat) = h1
	else if (lo == 0)
	    S_SMEDIAN(stat) = h1 + 0.5 / hdiff * S_HWIDTH(stat)
	else 
	    S_SMEDIAN(stat) = h1 + (0.5 - Memr[ihgm+lo-1]) /
		 hdiff * S_HWIDTH(stat)

#  Free memory
	call mfree (ihgm, TY_REAL)

end

#################################################################################
#                                                                               #
#  U_IMGTSEC --   To get a section from the image array				#
#  Last modified:                                                               #
#  	9 Feb 1994  by CYZhang		Initial implementation                  #

pointer procedure u_imgtsec (imgarr, npts, nrows, x1, x2, y1, y2)

#  Calling arguments
int 	npts, nrows
real	imgarr[npts, nrows]	# Image array
int	x1, x2, y1, y2		# Section ends
pointer	imgs			# Pointer to section

#  Local variables
int	npix			# Number of pixels
int	i, j, k

begin
	npix = (x2 - x1 + 1) * (y2 - y1 + 1)
	imgs = NULL
	call malloc (imgs, npix, TY_REAL)
	do j = y1, y2 {
	    do i = x1, x2 {
		k = (i - x1 + 1) + (x2 - x1 + 1) * (j - y1)
		Memr[imgs+k-1] = imgarr[i,j]
	    }
	}
	return (imgs)

end	

#################################################################################
#                                                                               #
#  U_DQFTSEC --   To get a section from the image array				#
#  Last modified:                                                               #
#  	9 Feb 1994  by CYZhang		Initial implementation                  #

pointer procedure u_dqftsec (dqfarr, npts, nrows, x1, x2, y1, y2)

#  Calling arguments
int 	npts, nrows
short	dqfarr[npts, nrows]	# Image array
int	x1, x2, y1, y2		# Section ends
pointer	dqfs			# Pointer to section

#  Local variables
int	npix			# Number of pixels
int	i, j, k

begin
	npix = (x2 - x1 + 1) * (y2 - y1 + 1)
	dqfs = NULL
	call malloc (dqfs, npix, TY_SHORT)
	do j = y1, y2 {
	    do i = x1, x2 {
		k = (i - x1 + 1) + (x2 - x1 + 1) * (j - y1)
		Mems[dqfs+k-1] = dqfarr[i,j]
	    }
	}
	return (dqfs)

end	
