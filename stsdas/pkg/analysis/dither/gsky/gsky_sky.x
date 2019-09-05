include <imhdr.h>
include "gsky.h"

define	MAX_BINS	40000	#maximum number of bins in the histogram


# gsky_sky  --  Computes the sky for a set of images.
#
#
# 12-Jun-1996   Adapted from xcrrej to account for flagged pixels 
#               on input images (I.Busko)
# 25-Feb-1999   Modify (add a new parameter 'exposure time keyword') to make it 
#		also work for count rate images (e.g. NIC) (JC Hsu)
# 19-May-1999   Add a new parameter 'bunit' to determine if the image is
#		count rate or counts (JC Hsu)

procedure gsky_sky (ipin, ipmask, nfiles, nmasks, dim_x, dim_y, 
                    lower, upper, dqpat, bunit, exptm_key, skyval)

# input:
pointer	ipin[ARB]	# input images' descriptors
pointer	ipmask[ARB]	# input masks' descriptors
int	nfiles		# number of images
int	nmasks		# number of masks
int	dim_x, dim_y	# dimensions of the input image
real	lower, upper, 	# lower and upper limits of usable data
short	dqpat		# data quality bits
char	bunit[ARB]	# bunit keyword
char	exptm_key[ARB]	# exposure time keyword

# output:
real	skyval[ARB]	# the sky level

# local:
pointer	histgrm		# pointer to the histogram
int	nbins		# number of bins
real	exptm		# exposure time
real	hwidth0, hmin0, hmax0
real	hwidth		# histogram resolution
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value
char	text[SZ_LINE]
char	hbunit[SZ_LINE]	# header bunit value
int	range
short	szero
pointer	sp, v, buf, buf2, buf_msk
int	i, k, npix

int     imgnlr()
short   imgnls(), ands()
real	imgetr()
bool	streq()
#===============================================================================
begin
	szero = short (0)

	# first try to use upper and lower as boundary of the histogram, and
	# use the bin size of 1 (DN)
	range = nint(upper) - nint(lower)
	if (range > 1 && range < MAX_BINS) {
	    hmin0 = real (nint(lower))
	    hmax0 = real (nint(upper))
	    hwidth0 = 1.
	    nbins = range + 1

	# if the range is too large
	} else {
	    call sprintf (text, SZ_LINE, 
		    "Data range too large, must be less than %d")
	 	call pargi (MAX_BINS)
	    call error (1, text)
	}

	# set up the histogram and clean-pixel arrays
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call malloc (histgrm, nbins, TY_INT)
	if (nmasks > 0)
	    call malloc (buf2, dim_x, TY_REAL)

	do k = 1, nfiles {

	    # if keyword 'bunit' is NULL (default), will use the header 
	    # keyword 'BUNIT'.  If the header does not have this keyword, 
	    # image is in counts.
	    # But the upper and lower limits are always in counts (or DN)
	    if (streq(bunit, "cps")) {
	    	exptm = imgetr (ipin[k], exptm_key)
	    } else if (streq(bunit, "counts")) {
		exptm = 1.
	    } else {
	    	iferr (call imgstr (ipin[k], "bunit", hbunit, SZ_LINE)) {
		    exptm = 1.
		    hbunit[1] = EOS
		}
		call strlwr (hbunit)
		if (streq(hbunit, "cps") || streq(hbunit, "counts/s") ||
		    streq(hbunit, "c/s") || streq(hbunit, "count/s")) 
	    	    exptm = imgetr (ipin[k], exptm_key)
		else
		    exptm = 1.
	    }
		 
	    hmin = hmin0 / exptm
	    hmax = hmax0 / exptm
	    hwidth = hwidth0 / exptm

	    # reset the line-counter
            call amovkl (long(1), Meml[v], IM_MAXDIM)

	    # reset the histogram
	    do i = 1, nbins
		Memi[histgrm+i-1] = 0

	    # construct the histogram line by line
            while (imgnlr (ipin[k], buf, Meml[v]) != EOF) {

	        # read mask
	        if (nmasks > 0) {
	            i = imgnls (ipmask[k], buf_msk, Meml[v])

	            # discard flagged pixels
	            npix = 0
	            do i = 0, dim_x - 1 {
	                if (ands (dqpat, Mems[buf_msk+i]) == szero) {
	                    Memr[buf2+npix] = Memr[buf+i]
	                    npix = npix + 1
	                }
	            }

	            # build histogram from cleaned-up data
                    call ahgmr (Memr[buf2],npix,Memi[histgrm],nbins,hmin,hmax)

	        } else 

	            # build histogram from raw data
                    call ahgmr (Memr[buf],dim_x,Memi[histgrm],nbins,hmin,hmax)
	    }
	
	    # calculate the mode from the histogram
	    call gsky_mode (Memi[histgrm],nbins,hwidth,hmin,hmax, skyval[k])
	}

	if (nmasks > 0)
	    call mfree (buf2, TY_REAL)
	call mfree (histgrm, TY_INT)
	call sfree (sp)
end
