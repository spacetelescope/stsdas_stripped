# crrej_sky -- Calculate the sky for an image.

include <imhdr.h>

define	MAX_BINS	40000	#maximum number of bins in the histogram

procedure crrej_sky (sky, ipin, nfiles, dim_x, dim_y, lower, upper,   skyval)

# input:
char	sky[ARB]	# parameter indicating how to do the sky
pointer	ipin[ARB]	# pointer of the input image
int	nfiles		# number of input images
int	dim_x, dim_y	# dimensions of the input image
real	lower, upper, 	# lower and upper limits of usable data

# output:
real	skyval[ARB]	# the sky level

# local:
pointer	histgrm		# pointer to the histogram
int	nbins		# number of bins
real	hwidth		# histogram resolution
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value
char	text[SZ_LINE]
int	range
pointer	sp, v, buf
int	i, k

bool	streq(), strne()
pointer imgnlr()
#===============================================================================
begin
	# decide what to do according to sky
	if (streq (sky, "none")) {
	    do k = 1, nfiles
	        skyval[k] = 0.
	    return
	}
	if (strne (sky, "mode")) {
	    call error (1, "illegal sky value")
	}

	# use the mode as sky value
	# first try to use upper and lower as boundary of the histogram, and
	# use the bin size of 1 (DN)
	range = nint(upper) - nint(lower)
	if (range > 1 && range < MAX_BINS) {
	    hmin = real (nint(lower))
	    hmax = real (nint(upper))
	    hwidth = 1.
	    nbins = range + 1

	# if the range is too large
	} else {
	    call sprintf (text, SZ_LINE, 
		    "Data range too large, must be less than %d")
	 	call pargi (MAX_BINS)
	    call error (1, text)
	}

	# set up the histogram array
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call malloc (histgrm, nbins, TY_INT)

	do k = 1, nfiles {

	    # reset the line-counter
            call amovkl (long(1), Meml[v], IM_MAXDIM)

	    # reset the histogram
	    do i = 1, nbins
		Memi[histgrm+i-1] = 0

	    # construct the histogram
            while (imgnlr (ipin[k], buf, Meml[v]) != EOF)
                call ahgmr (Memr[buf], dim_x, Memi[histgrm], nbins, hmin, hmax)
	
	    # calculate the mode from the histogram
	    call cr_mode (Memi[histgrm], nbins, hwidth, hmin, hmax,   skyval[k])
	}

	call mfree (histgrm, TY_INT)
	call sfree (sp)
end
