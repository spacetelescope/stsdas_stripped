include	<mach.h>
include <imio.h>
include	<imhdr.h>
include "wstat.h"

################################################################################
#										
# T_WSTATISTICS --  Compute and print the statistics of images.  This task is 	
#		    based upon images.imstatistics, but will allow any pixel 	
#		    to be excluded from the calculations if it is flagged in 	
#		    the DQF file.  						
#										
#	10/91 RAShaw	Initial code						
#	 1/92 RAShaw	Fixed errors caused by too few non-rejected pixels.	
#	10/95 JCHsu	Fix bugs for the column subsection case

procedure t_wstatistics ()

include "wstat.com"

#  Task parameters:
pointer	list			# List of input images
real	lower			# Lower limit of data value window
real	upper			# Upper limit of data value window
bool	dqfflag			# Use DQF mask?
char	datextn[SZ_FNAME]	# Filename extension for images
char	dqfextn[SZ_FNAME]	# Filename extension for DQFs

#  Local variables:
int	badbits			# Flagged DQF bits
pointer	buf			# Data from one image line
pointer dqf                     # Pointer to DQF image descriptor
pointer	dqbuf			# Data from one DQF image line
char	dqimage[SZ_FNAME]	# Current image DQF name 
int	group			# Current image group
pointer	hgm			# Data histogram
real	hmin, hmax, hwidth	# Histogram min, max, bin width
char	image[SZ_FNAME]		# Current image name
char	imcluster[SZ_FNAME]	# Current image name--w/o group or section
char	imdummy[SZ_FNAME]	# Current image name--including group spec.
int	i			# Loop index
pointer	im			# pointer to input image descriptor
bool	loop			# Loop over groups?
int	nbins			# Number of histogram bins
int	npts			# Number of non-rejected pixels
int	ngroup			# Number of image groups
pointer	sp			# Pointer to stack memory
pointer	work			# Non-rejected pixel array
pointer	wst			# Statistics structure
int	xysize			# Number of pixels in input image
pointer	v1, v2
int	dummy

#  Functions used:
bool	clgetb()		# Get BOOL cl value
real	clgetr()		# Get REAL cl value
int	gf_gcount()		# get number of groups/extensions
int	imgnlr()		# Fetch the next line of an REAL image
int	imgnli()		# Fetch the next line of an INTEGER image
pointer	gf_map()		# Map input image into memory
int	imtgetim()		# Fetch next filename from input list
pointer	imtopenp()		# Open an input file list
bool	streq()			# Are two input strings equal?

begin
	call smark (sp)
	call salloc (wst, LEN_WSTAT, TY_STRUCT)
 	call salloc (v1, IM_MAXDIM, TY_LONG)
 	call salloc (v2, IM_MAXDIM, TY_LONG)

	# Open the list of input images, and set the data value limits.
	list   = imtopenp ("images")
	lower  = clgetr ("lower")
	upper  = clgetr ("upper")

	# Set computation switches
	allst  = clgetb ("doall")
	if (allst) {
	    npix   = true
	    minmax = true
	    mean   = true
	    median = true
	    mode   = true
	    stddev = true
	    skew   = true
	    kurt   = true
	} else {
	    npix   = clgetb ("npix")
	    minmax = clgetb ("minmax")
	    mean   = clgetb ("mean")
	    median = clgetb ("midpt")
	    mode   = clgetb ("mmode")
	    stddev = clgetb ("stddev")
	    skew   = clgetb ("skew")
	    kurt   = clgetb ("kurt")

	    # comment out by JC Hsu 11/2/95, unnecessary relics from imstat
	    #if (median || mode) 
		#minmax = true
	}

	#  Set selected DQF flags
        dqfflag = clgetb ("usedqf")
        if (dqfflag) {

	    # comment out by JC Hsu 11/2/95, unnecessarily confusing
	    #npix = true
	    call dqfinit (badbits, datextn, dqfextn)
	}

	#  Loop through the input images.
	while (imtgetim (list, image, SZ_FNAME) != EOF) {
	    im = gf_map (image, READ_ONLY, 0)

	    #  Map input images (& DQFs if appropriate)
            if (dqfflag) {
                call splicstr (image, dqimage, datextn, dqfextn)
                dqf = gf_map (dqimage, READ_ONLY, 0)
            } else 
                dqf = NULL

	    #  Determine whether to loop over image groups
	    loop = false
	    iferr (ngroup = gf_gcount(im))
		group = INDEFI
	    else {
		group = IM_CLINDEX(im)
		call imgimage (image, imdummy, SZ_FNAME)
		call imgcluster (image, imcluster, SZ_FNAME)
		if (streq (imdummy, imcluster))
		    loop = true
	    }
	    
	    # Print the banner line.
	    call pheader (image, dqimage, dqfflag)

	    # Should not use reallo, changed back to malloc and allocating 
	    # memory only once, because the array sizes are the same for 
	    # all groups -- CY Zhang, 15/9/93
	    xysize = 1
	    do i = 1, IM_NDIM(im)
		xysize = xysize * IM_LEN(im, i)
	    call malloc (work, xysize, TY_REAL)

	    #  Accumulate the central moment statistics.
nxtgrp_
	    call winitialize (wst, lower, upper)
#	    xysize = IM_LEN(im, 1) * IM_LEN(im, 2)
#	    call realloc (work, xysize, TY_REAL)

	    # use imgnl instead of imgl2 to avoid the dimension reduction 
	    # problem for column subsection specification
            call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    if (dqfflag) {
                call amovkl (long(1), Meml[v2], IM_MAXDIM)
                while (imgnlr (im, buf, Meml[v1]) != EOF) {
                    dummy = imgnli (dqf, dqbuf, Meml[v2])
		    call dqwmean (wst, Memr[buf], Memi[dqbuf], IM_LEN(im, 1),
		        	  Memr[work], lower, upper, badbits)
		}
		call gf_unmap (dqf)
	    } else {
                while (imgnlr (im, buf, Meml[v1]) != EOF) {
		    call wmean (wst, Memr[buf], IM_LEN(im, 1),
		        	Memr[work], lower, upper)
		}
	    }
	    call gf_unmap (im)
	    npts = WS_NPIX(wst)

	    # Fix 1/92 RAS: If most all pixels are rejected...
#	    switch (npts) {
#	    case 0:
#		;
#	    case 1:
	    if (npts == 1) {
		WS_MEDIAN(wst) = Memr[work]
		WS_MODE(wst)   = Memr[work]
	    } else {

	    # There is no need to realloc the work array as the work array 
	    # will be smaller than or equal to the size of the image -- 
	    # CYZhang 15/9/93
#	    default:
#		call realloc (work, npts, TY_REAL)

		# Compute the central moment statistics.
		if (stddev || skew || kurt)
		    call moment (wst, Memr[work])

		# Accumulate the histogram.
		if (median || mode) {
		    hmin  = WS_MIN(wst)
		    hmax  = WS_MAX(wst)

		    # Fix 1/92 RAS: protect against divide by zero:
		    if (hmin == hmax) {
			WS_MEDIAN(wst) = hmin
			WS_MODE(wst)   = hmin
		    } else {
			nbins = min (npts/4, 32768)

			# Fix 1/92 RAS: protect against too few bins:
			nbins = max (nbins, 2)

			# Use malloc instead of realloc and de-allocate the 
			# memory inside the loop after finishing using the 
			# histogram. --  CYZhang 15/9/93
#			call realloc (hgm, nbins, TY_INT)
			call malloc (hgm, nbins, TY_INT)
			call aclri (Memi[hgm], nbins)
			call whist (Memr[work], npts, Memi[hgm], nbins, hmin, 
				hmax, hwidth) 

			# Estimate median & mode from histogram, and print 
			# the statistics.
			call hmedian (wst, Memi[hgm], nbins, hmin, hmax, hwidth)
			call hmode (wst, Memi[hgm], nbins, hmin, hmax, hwidth)
			call mfree (hgm, TY_INT)
		    }
		}
	    }
	    call wprint (wst, group)

	    # If looping over all groups, concatenate image+[group], map 
	    # next group (& DQF, if appropriate) into memory, and calculate 
	    # statistics.  
	    if (loop) {
		group = group + 1
		if (group > ngroup)
		    next
		call strcpy (image, imdummy, SZ_FNAME)
		call w_addgrp (imdummy, group)
		im = gf_map (imdummy, READ_ONLY, 0)
        	if (dqfflag) {
		    call splicstr (imdummy, dqimage, datextn, dqfextn)
		    dqf = gf_map (dqimage, READ_ONLY, 0)
		} else 
		    dqf = NULL
		goto nxtgrp_
	    }
	    call mfree (work, TY_REAL)
	}

	# Free memory
	call imtclose (list)

	# Free histogram memory within the loop -- CYZhang 15/9/93
	#call mfree (hgm, TY_INT)
	call sfree (sp)
end
