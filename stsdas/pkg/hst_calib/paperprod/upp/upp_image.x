include	<imhdr.h>
include	"upp.h"

define	SZ_HIST		4000
define	UP_HIST		0.98
define	LOW_HIST	0.001
define	HIST_BINS	4000

define	SECT_MIN	200
define	SECT_MAX	600
define	BOTTOM		35
define	NUM_EVAL	10

define	AREA_FAC	4.787

# Plot a gray scale image  and exposure information for WFPC2 IMAGEs 
#
# Added explicit use of '.hhh' for output 'tmp' images - 2 Jul 97 WJH
# Revised cutoff values for HIST to (0.98, 0.001) from (0.95, 0.001)
#	Also, shifted compass position to avoid overlapping images. 
#							- 12 Aug 97 WJH
#
#	Revised calculation of orient angle to account for different
#		orientations of chips in the mosaic.	- 20 Aug 97 WJH
#
#	Revised use of 'zsection' in 'onechip' and 'allchips' to eliminate
#		the need for temp files.		- 19 Nov 97 WJH
#
procedure upp_image ()


char	rootname[SZ_FNAME]
char	output[SZ_FNAME]
char	ftype[SZ_FNAME]
bool	calprt
int     page

int	fd
char	fname[SZ_FNAME]
char	img_name[SZ_FNAME]
char 	pc_name[SZ_FNAME]
char	wf_name[SZ_FNAME] 
char	tmp_name[SZ_FNAME]
char	img_root[SZ_FNAME]
pointer	c0h,shp
#pointer	section
char	shp_ext[SZ_EXT]
char	c0h_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	img_ext[SZ_EXT]
char	linenum[SZ_LINENUM]
char	propid[SZ_LINE]
char    pdftitle[SZ_LINE]
int     pgsect

double	ra, dec
real	targx, targy
int	chip, apergrp
int	detector[MAX_UGRPS]

pointer	nsection

real	goodmin[MAX_UGRPS]
real	goodmax[MAX_UGRPS]
real	grpmin, grpmax
real	hist[SZ_HIST]
int	k, ngroups

# related to checking the calibration data quality flag
int	calcheck
char	ref[SZ_PED, MAX_EXT]
char 	ped[SZ_PED, MAX_EXT]
char 	cflags[SZ_LINE, MAX_EXT]
int	nref
char	qual[SZ_LINE]
#char	qualcom[SZ_LINE, 1]
int	ncom


real	yoff
int	naxis1, naxis2
real	dmin, dmax
real	pscale, cd1_1, cd1_2, pc1_1, pc1_2
real	orientat
real 	vscale, width, npix
bool	mir_revr
bool	neg_img, BLANK

int	im_min1, im_max1, im_min2, im_max2

pointer	immap()
int	open()
bool	clgetb()
int     clgeti()
int	imgeti()
real	imgetr()
bool	imgetb()
double	imgetd()
int 	gf_gstfval()
real	alovr()
real	ahivr()
int	upp_calflag()
int	access()
bool	check_print()
bool	streq()

begin
	# set up memory debugging here
	#call memlog("Starting UPP_IMAGE...")

	# read parameters
	call clgstr ("rootname", rootname, SZ_LINE)
	call clgstr ("output", output, SZ_LINE)
	call clgstr ("fits", ftype, SZ_FNAME)
	calprt = clgetb ("calprt")
    page = clgeti ("page")
    	
	# construct necessary file name extensions
	if (streq(ftype,"fits")) {
	    call strcpy ("_shf.fits[0]", shp_ext, SZ_EXT) 
	    call strcpy ("_c0f.fits", c0h_ext, SZ_EXT)
	    call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
	 
	} else {
	    call strcpy (".shh", shp_ext, SZ_EXT) 
	    call strcpy (".d0h", d0h_ext, SZ_EXT) 
	    call strcpy (".c0h", c0h_ext, SZ_EXT) 
	}

	# construct file names
	call strcpy (rootname, fname, SZ_FNAME)
	call strcat (shp_ext, fname, SZ_FNAME)
	shp = immap (fname, READ_ONLY, 0)
	
	
	call strcpy (rootname, fname, SZ_FNAME)
	call strcat (c0h_ext, fname, SZ_FNAME)
	call strcpy (c0h_ext, img_ext, SZ_EXT)

	if (access (fname, 0, 0) == NO) {
		# Try to find a d0h image to work from
		call strcpy (rootname, fname, SZ_FNAME)
		call strcat (d0h_ext, fname, SZ_FNAME)
		call strcpy (d0h_ext, img_ext, SZ_EXT)
		if (access (fname, 0, 0) == NO) {
			# if there is no d0h image or c0h image, exit
			c0h = NULL
			call error (1, "UPP_IMAGE: No images to process")
		} else {
			# otherwise, use the d0h image
			if(streq(ftype,"fits"))
				call strcat("[0]",fname, SZ_FNAME)
			c0h = immap (fname, READ_ONLY, 0)
		}
	} else {
		# found a c0h image, so we use it...
		if(streq(ftype,"fits"))
			call strcat("[0]",fname, SZ_FNAME)
		c0h = immap (fname, READ_ONLY, 0)
	}

	# define base file name for later reference within IGI
	call strcpy (fname, img_name, SZ_FNAME)	

	# create full rootname with group extension for PC temp image
	call strcpy (img_name, pc_name, SZ_FNAME)
	if(streq(ftype,"geis")) {
		call strcat ("[1][*,-*]", pc_name, SZ_FNAME)
	} else {
		# 3-d FITS WFPC2 image
		call strcat ("[*,-*,1]", pc_name, SZ_FNAME)
	}		
	    
	# create full rootname with group extension for WF2 temp image
	call strcpy (img_name, wf_name, SZ_FNAME)
	if(streq(ftype,"geis")) {	
 		call strcat ("[3][-*,*]", wf_name, SZ_FNAME)
	} else {
		# 3-d FITS WFPC2 image
		call strcat ("[-*,*,3]", wf_name, SZ_FNAME)
	}		
		
	# read keywords
	
	# Check to see if we are working with BIAS/DARK/ECAL image
	# If we are, we then check 'calprt' to see if we should 
	# create a page for those images.  If not, we close all the files
	# and return to the calling program.
	if (!check_print(c0h, shp, output, calprt) ) 
		return

	# Create rootname for use in labeling output (without any
	# 	'tmp$PP' prefix...
	call imgstr (c0h, "ROOTNAME", img_root, SZ_FNAME)
	call strlwr (img_root)
	#call strcat (img_ext, img_root, SZ_FNAME)

        call imgstr (c0h, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (c0h, "PROPOSID", propid, SZ_LINE)
	ra = imgetd (shp, "RA_TARG")
	dec = imgetd (shp, "DEC_TARG")

	naxis1 = IM_LEN(c0h, 1)
	naxis2 = IM_LEN(c0h, 2)
	mir_revr = imgetb (c0h, "MIR_REVR")

	# read each group to find the maximum and minimum
	# and set the upper and lower limits for the histogram
	BLANK = false

	  # first determine the number of groups
	    if (streq(ftype,"geis") ) {
		    ngroups = gf_gstfval (c0h, "GCOUNT")
	    } else {
		# For a 3-d FITS WFPC2 image
		    ngroups = IM_LEN(c0h, 3)
	    }

	    do k=1,ngroups {
		if (naxis1 > SECT_MAX && naxis2 > SECT_MAX) {
		   if (streq(ftype,"geis")) {
		   call sprintf (tmp_name,  SZ_FNAME, "%s[%d][%d:%d,%d:%d]")
			call pargstr(img_name)
			call pargi(k)
			call pargi(SECT_MIN)
			call pargi(SECT_MAX)
			call pargi(SECT_MIN)
			call pargi(SECT_MAX)
		    } else {
		   call sprintf (tmp_name,  SZ_FNAME, "%s[%d:%d,%d:%d,%d]")
			call pargstr(img_name)
			call pargi(SECT_MIN)
			call pargi(SECT_MAX)
			call pargi(SECT_MIN)
			call pargi(SECT_MAX)
			call pargi(k)
		    }			
		} else {
			im_min1 = max (50., naxis1/10.)
			im_max1 = min (naxis1 - 50., naxis1 * 0.9)

			im_min2 = max (50., naxis2/10.)
			im_max2 = min (naxis2 - 50., naxis2 * 0.9)

		   if (streq(ftype,"geis")) {
		       call sprintf (tmp_name,SZ_FNAME,"%s[%d][%d:%d,%d:%d]")
				call pargstr(img_name)
				call pargi(k)
				call pargi(im_min1)
				call pargi(im_max1)
				call pargi(im_min2)
				call pargi(im_max2)
		    } else {
		       call sprintf (tmp_name,SZ_FNAME,"%s[%d:%d,%d:%d,%d]")
				call pargstr(img_name)
				call pargi(im_min1)
				call pargi(im_max1)
				call pargi(im_min2)
				call pargi(im_max2)
				call pargi(k)
		    }
		}

		# open the section of each group individually here
		nsection = immap(tmp_name,READ_ONLY,0)
		call get_hist (nsection, UP_HIST, LOW_HIST, hist, HIST_BINS, goodmin[k], goodmax[k])
		if (streq(ftype,"geis"))
			detector[k] = imgeti (nsection, "DETECTOR")
		else 
			call fit_detector(img_name,k,detector[k])

		call imunmap(nsection)

	    # End of loop over groups...
	    }		

	# if we have more than 1 group, open up a WF image to determine
	# the plate scale...
	if (streq(ftype,"geis")) {
		if (ngroups > 1) {
			call gf_opengr(c0h, 2, dmin, dmax, 0)
			cd1_1 = imgetr (c0h, "CD1_1")
			cd1_2 = imgetr (c0h, "CD1_2")
			call gf_opengr (c0h, 1, dmin, dmax, 0)
			pc1_1 = imgetr (c0h, "CD1_1")
			pc1_2 = imgetr (c0h, "CD1_2")
			call gf_opengr (c0h, ngroups, dmin, dmax, 0)
			orientat = imgetr (c0h, "ORIENTAT")
		} else {
			cd1_1 = imgetr (c0h, "CD1_1")
			cd1_2 = imgetr (c0h, "CD1_2")
			orientat = imgetr (c0h, "ORIENTAT")
		}
	} else {	
	# We are working with a 3-d FITS WFPC2 image
	    call get_fitscd(img_name, ngroups, pc1_1, pc1_2, cd1_1, cd1_2, orientat)		
	}

	# Correct orientat value for chip orientation
	# 20 Aug 97 WJH

		orientat = orientat + ((4-detector[ngroups]) * 90.)

		if (orientat < 180.) 
			orientat = orientat + 360.
		if (orientat > 180. )
			orientat = orientat - 360.
			
	# Now determine the lowest 'min' and largest 'max' for scaling
	# the image display
		grpmin = alovr (goodmin, ngroups)
		grpmax = ahivr (goodmax, ngroups)

		if (grpmin == grpmax ) {
			 BLANK = TRUE
		} else {
		# Add in a 5% buffer to the values for the benefit of better
		# displaying nearly flat observations...
			grpmin = grpmin * 0.99	    
			grpmax = grpmax * 1.01
			goodmin[1] = goodmin[1] * 0.99
			goodmax[1] = goodmax[1] * 1.01
		}

	# determine which chip and group corresponds to the specified 
	# aperture
	call det_uaper (c0h, shp, ngroups, chip, apergrp, img_name, ftype)
	
	# set the switch for negative image display
	neg_img = TRUE 

	# determine the status of the calibration quality flag
	calcheck = upp_calflag (c0h, ref, ped, cflags, nref)
	
	if (streq(ftype,"geis")) {
		call pp_qual(rootname, qual, cflags[1,1], ncom)
	} else {
		call strcpy(rootname, tmp_name, SZ_FNAME)
		call strcat("_pdq.fits",tmp_name, SZ_FNAME)
		call pp_qualfits(tmp_name, qual, cflags[1,1], ncom)
	}

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# start a new page
	call pp_erase (fd)
    
    # Create section heading here for bookmarks
    if (ngroups > 1 && detector[1] == 1)  
        pgsect = -3
    else 
        pgsect = -2
    
    call pp_pdfsection (fd, page, pgsect, rootname)
    
    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Full FOV")
    call pp_pdfbook(fd, page, pdftitle)

	call fprintf (fd, "fontset hard; vpage 0. 1. 0. 1.; location 0. 1. 0. 1.\n")

	# draw the banner
	if (ngroups > 1) call uimg_banner (fd, linenum, propid, "WFPC2")
	else call obs_banner (fd, linenum, img_root, propid, "WFPC2", 0.)

	call fprintf (fd, "reset\n")
	call fprintf (fd, "vpage 0.26 1.0 0.01 0.99; fontset hard\n")

	# place markers at target position in image
	if (ra != 0. && dec != 0.) 
		call utarg_pos (fd, img_name, ra, dec, targx, targy, ngroups, detector, ftype)
	else
		call eprintf ("No pointed target for observation...\n")
	
	# plot the gray scale image
	if (ngroups == 1 && !BLANK) {
	  # set the number of pixels for determining the plate scale
	  npix = naxis1

	  # call the routine to properly display 1 chip by itself
	  call onechip(fd, chip, img_name, grpmax, grpmin, ftype)

	} else if ( ngroups > 1 && ngroups <= 4 && !BLANK) {
	  # set the number of pixels across all chips read out
	  # for determining the plate scale based on IM_LEN
		npix = naxis1
	  # display all four chips in proper orientation relative to each other
	  call allchips(fd, ngroups, detector, img_name, grpmax, grpmin, ftype)

	# draw in dividing lines between chip displays
	    call fprintf (fd, "vmove 0. 0.5; vdraw 1.0 0.5\n")
	    call fprintf (fd, "vmove 0.5 0.0; vdraw 0.5 1.0\n")
	    
	} else {
	    call fprintf (fd, "vmove 0.5 0.5; label 'BLANK image'\n")
	    call error (1, "UPP_IMAGE: Unknown number of WFPC2 groups or BLANK images")
	}

	# calculate the plate scale in arc seconds		
	pscale = sqrt(cd1_1**2 + cd1_2**2) * 3600.


	# put in the grey scale bar
 	call pp_gsbar (fd, 0.05, 0.45, 0.87, 0.9, grpmin, grpmax, 0.65, neg_img)
        call fprintf (fd, "vmove 0.25 0.86; label '(WF)'\n")
        call fprintf (fd, "vmove 0.25 0.92; label '(PC)'\n")
        call fprintf (fd, "vmove 0.05 0.92; label '%0.5g'\n")
	    call pargr (grpmin)
        call fprintf (fd, "vmove 0.45 0.92; label '%0.5g'\n")
	    call pargr (grpmax/AREA_FAC)
	
	# calculate the width across the vpage each image pixel covers
	# width: width across the page a single image covers...
	if (ngroups == 1) width = 0.74 * 0.75
	else width = 0.74 / 2.
	
 	vscale = width / npix

	# take care of problems with INTFLAT and DARK images having
	# invalid CD matrices.  
	# If we have a valid plate scale, put one on the page, 
	# otherwise leave it off entirely.
	if (pscale < 1.)  {
		# draw the compass
		call pp_compass (fd, 0.33, 0.62, 0.06, orientat, mir_revr)

		# draw the plate scale
		call pp_pscale (fd, 0.4, 0.8, 0.1, pscale, vscale)
	}

	# put in exposure information on left side of page
	# set up parameters for page size and line limits
	yoff = 0.
	call fprintf (fd, "reset; fontset hard; location 0. 1. 0. 1.\n")
	call fprintf (fd, "vpage 0.01 0.25 0.01 0.9\n")
	call fprintf (fd, "limits 0 40 %d 0\n")
		call pargi (BOTTOM)
	call fprintf (fd, "move 40 0; draw 40 %d\n")
		call pargi (BOTTOM)

	# use full image name plus extension as column label
	call fprintf (fd, "move 20 0; expand 1; justify 2\n")
	call fprintf (fd, "label '%s'\n")
		call pargstr (img_name)	
	call fprintf (fd, "justify 3\n")
	yoff = yoff + 2

	# set scaling factor for remaining information
	# and use column 16 for the start of the output
	call fprintf (fd, "expand 0.7\n")

	# start printing out information for column here
	call uimg_sum(fd, yoff, c0h, shp, propid, ra, dec, orientat, rootname, calcheck, qual)
    
    # Finished with this page, advance page counter
    page = page + 1

	# If more than 1 chip was read out in the observation, 
	# AND it contains a PC image,
	# print out a page with an enlarged display of just the PC chip
	if (goodmin[1] == goodmax[1]) BLANK = true

	if (ngroups > 1 && detector[1] == 1) {
	# start a new page
		call pp_erase (fd)

        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "PC FOV")
        call pp_pdfbook(fd, page, pdftitle)

		call fprintf (fd, "vpage 0. 1. 0. 1.; location 0. 1. 0. 1.\n")

		# draw the banner
		#call obs_banner (fd, linenum, rootname, propid, "WFPC2", 0.)
		call obs_banner (fd, linenum, img_root, propid, "WFPC2", 0.)

		call fprintf (fd, "reset; fontset hard\n")
		call fprintf (fd, "vpage 0.26 1.0 0.01 0.99\n")

		# plot the gray scale image
		# set the number of pixels for determining the plate scale
		npix = naxis1
		
		if (!BLANK) {
#	 	   call fprintf (fd, "location 0.25 1. 0.0 0.75\n")
		   chip = 1
		   call onechip(fd, chip, img_name, goodmax[1], goodmin[1], ftype)

		   # put in the grey scale bar
 		   call pp_gsbar (fd, 0.05, 0.45, 0.87, 0.9, goodmin[1], goodmax[1], 0.7, neg_img) 
		
		   # calculate the plate scale in arc seconds		
		   pscale = sqrt(pc1_1**2 + pc1_2**2) * 3600.	
		   # calculate the width across the vpage each image pixel covers
		   width = 0.74 * 0.75
	
	 	   vscale = width / npix
		} else 
		   call fprintf (fd, "vmove 0.5 0.5; label 'BLANK Image'\n")

		# take care of problems with INTFLAT and DARK images having
		# invalid CD matrices.  
		# If we have a valid plate scale, put one on the page, 
		# otherwise leave it off entirely.
		if (pscale < 1.)  {
			# Read in MIR_REVR again to make sure it is correct...
 		        mir_revr = imgetb (c0h, "MIR_REVR")

			# draw the compass
			call pp_compass (fd, 0.33, 0.62, 0.06, orientat, mir_revr)
			# draw the plate scale
			call pp_pscale (fd, 0.4, 0.8, 0.1, pscale, vscale)
		} 

		# put in exposure information on left side of page
		# set up parameters for page size and line limits
		yoff = 0.
		call fprintf (fd, "reset; fontset hard; location 0. 1. 0. 1.\n")
		call fprintf (fd, "vpage 0.01 0.25 0.01 0.9\n")
		call fprintf (fd, "limits 0 40 %d 0\n")
			call pargi (BOTTOM)
		call fprintf (fd, "move 40 0; draw 40 %d\n")
			call pargi (BOTTOM)
	
		# use full image name plus extension as column label
		call fprintf (fd, "move 20 0; expand 1; justify 2\n")
		call fprintf (fd, "label '%s[1]'\n")
			call pargstr (img_name)	
		call fprintf (fd, "justify 3\n")
		yoff = yoff + 2
	
		# set scaling factor for remaining information
		# and use column 16 for the start of the output
		call fprintf (fd, "expand 0.7\n")

		call uimg_sum(fd, yoff, c0h, shp, propid, ra, dec, orientat, rootname, calcheck, qual)

        # Finished with this page, advance page counter
        page = page + 1

	# Finished with PC image page
	}

    # Update the number of pages created
    call clputi("page", page)

	# close files
	call imunmap (c0h)
	#call imunmap (section)
	call imunmap (shp)


	# finished with image display pages
	call close (fd)
end



# produce the shortened observation banner for WFPC2 image display pages

procedure uimg_banner (fd, linenum, propid, instru)

int	fd
char	linenum[ARB]
char	propid[ARB]
char	instru[ARB]

char	tmp_banner[SZ_FNAME]

begin
	call mktemp ("tmp$ban", tmp_banner, SZ_FNAME)

	call fprintf (fd, 
		"reset; fontset hard; vpage 0.0 0.6 0.05 0.98; expand 1.\n")
	call fprintf (fd, "location 0 1 0 1\n")

	# print the shading
	call fprintf (fd, 
		"!printf ('0 .94%sn1 .94%sn1 1%sn0 1%sn',> '%s')\n")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr (tmp_banner)
	call fprintf (fd, "data %s\n")
	    call pargstr (tmp_banner)
	call fprintf (fd, "xcol c1; ycol c2; color 5; fillpat 2; polygon\n")
	call fprintf (fd, "color 1; fillpat 1\n")
	call fprintf (fd, "!delete %s verify-\n")
	    call pargstr (tmp_banner)

	call fprintf (fd, "vmove 0.02 0.98; justify 3\n")
	call fprintf (fd, "label '%sfBLogsheet Line# %s'\n")
	    call pargstr ("\\")
	    call pargstr (linenum)

	call fprintf (fd, "vmove 0.75 0.98; justify 1\n")
	call fprintf (fd, "label '%sfBProposal: %s'\n")
	    call pargstr ("\\")
	    call pargstr (propid)

	call fprintf (fd, "vmove 0.98 0.98; justify 1\n")
	call fprintf (fd, "label '%sfI%s'\n")
	    call pargstr ("\\")
	    call pargstr (instru)

	# put in reduced vpage area for placement of gray-scale image
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.93; expand 0.9\n")
end


procedure utarg_pos (fd, img, r, d, tx, ty, ngroups, detector, ftype)

char 	img[ARB]
int 	fd
double 	r, d
real 	tx, ty, kx, ky
int  	chip, ngroups, k
int	detector[ARB]
char	ftype[ARB]

real 	offx, offy, scale
real 	chx, chy
real 	theta1, theta2, theta3, theta4
char 	command[SZ_LINE]
#char 	invx[SZ_LINE], invy[SZ_LINE], invch[SZ_LINE]

#real	deltax, deltay
real	mx, my

real 	clgetr()
bool	streq()

begin

	# we can NOT assume that the target position is in the chip
	# corresponding to the selected aperture.  Therefore, we determine
	# what chip the target is in independent of the aperture information.

	chip = 0
	# build command string for call 'rd2xy' (since it can handle
	#	single group images)
	#   rd2xy w1234567t.d0h[n] "19:12:23.1" "33:22:11.123" hour="yes"
	do k=1,ngroups {
	    if (streq(ftype,"geis")) {
	    	call sprintf (command, SZ_LINE, "rd2xy %s[%d] %.6f %.6f hour=no >& dev$null")
			call pargstr (img)
			call pargi  (k)
			call pargd (r)
			call pargd (d) 			
	    	# now, run 'rd2xy'
	    	# RD2XY only understands GEIS WFPC2 images... 16 Oct 97 WJH
		call clcmdw (command)

	    	# Get the output coordinates from RD2XY
 		kx = clgetr("rd2xy.x")
		ky = clgetr("rd2xy.y")
	    } else {
		# We are working with an WFPC2 FITS image with a table
		call upp_rd2xy (img, k, r, d, kx, ky)
	    }

	    if (kx >= 0. && kx <= 800. && ky >= 0. && ky <= 800.) {
	        # if so, copy the results from rd2xy to variables
		chip = detector[k]
		tx = kx
		ty = ky
		break
	    }
	}

	# check to make sure target coordinates were found within image
	# if not, set variables to harmless default values and leave 
	#	function without printing out target markers
	if (chip == 0) {
		call printf ("Target coordinates not within image %s...\n")
			call pargstr(img)
		tx = 1.
		ty = 1.
	
 	} else { 
		# set up conversion factors for rotation of coordinates
		# scaled to 800 pixel wide image	
		theta1 = ((800 - ty) / 800.)
		theta2 = ((800 - tx) / 800.)
		theta3 = ty / 800.
		theta4 = tx / 800.

		# check to see if we are looking at one group, or 4 groups
		# and set the scale and offset appropriately
	  	# we are looking at 4 groups
	  	if (chip == 1) {
			# Account for PC's scale of 0.457 of WF
			scale = 0.5 * 0.457
			offx = 0.5*(1-0.457)
			offy = 0.5 
	  	} else if (chip == 2) {
			scale = 0.5
			offx = 0.
			offy = 0.
	 	} else if (chip == 3) {
			scale = 0.5
			offx = 0.5
			offy = 0.
	  	} else {
			scale = 0.5
			offx = 0.5
			offy = 0.5
	  	}
	  	# regardless of what chip it is, if there is only 1 in
		# observation, set scale to 1 and offsets to 0.  
		if (ngroups == 1) {
			scale = 0.75
			offx = 0.24
			offy = 0.
		}
		# rotate position based on chip orientation
		if (chip == 1) {
			# relative position of target in vpage coordinates
			chx = (theta1 * scale) + offx
			chy = (theta4 * scale) + offy
	   	} else if (chip == 2) {
			chx = (theta2 * scale) + offx
			chy = (theta1 * scale) + offy
	   	} else if (chip == 3) {
			chx = (theta3 * scale) + offx
			chy = (theta2 * scale) + offy
	 	} else {
			chx = (theta4 * scale) + offx
			chy = (theta3 * scale) + offy
	   	}

	# plot a triangle at target position in vpage coordinates
	# need to change to marks at edge of image display
		call fprintf (fd, "location 0. 1. 0. 1.\n")
		
		if (ngroups > 1 && ngroups <= 4) {
		# determine y coordinate for TARGET X position marker
			if (chx < 0.25 ) my = 0.51
			else if (chx > 0.5) my = 1.01
			else my = 0.76
	
			if (chy > 0.75) mx = 0.49
			else if (chy < 0.5) mx = -0.01
			else mx = 0.24
		} else {
			mx = 0.24
			my = 0.765
		}
	
		# put in marker qt TARGET's X Position
		call fprintf (fd, "ptype 3 0; expand 0.7\n")
		call fprintf (fd, "vmove %f %f\n")
			call pargr (chx)
			call pargr (my)
		call fprintf (fd, "angle 60; dot\n")

		# put in marker TARGET's X Position
		call fprintf (fd, "vmove %f %f\n")
			call pargr (mx)
			call pargr (chy)
		call fprintf (fd, "angle 30; dot\n")
		call fprintf (fd, "angle 0; expand 1\n")	

	}

end	


procedure onechip(fd, chip, img_name, grpmax, grpmin, ftype)

int	fd
int	chip
char	img_name[ARB]
real	grpmax, grpmin
char	ftype[ARB]

bool	streq()

begin

	  call fprintf(fd, "expand 1.0\n")
	  call fprintf(fd, "vmove 0.75 0.85; label Chip %d display\n")
		call pargi (chip)
	  call fprintf(fd, "expand 1.0\n")

	  if (chip == 1) {
	  # rotate PC1(Chip 1) image by 90 degrees
	      if (streq(ftype,"geis")) {
	      # display single chip at full scale
		call fprintf (fd, "zsection %s[1] 1 90\n")
		    call pargstr (img_name)
	      } else {
	      # display single chip at full scale
		call fprintf (fd, "zsection %s[*,*,1] 1 90\n")
		    call pargstr (img_name)
	      }

	  } else  if (chip == 3) {

	    # rotate WF3 (Chip 3) image by -90 degrees
	    if (streq(ftype,"geis")) {
		# display single chip at full scale
		call fprintf (fd, "zsection %s[1] 1 -90\n")
		    call pargstr (img_name)
    
	    } else {
	    # display single chip at full scale
		call fprintf (fd, "zsection %s[*,*,1] 1 -90\n")
		    call pargstr (img_name)
	    }		    

	  } else if (chip == 2) {	  

 		  # display single chip at full scale
	  	call fprintf (fd, "zsection %s[-*,*]\n")
			call pargstr (img_name)
	  } else {
	  # display unrotated single chip at full scale
	    call fprintf (fd, "zsection %s\n")
		call pargstr (img_name)
	  }

	  call fprintf (fd, "fitpix 0.25 1. 0.0 0.75\n")
	  call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (grpmax)
		call pargr (grpmin)
	  call fprintf (fd, "pixmap\n")

end




procedure allchips(fd, ngroups, detector, img_name, grpmax, grpmin, ftype)

int	fd
int	ngroups
char	img_name[ARB]
real	grpmax, grpmin
int	detector[ARB]
char	ftype[ARB]

real pcmax
int	i 

bool	streq()

begin

	# loop over the actual number of groups in the image
	 do i = 1, ngroups {

	  if (detector[i] == 1) {
	    # rotate PC1(Chip 1) image by 90 degrees
	    if (streq(ftype,"geis")) {
	    call fprintf (fd, "zsection %s[%d] 2 90\n")
		call pargstr (img_name)
		call pargi(i)
	    } else {
	    call fprintf (fd, "zsection %s[*,*,%d] 2 90\n")
		call pargstr (img_name)
		call pargi(i)
	    }	
		
		# Adjust the maximum grey scale value to match PC scaling
		pcmax = grpmax / AREA_FAC
		
	  # display PC in its quadrant, scaled to proper size
	    call fprintf (fd, "fitpix 0.2715 0.5 0.5 0.7285\n")
	    call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (pcmax)
		call pargr (grpmin)
	    call fprintf (fd, "pixmap\n")
	# Now, delete block averaged image so next group's image can replace it
	  }

	  if (detector[i] == 2) {

	  # display Chip 2 (WF2) in its quadrant
	    if (streq(ftype, "geis")) {
	 	   call fprintf (fd, "zsection %s[%d][-*,-*] 2\n")
			call pargstr (img_name)
			call pargi(i)
	    } else {
	 	   call fprintf (fd, "zsection %s[-*,-*,%d] 2\n")
			call pargstr (img_name)
			call pargi(i)
	    }
			
#	    call fprintf (fd, "fitpix; limits; zrange %6.1f %6.1f\n")
	    call fprintf (fd, "fitpix 0.0 0.5 0.0 0.5\n")
	    call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (grpmax)
		call pargr (grpmin)
	    call fprintf (fd, "pixmap\n")
	  }

	  if (detector[i] == 3) {
	  # rotate WF3 (Chip 3) image by -90 degrees
	    if (streq(ftype,"geis")) {	
		call fprintf (fd, "zsection %s[%d] 2 -90\n")
		    call pargstr (img_name)	    
		    call pargi(i)
	    } else {
		call fprintf (fd, "zsection %s[*,*,%d] 2 -90\n")
		    call pargstr (img_name)
		    call pargi(i)
	    }

	  # display Chip 3 (WF3) in its quadrant after rotation
#	    call fprintf (fd, "location 0.5 1.0 0. 0.5\n")
#	    call fprintf (fd, "fitpix; limits; zrange %6.1f %6.1f\n")

	    call fprintf (fd, "fitpix 0.5 1.0 0. 0.5\n")
	    call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (grpmax)
		call pargr (grpmin)

	    call fprintf (fd, "pixmap\n")
	  }
	    
	  if (detector[i] == 4) {
			    
	  # display Chip 4 (WF 4) in its quadrant
#	    call fprintf (fd, "location 0.5 1.0 0.5 1.0\n")
		
	    if (streq(ftype,"geis") ){
	    	call fprintf (fd, "zsection %s[%d] 2\n")
			call pargstr(img_name)
			call pargi(i)
	    } else {
	    	call fprintf (fd, "zsection %s[*,*,%d] 2\n")
			call pargstr(img_name)
			call pargi(i)
	    }		
#	    call fprintf (fd, "fitpix; limits; zrange %6.1f %6.1f\n")
	    call fprintf (fd, "fitpix 0.5 1.0 0.5 1.0\n")
	    call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (grpmax)
		call pargr (grpmin)
	    call fprintf (fd, "pixmap\n")
	  }	

	# end loop over 'ngroups'
	}
end


procedure uimg_sum(fd, yoff, c0h, shp, propid, ra, dec, orientat, rootname, calcheck, qual)

int	fd
pointer	c0h, shp
char	propid[ARB]
real	orientat
char	rootname[ARB]
double	ra, dec
int	calcheck
char	qual[ARB]

real	yoff, infocol, exptime
char	targname[SZ_TARG]
char	targ1[MAX_TARG]
char	targ2[MAX_TARG]
char	filter1[SZ_UFILT]
char	filter2[SZ_UFILT]
char	filtnam[SZ_UFILTNAM]
char	dateobs[SZ_DATEOBS]
char	full_date[SZ_FDATE]

bool	streq()
real	imgetr()
bool	qual_check()

begin
	# read target name
	call imgstr (shp, "TARGNAME", targname, SZ_TARG)
	call split_str (targname, targ1, targ2, MAX_TARG)

	# read in aperture and filter info here
	call imgstr (c0h, "FILTNAM1", filter1, SZ_UFILT)
	call imgstr (c0h, "FILTNAM2", filter2, SZ_UFILT)
	call strcpy (filter1, filtnam, SZ_UFILTNAM)
	if(!streq(filter2,"")) {
		call strcat (",", filtnam, SZ_UFILTNAM)
		call strcat (filter2, filtnam, SZ_UFILTNAM)
	}

	call imgstr (c0h, "DATE-OBS", dateobs, SZ_DATEOBS)
	call date_str (dateobs, full_date)

	exptime = imgetr(c0h, "EXPTIME")

	infocol = 16.

	call pp_label (fd, 0., yoff, "PI:")
#	call pp_label (fd, infocol, yoff, piname)
	call pp_fmtkw (fd, infocol, yoff, shp, "pr_inv_l", "%s")
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Proposal:")
	call pp_label (fd, infocol, yoff, propid)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Target:")
	call pp_label (fd, infocol, yoff, targ1)
	    # if the target name is long
	    if (targ2[1] != EOS) {
		yoff = yoff + 1
	        call pp_label (fd, infocol, yoff, targ2)
	    }

	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Filters:")
	call pp_label (fd, infocol, yoff, filtnam)

	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Exp. Time:")
	#call pp_fmtkw (fd, infocol, yoff, c0h, "exptime", "%0.2f")
	call fprintf (fd, "justify 3; move %4.1f %4.1f\n")
		call pargr (infocol)
		call pargr (yoff)
	call fprintf (fd, "label '%0.2f sec'\n")
		call pargr (exptime)

	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Date:")
	call pp_label (fd, infocol, yoff, full_date)

	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Time:")
	#call pp_label (fd, infocol, yoff, timeobs)
	call pp_fmtkw (fd, infocol, yoff, c0h, "time-obs", "%s")

	# Put in an extra line here to separate the data sections
	yoff = yoff + 2

	call pp_label (fd, 0., yoff, "RA(J2000):")
	call pp_move (fd, infocol, yoff)
	call fprintf (fd, "label '%0.2H'\n")
		call pargd (ra)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Dec(J2000):")
	call pp_move (fd, infocol, yoff)
        call fprintf (fd, "label '%0.1h'\n")
                call pargd (dec)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "X POSTARG:")
	call pp_label (fd, infocol, yoff, "N/A")
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Y POSTARG:")
	call pp_label (fd, infocol, yoff, "N/A")
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Posn. Angle:")
	call pp_rlabel (fd, infocol, yoff, orientat)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "V:")
	#call pp_rlabel (fd, infocol, yoff, magv)
	call pp_fmtkw (fd, infocol, yoff, shp, "mag_v", "%0.2f")
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "B-V:")
	#call pp_rlabel (fd, infocol, yoff, colbv)
	call pp_fmtkw (fd, infocol, yoff, shp, "col_b_v", "%0.2f")

	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Spec. Type:")
	#call pp_label (fd, infocol, yoff, spectype)
	call pp_fmtkw (fd, infocol, yoff, shp, "sp_type", "%s")

		
	# print out legend for Quality flags at bottom of column
	yoff = BOTTOM - 6
	call pp_label (fd, 0., yoff, "QUALITY FLAGS:")
	yoff = yoff + 1
	call fprintf (fd, "expand 0.65\n")

	call pp_label (fd, 0., yoff, "Observation:")

	# determine observation quality flag from pdq file


	#call pp_qual(rootname, qual, qualcom[1,1], ncom)
	#call qual_parse(qual, qualeval, neval)

	# first, let's see if the 'quality' keyword from the
	# PDQ file say's 'OK' or 'NO-EVAL'
	# otherwise, give an indication of an error
	 if (qual[1] != EOS) {
	 	call pp_move (fd, 35., yoff+0.25)
	    if( qual_check(qual, "OK, NO-EVAL, NO-TLM, TM_GAP") ) {

		#call eprintf("Quality check: OK\n")
		    call fprintf (fd, "ptype 25 0; dot\n")
	    } else {

		#call eprintf("Quality check: Not OK!\n")
		    call fprintf (fd, "ptype 25 3; dot\n")
	    }
	}

	yoff = yoff + 1

	call pp_label (fd, 0., yoff, "Calibration:")
	call pp_move (fd, 35., yoff+0.25)
	if (calcheck > 0) {
	    call fprintf (fd, "ptype 25 3; dot\n")
	} else 
	    call fprintf (fd, "ptype 25 0; dot\n")

	yoff = yoff + 2
	
	call pp_label (fd, 0., yoff, "OK:")
	call fprintf (fd, "ptype 25 0; move 7 %4.1f; dot\n")
		call pargr (yoff)
	call pp_label (fd, 15., yoff, "Not OK:")
	call fprintf (fd, "ptype 25 3; move 27 %4.1f; dot\n")
		call pargr (yoff)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Unknown or File Missing: Blank")


end


bool procedure check_print(c0h, shp, igiout, calprt)

pointer	c0h, shp
char	igiout[ARB]
bool	calprt

char	imtype[SZ_LINE]
string 	imdict "|DARK|BIAS|ECAL|"
bool	check
pointer	fd

int	open()

char	strsearch()

begin
	# default case: print out image page
	check = TRUE

	# Check to see if we are working with BIAS/DARK/ECAL image
	# If we are, we then check 'calprt' to see if we should 
	# create a page for those images.  If not, we close all the files
	# and return to the calling program.

	# Read in the IMAGE TYPE (BIAS/ECAL/EXT/...)
	call imgstr (c0h, "IMAGETYP", imtype, SZ_LINE)
	# If we are working on a BIAS/DARK/ECAL image, then return without
	#	creating an IMAGE page
	if ( !calprt && (strsearch(imdict, imtype) != EOS) ) {
		# close images that have been opened...
		call imunmap(c0h)
		call imunmap(shp)

		# create a dummy output page for IGI...
		fd = open (igiout, NEW_FILE, TEXT_FILE)
		call pp_erase(fd)
		call close(fd)
		# set the value of check to NOT produce an image page
		check = FALSE
	}

	return(check)
end

procedure fit_detector(fname, group, chip)

char	fname[ARB]
int	group
int	chip

pointer	gtab, cptr
char	tabname[SZ_FNAME]
int	extnum

pointer	tbtopn()

begin
	# We want to access FITS extension '[1]' as a table
	extnum = 1

	# Create new filename with FITS extension of '[1]'
	call get_fitextn(fname, extnum, tabname)
	
	# Open table
	gtab = tbtopn(tabname, READ_ONLY, NULL)
	
	# Get information from each column here, from rownum = group
	# Consider adding a check to make sure the column is found	
	call tbcfnd (gtab, "DETECTOR", cptr, 1)
	call tbegti (gtab, cptr, group, chip)
	
	# Close table
	call tbtclo(gtab) 



end

procedure get_fitscd(fname, ngroups, pc1_1, pc1_2, cd1_1, cd1_2, orientat)

char	fname[ARB]
real	pc1_1, pc1_2, cd1_1, cd1_2, orientat
int	ngroups

pointer gtab, cptr
char	tabname[SZ_FNAME]
int	extnum
int	chip

pointer	tbtopn()

begin
	# We want to access FITS extension '[1]' as a table
	extnum = 1

	# Create new filename with FITS extension of '[1]'
	call get_fitextn(fname, extnum, tabname)
	
	# Open table
	gtab = tbtopn(tabname, READ_ONLY, NULL)

	# Get information from each column here, from rownum = group
	# Consider adding a check to make sure the column is found	
	call tbcfnd (gtab, "CD1_1", cptr, 1)
	chip = 1
	call tbegtr (gtab, cptr, chip, pc1_1)
	if(ngroups > 1) {
		chip = 2
		call tbegtr (gtab, cptr, chip, cd1_1)
	} else {
		cd1_1 = pc1_1
		pc1_1 = 0.
	} 
	call tbcfnd (gtab, "CD1_2", cptr, 1)
	chip = 1
	call tbegtr (gtab, cptr, chip, pc1_2)
	if (ngroups > 1) {
		chip = 2
		call tbegtr (gtab, cptr, chip, cd1_2)
	} else {
		cd1_2 = pc1_2
		pc1_2 = 0.
	}
	call tbcfnd (gtab, "ORIENTAT", cptr, 1)
	chip = ngroups
	call tbegtr (gtab, cptr, chip, orientat)	
	
	# Close table
	call tbtclo(gtab)
end
