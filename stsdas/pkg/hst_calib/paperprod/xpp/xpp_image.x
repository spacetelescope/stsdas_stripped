include	<imhdr.h>
include	"xpp.h"

define	SZ_HIST		4000
define	UP_HIST		0.99
define	LOW_HIST	0.02
define	HIST_BINS	1000

# Plot a gray scale image  and exposure information for FOC IMAGEs 
#
# Added explicit use of '.hhh' for 'tmp' image - 2 Jul 97 WJH
#
# Added generalized routine to check PDQ QUALITY keyword - 14 Aug 97 WJH
#
procedure xpp_image ()


char	rootname[SZ_FNAME]
char	output[SZ_FNAME]
char	ftype[SZ_FNAME]
bool	calprt
int     page

int	fd
char	fname[SZ_FNAME]
char	pdqname[SZ_FNAME]
char	img_name[SZ_FNAME]
char	log_img[SZ_FNAME]
char	img_root[SZ_FNAME]

pointer	c0h,shp, limg, bpt
char	shp_ext[SZ_EXT]
char	c0h_ext[SZ_EXT]
char	c1h_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	img_ext[SZ_EXT]
char	linenum[SZ_LINENUM]
char	propid[SZ_LINE]
char	targname[SZ_TARG]
char	targ1[MAX_TARG]
char	targ2[MAX_TARG]
char	filter1[SZ_XFILTNAM]
char	filter2[SZ_XFILTNAM]
real	exptime
real	targx, targy
char	detector[SZ_LINENUM]
char	pxformt[SZ_LINENUM]
char	smmmode[SZ_LINENUM]
char    pdftitle[SZ_LINE]
int     pgsect

double	ra, dec
real	orientat
char	piname[SZ_TARG]
real	grpmin, grpmax
char	dateobs[SZ_DATEOBS], timeobs[SZ_DATEOBS]
char	full_date[SZ_FDATE]

char	qual[SZ_LINE]
char	qualcom[SZ_LINE, 10]
int	ncom
bool	filt_split

# related to checking the calibration data quality flag
int	calcheck
char    ref[SZ_PED, SZ_EXT]
char    ped[SZ_PED, SZ_EXT]
char    cflags[SZ_LINE, SZ_EXT]
int     nref

real	hist[SZ_HIST]

real	yoff, infocol
int 	bottom
int	naxis1, naxis2, totpix
real	pscale, cd1_1, cd1_2
real 	vscale, width, npix
bool	mir_revr
bool	neg_img, BLANK

real	deltax, deltay, lx_min, lx_max, ly_min, ly_max

pointer	immap()
int	open()
bool	clgetb()
int     clgeti()
real	imgetr()
bool	imgetb()
double	imgetd()
int	strmatch()
bool	streq()
bool	read_xfilt()
pointer	xpp_c0h()
pointer	imgs2r()
int	xpp_calflag()
bool	qual_check()

begin
	# set up memory debugging here
	#call memlog("Starting XPP_IMAGE...")

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
	    call strcpy ("_c1f.fits", c1h_ext, SZ_EXT)
	    call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
	} else {
	    call strcpy (".shh", shp_ext, SZ_EXT) 
	    call strcpy (".d0h", d0h_ext, SZ_EXT) 
	    call strcpy (".c0h", c0h_ext, SZ_EXT) 
	    call strcpy (".c1h", c1h_ext, SZ_EXT) 
	}

	# construct file names
	call strcpy (rootname, fname, SZ_FNAME)
	call strcat (shp_ext, fname, SZ_FNAME)
	shp = immap (fname, READ_ONLY, 0)

	
	c0h = xpp_c0h (rootname, img_name, d0h_ext, c0h_ext, c1h_ext, ftype, img_ext)

	# read keywords
	# read target name
	call imgstr (shp, "TARGNAME", targname, SZ_TARG)
	call split_str (targname, targ1, targ2, MAX_TARG)

        call imgstr (shp, "ROOTNAME", img_root, SZ_FNAME)
	call strcat (img_ext, img_root, SZ_FNAME)
        call imgstr (c0h, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (c0h, "SMMMODE", smmmode, SZ_LINENUM)
        call imgstr (c0h, "PROPOSID", propid, SZ_LINE)
	call imgstr (shp, "PR_INV_L", piname, SZ_TARG)
	call imgstr (c0h, "PXFORMT", pxformt, SZ_LINENUM)
	call imgstr (shp, "CONFIG", detector, SZ_LINENUM)

	ra = imgetd (shp, "RA_TARG")
	dec = imgetd (shp, "DEC_TARG")
	exptime = imgetr (c0h, "EXPTIME")
	orientat = imgetr (c0h, "ORIENTAT")

	# read in filter info here
	# read_xfilt will always return at least NULL for filter2
	filt_split = read_xfilt(c0h, filter1, filter2)

	naxis1 = IM_LEN(c0h, 1)
	naxis2 = IM_LEN(c0h, 2)
	totpix  = naxis1 * naxis2
	mir_revr = imgetb (c0h, "MIR_REVR")

	if(strmatch(targname,"INTFLAT") != 0) {
		call get_hist(c0h, UP_HIST, LOW_HIST, hist, HIST_BINS, grpmin, grpmax)
		call strcpy(img_name, log_img, SZ_FNAME)
	} else {
		# Take the log10(c0h + 1.0) and output to 'limg'/log_img
		#
		call mktemp("tmp$PPXtmp", log_img, SZ_FNAME)
		call strcat(".hhh",log_img,SZ_FNAME)

		limg = immap (log_img, NEW_COPY, c0h)
		call log10p_img(c0h, limg)
	
		# Determine Min and Max
		#
		bpt = imgs2r (limg, 1, naxis1, 1, naxis2)
		call alimr(Memr[bpt], totpix, grpmin, grpmax)
		call imunmap (limg)

	}	
	if (grpmax < 2.0) grpmax = 2.0
	if (grpmin < 0.0) grpmin = 0.0

	call imgstr (c0h, "DATE-OBS", dateobs, SZ_DATEOBS)
	call date_str (dateobs, full_date)

	call imgstr (c0h, "TIME-OBS", timeobs, SZ_DATEOBS)

	# read each group to find the maximum and minimum
	# and set the upper and lower limits for the histogram
	BLANK = false
	
	# if we have more than 1 group, open up a WF image to determine
	# the plate scale...
	cd1_1 = imgetr (c0h, "CD1_1")
	cd1_2 = imgetr (c0h, "CD1_2")

	if (grpmin == grpmax ) BLANK = TRUE	    
	
	# set the switch for negative image display
	neg_img = TRUE 

	# determine the status of the calibration quality flag
	calcheck = xpp_calflag (c0h, ref, ped, cflags, nref)

	# close files
	#call imunmap (c0h)
	#call imunmap (shp)

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# start a new page
	call pp_erase (fd)

    # Create section heading here for bookmarks
    # Start by setting the number of pages in this section
    pgsect = -2
    
    call pp_pdfsection (fd, page, pgsect, rootname)
    
    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Full FOV")
    call pp_pdfbook(fd, page, pdftitle)

	call fprintf (fd, "vpage 0. 1. 0. 1.; location 0. 1. 0. 1.\n")

	# draw the banner
	call obs_banner (fd, linenum, img_root, propid, "FOC", 0.)

	call fprintf (fd, "reset\n")
	call fprintf (fd, "vpage 0.26 1.0 0.01 0.99\n")

	
	# plot the gray scale image
	
	# set the number of pixels for determining the plate scale
	npix = max(naxis1, naxis2)

	if (!BLANK && npix > 0) {
	  if (naxis1 == naxis2) {
#	  	call fprintf (fd, "location 0.25 1. 0.0 0.75\n")
		lx_min = 0.25
		lx_max = 1
		ly_min = 0.0
		ly_max = 0.75
		
		deltax = 0.
		deltay = 0.
	  } else {
		call fprintf(fd, "location 0.25 1. 0.0 0.75\n")
		call fprintf(fd, "vmove 0.249 0; vdraw 0.249 0.751\n")
		call fprintf(fd, "vdraw 1.001 0.751; vdraw 1.001 -0.001; vdraw 0.249 -0.001\n")

		deltax = (0.75 - ((naxis1/npix) * 0.75))/2.
		deltay = (0.75 - ((naxis2/npix) * 0.75))/2.

		lx_min = 0.25 + deltax
		lx_max = 1 - deltax
		ly_min = deltay
		ly_max = 0.75 - deltay
#		call fprintf (fd, "location %f %f %f %f\n")
		
	  } 
	  
	  call fprintf(fd, "expand 1.0\n")
	  # display unrotated single chip at full scale
	  call fprintf (fd, "zsection %s\n")
		call pargstr (log_img)
		#call pargstr (img_name)
	  call fprintf (fd, "fitpix %f %f %f %f\n")
		call pargr(lx_min)
		call pargr(lx_max)
		call pargr(ly_min)
		call pargr(ly_max)

#	  call fprintf (fd, "fitpix ; limits; zrange %6.1f %6.1f\n")
	  call fprintf (fd, "limits; zrange %6.1f %6.1f\n")
		call pargr (grpmax)
		call pargr (grpmin)
	  call fprintf (fd, "pixmap\n")

	}
	# place markers at target position in image
	if (ra != 0. && dec != 0. && strmatch(smmmode,"INBEAM") == 0 ) {

		call xtarg_pos (fd, img_name, ra, dec, targx, targy, naxis1, naxis2, deltax, deltay)
	 } else {
		if (strmatch(smmmode,"INBEAM") == 0)
			call eprintf ("No pointed target for observation...\n")
	 }
	# calculate the plate scale in arc seconds		
	pscale = sqrt(cd1_1**2 + cd1_2**2) * 3600.


	# put in the grey scale bar
 	call pp_gsbar (fd, 0.05, 0.45, 0.87, 0.9, grpmin, grpmax, 0.6, neg_img)

	if (strmatch(targname, "INTFLAT") == 0) {
		# put in labels for intermediate values
		call gsbar_xlabel(fd, 0.05, 0.45, 0.85, grpmax, 0.6) 
	}

	# calculate the width across the vpage each image pixel covers
	width = 0.74 * 0.75
	
 	vscale = width / npix

	# take care of problems with INTFLAT and DARK images having
	# invalid CD matrices.  
	# If we have a valid plate scale, put one on the page, 
	# otherwise leave it off entirely.
	if (pscale < 1.)  {
		# draw the compass
		call pp_compass (fd, 0.35, 0.55, 0.065, orientat, mir_revr)

		# draw the plate scale
		call pp_pscale (fd, 0.35, 0.725, 0.1, pscale, vscale)
	}

	# put in exposure information on left side of page
	# set up parameters for page size and line limits
	bottom = 35
	yoff = 0.
	call fprintf (fd, "reset; location 0. 1. 0. 1.\n")
	call fprintf (fd, "vpage 0.01 0.25 0.01 0.9\n")
	call fprintf (fd, "limits 0 40 %d 0\n")
		call pargi (bottom)
	call fprintf (fd, "move 40 0; draw 40 %d\n")
		call pargi (bottom)

	# use full image name plus extension as column label
	call fprintf (fd, "move 20 0; expand 1; justify 2\n")
	call fprintf (fd, "label '%s'\n")
		call pargstr (img_root)	
	call fprintf (fd, "justify 3\n")
	yoff = yoff + 2

	# set scaling factor for remaining information
	# and use column 16 for the start of the output
	call fprintf (fd, "expand 0.7\n")
	infocol = 16.

	# start printing out information for column here
	call pp_label (fd, 0., yoff, "PI:")
	call pp_label (fd, infocol, yoff, piname)
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
	call pp_label (fd, 0., yoff, "Config:")
	call pp_label (fd, infocol, yoff, detector)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Filters:")
	call pp_label (fd, infocol, yoff, filter1)
	if (filt_split) {
		yoff = yoff + 1
		call pp_label (fd, infocol, yoff, filter2)
	}
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Exp. Time:")
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
	call pp_label (fd, infocol, yoff, timeobs)
	# Put in an extra line here to separate the data sections
	yoff = yoff + 2
	
	call ximg_info(c0h,shp, fd, infocol, yoff, ra, dec, orientat)

	# print out legend for Quality flags at bottom of column
	yoff = bottom - 6
	call pp_label (fd, 0., yoff, "QUALITY FLAGS:")
	yoff = yoff + 1
	call fprintf (fd, "expand 0.65\n")

	call pp_label (fd, 0., yoff, "Observation:")

	# determine observation quality flag from pdq file
	# first, let's see if the 'quality' keyword from the
	# PDQ file say's 'OK' or 'NO-EVAL'
	# otherwise, give an indication of an error
	if (streq(ftype,"geis")) {
		call pp_qual(rootname, qual, qualcom[1,1], ncom)
	} else {
		call strcpy(rootname, pdqname, SZ_FNAME)
		call strcat("_pdq.fits",pdqname, SZ_FNAME)
		call pp_qualfits(pdqname, qual, qualcom[1,1], ncom)
	}
	#call pp_qual (rootname, qual, qualcom[1,1], ncom)

	if (qual[1] != EOS) {
	    if( qual_check(qual, "OK; NO-EVAL, NO-TLM; TM_GAP")) {

		#call eprintf("Quality check: OK\n")
		    call fprintf (fd, "ptype 25 0\n")
	    } else {

		#call eprintf("Quality check: Not OK!\n")
		    call fprintf (fd, "ptype 25 3\n")
	    }
	
	    call fprintf (fd, "move 35 %4.1f; dot\n")
		call pargr (yoff)
	}

	yoff = yoff + 1

	call pp_label (fd, 0., yoff, "Calibration:")
	if (calcheck >= 0){
		if(calcheck > 0)  
	    		call fprintf (fd, "ptype 25 3\n")
		else 
	   		call fprintf (fd, "ptype 25 0\n")
	call fprintf (fd, "move 35 %4.1f; dot\n")
		call pargr (yoff)
	}

	yoff = yoff + 2
	
	call pp_label (fd, 0., yoff, "OK:")
	call fprintf (fd, "ptype 25 0; move 7 %4.1f; dot\n")
		call pargr (yoff)
	call pp_label (fd, 15., yoff, "Not OK:")
	call fprintf (fd, "ptype 25 3; move 27 %4.1f; dot\n")
		call pargr (yoff)
	yoff = yoff + 1
	call pp_label (fd, 0., yoff, "Unknown or File Missing: Blank")

    
    # Finished with this page, advance page counter
    page = page + 1
    # Update the number of pages created
    call clputi("page", page)

	# finished with image display pages
	# close files
	call imunmap (c0h)
	call imunmap (shp)

	call close (fd)
end



procedure xtarg_pos (fd, img, r, d, tx, ty, nx, ny, deltax, deltay)

char 	img[SZ_FNAME]
#pointer img
int 	fd
double 	r, d
real 	tx, ty, kx, ky
int	nx, ny
bool 	onchip

real 	offx, offy, scale
real 	chx, chy
real 	theta1, theta2
char 	command[SZ_LINE]
real	mx, my
real	deltax, deltay

real 	clgetr()

begin

	# we can NOT assume that the target position is in the chip
	# corresponding to the selected aperture.  Therefore, we determine
	# what chip the target is in independent of the aperture information.

	onchip = false
	# build command string for call 'rd2xy' (since it can handle
	#	single group images)
	#   rd2xy w1234567t.d0h[n] "19:12:23.1" "33:22:11.123" hour="yes"
	    call sprintf (command, SZ_LINE, "rd2xy %s %.6f %.6f hour=no >& dev$null")
		call pargstr (img)
		call pargd (r)
		call pargd (d)

	    # now, run 'rd2xy'
	    call clcmdw (command)
	    # is the target in this chip?
 		kx = clgetr("rd2xy.x")
		ky = clgetr("rd2xy.y")

	    if (kx >= 0. && kx <= nx && ky >= 0. && ky <= ny) {
	        # if so, copy the results from rd2xy to variables
		tx = kx
		ty = ky
		onchip = true
	    }

	# check to make sure target coordinates were found within image
	# if not, set variables to harmless default values and leave 
	#	function without printing out target markers
	if (!onchip) {
		call printf ("Target coordinates not within image...\n")
		tx = 1.
		ty = 1.
	
 	} else { 
		# set up conversion factors for rotation of coordinates
		# scaled to 800 pixel wide image	
		theta2 = ty / max(nx,ny) 
		theta1 = tx / max(nx,ny)

		# set scale to 0.75 and offsets to 0.  
			scale = 0.75
			offx = 0.25
			offy = 0.
		# rotate position based on chip orientation
			chx = (theta1 * scale)  + deltax + offx
			chy = (theta2 * scale)  + deltay + offy

	# plot a triangle at target position in vpage coordinates
	# need to change to marks at edge of image display
		call fprintf (fd, "location 0. 1. 0. 1.\n")
		
			mx = 0.24
			my = 0.765			

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


procedure gsbar_xlabel (fd, vleft, vright, vbottom, max, label_size)

int     fd
real    vleft, vright, vbottom    # the location of the gray scale bar
real    max        # data extremes represented by the gray scale bar
real    label_size      # the size of the label

real	scale_len, pos, expnt
int	i, limit

begin
	call fprintf(fd,"justify 3; expand %0.3f\n")
		call pargr (label_size)

	call fprintf(fd,"vmove %0.3f %0.3f; label log(Counts)\n")
		call pargr(vright + 0.03)
		call pargr(vbottom - 0.02)
	call fprintf(fd,"vmove %0.3f %0.3f; label Counts \n")
		call pargr(vright + 0.03)
		call pargr(vbottom - 0.04)
	call fprintf(fd,"justify 2\n")

	limit = int(max)

	scale_len = (vright  - vleft) / max

	do i = 0, limit {
		expnt = 10**(i) 
		pos = log10(expnt +1.) * scale_len + vleft
		call fprintf(fd,"vmove %0.3f %0.3f; vdraw %0.3f %0.3f\n")
			call pargr(pos)
			call pargr(vbottom - 0.005)
			call pargr(pos)
			call pargr(vbottom - 0.015)
		call fprintf(fd,"vmove %0.3f %0.3f; label '%0.2g'\n")
			call pargr(pos)
			call pargr(vbottom - 0.02)
			call pargr(log10(expnt))
		call fprintf(fd,"vmove %0.3f %0.3f; label '%0.2g'\n")
			call pargr(pos)
			call pargr(vbottom - 0.04)
			call pargr(expnt)

	}
	
	call fprintf(fd,"justify 3\n")
end


procedure ximg_info (c0h, shp, fd, infocol, yoff, ra, dec, orientat)

pointer	c0h, shp
int	fd
real	infocol
real 	yoff
bool	POSTARG

double	ra, dec
real	orientat
real	magv, colbv
char	spectype[SZ_SPEC]
char	shtmode[SZ_LINENUM]

real	imgetr()
int	strsearch()


begin
	POSTARG = false

	# Read in variables to be printed out...
        call imgstr (c0h, "SHTMODE", shtmode, SZ_LINENUM)
	magv = imgetr (shp, "MAG_V")
	colbv = imgetr (shp, "COL_B_V")
	call imgstr (shp, "SP_TYPE", spectype, SZ_SPEC)


	if (strsearch(shtmode,"INBEAM") == 0) {
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
	}
	if (POSTARG) {
		call pp_label (fd, 0., yoff, "X POSTARG:")
		call pp_label (fd, infocol, yoff, "N/A")
		yoff = yoff + 1
		call pp_label (fd, 0., yoff, "Y POSTARG:")
		call pp_label (fd, infocol, yoff, "N/A")
		yoff = yoff + 1
	}
	call pp_label (fd, 0., yoff, "Posn. Angle:")
	call pp_rlabel (fd, infocol, yoff, orientat)
	yoff = yoff + 1
	if (strsearch(shtmode,"INBEAM") == 0) {
		call pp_label (fd, 0., yoff, "V:")
		call pp_rlabel (fd, infocol, yoff, magv)
		yoff = yoff + 1
		call pp_label (fd, 0., yoff, "B-V:")
		call pp_rlabel (fd, infocol, yoff, colbv)
		yoff = yoff + 1
		call pp_label (fd, 0., yoff, "Spec. Type:")
		call pp_label (fd, infocol, yoff, spectype)
	}	

end
