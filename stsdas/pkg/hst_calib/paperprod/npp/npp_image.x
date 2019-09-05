# plot the gray scale exposure image for NICMOS

include	<imhdr.h>
include	"npp.h"

procedure npp_image (im, fname, file, fd)

pointer	im			# input image pointer
char	fname[SZ_FNAME]		# file name to use
int	file			# file used
int     fd                      # output file pointer

char	target[SZ_LINE]     	# target name
char	bunit[SZ_LINE]     	# brightness unit
real	cd[2,2]			# the CD matrix from the input image
real	det			# determinant of the CD matrix
real	hist[SZ_HIST]		# the histogram of usable pixels
real	zmax, zmin		# max/min used in gray scale image plot.
int	xdim, ydim		# image dimensions
int	camera
real	photflam
real	imscale
real	xmax, ymax
double	ra, dec
real	x, y
real	xlen, ylen, x0, y0, maxlen
char	filter[SZ_LINE]
char	targname[SZ_LINE]
char	command[SZ_LINE]
int	x1, x2
real	bx1, bx2, by1, by2

real	imgetr()
double	imgetd()
int	imgeti()
real	clgetr()
int	strmatch()
bool	streq()
	
begin

	# print the exposure summary
	call npp_expsum (im, fd)

	# initialize the page
	call fprintf (fd, "vpage 0.03 0.97 0.03 0.97\n")
        call fprintf (fd, "fontset hard\n")
	call fprintf (fd, "expand 0.65\n")

        # get dimensions of the image
        xdim = IM_LEN(im, 1)
        ydim = IM_LEN(im, 2)

	# read the CD matrix
	cd[1,1] = imgetr (im, "CD1_1")
	cd[1,2] = imgetr (im, "CD1_2")
	cd[2,1] = imgetr (im, "CD2_1")
	cd[2,2] = imgetr (im, "CD2_2")
	det = cd[1,1]*cd[2,2] - cd[1,2]*cd[2,1]

	xdim = max (xdim, ydim)
        x1 = 10**(int(log10(real(xdim))))
        x2 = int(xdim/x1) * x1

	# determine the plate scale
	camera = imgeti (im, "CAMERA")
        if (camera == 1) imscale = 0.043
        else if (camera == 2) imscale = 0.075
        else if (camera == 3) imscale = 0.20
	xmax = imscale * real(xdim) / 2.
	ymax = imscale * real(ydim) / 2.

	# read other keywords
	call imgstr (im, "FILTER", filter, SZ_LINE)
	call imgstr (im, "TARGNAME", targname, SZ_LINE)

	# determine the brightness range 
	call get_hist (im, 0.99, 0.01, hist, 2000, zmin, zmax)

	# plot the image
	maxlen = 0.55
	if (xdim >= ydim) {
	    xlen = maxlen
	    ylen = maxlen * ASPECT * real(ydim) / real(xdim)
	} else {
	    xlen = maxlen * real(xdim) / real(ydim)
	    ylen = maxlen * ASPECT
	}
	x0 = 0.06 + (maxlen - xlen)/2.
	y0 = 0.06 
	
	call fprintf (fd, "zsection %s\n")
	    call pargstr (fname)
	call fprintf (fd, "fitpix %0.4f %0.4f %0.4f %0.4f\n")
	    call pargr (x0)
	    call pargr (x0+xlen)
	    call pargr (y0)
	    call pargr (y0+ylen)

	call fprintf (fd, "limits\n")

        # this is to avoid "divide by zero" error in igi
        call fprintf (fd, "zrange %g %g\n")
            if (zmin == zmax)
                call pargr (zmin+0.1)
            else
                call pargr (zmax)
            call pargr (zmin)

	call fprintf (fd, "pixmap\n")

	# if the CD matrix is singular, or the filter name is empty (i.e. a 
	# dark observation) do not plot the wcs labels, or the compass
	if (det == 0. || strmatch(filter, "BLANK") != 0) {
	    call fprintf (fd, "box\n")
	    call fprintf (fd, "xlabel 'Pixel'\n")
	    call fprintf (fd, "ylabel 'Pixel'\n")
	} else {
	    call fprintf (fd, "wcslab\n")

	    # draw the compass
	    call pp_cdcompass (fd, 0.05, 0.845, 0.035, cd)

	    call fprintf (fd, "reset\n")
	    call fprintf (fd, "vpage 0.03 0.97 0.03 0.97\n")
	    call fprintf (fd, "fontset hard\n")
	    call fprintf (fd, "expand 0.65\n")
	}

	# plot axes
	#call fprintf (fd, "ticksize %d %d %d %d\n")
	    #call pargi (x2/20)
	    #call pargi (x2/2)
	    #call pargi (x2/20)
	    #call pargi (x2/2)
	##call fprintf (fd, "limits %f %f %f %f\n")
	    ##call pargr (-xmax)
	    ##call pargr (xmax)
	    ##call pargr (-ymax)
	    ##call pargr (ymax)
	##call fprintf (fd, "box\n")

	# plot labels and title
	##call fprintf (fd, "xlabel 'Arcsec'\n")
	##call fprintf (fd, "ylabel 'Arcsec'\n")

	call imgstr (im, "TARGNAME", target, SZ_LINE)
        call fprintf (fd, "vmove %0.4f %0.4f; justify 8\n")
            call pargr (x0+xlen/2.)
            call pargr (y0+ylen+0.02)
	call fprintf (fd, "label '%s: %s'\n")
	    call pargstr (target)
	    if (file == RAW)
	        call pargstr ("Raw Image")
	    else
	        call pargstr ("Final Calibrated Image")

	# draw the target marker
	if (det == 0.) {
	    call printf ("%s: sigular CD matrix, no target marker is generated.\n")
		call pargstr (fname)
	} else if (strmatch(filter, "BLANK") != 0) {
	    call printf ("%s: dark observation, no target marker is generated.\n")
		call pargstr (fname)
	} else {
	    ra = imgetd (im, "RA_TARG")
	    dec = imgetd (im, "DEC_TARG")
            call sprintf (command, SZ_LINE, "rd2xy %s %.6f %.6f hour=no >& dev$null")
                call pargstr (fname)
                call pargd (ra)
                call pargd (dec)
 
            # now, run 'rd2xy'
            call clcmdw (command)
            x = clgetr("rd2xy.x")
            y = clgetr("rd2xy.y")
 
            # is the target in the image?
            if (x >= 0. && x <= xdim && y >= 0. && y <= ydim) {
	        x = x0 + xlen * (x - 0.5)/real(xdim)
	        y = y0 + ylen * (y - 0.5)/real(ydim)

                # put in marker TARGET's X Position
                call fprintf (fd, "ptype 3 3; expand 0.7\n")
                call fprintf (fd, "vmove %0.4f %0.4f\n")
                    call pargr (x)
                    call pargr (y0+ylen+0.01)
                call fprintf (fd, "angle 60; dot\n")
 
                # put in marker TARGET's Y Position
                call fprintf (fd, "vmove %0.4f %0.4f\n")
                    call pargr (x0+xlen+0.01)
                    call pargr (y)
                call fprintf (fd, "angle -30; dot\n")
                call fprintf (fd, "angle 0; justify 6; label '  Target'\n")
                call fprintf (fd, "expand 0.65\n")
	    } else {
	        call printf ("%s: target is outside the image x = %0.1f y = %0.1f\n")
		    call pargstr (fname)
	            call pargr (x)
	            call pargr (y)
	    }
	}


	# draw the gray scale bar
	bx1 = 0.35
	bx2 = 0.55
	by1 = y0 + maxlen * ASPECT + 0.07
	by2 = by1 + 0.04
	call pp_gsbar (fd, bx1, bx2, by1, by2, zmin, zmax, 0.55, true)
        call fprintf (fd, "expand 0.55\n")
        call fprintf (fd, "justify 3\n")

        call imgstr (im, "BUNIT", bunit, SZ_LINE)
        if (streq(bunit, "COUNTS"))
            call fprintf (fd, "vmove %0.3f %0.3f; label 'Counts'\n")
	else
            call fprintf (fd, "vmove %0.3f %0.3f; label 'Count rate'\n")
                call pargr (bx2+0.05)
                call pargr (by1-0.01)

	# label the flux
	photflam = imgetr (im, "PHOTFNU")
	if (photflam > 0.) {
            call fprintf (fd, "justify 9\n")
            call fprintf (fd, "vmove %0.3f %0.3f; label 'Flux (mJy)'\n")
                call pargr (bx2+0.05)
                call pargr (by2+0.01)
            call fprintf (fd, "justify 8\n")
            call fprintf (fd, "vmove %0.3f %0.3f; label '%0.5g'\n")
                call pargr (bx1)
                call pargr (by2+0.01)
                call pargr (photflam*zmin*1000.)
            call fprintf (fd, "vmove %0.3f %0.3f; label '%0.5g'\n")
                call pargr (bx2)
                call pargr (by2+0.01)
                call pargr (photflam*zmax*1000.)
	} else {
	    call printf ("%s: PHOTFNU <= 0, no flux label for the gray scale bar.\n")
		call pargstr (fname)
	}
end
