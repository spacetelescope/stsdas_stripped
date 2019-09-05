# plot the gray scale ACQ images for NICMOS

include	<imhdr.h>
include	"npp.h"

procedure npp_acq (im, fname, root, file, fd)

pointer	im			# input image pointer
char	fname[SZ_FNAME]		# file name to use
char	root[SZ_FNAME]		# file root name
int	file			# file used
int     fd                      # output file pointer

char	target[SZ_LINE]     	# target name
char    bunit[SZ_LINE]          # brightness unit
char	fname2[SZ_FNAME]	# second image name
real	cd[2,2]			# the CD matrix from the input image
real	hist[SZ_HIST]		# the histogram of usable pixels
real	zmax, zmin		# max/min used in gray scale image plot.
int	xdim, ydim		# image dimensions
int	x1, x2
real	bx1, bx2, by1, by2
int	j

real	imgetr()
bool	streq()
	
begin

	# construct the second image name
	call strcpy (root, fname2, SZ_FNAME)
	if (file == RAW)  
	    call strcat ("_raw.fits[sci,2]", fname2, SZ_FNAME)
	else if (file == CAL)  
	    call strcat ("_cal.fits[sci,2]", fname2, SZ_FNAME)

	# print the exposure summary
	call npp_expsum (im, fd)

	# initialize the page
	call fprintf (fd, "vpage 0.03 0.97 0.03 0.97\n")
        call fprintf (fd, "fontset hard\n")
	call fprintf (fd, "expand 0.65\n")

        # get dimensions of the image
        xdim = IM_LEN(im, 1)
        ydim = IM_LEN(im, 2)

	xdim = max (xdim, ydim)
        x1 = 10**(int(log10(real(xdim))))
        x2 = int(xdim/x1) * x1

	# determine the brightness range 
	call get_hist (im, 0.99, 0.01, hist, 2000, zmin, zmax)

	# plot the images
	do j = 1, 2 {
	    call fprintf (fd, "zsection %s\n")
	        if (j == 1) call pargstr (fname)
	        if (j == 2) call pargstr (fname2)
	    if (j == 1) call fprintf (fd, "fitpix 0.06 0.31 0.25 0.65\n")
	    if (j == 2) call fprintf (fd, "fitpix 0.40 0.65 0.25 0.65\n")

	    call fprintf (fd, "limits\n")

	    # this is to avoid "divide by zero" error in igi
	    call fprintf (fd, "zrange %g %g\n")
	        if (zmin == zmax)
	            call pargr (zmin+0.1)
		else
	            call pargr (zmax)
	        call pargr (zmin)
	    call fprintf (fd, "pixmap\n")

	    # plot axes
	    call fprintf (fd, "ticksize %d %d %d %d\n")
	        call pargi (x2/20)
	        call pargi (x2/2)
	        call pargi (x2/20)
	        call pargi (x2/2)
	    call fprintf (fd, "box\n")

	    # plot labels and title
	    call fprintf (fd, "xlabel 'Pixel'\n")
	    call fprintf (fd, "ylabel 'Pixel'\n")

	    call imgstr (im, "TARGNAME", target, SZ_LINE)
	    call fprintf (fd, "title '%s ACQ Image for %s'\n")
	        if (j == 1) call pargstr ("First")
	        if (j == 2) call pargstr ("Second")
	        call pargstr (target)
	}

	# draw the compass
	cd[1,1] = imgetr (im, "CD1_1")
	cd[1,2] = imgetr (im, "CD1_2")
	cd[2,1] = imgetr (im, "CD2_1")
	cd[2,2] = imgetr (im, "CD2_2")
	call pp_cdcompass (fd, 0.15, 0.84, 0.045, cd)

	# draw the gray scale bar
	bx1 = 0.25
	bx2 = 0.45
	by1 = 0.82
	by2 = 0.86
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
end
