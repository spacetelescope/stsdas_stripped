# plot the gray scale exposure image for NICMOS

include	<imhdr.h>
include	"npp.h"

define	NX	8
define	NY	4

procedure npp_mos (im, fname, root, fd, tmproot, bstr, timetag, page)

pointer	im			# input image pointer
char	fname[ARB]		# file name to use
char	root[ARB]		# input file root name
int     fd                      # output file pointer
char	tmproot[ARB]		# temporary file root name
char    bstr[SZ_LINE,ARB]       # strings for the banner
char    timetag[ARB]
int     page

char	asnname[SZ_FNAME]	# asn file name
char	bklist[SZ_FNAME]	# file of a list of background file root names
char	fave[SZ_FNAME]		# averaged background image
char	fexp[SZ_FNAME]		# individual exposure image
char	dummy[SZ_FNAME]	
char	command[SZ_LINE]	# CL command line
pointer	imbk, imexp
int	nbk, nexp		# number of backgrounds and number of exposures
int     fd_bk                   # background list file pointer
char	bkroot[SZ_FNAME, MAX_OBS]	# background root names
char	exproot[SZ_FNAME, MAX_OBS]	# exposure root names
char	target[SZ_LINE]     	# target name
real	hist[SZ_HIST]		# the histogram of usable pixels
real	zmax, zmin		# max/min used in gray scale image plot.
int	xdim1, ydim1		# image dimensions
int	xdim2, ydim2		# image dimensions
int	x1, x2
int	numpos
real	dithsize, chopsize	# dither and chop size
char	pattern[SZ_LINE]	# dither/chop pattern
int	pattpos, npos[MAX_OBS]
real	xlen0, xlen1, xlen2, ylen1, ylen2
real	pgwidth, pgheight	# view port's width and height
real	width, height		# width and height of EACH thumbnail image
real	x0, y0			# initial position of the thumbnail plot
real	x, y, xx, yy		# position of the thumbnail plot
int	nx, ny			# no. of thumbnail images in each row and column
int	i, j, m, n, nn

char    pdftitle[SZ_LINE]

pointer	immap()
int	imgeti()
int	open()
int	access()
real	imgetr()
bool	strne()
bool	streq()

begin
	# maximum length of the box sides
	xlen0 = 0.4

	do i = 1, MAX_OBS
	    npos[i] = 0

	# decide which files to use by looking up the asn table
        call sprintf (asnname, SZ_FNAME, "%s_asn.fits")
            call pargstr (root)
	call npp_assoc (asnname, bkroot, nbk, exproot, nexp)

	# skip the mosaic/background page if pattern = NONE
	call imgstr (im, "PATTERN", pattern, SZ_LINE)
	if (strne(pattern, "NONE")) {

	    # plot the banner
            call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], bstr[1,4],
                                timetag, page)
        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "%s MOS Image")
            call pargstr(root)
        call pp_pdfbook(fd, page, pdftitle)

	    # initialize the page
	    call fprintf (fd, "vpage 0.03 0.97 0.03 0.97\n")
            call fprintf (fd, "fontset hard\n")
	    call fprintf (fd, "expand 0.65\n")

	    # draw the dither/chopping pattern
	    numpos = imgeti (im, "NUMPOS")
	    dithsize = imgetr (im, "DITHSIZE")
	    chopsize = imgetr (im, "CHOPSIZE")
	    call npp_dithchop (fd, pattern, numpos, dithsize, chopsize)

            # get dimensions of the image
            xdim1 = IM_LEN(im, 1)
            ydim1 = IM_LEN(im, 2)

	    xdim1 = max (xdim1, ydim1)
            x1 = 10**(int(log10(real(xdim1))))
            x2 = int(xdim1/x1) * x1

	    # determine the brightness range 
	    call get_hist (im, 0.99, 0.01, hist, 2000, zmin, zmax)

	    # determine the lengths in each dimension
	    if (xdim1 >= xdim2) {
	        xlen1 = xlen0
	        ylen1 = xlen0 * ASPECT * real(ydim1) / real(xdim1)
	    } else {
	        ylen1 = xlen0 * ASPECT
	        xlen1 = xlen0 * real(xdim1) / real(ydim1)
	    }

	    # plot the image
	    call fprintf (fd, "zsection %s\n")
	        call pargstr (fname)
	    call fprintf (fd, "fitpix 0.06 %0.4f 0.05 %0.4f\n")
	        call pargr (0.06+xlen1)
	        call pargr (0.05+ylen1)

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
	    call fprintf (fd, "angle 0\n")

	    call imgstr (im, "TARGNAME", target, SZ_LINE)
	    call fprintf (fd, "title '%s: Mosaicked Calibrated Image'\n")
	        call pargstr (target)

	    # Draw the subtracted background 
	    # ------------------------------
	    if (nbk > 0) {

	        # if there is only one background file, use it
	        if (nbk == 1) {
		    call strlwr (bkroot[1,1])
		    call sprintf (fave, SZ_FNAME, "%s_mos.fits[sci,1]")
		        call pargstr (bkroot[1,1])
	        } else {

		    # write out the background file names to a list
		    call sprintf (bklist, SZ_FNAME, "%s_bk.list")
		        call pargstr (tmproot)
		    call sprintf (fave, SZ_FNAME, "%s_ave.hhh")
		        call pargstr (tmproot)
		    fd_bk = open (bklist, NEW_FILE, TEXT_FILE)
		    do i = 1, nbk {
		        call strlwr (bkroot[1,i])
		        call sprintf (dummy, SZ_FNAME, "%s_mos.fits")
			    call pargstr (bkroot[1,i])
		        if (access (dummy, 0, 0) == NO) {
			    call printf ("Can't access MOS file '%s'\n")
			        call pargstr (dummy)
		        } else {
		            call fprintf (fd_bk, "%s[sci,1]\n")
			        call pargstr (dummy)
		        }
		    }

		    call close (fd_bk)

                    # use CL command "imsum"
                    call sprintf (command, SZ_LINE, "imsum @%s %s option=\"average\" low_reject=0. high_reject=0. mode=h >& dev$null")
                        call pargstr (bklist)
                        call pargstr (fave)
                    call clcmdw (command)

		    call delete (bklist)
	        }

	        imbk = immap (fave, READ_ONLY, 0)

                # get dimensions of the image
                xdim2 = IM_LEN(imbk, 1)
                ydim2 = IM_LEN(imbk, 2)
    
	        xdim2 = max (xdim2, ydim2)
                x1 = 10**(int(log10(real(xdim2))))
                x2 = int(xdim2/x1) * x1

	        # determine the brightness range 
	        call get_hist (imbk, 0.99, 0.01, hist, 2000, zmin, zmax)

	        call imunmap (imbk)

	        # this image will never be larger than the first image, so 
		# use the scale.
	        xlen2 = xlen1* real(xdim2) / real(xdim1)
	        ylen2 = ylen1* real(ydim2) / real(ydim1)

	        # plot the image
	        call fprintf (fd, "zsection %s\n")
	            call pargstr (fave)
	        call fprintf (fd, "fitpix 0.55 %0.4f 0.05 %0.4f\n")
	            call pargr (0.55+xlen2)
	            call pargr (0.05+ylen2)

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
	        call fprintf (fd, "angle 0\n")

	        call fprintf (fd, 
			"title 'Mosaicked Background (average of %d files)'\n")
	            call pargi (nbk)
	    }
	}

	# plot the thumbnail gray scale images for individual exposures
	# =============================================================
	if (streq (pattern, "NONE") && imgeti (im, "NUMITER") <= 1) return
	pgwidth = 0.95
	pgheight = 0.90
	nx = NX
	ny = NY

	width = pgwidth / float(nx)
	height = pgheight / float(ny)
	x0 = 0.03
	y0 = 0.05 + pgheight - height

	nn = 0
	do n = 1, nexp {
		
	    # open each image
	    call strlwr (exproot[1,n])
	    call sprintf (fexp, SZ_FNAME, "%s_cal.fits[sci,1]")
		    call pargstr (exproot[1,n])
	    iferr (imexp = immap (fexp, READ_ONLY, 0)) {
		call printf ("Can't access %s, skip\n")
		    call pargstr (fexp)
		next
	    }
	    
	    nn = nn + 1
	    m = mod (nn-1, nx*ny) + 1
	    i = mod (m-1, nx) + 1
	    j = (m-1) / nx + 1

	    x = x0 + (i-1) * width
	    y = y0 - (j-1) * height
	    xx = x + width
	    yy = y + width * ASPECT

	    # plot the banner
	    if (i == 1 && j == 1) {
                call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], bstr[1,4],
                                timetag, page)
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Pattern Thumbnails")
            call pp_pdfbook(fd, page, pdftitle)

	        call fprintf (fd, "reset\n")
	        call fprintf (fd, "fontset hard\n")
	        call fprintf (fd, "location 0 1 0 1\n")
	        call fprintf (fd, "vpage 0 1 0 1\n")
	        call fprintf (fd, "expand 0.45\n")
	    }

	    # determine the brightness range 
	    call get_hist (imexp, 0.99, 0.01, hist, 2000, zmin, zmax)

	    pattpos = imgeti (imexp, "PATT_POS")
	    if (pattpos <= 0) pattpos = 1
	    npos[pattpos] = npos[pattpos] + 1

	    # plot the image
	    # block average by 2
	    call fprintf (fd, "zsection %s 2\n")
		call pargstr (fexp)
	    call fprintf (fd, "fitpix %0.4f %0.4f %0.4f %0.4f\n")
	 	call pargr (x)
	 	call pargr (xx)
	 	call pargr (y)
	 	call pargr (yy)

	    call fprintf (fd, "limits\n")

            # this is to avoid "divide by zero" error in igi
            call fprintf (fd, "zrange %g %g\n")
                if (zmin == zmax)
                    call pargr (zmin+0.1)
                else
                    call pargr (zmax)
                call pargr (zmin)
	    call fprintf (fd, "pixmap\n")

	    # draw the box
	    call fprintf (fd, "vmove %0.4f %0.4f\n")
		call pargr (x)
		call pargr (y)
	    call fprintf (fd, "vdraw %0.4f %0.4f\n")
		call pargr (xx)
		call pargr (y)
	    call fprintf (fd, "vdraw %0.4f %0.4f\n")
		call pargr (xx)
		call pargr (yy)
	    call fprintf (fd, "vdraw %0.4f %0.4f\n")
		call pargr (x)
		call pargr (yy)
	    if (i == 1) {
	        call fprintf (fd, "vdraw %0.4f %0.4f\n")
		    call pargr (x)
		    call pargr (y)
	    }

	    # plot labels and title
	    if (i == 1 && j == 1) {
	    	call fprintf (fd, "justify 4\n")
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (x)
		    call pargr (y-0.015)
	    	call fprintf (fd, "label '1'\n")
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (xx)
		    call pargr (y-0.015)
	    	call fprintf (fd, "label '%d'\n")
		    call pargi (IM_LEN(imexp, 1))
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr ((x+xx)/2.)
		    call pargr (y-0.02)
	    	call fprintf (fd, "label 'Pixel'\n")
	        call fprintf (fd, "angle 90\n")
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (x-0.015)
		    call pargr (y)
	    	call fprintf (fd, "label '1'\n")
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (x-0.015)
		    call pargr (yy)
	    	call fprintf (fd, "label '%d'\n")
		    call pargi (IM_LEN(imexp, 2))
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (x-0.02)
		    call pargr ((y+yy)/2.)
	    	call fprintf (fd, "label 'Pixel'\n")
	        call fprintf (fd, "angle 0\n")
	    }
	    call imunmap (imexp)

	    call fprintf (fd, "title 'PATT-POS: %d  # %d'\n")
	        call pargi (pattpos)
	        call pargi (npos[pattpos])

	    call fprintf (fd, "vmove %0.4f %0.4f\n")
		call pargr ((x+xx)/2.)
		call pargr (yy+0.025)
	    call fprintf (fd, "justify 5\n")
	    call fprintf (fd, "label '%d'\n")
		call pargstr (exproot[1,n])
	}
end
