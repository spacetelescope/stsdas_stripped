# plot the gray scale input exposures for the given ACS product

include	<imhdr.h>
include	"jpp.h"

define	NX	4
define	NY	2
define  SZ_HIST         4000
define  ASPECT          1.324


procedure jpp_thumbs (fd, root, prodext)

char	root[ARB]		# input file root name
char    prodext[ARB]    # extension for product image
int     fd                      # output file pointer

int	npatt           		# number of pattern positions used for product
int nrpt                    # number of repeat-obs exposures in product
int nsplit                  # number of cr-split exposures in product
char explist[SZ_FNAME, MAX_OBS]	# list of input file root names
char asnname[SZ_FNAME]	    # name of association table to look for
char fexp[SZ_FNAME]	 
char fexp2[SZ_FNAME]  

pointer imexp
real	hist[SZ_HIST]		# the histogram of usable pixels
real	zmax, zmin		# max/min used in gray scale image plot.
int	numpos
int	pattpos, npos[MAX_OBS]
real	pgwidth, pgheight	# view port's width and height
real	width, height		# width and height of EACH thumbnail image
real	x0, y0			# initial position of the thumbnail plot
real	x, y, xx, yy, yy1, yy2		# position of the thumbnail plot
int	nx, ny			# no. of thumbnail images in each row and column
int	i, j, m, n, nn
int nexp, xdim,ydim, ybuff, yybuff
int    scale
int     ngroups

pointer	immap()
int	imgeti()

begin

    
	do i = 1, MAX_OBS
	    npos[i] = 0
    
	# decide which files to use by looking up the asn table
        call sprintf (asnname, SZ_FNAME, "%s_asn.fits")
            call pargstr (root)

	call jpp_assoc (root, prodext, explist, npatt, nrpt, nsplit)
        

	# plot the thumbnail gray scale images for individual exposures
	# =============================================================
	if (npatt == 1 && nrpt == 1 && nsplit == 1) return
	pgwidth = 0.95
	pgheight = 0.90
	nx = NX
	ny = NY
    ybuff = 35
    
    numpos = max (nrpt, nsplit)
    nexp = max(1, npatt) * numpos
        
	width = pgwidth / float(nx)
	height = pgheight / float(ny)
	x0 = 0.03
	y0 = pgheight - height + 0.1

	nn = 0
	do n = 1, nexp {
		
	    # open each image
	    call strlwr (explist[1,n])
	    call sprintf (fexp, SZ_FNAME, "%s_raw.fits[sci,1]")
		    call pargstr (explist[1,n])
	    iferr (imexp = immap (fexp, READ_ONLY, 0)) {
		call printf ("Can't access %s, skip\n")
		    call pargstr (fexp)
		next
	    }
	    
        if (n == 1) {
            ngroups = imgeti(imexp, "NEXTEND") / 3
            if (ngroups < 1) ngroups = 1
            xdim = IM_LEN(imexp, 1)
            if (ngroups > 1) {
	            call sprintf (fexp2, SZ_FNAME, "%s_raw.fits[sci,2]")
		            call pargstr (explist[1,n])
                ydim = IM_LEN(imexp, 2) * 2 + ybuff                
            } else {
                call strcpy ("", fexp2, SZ_FNAME)
                ydim = IM_LEN(imexp, 2)
            }
            scale = max (xdim, ydim) / 512
        }
        
	    nn = nn + 1
	    m = mod (nn-1, nx*ny) + 1
	    i = mod (m-1, nx) + 1
	    j = (m-1) / nx + 1

	    x = x0 + (i-1) * width
	    y = y0 - (j-1) * height
	    xx = x + width
        yybuff = (ybuff / ydim) * height / 2.
	    yy1 = y + width * ASPECT
        yy2 = y + (width/2.) * ASPECT
        if (ngroups == 1) {
            yy = yy1
        } else {
            yy = yy2 - yybuff
        }
        
	    # determine the brightness range 
	    call get_hist (imexp, 0.99, 0.01, hist, 2000, zmin, zmax)
        # this is to avoid "divide by zero" error in igi
        if (zmin == zmax) zmax = zmax + 0.1

	    iferr (pattpos = imgeti (imexp, "PATT_POS") )
            pattpos = 1
            
        # Close the first image to keep things clean...    
	    call imunmap (imexp)
            
	    if (pattpos <= 0) pattpos = 1
	    npos[pattpos] = npos[pattpos] + 1

	    # plot the image
	    # block average down to 512x512 pixels
	    call fprintf (fd, "zsection %s %d\n")
		call pargstr (fexp)
        call pargi (scale)
                 
	    call fprintf (fd, "fitpix %0.4f %0.4f %0.4f %0.4f\n")
	 	call pargr (x)
	 	call pargr (xx)
	 	call pargr (y)
	 	call pargr (yy)

	    call fprintf (fd, "limits\n")

        call fprintf (fd, "zrange %g %g\n")
            call pargr (zmax)
            call pargr (zmin)
	    call fprintf (fd, "pixmap\n")
        
        if (ngroups > 1) {
            # Reset upper y limit for labels and boxes
            yy = yy1
            
	        call fprintf (fd, "zsection %s %d\n")
		    call pargstr (fexp2)
            call pargi (scale)
            
	        call fprintf (fd, "fitpix %0.4f %0.4f %0.4f %0.4f\n")
	 	    call pargr (x)
	 	    call pargr (xx)
	 	    call pargr ((yy2+yybuff))
	 	    call pargr (yy1)

	        call fprintf (fd, "limits\n")

            call fprintf (fd, "zrange %g %g\n")
                call pargr (zmax)
                call pargr (zmin)
	        call fprintf (fd, "pixmap\n")        
        }

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
		    call pargi (xdim)
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
		    call pargi (ydim)
	    	call fprintf (fd, "vmove %0.4f %0.4f\n")
		    call pargr (x-0.02)
		    call pargr ((y+yy)/2.)
	    	call fprintf (fd, "label 'Pixel'\n")
	        call fprintf (fd, "angle 0\n")
	    }

        call fprintf (fd, "vrelocate %0.3g %0.3g\n")
            call pargr ((((xx-x)/2.)+x))
            call pargr ((yy+0.01)) 
	    call fprintf (fd, "label 'PATT-POS: %d  # %d'\n")
	        call pargi (pattpos)
	        call pargi (npos[pattpos])

	    call fprintf (fd, "vmove %0.4f %0.4f\n")
		call pargr ((x+xx)/2.)
		call pargr (yy+0.025)
	    call fprintf (fd, "justify 5\n")
	    call fprintf (fd, "label '%d'\n")
		call pargstr (explist[1,n])
	}
end
