# plot the 2-D spectral gray scale image for STIS

procedure opp_2dsp (rootname, igi_output)

char	rootname	{prompt="root name of the input file"}
char	igi_output 	{prompt="output igi script file name"}
char	fname0		{prompt="file name used"}

begin

	# Declarations
	string	root		# root name
	string	script		# igi script.
	string	ext		# file extension name
	string	fname
	string	rect, apprx
	string	grating		# grating name
	real	zmax, zmin	# max/min used in gray scale image plot.
	real	xdim, ydim
	real 	x, y, xlen, ylen, x0, y0, len0, aspect
	real	xmin, xmax, ymin, ymax
	real	tick
	real	crval1, crval2, crpix1, crpix2, cd11, cd22
        int     b1, base        # block average factors
        string  blktext         # block average text
 
	# Get input parameters
	root  = rootname
	script = igi_output

	# decide which file to use
        if (access(root//"_sx2.fits")) {
            fname0 = root//"_sx2.fits"
	    ext = "_sx2"
        } else if (access(root//"_x2d.fits")) {
            fname0 = root//"_x2d.fits"
	    ext = "_x2d"
        } else if (access(root//"_sfl.fits")) {
            fname0 = root//"_sfl.fits"
	    ext = "_sfl"
        } else if (access(root//"_crj.fits")) {
            fname0 = root//"_crj.fits"
	    ext = "_crj"
        } else if (access(root//"_flt.fits")) {
            fname0 = root//"_flt.fits"
	    ext = "_flt"
        } else if (access(root//"_raw.fits")) {
            fname0 = root//"_raw.fits"
	    ext = "_raw"
	} else 
	    bye

	# get the dimensions
	fname = fname0//"[sci,1]"
        keypar (fname, "i_naxis1", silent=yes)
        xdim = real (keypar.value)
        keypar (fname, "i_naxis2", silent=yes)
        ydim = real (keypar.value)

	# determine if it is echelle
        keypar (fname, "opt_elem", silent=yes)
        grating = keypar.value

        # block average parameters
        base = 512
        b1 = min (xdim/base, ydim/base)
        #b1 = 1
        #if (xdim >= 1024 && ydim >= 1024) b1 = 2
        #if (xdim >= 1536 && ydim >= 1536) b1 = 3
        #if (xdim >= 2048 && ydim >= 2048) b1 = 4
        if (b1 > 1) {
            blktext = "  "//b1//" by "//b1//" block averaged"
        } else {
            blktext = ""
        }
 
	# Initialize the page
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

	# determine the brightness range 
        t_gethist (fname, 0.01, 0.99, 2000)
        zmin = t_gethist.hmin
        zmax = t_gethist.hmax

	# read in the image data
	printf ("zsection %s %d\n", fname, b1, >> script)

	# location of the lower left corner of the plot
	x0 = 0.06
	y0 = 0.06
	len0 = 0.53
	aspect = 1.324

	# determine the window size
        if (xdim >= ydim) {
            xlen = len0
            ylen = len0 * aspect * real(ydim) / real(xdim)
        } else {
            ylen = len0 * aspect
            xlen = len0 * real(xdim) / real (ydim)
        }
        x0 = x0 + (len0 - xlen)/2.
        y0 = y0 + (len0*aspect - ylen)/2.
 
	# plot the image
        printf ("fitpix %0.4f %0.4f %0.4f %0.4f\n", x0, (x0+xlen),
                    y0, (y0+ylen), >> script)
 
        printf ("limits\n", >> script)
 
        # this is to avoid "divide by zero error" by igi
        if (zmin == zmax)
            printf ("zrange %g %g\n", (zmin+0.1), zmin, >> script)
        else
            printf ("zrange %g %g\n", zmax, zmin, >> script)
        printf ("pixmap\n", >> script)

	# plot the labels
	if (substr (grating, 1, 1) == "E") {
	    printf ("title '2-D Image (%s) %s'\n", ext, blktext, >> script)
	    printf ("expand 0.5\n", >> script)
	    printf ("xlabel 'Pixel'\n", >> script)
	    printf ("ylabel 'Pixel'\n", >> script)
	    xmin = 1.
	    ymin = 1
	    xmax = real(xdim)
	    ymax = real(ydim)
	} else {
	    if (ext == "_x2d" || ext == "_sx2") {
	        rect = "Rectified"
	        apprx = ""
	    } else {
	        rect = ""
	        apprx = "Approximate"
	    }

	    printf ("title '%s 2-D Image (%s) %s'\n", rect, ext, blktext,
			>> script)
	    printf ("expand 0.5\n", >> script)
	    printf ("xlabel '%s Wavelength (Angstrom)'\n", apprx, >> script)
	    printf ("ylabel '%s Position along Slit (arcsec)'\n", apprx, 
			>> script)

            keypar (fname, "crval1", silent=yes)
            crval1 = real (keypar.value)
            keypar (fname, "crval2", silent=yes)
            crval2 = real (keypar.value)
            keypar (fname, "crpix1", silent=yes)
            crpix1 = real (keypar.value)
            keypar (fname, "crpix2", silent=yes)
            crpix2 = real (keypar.value)
            keypar (fname, "cd1_1", silent=yes)
            cd11 = real (keypar.value)
            keypar (fname, "cd2_2", silent=yes)
            cd22 = real (keypar.value) * 3600.	# convert to arcseconds
	    xmin = crval1 - (crpix1-1) * cd11
	    ymin = crval2 - (crpix2-1) * cd22
	    xmax = xmin + (xdim-1.) * cd11
	    ymax = ymin + (ydim-1.) * cd22
	}
	
	# draw the axes
	printf ("limits %0.6g %0.6g %0.6g %0.6g\n", xmin, xmax, ymin, ymax, 
		>> script)

	# if the aspect ratio is too large or small, only print out the 
	# min/max values and no ticks on the smaller dimension
	if (ydim > (10*xdim)) {
	    tick = (xmax - xmin) * 1000.
            printf ("ticksize %0.4g %0.4g 0 0\n", tick, tick, >> script)
            printf ("justify 5\n", >> script)
            printf ("vmove %0.4g %0.4g\n", x0, (y0-0.01), >> script)
            printf ("label %0.1g\n", xmin, >> script)
            printf ("vmove %0.4g %0.4g\n", (x0+xlen), (y0-0.01), >> script)
            printf ("label %0.1g\n", xmax, >> script)
	} else if (xdim > (10*ydim)) {
	    tick = (ymax - ymin) * 1000.
            printf ("ticksize 0 0 %0.4g %0.4g\n", tick, tick, >> script)
            printf ("justify 4\n", >> script)
            printf ("vmove %0.4g %0.4g\n", (x0-0.005), y0, >> script)
            printf ("label %0.2g\n", ymin, >> script)
            printf ("vmove %0.4g %0.4g\n", (x0-0.005), (y0+ylen), >> script)
            printf ("label %0.2g\n", ymax, >> script)
	}

	printf ("box\n", >> script)

        if (zmin == zmax) {
            printf ("vmove 0.35 0.3; justify 5; expand 0.8\n", >> script)
            printf ("label 'Image has a constant value of %0.3g'\n", zmin,
                        >> script)
        } else {
 
            # draw the gray scale bar
            t_gsbar (script, 0.44, 0.62, 0.82, 0.86, zmin, zmax, 0.5, yes)
 
            # draw the histogram
            opp_hist (fname, script, 0.05, 0.26, 0.82, 0.90, zmin, zmax)
        }
end
