# plot the CCD/ACCUM mode or MAMA/LRC gray scale images for STIS

procedure opp_accum (rootname, igi_output, detector)

char	rootname	{prompt="root name of the input file"}
char	igi_output 	{prompt="output igi script file name"}
char	detector	{prompt = "detector (MAMA or CCD)"}
char	fname0		{prompt = "file name used"}

begin
	# Declarations
	string	root		# root name
	string	script		# igi script.
	string	det		# detector
        string  ext             # file extension name
	string	fname
	real	zmax, zmin	# max/min used in gray scale image plot.
	int	xdim, ydim	# image dimensions
	int	maxdim
	int	x1, x2
	bool	internal1, internal2
	int	b1, base	# block average factors
	string	blktext		# block average text
	
	# Get interactive parameters
	root = rootname
	script = igi_output
	det = detector

	# decide on the file name
	if (det == "MAMA") {
	    fname0 = root//"_lrc.fits"
	    ext = "_lrc"
	} else {
	    if (access(root//"_geo.fits")) {
		fname0 = root//"_geo.fits"
		ext = "_geo"
	    } else if (access(root//"_sfl.fits")) {
		fname0 = root//"_sfl.fits"
		ext = "_sfl"
	    } else if (access(root//"_crj.fits")) {
		fname0 = root//"_crj.fits"
		ext = "_crj"
	    } else if (access(root//"_flt.fits")) {
		fname0 = root//"_flt.fits"
		ext = "_flt"
	    } else {
		fname0 = root//"_raw.fits"
		ext = "_raw"
	    }
	}

	if (!access (fname0)) {
	    printf ("Can not access file '%s', skip.\n", fname0)
	    bye
	}
	fname = fname0//"[sci,1]"

	# initialize the page
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

	# get the image size
        keypar (fname, "i_naxis1", silent=yes)
        xdim = int (keypar.value)
        keypar (fname, "i_naxis2", silent=yes)
        ydim = int (keypar.value)
	maxdim = max (xdim, ydim)
        x1 = 10**(int(log10(maxdim)))
        x2 = int(maxdim/x1) * x1

	# block average parameters
	base = 512
	b1 = min (xdim/base, ydim/base)
	if (b1 > 1) {
	    blktext = "  "//b1//" by "//b1//" block averaged"
	} else {
	    blktext = ""
	}

	# determine the brightness range 
	t_gethist (fname, 0.01, 0.99, 2000)
	zmin = t_gethist.hmin
	zmax = t_gethist.hmax

	# plot the image
	printf ("zsection %s %d\n", fname, b1, >> script)
	printf ("fitpix 0.06 0.66 0.05 0.75\n", >> script)

 	printf ("limits\n", >> script)

	# this is to avoid "divide by zero error" by igi
	if (zmin == zmax) 
	    printf ("zrange %g %g\n", (zmin+0.1), zmin, >> script)
	else
	    printf ("zrange %g %g\n", zmax, zmin, >> script)
	printf ("pixmap\n", >> script)

	# make sure the ticks are not too crowded
	if (xdim > (5*ydim)) {
	    printf ("ticksize %d %d %d %d\n", x2/20, x2/2, x2/2, x2/2,
			>> script)
	} else if (ydim > (5*xdim)) {
	    printf ("ticksize %d %d %d %d\n", x2/2, x2/2, x2/20, x2/2,
			>> script)
	} else {
	    printf ("ticksize %d %d %d %d\n", x2/20, x2/2, x2/20, x2/2,
			>> script)
	}
 	printf ("limits 1 %d 1 %d\n", xdim, ydim, >> script)
	printf ("box\n", >> script)

	# plot labels and title
	printf ("xlabel 'Pixel'\n", >> script)
	printf ("ylabel 'Pixel'\n", >> script)
	printf ("angle 0\n", >> script)
	if (det == "MAMA")
	    printf ("title 'MAMA Local Rate Check Image (%s) %s'\n", ext, blktext, >> script)
	else
	    printf ("title 'ACCUM Image (%s) %s'\n", ext, blktext, >> script)

        keypar (fname, "SCLAMP", silent=yes)
        internal1 = (keypar.found && keypar.value == "NONE")
        keypar (fname, "TARGNAME", silent=yes)
        internal2 = (keypar.found && keypar.value != "BIAS" &&
				     keypar.value != "DARK")

	# draw the compass, skip for internal exposures
	if (internal1 && internal2)
	    t_cdcompass (fname, script, 0.36, 0.84, 0.045)

	if (zmin == zmax) {
            printf ("move 0.35 0.3; justify 5; expand 0.8\n", >> script)
            printf ("label 'Image has a constant value of %0.3g'\n", zmin,
			>> script)
	} else {

	    # draw the gray scale bar
	    t_gsbar (script, 0.44, 0.62, 0.82, 0.86, zmin, zmax, 0.5, yes)

	    # draw the histogram
	    opp_hist (fname, script, 0.05, 0.26, 0.82, 0.90, zmin, zmax)
	}
end
