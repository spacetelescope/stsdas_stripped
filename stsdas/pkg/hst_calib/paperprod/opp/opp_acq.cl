# plot the ACQ mode gray scale images for STIS

procedure opp_acq (rootname, igi_output)

char	rootname	{prompt="rootname of the input files"}
char	igi_output 	{prompt="output igi script file name"}
char	fname0		{prompt="file name used"}

begin

	# Declarations
	string	root		# root name
	string	raw, jih	# input file names
	string	script		# igi script.
	string	fname
	string	maxchcnt
	real	zmax, zmin	# max/min used in gray scale image plot.
	int	xdim, ydim, maxdim, x1, x2
	int	i
	int	len
	real	aspect, len0, x0, y0, xlen, ylen

	aspect = 1.324
	len0 = 0.23

	# Get input parameters
	root = rootname
	script = igi_output

	raw = root//"_raw.fits"
	len = strlen (root)
	if (substr(root,len,len) >= "0" && substr(root,len,len) <= "9")
	    jih = root//"_jif.fits"
	else
	    jih = substr(root, 1, len-1)//"j_jif.fits"
	fname0 = raw

	# Draw the acquisition images
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

	i = 1
	while (i <= 3) {
	    fname = fname0//"[sci,"//i//"]"
            keypar (fname, "i_naxis1", silent=yes)
            xdim = int (keypar.value)
            keypar (fname, "i_naxis2", silent=yes)
            ydim = int (keypar.value)
	    maxdim = max (xdim, ydim)
            x1 = 10**(int(log10(maxdim)))
            x2 = int(maxdim/x1) * x1

	    # determine the brightness range from the first image
	    if (i == 1) {
		minmax (fname, force = yes, update = no, verbose = no)
		zmin = minmax.minval
		zmax = minmax.maxval
	    }

	    printf ("zsection %s\n", fname, >> script)
	    if (i == 1) {
		x0 = 0.09
		y0 = 0.46
	    } else if (i == 2) {
		x0 = 0.42
		y0 = 0.46
	    } else if (i == 3) {
		x0 = 0.09
		y0 = 0.06
	    }
	    if (xdim >= ydim) {
		xlen = len0
		ylen = len0 * aspect * real(ydim) / real(xdim)
	    } else {
		ylen = len0 * aspect
		xlen = len0 * real(xdim) / real (ydim)
	    }
	    x0 = x0 + (len0 - xlen)/2.
	    y0 = y0 + (len0 * aspect - ylen)/2.

	    printf ("fitpix %0.4f %0.4f %0.4f %0.4f\n", x0, (x0+xlen), 
			y0, (y0+ylen), >> script)

	    printf ("limits\n", >> script)

            # this is to avoid "divide by zero error" by igi
            if (zmin == zmax)
                printf ("zrange %g %g\n", (zmin+0.1), zmin, >> script)
            else
                printf ("zrange %g %g\n", zmax, zmin, >> script)

	    printf ("pixmap\n", >> script)

	    printf ("ticksize %d %d %d %d\n", x2/20, x2/2, x2/20, x2/2, 
		    >> script)
	    printf ("box\n", >> script)
	    printf ("xlabel 'Pixel'\n", >> script)
	    printf ("ylabel 'Pixel'\n", >> script)
	    printf ("angle 0\n", >> script)
	    if (i == 3) 
	        printf ("title 'Slit Illumination Image'\n", >> script)
	    else {
            	keypar (fname, "maxchcnt", silent=yes)
	        if (keypar.value == "") maxchcnt = "N/A"
	        else maxchcnt = keypar.value

	    	printf ("vmove %g %g; justify 5\n", (x0+xlen/2.), 
			(y0+ylen+0.035), >> script)
	        printf ("label 'Acquisition Image #%d'\n", i, >> script)
	        printf ("title 'Target flux in MAX checkbox (DN): %s'\n", 
			maxchcnt, >> script)
	    }

	    i = i + 1
	}

	# draw the jitter image
	if (access(jih)) {
            t_oms (jih)
            jih = t_oms.output
	    opp_jitter (jih, script, 0.42, 0.72, 0.06, 0.36)
	} else {
	    printf ("vmove 0.53 0.21; justify 5\n", >> script)
	    printf ("label '(Jitter file not available)'\n", >> script)
	}

	# draw the compass
        t_cdcompass (fname, script, 0.36, 0.84, 0.04)

	# draw the gray scale bar
	t_gsbar (script, 0.44, 0.62, 0.82, 0.86, zmin, zmax, 0.5, yes)
end
