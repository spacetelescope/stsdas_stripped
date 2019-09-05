# plot the ACQ/PEAKUP mode plots for STIS

procedure opp_peakup (rootname, igi_output)

char	rootname	{prompt="root name of the input files"}
char	igi_output 	{prompt="output igi script file name"}
char	fname0		{prompt="file name used"}

begin
	# Declarations
	string	root		# root name
	string	raw, jih	# input file names
	string	script		# igi script.
	string	fname
	string	fsci		# file name of the science extension
	string	scisub		# science subimage name
	string	fpeak		# file name of the peakup extension
	int	xdim, ydim
	int	left, right	# subimage indices
        int     len
	real	zmax, zmin	# max/min used in gray scale image plot.
	real	x1, x2
	real	y0, y1, y2
	real	xlen1, ylen1
	real	dummy, flux
	
	# Get interactive parameters
	root = rootname
	script = igi_output

	raw = root//"_raw.fits"
        len = strlen (root)
        if (substr(root,len,len) >= "0" && substr(root,len,len) <= "9")
            jih = root//"_jif.fits"
        else
            jih = substr(root, 1, len-1)//"j_jif.fits"

	# plot sizes
	x1 = 0.06
	x2 = 0.39
	y0 = 0.05
	y1 = 0.25
	y2 = 0.45
	xlen1 = 0.22
	ylen1 = xlen1 * 1.324

	# Draw the ACQ/PEAKUP images
	printf ("reset\n", >> script)
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("expand 0.65\n", >> script)

	fname0 = raw
	fsci = fname0//"[sci,1]"
	fpeak = fname0//"[peakup]"

        keypar (fsci, "i_naxis1", silent=yes)
        xdim = int (keypar.value)
        keypar (fsci, "i_naxis2", silent=yes)
        ydim = int (keypar.value)

	# determin the subimage size
	left = 1
	right = xdim
	if (xdim > 200 && xdim > 10*ydim) {
	    left = xdim/2 - 99
	    right = xdim/2 + 100
	}
	scisub = fsci//"["//left//":"//right//",1:"//ydim//"]"

        # determine the brightness range
        t_gethist (scisub, 0.01, 0.99, 2000)
        zmin = t_gethist.hmin
        zmax = t_gethist.hmax
 
	printf ("location 0 1 0 1\n", >> script)

	# plot the peakup image
	t_opeakup (fpeak, script, x2, (x2+xlen1), y1, (y1+ylen1))

        # plot the science image
        printf ("zsection %s\n", scisub, >> script)
        printf ("fitpix %0.4g %0.4g %0.4g %0.4g\n", 
		x1, (x1+xlen1), y2, (y2+ylen1), >> script)
        printf ("limits\n", >> script)
 
        # this is to avoid "divide by zero error" by igi
        if (zmin == zmax)
            printf ("zrange %g %g\n", (zmin+0.1), zmin, >> script)
        else
            printf ("zrange %g %g\n", zmax, zmin, >> script)
        printf ("pixmap\n", >> script)
        printf ("limits %d %d 1 %d\n", left, right, ydim, >> script)
        printf ("box\n", >> script)
        printf ("xlabel 'Pixel'\n", >> script)
        printf ("ylabel 'Pixel'\n", >> script)
        printf ("vmove %g %g; justify 5\n", (x1+xlen1/2.), 
		(y2+ylen1*real(ydim)/real(right-left+1)+0.035), >> script)
        printf ("label 'Raw Image'\n", >> script)

        keypar (fsci, "pedestal", silent=yes)
        if (keypar.value == "") {
            printf ("title 'Flux in confirmation image: N/A'\n", >> script)
	} else {
	    flux = real(keypar.value)
            keypar (fsci, "ngoodpix", silent=yes)
	    dummy = real(keypar.value)
            keypar (fsci, "goodmean", silent=yes)
	    dummy = dummy*real(keypar.value)
            printf ("title 'Flux in confirmation image (DN): %0.1f'\n", 
			(dummy-flux), >> script)
	}

	# draw the jitter image
	if (access(jih)) {
	    t_oms (jih)
            jih = t_oms.output
	    opp_jitter (jih, script, x1, (x1+xlen1), y0, (y0+ylen1))
	} else {
	    printf ("vmove %0.2f %0.2f; justify 6\n", x1, (y0+ylen1/2.), 
			>> script)
	    printf ("label '(Jitter file not available)'\n", >> script)
	}

	# draw the compass
	t_cdcompass (fsci, script, 0.36, 0.84, 0.045)
 
        if (zmin == zmax) {
            printf ("move %0.4g %0.4g; justify 5; expand 0.35\n", 
			(x1+xlen1/2.), (y2+ylen1/2.), >> script)
            printf ("label 'Image has a constant value of %0.3g'\n", zmin,
                        >> script)
        } else {
 
            # draw the gray scale bar
            t_gsbar (script, 0.44, 0.62, 0.82, 0.86, zmin, zmax, 0.5, yes)
 
            # draw the histogram
            opp_hist (scisub, script, 0.05, 0.26, 0.82, 0.90, zmin, zmax)
        }
end
