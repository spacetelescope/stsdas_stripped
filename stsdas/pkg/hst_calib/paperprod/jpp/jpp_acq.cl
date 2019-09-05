# plot the ACQ mode gray scale images for ACS

procedure jpp_acq (rootname, rootext, igi_output)

char	rootname	{prompt="rootname of the input files"}
char	rootext	    {prompt="Filename extension of observation"}
char	igi_output 	{prompt="output igi script file name"}

begin

	# Declarations
	string	root		# root name
    string  prodext     # product extension
	string	raw	        # input file names
	string	script		# igi script.
	string	fname
	real	zmax, zmin	# max/min used in gray scale image plot.
	int	xdim, ydim, maxdim, x1, x2
	int	i
	int	len
	real	aspect, len0, x0, y0, xlen, ylen
    real    ratarg, dectarg
    real    xpos, ypos
    real    x1,y1
    real    dummy, ratioxy
    
	aspect = 1.324
	len0 = 0.6
	x0 = 0.05
	y0 = 0.06


	# Get input parameters
	root = rootname
    prodext = rootext
	script = igi_output

	raw = root//prodext

	# Draw the acquisition images
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

	i = 1
	fname = raw//"[sci,"//i//"]"
        keypar (fname, "i_naxis1", silent=yes)
        xdim = int (keypar.value)
        keypar (fname, "i_naxis2", silent=yes)
        ydim = int (keypar.value)
	maxdim = max (xdim, ydim)
        x1 = 10**(int(log10(maxdim)))
        x2 = int(maxdim/x1) * x1

    ratioxy = real(xdim) / real(ydim)

	# determine the brightness range from the first image
	minmax (fname, force = yes, update = no, verbose = no)
	zmin = minmax.minval
	zmax = minmax.maxval
    # this is to avoid "divide by zero error" by igi
    if (zmin == zmax) zmax = zmin + 0.1

	printf ("zsection %s\n", fname, >> script)

	if (xdim >= ydim) {
	    xlen = len0
	    ylen = len0 * aspect * real(ydim) / real(xdim)
	} else {
	    ylen = len0 * aspect
	    xlen = len0 * real(xdim) / real (ydim)
	}

	printf ("fitpix %0.4f %0.4f %0.4f %0.4f\n", x0, (x0+xlen), 
		y0, (y0+ylen), >> script)

	printf ("limits\n", >> script)
    printf ("zrange %g %g\n", zmax, zmin, >> script)
	printf ("pixmap\n", >> script)

	printf ("ticksize %d %d %d %d\n", x2/20, x2/2, x2/20, x2/2, 
		>> script)
	printf ("box\n", >> script)
	printf ("xlabel 'Pixel'\n", >> script)
	printf ("ylabel 'Pixel'\n", >> script)
	printf ("angle 0\n", >> script)

	printf ("vmove %g %g; justify 5\n", (x0+xlen/2.), 
	(y0+ylen+0.035), >> script)
	printf ("label 'Acquisition Image #%d'\n", i, >> script)


    # Compute position of target in image
    keypar (fname, "RA_TARG", silent=yes)
    ratarg = real(keypar.value)
    keypar (fname, "DEC_TARG", silent=yes)
    dectarg = real(keypar.value)
    rd2xy(fname, ratarg, dectarg, hour=no)
    x1 = int(rd2xy.x)
    y1 = int(rd2xy.y)
    
    if (aspect > 1) {
        xpos = (x1/xdim) * xlen + x0
        ypos = ((y1/ydim) * ylen)/ aspect + y0 
    } else {
        xpos = ((x1/xdim) * xlen)/ aspect + x0
        ypos = (y1/ydim) * ylen + y0     
    }
     
    # Now, place the markers at the X,Y positions along the side of the image  
    dummy = y0 - 0.02
    printf("expand 1.3; ptype 3 3; vrelocate %0.3g %0.3g; dot\n",xpos,dummy, >> script)
    dummy = x0 + xlen + 0.015
    printf("vrelocate %0.3g %0.3g; angle 90; dot; expand 1; angle 0\n",dummy,ypos, >> script)


	# draw the compass
    t_cdcompass (fname, script, 0.75, 0.25, 0.04)

	# draw the gray scale bar
	t_gsbar (script, 0.72, 0.90, 0.12, 0.15, zmin, zmax, 0.5, yes)
end
