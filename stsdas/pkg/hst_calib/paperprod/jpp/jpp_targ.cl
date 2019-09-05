# plot the target region from a full WFC image for ACS

procedure jpp_targ (igi_output, rootname, rootext )

char	igi_output 	{prompt="output igi script file name"}
char	rootname	{prompt="rootname of the input files"}
char	rootext	    {prompt="Filename extension of observation"}

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
	real	x0, y0
    real    ratarg, dectarg
    real    xpos, ypos
    real    targx,targy
    real    dummy
    real    xmax, ymax, xmin, ymin, dx, dy
    int     rx1, rx2, ry1, ry2
    real    scale, rsize

    scale = 0.05    # plate scale in arc-seconds
    rsize = 25.0 / scale  # 25x25 arc-second region to be extracted
    xmin = 0.06
    xmax = 0.66
    ymin = 0.05+0.05
    ymax = 0.83
    dx = xmax - xmin
    dy = ymax - ymin

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

    # Compute position of target in image
    keypar (fname, "RA_TARG", silent=yes)
    ratarg = real(keypar.value)
    keypar (fname, "DEC_TARG", silent=yes)
    dectarg = real(keypar.value)
    rd2xy(fname, ratarg, dectarg, hour=no)
    targx = int(rd2xy.x)
    targy = int(rd2xy.y)

    # Determine which extension/chip the target position was set to
    if (targx < 0 || targx > xdim || targy < 0 || targy > ydim) {
	    i = 2
	    fname = raw//"[sci,"//i//"]"
        rd2xy(fname, ratarg, dectarg, hour=no)
        targx = int(rd2xy.x)
        targy = int(rd2xy.y)        
    }

    # Now, establish the pixel region to be extracted and displayed
    rx1 = int(targx - (rsize/2))
    rx2 = int(targx + (rsize/2))
    ry1 = int(targy - (rsize/2))
    ry2 = int(targy + (rsize/2))
    # Do bounds-checking
    if (rx1 < 1) rx1 = 1
    if (rx2 > xdim) rx2 = xdim

    if (ry1 < 1) ry1 = 1
    if (ry2 > ydim) ry2 = ydim
      
    fname = raw//"[sci,"//i//"]["//rx1//":"//rx2//","//ry1//":"//ry2//"]"
        
	# determine the brightness range from the first image
	minmax (fname, force = yes, update = no, verbose = no)
	zmin = minmax.minval
	zmax = minmax.maxval

    # size of extracted image in pixels (actual)
    x0 = rx2 - rx1
    y0 = ry2 - ry1
	maxdim = max (x0, y0 )
    x1 = 10**(int(log10(maxdim)))
    x2 = int(maxdim/x1) * x1
    
    # this is to avoid "divide by zero error" by igi
    if (zmin == zmax) zmax = zmin + 0.1

	printf ("zsection %s\n", fname, >> script)

	printf ("fitpix %0.4f %0.4f %0.4f %0.4f\n", xmin, xmax, ymin, ymax, >> script)
	printf ("limits\n", >> script)
    printf ("zrange %g %g\n", zmax, zmin, >> script)
	printf ("pixmap\n", >> script)

	printf ("ticksize %d %d %d %d\n", x2/20, x2/2, x2/20, x2/2, 
		>> script)
	printf ("box\n", >> script)
	printf ("xlabel 'Pixel'\n", >> script)
	printf ("ylabel 'Pixel'\n", >> script)
	printf ("angle 0\n", >> script)

	printf ("vmove %g %g; justify 5\n", (xmin+dx/2.), (ymin+dy+0.05), >> script)
	printf ("label 'Target Position (25 X 25 arc-second field)'\n", >> script)
     
    # Compute the position of the target marker (center of region)
    xpos = dx/2. + xmin
    ypos = dy/2. + ymin
    # Now, place the markers at the X,Y positions along the side of the image  
    dummy = ymin - 0.065
    printf("expand 1.3; ptype 3 3; vrelocate %0.3g %0.3g; dot\n",xpos,dummy, >> script)
    dummy = xmin + dx + 0.015
    printf("vrelocate %0.3g %0.3g; angle 90; dot; expand 1; angle 0\n",dummy,ypos, >> script)

	# draw the compass
    t_cdcompass (fname, script, 0.75, 0.25, 0.04)

	# draw the gray scale bar
	t_gsbar (script, 0.72, 0.90, 0.12, 0.15, zmin, zmax, 0.5, yes)
	# indicate the units for the gray scale bar
	printf("vrelocate 0.81 0.11; label 'Counts'\n",>> script)
end
