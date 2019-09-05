# plot the CCD/ACCUM mode gray scale images for ACS

procedure jpp_accum (rootname, rootext, igi_output, detector)

char	rootname	{prompt="root name of the input file"}
char	rootext	    {prompt="Filename extension of observation"}
char	igi_output 	{prompt="output igi script file name"}
char	detector	{prompt = "detector (MAMA or CCD)"}

begin
	# Declarations
	string	root		# root name
    string  prodext     # product extension
	string	script		# igi script.
	string	det		# detector
    string  raw
	string	fname,fname2
	real	zmax, zmin	# max/min used in gray scale image plot.
	int	xdim, ydim, yimg	# image dimensions
	bool	internal1
	int	b1, base	# block average factors
	string	blktext		# block average text
	int ngroups,ybuff
    real    ratarg, dectarg
    real    xpos, ypos
    real    xmax, ymax, xmin, ymin, dx, dy, ysplit
    real    x1,y1
    real    dummy, aspect
	int 	targchip
    
	# Get interactive parameters
	root = rootname
    prodext = rootext
	script = igi_output
	det = detector
    ybuff = 35
    xmin = 0.06
    xmax = 0.66
    ymin = 0.05
    ymax = 0.83
    dx = 0.6
    dy = 0.78
    ysplit = (ymax - ymin) / 2 + ymin

	# build the file name
    raw = root//prodext

	# initialize the page
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

    # determine how many groups are in the image 
   	fname = raw//"[sci,1]"
    keypar (fname, "nextend", silent=yes)
    ngroups = int(keypar.value) / 3
    
    
	# get the image size
    keypar (fname, "i_naxis1", silent=yes)
    xdim = int (keypar.value)
    keypar (fname, "i_naxis2", silent=yes)
    yimg = int (keypar.value)
	
    if (ngroups > 1) {
        fname2 = raw//"[sci,2]"
        keypar (fname2, "i_naxis2", silent=yes)
        ydim = yimg + int (keypar.value) + ybuff
    } else {
		ydim = yimg
	}
	
    aspect = real(xdim) / real(ydim)
    
  	# block average parameters
	base = 1024
	b1 = min (xdim/base, ydim/base)
	if (b1 > 1) {
	    blktext = "  "//b1//" by "//b1//" block averaged"
	} else {
	    blktext = ""
	}

	# determine the brightness range 
	t_gethist (fname, 0.02, 0.98, 2000)
	zmin = t_gethist.hmin
	zmax = t_gethist.hmax
    
    if (ngroups > 1) {
	    t_gethist (fname2, 0.02, 0.98, 2000)
	    zmin = min(zmin,t_gethist.hmin)
	    zmax = max(zmax,t_gethist.hmax)
    } 
	# this is to avoid "divide by zero error" by igi
    if (zmin == zmax) zmax = zmax + 0.1

	# plot the image
	printf ("zsection %s %d\n", fname, b1, >> script)
    if (ngroups == 1) {
        dummy = ymin
    } else {
        dummy = ysplit + 0.0025
    } 
    printf ("fitpix %0.3g %0.3g %0.3g %0.3g \n",xmin,xmax,dummy,ymax, >> script)
 	printf ("limits\n", >> script)
	printf ("zrange %g %g\n", zmax, zmin, >> script)
	printf ("pixmap\n", >> script)

    # Now plot second chip if present...
    if (ngroups > 1) {
        dummy = ysplit - 0.0025
	    printf ("zsection %s %d\n", fname2, b1, >> script)
        printf ("fitpix %0.3g %0.3g %0.3g %0.3g\n",xmin,xmax,ymin,dummy, >> script)
  	    printf ("limits\n", >> script)
	    printf ("zrange %g %g\n", zmax, zmin, >> script)
	    printf ("pixmap\n", >> script)    
    }
	# plot labels and title
	printf ("angle 0\n", >> script)
    dummy = xmax / 2.0 + xmin
    printf ("vrelocate %0.3g %0.3g\n",dummy, (ymax+0.05), >> script)
	printf ("putlabel 8 'ACCUM Image (%s) %s'\n", prodext, blktext, >> script)

    keypar (fname, "TARGNAME", silent=yes)
    internal1 = (keypar.found && keypar.value != "BIAS" &&
				 keypar.value != "DARK")

    # Compute position of target in image
    keypar (fname, "RA_TARG", silent=yes)
    ratarg = real(keypar.value)
    keypar (fname, "DEC_TARG", silent=yes)
    dectarg = real(keypar.value)
    rd2xy(fname, ratarg, dectarg, hour=no)
    x1 = real(rd2xy.x)
    y1 = real(rd2xy.y)

    if ( (x1 > xdim || x1 < 0) || (y1 > yimg || y1 < 0)) {
		if (ngroups > 1) {
        	rd2xy (fname2, ratarg, dectarg, hour=no)
    		x1 = real(rd2xy.x)
    		y1 = real(rd2xy.y)
		} else {
			x1 = 0.0
			y1 = 0.0
		}
	} else {
		# If target is in Chip 1 (at top) of a 2-chip display,
		#	we need to convert that y position to the meta-chip
		#	position.  
		if (ngroups > 1) 
			y1 = y1 + yimg + ybuff
	}
    
    if (aspect > 1) {
        xpos = (x1/xdim) * dx + xmin
        ypos = ((y1/ydim) * dy)/ aspect + ymin 
    } else {
        xpos = ((x1/xdim) * dx)/ aspect + xmin
        ypos = (y1/ydim) * dy + ymin     
    }
    
    # Now, place the markers at the X,Y positions along the side of the image  
    dummy = ymin - 0.02
    printf("expand 1.3; ptype 3 3; vrelocate %0.3g %0.3g; dot\n",xpos,dummy, >> script)
    dummy = xmax + 0.015
    printf("vrelocate %0.3g %0.3g; angle 90; dot; expand 1; angle 0\n",dummy,ypos, >> script)
   
    
	# draw the compass, skip for internal exposures
	if (internal1 )
	    t_cdcompass (fname, script, 0.75, 0.25, 0.04)

	if (zmin == zmax) {
            printf ("move 0.35 0.3; justify 5; expand 0.8\n", >> script)
            printf ("label 'Image has a constant value of %0.3g'\n", zmin,
			>> script)
	} else {

	    # draw the gray scale bar
	    t_gsbar (script, 0.72, 0.90, 0.12, 0.15, zmin, zmax, 0.5, yes)
		# indicate the units for the gray scale bar
		printf("vrelocate 0.81 0.11; label 'Counts'\n",>> script)

	}
end
