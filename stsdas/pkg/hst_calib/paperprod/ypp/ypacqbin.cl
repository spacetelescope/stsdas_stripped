procedure ypacqbin (root, tmproot, igi_list, ftype)

char	root	{prompt="Rootname of observation"}
char	tmproot	{prompt="Rootname for temporary files"}
char	igi_list {prompt="Name of the file containing the igi script names"}
char	ftype	{prompt="Image type of root (FITS or GEIS)"}
int	nx	{3,min=1,prompt="Number of plots in X"}
real	left	{.2,min=0,max=1,prompt="Left extent of plots"}
real	right	{.9,min=0,max=1,prompt="Right extent of plots"}
real	bottom	{.2,min=0,max=1,prompt="Bottom extent of plots"}
real	top	{.9,min=0,max=1,prompt="Top extent of plots"}

begin
	# Declarations
	bool	costar			# Post-COSTAR data?
	char	d0h			# d0h image.
	real	off=0.01		# Group title offset.
	int	gcount			# # of groups in d0h image.
	real	h			# Height of each plot.
	int	i			# Generic.
	int	ix, iy			# Plot number in x, y.
	real	labsize=1.5		# Label size.
	int	ny			# Number in y.
	real	pb, pl, pr, pt		# Position of current plot.
	char	script			# Igi script.
	char	shh			# SHP image.
	real	w			# Width of each plot.
	real	x			# Generic.
	int	xlab, ylab		# Label each plot.
	real	xoff, yoff		# Offsets.
	real	xoffpix, yoffybs	# Offsets in pixels/ybase.
	real	v2off, v3off		# Offsets in V2/V3.
	real	yposinit, yposold	# Used for creating YPOS offsets 
	real	ypostarg		# Final Target YPOS position
	real	yposval[20]		# YPOS value for each group
	real	xglabel, xplabel, xtick	# positions for YPOS plot labels
	real	yplabel			# position for YPOS label
	real	x0, xl			# positions for YPOS plot labels
	real	xcomp, ycomp, compsize	# values for placing the compass
	real	orientat		# orientation keyword value
	real	xaxes, yaxes		# length of X/Y axes on compass

	real	expsize 
	
	# Create file names.
	script = mktemp(tmproot//"BIN")//".igi"
	if (ftype == "geis") {
		d0h = root//".d0h"
		shh = root//".shh"
	} else {
		d0h = root//"_d0f.fits[0]"
		shh = root//"_shf.fits[0]"
	}		
	# Start a new page.
	print ("erase", >> script)
	ypbanner (root, script, ftype)

	# Get limits.
	gstatistics (d0h, masks="", groups="*", g_accum=yes, fields="max",
		     lower=INDEF, upper=INDEF, gstpar="", >& "dev$null")
	keypar (d0h, "i_naxis1", silent=yes)
	i = int	(keypar.value)
	printf ("limits 1 %d 0 %.6g\n", i, gstpar.max, >> script)

	if (ftype == "geis") {
	    keypar (d0h, "gcount", silent=yes)
	} else {
	# FITS files don't have the 'gcount' keyword...
	    keypar (d0h, "naxis3", silent=yes)
	}
	#keypar (d0h, "gcount", silent=yes)
	gcount = int (keypar.value)
	
	ny = (gcount-1) / nx + 1
	w = (right - left) / real (nx)
	h = (top - bottom) / real (ny)

	# read in initial YPOS value
	if (ftype == "geis") {
		keypar (d0h//"[1]", "ypos", silent=yes)
		yposinit = real (keypar.value)
	} else {
		tabpar (root//"_d0f.fits[1]", "ypos", 1)
		yposinit = real (tabpar.value)
	}	
	yposold = 0.
	

	# Produce the plots.
	iy = 0
	ix = 0
	ny = max(ny-1, 1)
	expsize = min(labsize/ny, 1)

	#printf("NY = %g, EXPSIZE = %g\n",ny, expsize)

	printf ("justify 1; expand %.6g; xflip\n", expsize, >> script)
	for (i = 1; i <= gcount; i=i+1) {
	# first, let's determine all the YPOS information for each group
	   	if (ftype == "geis") {
			keypar (d0h//"["//i//"]", "ypos", silent=yes)
			yposval[i] = real (keypar.value) - yposinit
		} else {
			tabpar (root//"_d0f.fits[1]", "ypos", i)
			yposval[i] = real (tabpar.value) - yposinit
		}
	
	    if (ix >= nx) {
		ix = 0
		iy = iy + 1
	    }
	    pl = left + (w*ix)
	    pr = pl + w
	    pt = top - (h*iy)
	    pb = pt - h
	    printf ("location %.6g %.6g %.6g %.6g\n", pl, pr, pb, pt, >> script)
	    if (ftype == "geis") {
	    	printf ("ysection %s[%d]\n", d0h, i, >> script)
	    } else { 
		printf ("ysection %s[*,*,%d]\n", d0h, i, >> script)
	    }
	    if (i == 1)
	   	if (ftype == "geis") {
	    	    printf ("xsection %s[%d]\nxeval r\n", d0h, i, >> script)
		} else { 
	    	    printf ("xsection %s[*,*,%d]\nxeval r\n", d0h, i, >> script)
		}
	    if (ix == 0)
		ylab = 2
	    else
		ylab = 0
	    if (iy >= ny)
		xlab = 1
	    else
		xlab = 0
	    printf ("box %d %d; connect\n", xlab, ylab, >> script)
	    pr = pr-off
	    pt = pt-off
	    printf ("vmove %.6g %.6g; label 'Group %d'\n", pr, pt, i,
		    >> script)

	    # print the YPOS value in the plot as well
	    pt = pt - (off * expsize * 2.)*1.2
	    printf ("vmove %.6g %.6g; label 'YPOS= %.1g'\n", pr, pt, yposval[i],
		    >> script)

	    ix = ix + 1
	}
	printf ("location %.6g %.6g %.6g %.6g\n", left, right, bottom, top,
		>> script)
	print ("expand 1; title 'Binary Target Acquisition'", >> script)
	print ("xlabel 'Pixel'", >> script)
	print ("ylabel 'Counts'", >> script)

	# Get offset information.
	gcount = gcount + 2
	if (ftype == "geis") {
		listpix (shh//"["//gcount//"][894:894]", wcs="logical",
			 formats="", verbose=no) | scan (x, ix)
		listpix (shh//"["//gcount//"][896:896]", wcs="logical",
			 formats="", verbose=no) | scan (x, iy)
	} else {
		listpix (shh//"[894:894,"//gcount//"]", wcs="logical",
			 formats="", verbose=no) | scan (x, ix)
		listpix (shh//"[896:896,"//gcount//"]", wcs="logical",
			 formats="", verbose=no) | scan (x, iy)
	}
	if (ix > 32767)
	    ix = ix-65536
	if (iy > 32767)
	    iy = iy-65536
	xoff = ix * 2.25e-4 * 0.86
	yoff = iy * 2.25e-4 * 0.86	
	keypar (d0h, "expstart", silent=yes)
	costar = (int (keypar.value) >= 49348)
	keypar (d0h, "detector", silent=yes)
	yv2v3_calculate (xoff, yoff, keypar.value, costar=costar)
	v2off = yv2v3_calculate.v2
	v3off = yv2v3_calculate.v3

	# Calculate plate scales.
	if (costar) {
	    xoffpix = xoff * 13.11
	    yoffybs = yoff * 207.79
	} else {
	    xoffpix = xoff * 11.11
	    yoffybs = yoff * 179.0
	}
	h = (bottom - 0.075) / 6.
	w = (right + left) / 2.
	printf ("expand .65; vmove %.6g %.6g\n", w, h*4, >> script)
	printf ("justify 6; label 'X_OFFSET: %8.3f arcsec  %8.3f pixel'\n", 
			xoff, xoffpix, >> script)
	#printf ("justify 4; label 'X_OFFSET (pixel):  '\n", >> script)
	#printf ("justify 6; label '%.3f'\n", xoff, >> script)

	printf ("vmove %.6g %.6g\n", w, h*3, >> script)
	printf ("justify 6; label 'Y_OFFSET: %8.3f arcsec  %8.3f ybase'\n", 
			yoff, yoffybs, >> script)
	#printf ("justify 4; label 'Y_OFFSET (ybase):  '\n", >> script)
	#printf ("justify 6; label '%.3f'\n", yoff, >> script)

	printf ("vmove %.6g %.6g\n", w, h*2, >> script)
	printf ("justify 6; label 'V2_OFFSET: %7.3f arcsec'\n", 
			v2off, >> script)
	#printf ("justify 4; label 'V2_OFFSET (arcsec):  '\n", >> script)
	#printf ("justify 6; label '%.3f'\n", v2off, >> script)

	printf ("vmove %.6g %.6g\n", w, h, >> script)
	printf ("justify 6; label 'V3_OFFSET: %7.3f arcsec'\n", 
			v3off, >> script)
	#printf ("justify 4; label 'V3_OFFSET (arcsec):  '\n", >> script)
	#printf ("justify 6; label '%.3f'\n", v3off, >> script)

	# Add YPOS bar plot to page now
	# set boundaries for box
	x0 = 0.01
	xl = 0.17
	xglabel = x0 - 0.01
	xplabel = xl + 0.01
	xtick = x0 - 0.01
	yposold = 0.

	# reset value of gcount to reflect actual pointings
	gcount = gcount - 2
	
	# put in outline of box for YPOS plot
	printf("location 0. 0.25 0.05 0.95; expand 0.5\n", >> script)
	printf("limits 0 1 -265 260\n", >> script)
	printf("move %.2g -256; draw %.2g -256\n", x0, xl, >> script)
	printf("move %.2g -256;  draw %.2g 256 \n",x0, x0, >> script)
	printf("draw %.2g 256; draw %.2g -256; expand 0.5\n", xl, xl, >> script)
	printf("move %.2g -256; label '-256'\n", xplabel, >>script)
	printf("move %.2g 256; label '256'\n", xplabel,  >> script)
	printf("move %.2g 0; label '0'\n", xplabel, >> script)
	printf("move %.2g -265; label 'YPOS'\n", xplabel, >> script) 
	printf("move %.2g -273; label '(ybase)'\n", xplabel, >> script) 
	printf("move -0.1 -265; label 'Group'\n", >> script)
	printf("move 0. 265; label 'Scan Positions'\n", >> script)

	for (i=1; i<= gcount; i=i+1) {
		if ( abs(yposval[i] - yposold) < 5)
			xglabel = xglabel - 0.05
		printf("move %.2g %.2g\n", xtick, yposval[i], >> script)
		printf("draw %.2g %.2g\n", xl, yposval[i], >> script)
		printf("move %.2g %.2g; label '%d'\n",xglabel, yposval[i], i, >> script)
		
		yposold = yposval[i]
		#printf ("YPOS[%d] = %.1f\n",i,yposval)
	}
	printf("move %.2g %.2g; label '< Diode Center'\n", xplabel, yposval[gcount], >> script)
	ypostarg = yposval[gcount] - 8.
	printf("move %.2g %.2g; label '   Final Posn.'\n", xplabel, ypostarg, >> script)

	#Print out target YPOS value: 
	#	if ypos > 0, ypostarg = ypos - 128
	#	if ypos < 0, ypostarg = ypos + 128
	if (yposval[gcount] >= 0.) 
		ypostarg = yposval[gcount] - 128.
	else
		ypostarg = yposval[gcount] + 128.

	# Now put in the label for the Target Posn...
	xglabel = x0 + 0.05
	printf("move %.2g %.2g; ptype 6 1; dot\n",xglabel, ypostarg, >> script)
	printf("draw %.2g %.2g\n", xplabel, ypostarg, >> script)
	printf("move %.2g %.2g; label ' Target Posn.'\n", xplabel, ypostarg, >> script)
	yplabel = ypostarg - 10
	printf("move %.2g %.2g; label ' offset = %.1g'\n", xplabel, yplabel, ypostarg, >> script)



	# Add the compass with X/Y axes here...
	xcomp = 0.35
	ycomp = 0.075
	compsize = 0.05
	keypar(d0h, "orientat", silent=yes)
	orientat = real (keypar.value)

	printf("lweight 1\n",>> script)

	t_compass (script, xcomp, ycomp, compsize, orientat, yes)

	# Now put in the X and Y axes on the compass
	xaxes = xcomp - compsize
	yaxes = ycomp + compsize*1.5

	# Add the X and Y axes to the compass...
	printf("expand 0.7\n", >> script)
	printf("move %.2g %.2g; draw %.2g %.2g\n",xcomp, ycomp, xaxes, ycomp, >> script)
	xaxes = xcomp - compsize - (compsize / 5.)
	printf("move %.2g %.2g; label 'X'\n", xaxes, ycomp, >> script)

	printf("move %.2g %.2g; draw %.2g %.2g\n",xcomp, ycomp, xcomp, yaxes, >> script)
	yaxes = yaxes + (compsize / 5.)
	xaxes = xcomp + (compsize / 5.)
	printf("move %.2g %.2g; label 'Y'\n", xaxes, yaxes, >> script)
	
	
	# Add the script to the list.
	print (script, >> igi_list)
end
