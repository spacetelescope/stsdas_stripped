# plot the jitter gray scale images for STIS

procedure opp_jitter (jih, igi_output, vleft, vright, vbottom, vtop)

char	jih		{prompt = "jih data file name"}
char	igi_output 	{prompt = "output igi script file name"}
real	vleft		{prompt = "left location of the plot"}
real	vright		{prompt = "right location of the plot"}
real	vbottom		{prompt = "bottom location of the plot"}
real	vtop		{prompt = "top location of the plot"}

begin
	# Declarations
	char	ljih
	char	script			# Igi script.
	real	vl, vr, vb, vt

	real	zmax, zmin		# max/min used in gray scale image plot.
	real	xpixinc, ypixinc
	char	fname
	int	xdim, ydim, x1, x2
	
	ljih = jih
	script = igi_output
	vl = vleft
	vr = vright
	vb = vbottom
	vt = vtop

	# Draw the jitter images
	printf ("fontset hard\n", >> script)
	printf ("expand 0.65\n", >> script)

	fname = ljih
        imgets (fname, "i_naxis1")
        xdim = int (imgets.value)
        imgets (fname, "i_naxis2")
        ydim = int (imgets.value)
	if (xdim <= 1 || ydim <= 1) {
	    printf ("vmove %0.4f %0.4f\n", (vl+vr)/2., (vb+vt)/2., >> script)
	    printf ("justify 5\n", >> script)
	    printf ("label '(Jitter image has no 2-D data)'\n", >> script)
	    bye
	}
	xdim = max (xdim, ydim)
        x1 = 10**(int(log10(xdim)))
        x2 = int(xdim/x1) * x1

	# get the pixel scale
        imgets (fname, "xpixinc")
        xpixinc = real (imgets.value)
        imgets (fname, "ypixinc")
        ypixinc = real (imgets.value)

	# find the brightness range by using min/max
	minmax (fname, force = yes, update = no, verbose = no)
	zmin = minmax.minval
	zmax = minmax.maxval

	# generate the gray scale plot
	printf ("zsection %s\n", fname, >> script)
	printf ("fitpix %0.2f %0.2f %0.2f %0.2f\n", vl, vr, vb, vt, >> script)

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

	# plot the labels
	printf ("xlabel 'V2 (%4.1f mas/pixel)'\n", xpixinc, >> script)
	printf ("ylabel 'V3 (%4.1f mas/pixel)'\n", ypixinc, >> script)
	printf ("angle 0\n", >> script)
	printf ("title '%s'\n", fname, >> script)
end
