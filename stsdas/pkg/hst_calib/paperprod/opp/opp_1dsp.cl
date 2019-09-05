# plot the 1-D spectrum for STIS

procedure opp_1dsp (rootname, igi_output)

char	rootname	{prompt="root name of the input file"}
char	igi_output 	{prompt="output igi script file name"}
char	fname0		{prompt="file name used"}

begin

	# Declarations
	string	root		# root name
	string	script		# igi script.
	string	ext		# extension name
	string	fname
	real 	x, y, xlen, ylen
	real	minwave, maxwave, minflux, maxflux
	int	i
	
	# Get input parameters
	root = rootname 
	script = igi_output

        # decide which file to use
        if (access(root//"_sx1.fits")) {
            fname0 = root//"_sx1.fits"
            ext = "_sx1"
        } else if (access(root//"_x1d.fits")) {
            fname0 = root//"_x1d.fits"
            ext = "_x1d"
	}
	fname = fname0//"[0]"

	# initialize the page
	printf ("vpage 0.03 0.97 0.03 0.97\n", >> script)
	printf ("fontset hard\n", >> script)
	printf ("expand 0.6\n", >> script)

	x = 0.1
	y = 0.12

	# determine the window size
	xlen = 0.56
	ylen = xlen * 1.324

	# how many rows (orders)?
	tinfo (fname0, >& "dev$null")

	# read the X (wavelength) limits
        keypar (fname, "minwave", silent=yes)
        minwave = real (keypar.value)
        keypar (fname, "maxwave", silent=yes)
        maxwave = real (keypar.value)

	t_o1drange (fname0)
	minflux = t_o1drange.fluxmin
	maxflux = t_o1drange.fluxmax
	if (maxflux <= 0.) {
	    maxflux = 5.e-11
	    minflux = 0.
	} else if (minflux/maxflux < 0.7) minflux = 0.

	printf ("location %g %g %g %g\n", x, (x+xlen), y, (y+ylen), >> script)

	# plot the image
	printf ("data %s\n", fname0, >> script)
	printf ("limits %0.6g %0.6g %0.4g %0.4g\n", minwave, maxwave,
		minflux, maxflux, >> script)
	printf ("box\n", >> script)

	# loop through the orders
	i = 1
	while (i <= tinfo.nrows) {
	    printf ("xcolumn wavelength %d\n", i, >> script)
	    printf ("ycolumn flux %d\n", i, >> script)
	    printf ("connect\n", >> script)
	    i = i + 1
	}

	# write labels
	printf ("expand 0.65\n", >> script)
	printf ("title 'Extracted 1-D Spectrum (%s)'\n", ext, >> script)

        printf ("justify 5; vmove 0.38 0.05; angle 0\n", >> script)
	printf ("label 'Wavelength (Angstrom)'\n", >> script)

        printf ("justify 5; vmove 0.01 0.5; angle 90\n", >> script)
	printf ("label 'Flux (erg/s/cm**2/A)'\n", >> script)
        printf ("angle 0\n", >> script)
end
