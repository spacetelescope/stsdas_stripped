# SIMPRISM -- Simulate FOC prism images 

procedure simprism (spectrum, output, obsmode)

string	spectrum = "crcalspec$hz4_001.tab" {prompt = "spectrum to be simulated"}
string	output = "" {prompt = "output image"}
string	obsmode = "foc,f/96,f140m,prism1" {prompt = "observation mode"}
string	psffile = "" {prompt = "name of PSF for convolution"}
int	npix1 = 256 {prompt = "X size of output image", min = 1}
int	npix2 = 1024 {prompt = "Y size of output image", min = 1}
real	wstart = 1100. {prompt = "starting wavelength", min = 1.}
real	wstop = 7000. {prompt = "ending wavelength", min = 1.}
real	dw = 10. {prompt = "wavelength spacing for scratch array", min = 0.001}
real	texp = 900. {prompt = "exposure time in seconds", min = 0.001}
real	yzero = 100. {prompt = "row number of undispersed image in pixels"}
real	angle = 8.5 {prompt = \
		"rotation angle in degrees clockwise from vertical"}
string	graphtab = "crcomp$hstgraph_*" {prompt = "graph table"}
string	comptab = "crcomp$hstcomp_*" {prompt = "components table"}
bool	verbose = yes  {prompt = "list steps as performed?"}
pset	dispfiles = "" {prompt = "pset for names of dispersion files"}
real	area = 45238.93416 {prompt = "telescope area in cm^2"}
string	Version = "1July1994" {prompt = "date of installation"}
string	mode = "al"

begin
	# This is the name of a scratch image.
	string	tempfile

	# Check that IMAGES is loaded
	if ( !defpac( "images" ) || !defpac( "fourier" ) ) {
	      error (1,
	"IMAGES and FOURIER packages must be loaded to use this task.")
	}

	# call PRISMSIM
	tempfile = mktemp ("pout")
	if  (verbose) {
	   print ("Simulating 1-D prism spectrum:  ", tempfile)
	}
	# Note that several of the parameters have different names.
	prismsim (spectrum=spectrum, output=tempfile, obsmode=obsmode,
		npix1=npix1, npix2=npix2, 
		wstart=wstart, wstop=wstop, dw=dw, texp=texp, offset=yzero,
		graphtab=graphtab, comptab=comptab, dispfiles=dispfiles,
		area=area)

	# Call IMAGES.GEOTRAN.
	if (angle != 0.) {
	    if (verbose) {
	       print ("Rotating image ", tempfile)
	    }
	    geotran (input=tempfile, output=tempfile, database="",
		xrotation=-angle,
		yrotation=-angle, xin=INDEF, yin=INDEF, xout=INDEF,yout=INDEF,
		xshift=INDEF, yshift=INDEF, xmin=1.0, xmax=INDEF, ymin=1.0,
		ymax=INDEF, xscale=1.0, yscale=1.0, ncols=INDEF, nlines=INDEF,
		xmag=INDEF, ymag=INDEF, interpolant="spline3",
		boundary="nearest", constant=0, xsample=1., ysample=1.,
		fluxconserve=yes, nxblock=256, nyblock=256)
	}

	# Convolve image with PSF.
	if (psffile == "") {
	    if (npix1 == 1) {
		# Extract only a 1-D image section.
		imcopy (tempfile // "[1,*]", output)
	    } else {
		# This is a 2-D image.
		imcopy (tempfile, output)
	    }
	} else {
	    if  (verbose) {
	       print("Convolving image with PSF")
	    }
	    fconvolve (tempfile, psffile, output, inreal1=yes, inimag1=no,
		    inreal2=yes, inimag2=no, outreal=yes, outimag=no, pad=no,
		    inmemory=yes, len_blk=256, verbose=yes)
	}

	# delete the temporary image
	imdelete (images=tempfile, verify=no)
end
