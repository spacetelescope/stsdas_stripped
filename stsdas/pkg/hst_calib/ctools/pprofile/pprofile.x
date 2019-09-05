include <fset.h>		# defines F_REDIR
include <gset.h>
include <imhdr.h>

define	PIXELS			1	# option for ixunits
define	ARCSEC			2	# option for ixunits
define	N_IMAGES_INCR		5	# for allocating space
define	X_EXTEND_BORDER		0.05	# fraction to extend X axis in plot
define	Y_EXTEND_BORDER		0.0	# fraction to extend Y axis in plot
define	YMAX_LOG_PROFILE	0.2	# max Y for log profile plot
define	YMIN_LOG_PROFILE       -5.0	# min Y for log profile plot
define	LOG_NEGATIVE	       -6.0	# logarithm of a negative number
define	YMIN_ENERGY		0.0	# min Y for encircled energy plot
define	F48_PIXSIZE		0.04497	# FOC f/48
define	F75_PIXSIZE		0.02825	# FOC f/48 + COSTAR
define	F96_PIXSIZE		0.02237	# FOC f/96
define	F151_PIXSIZE		0.01435	# FOC f/96 + COSTAR
define	F288_PIXSIZE		0.00746	# FOC f/96 + HRA
define	UNIT_PIXSIZE		1.0	# pixel size if optcrly not found
define	SZ_KEYVAL		11	# size of short string for keyword value

# pprofile -- plot profile & encircled energy
#
# This task is based on a Fortran program written by Robert Jedrzejewski.
#
# Phil Hodge, 20-Aug-1992  Task created.
# Phil Hodge, 13-May-1994  Use F_RATIO instead of OPTCRLY for focal ratio.

procedure pprofile()

pointer infile			# scratch for cl parameter "input"
real	xc, yc			# location of center of PSF
real	background		# background to subtract
real	target			# max value for normalizing encircled energy
real	p_ymax			# upper limit for profile plot
real	p_ymin			# lower limit for profile plot
real	e_ymax			# upper limit for encircled energy plot
int	nrmax			# max radius in pixels
bool	plot_stddev		# plot standard deviation for profile?
bool	improve_position	# should we call mpc2_cntr?
int	cboxsize		# box size for mpc2_cntr
pointer plot_what		# make what plots (profile, energy, both)?
pointer outfile			# scratch for cl parameter "outfile"
char	xunits[SZ_KEYVAL]	# X units in pixels or arcsec?  (value ignored)
int	ixunits			# code for value of xunits
real	imscale			# arcsec per pixel or INDEF
bool	logy			# logarithmic Y axis?
bool	verbose			# print image name, xc, yc, background?
pointer device			# plot device
#--
pointer sp
pointer input			# scratch for name of input image
pointer title			# scratch for plot title
pointer xtitle			# scratch for title for X axis
pointer ytitle			# scratch for title for Y axis
pointer im
pointer pix			# points to image data
pointer radius			# array of radius values
pointer profile			# array of profile values (or log10 profile)
pointer sigma			# array of standard deviations of profile
pointer profile_u		# upper profile = profile plus one sigma
pointer profile_l		# lower profile = profile minus one sigma
pointer energy			# array of encircled energy values
pointer npts			# array of array sizes
pointer npts_ee			# array of array sizes for encircled energy
char	camera[SZ_KEYVAL]	# f/48, f/96 or f/288 for plot title
char	cam2[SZ_KEYVAL]		# from a subsequent image, compare with camera
real	pixsize			# pixel size in arcseconds per pixel
real	junk			# pixel size for image after the first
real	ymin, ymax		# plot range in Y
real	xcent, ycent		# xc & yc offset relative to image section
real	prof1			# non-normalized value of profile at center
int	ofd			# fd for output file, or zero if none
int	max_images		# current size of allocated arrays
int	nimages			# number of images
int	imin, imax, jmin, jmax	# limits of image section
int	nx, ny			# size of image section
int	off			# offset from start of allocated memory
bool	plot_profile		# should we make a profile plot?
bool	plot_energy		# should we make an encircled-energy plot?
bool	init			# if init=true, open input file
bool	done
pointer immap()
int	clgwrd()
real	clgetr()
int	clgeti()
int	access(), open()
bool	clgetb()
bool	strne()

begin
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (plot_what, SZ_FNAME, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_FNAME, TY_CHAR)
	call salloc (xtitle, SZ_FNAME, TY_CHAR)
	call salloc (ytitle, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)

	call clgstr ("input", Memc[infile], SZ_FNAME)

	# Get input image name, position, etc.  These are either taken
	# from a text file or gotten from cl parameters.  This is called
	# again in the while loop below.  If the input is a text file
	# we will open it in this call.
	init = true
	call ee_next_i (Memc[infile], Memc[input], SZ_FNAME,
			xc, yc, background, target, done, init)
	init = false

	# Get the rest of the cl parameters.
	plot_stddev = clgetb ("stddev")
	improve_position = clgetb ("improve_pos")
	if (improve_position)
	    cboxsize = clgeti ("cboxsize")
	nrmax = clgeti ("rmax")
	p_ymax = clgetr ("pymax")			# limits for plot
	p_ymin = clgetr ("pymin")
	e_ymax = clgetr ("eymax")
	call clgstr ("plot_what", Memc[plot_what], SZ_FNAME)
	if (Memc[plot_what] == 'b') {			# both
	    plot_profile = true
	    plot_energy = true
	} else if (Memc[plot_what] == 'p') {		# profile only
	    plot_profile = true
	    plot_energy = false
	} else if (Memc[plot_what] == 'e') {		# encircled energy only
	    plot_profile = false
	    plot_energy = true
	} else if (Memc[plot_what] == 'n') {		# neither
	    plot_profile = false
	    plot_energy = false
	}
	call clgstr ("outfile", Memc[outfile], SZ_FNAME)

	ixunits = clgwrd ("xunits", xunits, SZ_KEYVAL, "|pixels|arcsec|")
	if (ixunits < 1)
	    call error (1, "invalid value of xunits")
	if (ixunits == ARCSEC)
	    imscale = clgetr ("imscale")
	else
	    imscale = INDEF

	logy = clgetb ("logy")
	verbose = clgetb ("verbose")
	call clgstr ("device", Memc[device], SZ_FNAME)

	# allocate memory
	max_images = N_IMAGES_INCR
	call malloc (radius, max_images*nrmax, TY_REAL)
	call malloc (profile, max_images*nrmax, TY_REAL)
	call malloc (sigma, max_images*nrmax, TY_REAL)
	call malloc (profile_u, max_images*nrmax, TY_REAL)
	call malloc (profile_l, max_images*nrmax, TY_REAL)
	call malloc (energy, max_images*nrmax, TY_REAL)
	call malloc (npts, max_images, TY_INT)
	call malloc (npts_ee, max_images, TY_INT)

	# If an output file was specified, open it.  If it already
	# exists, append to it rather than clobbering it.
	if (Memc[outfile] == EOS || Memc[outfile] == ' ') {
	    ofd = NULL
	} else {
	    if (access (Memc[outfile], 0, TEXT_FILE) == YES)
		ofd = open (Memc[outfile], APPEND, TEXT_FILE)
	    else
		ofd = open (Memc[outfile], NEW_FILE, TEXT_FILE)
	}

	# Read each image and compute profile and encircled energy.
	nimages = 0
	while (!done) {

	    nimages = nimages + 1

	    if (nimages > max_images) {
		# Allow more space.
		max_images = max_images + N_IMAGES_INCR
		call realloc (radius, max_images*nrmax, TY_REAL)
		call realloc (profile, max_images*nrmax, TY_REAL)
		call realloc (sigma, max_images*nrmax, TY_REAL)
		call realloc (profile_u, max_images*nrmax, TY_REAL)
		call realloc (profile_l, max_images*nrmax, TY_REAL)
		call realloc (energy, max_images*nrmax, TY_REAL)
		call realloc (npts, max_images, TY_INT)
		call realloc (npts_ee, max_images, TY_INT)
	    }

	    im = immap (Memc[input], READ_ONLY, NULL)

	    # Get the focal ratio and the pixel size.
	    if (nimages == 1) {
		call ee_fratio (im, ixunits, imscale, camera, SZ_KEYVAL,
				pixsize)
		call strcpy (camera, cam2, SZ_KEYVAL)
	    } else {
		call ee_fratio (im, ixunits, imscale, cam2, SZ_KEYVAL, junk)
	    }

	    if (strne (camera, cam2)) {
		call eprintf ("image `%s' is not the same relay; will skip\n")
		    call pargstr (Memc[input])
		nimages = nimages - 1

	    } else {

		# If some of the parameters are INDEF, do something.
		call ee_check_input (im, xc, yc, background, target)

		if (improve_position)
		    call mpc2_cntr (im, xc, yc, cboxsize, xc, yc)

		call ee_read_section (im, xc, yc, nrmax,
			    pix, imin, imax, jmin, jmax)

		nx = imax - imin + 1
		ny = jmax - jmin + 1
		xcent = xc - imin + 1.		# offset for image section
		ycent = yc - jmin + 1.

		# Compute encircled energy, etc.
		off = (nimages-1) * nrmax	# offset to current array
		call encirc (Memr[pix], nx, ny, xcent, ycent, pixsize,
			nrmax, background, target, plot_stddev,
			Memr[radius+off], Memr[profile+off], Memr[sigma+off],
			Memr[profile_u+off], Memr[profile_l+off],
			Memr[energy+off],
			Memi[npts+nimages-1], Memi[npts_ee+nimages-1], prof1)

		if (verbose) {
		    call printf ("%s, center = [%0.3f,%0.3f]\n")
			call pargstr (Memc[input])
			call pargr (xc)
			call pargr (yc)
		    call printf (
		"    value = %0.6g, background = %0.6g, target = %0.6g\n")
			call pargr (prof1)
			call pargr (background)
			call pargr (target)
		    call flush (STDOUT)
		}

		if (ofd != NULL) {
		    call ee_print (Memc[input], xc, yc,
			background, target, prof1, ofd,
			Memr[radius+off], Memr[profile+off],
			Memr[sigma+off], Memr[energy+off], 
			Memi[npts+nimages-1], Memi[npts_ee+nimages-1],
			plot_stddev)
		}
	    }

	    # Get the name of the next image.  If we have reached the end
	    # of the file, close the file and set done=true.
	    call ee_next_i (Memc[infile], Memc[input], SZ_FNAME,
			xc, yc, background, target, done, init)

	    # Leave the last image open so we can replot with another set
	    # of values for background and target counts.  We really only
	    # need to leave it open if it's the only image.
	    if (!done) {
		call imunmap (im)
		im = NULL
	    }
	}

	# Update the coordinates, but only if there is just one image.
	if (improve_position && nimages == 1) {
	    call clputr ("xc", xc)
	    call clputr ("yc", yc)
	}

	if (pixsize == UNIT_PIXSIZE)
	    call strcpy ("Pixels", Memc[xtitle], SZ_FNAME)
	else
	    call strcpy ("Arcseconds", Memc[xtitle], SZ_FNAME)

	done = nimages <= 0

	# Make the plots.  This is a loop so the user can change the
	# background and replot (if nimages = 1).
	while (!done) {

	    # Plot profile.
	    if (plot_profile) {
		call strcpy ("Azimuth Averaged Profile", Memc[title],
			SZ_FNAME)
		if (camera[1] != EOS) {
		    call strcat (" - ", Memc[title], SZ_FNAME)
		    call strcat (camera, Memc[title], SZ_FNAME)
		}
		if (logy) {
		    call strcpy ("Log Relative Intensity", Memc[ytitle],
				SZ_FNAME)
		    if (p_ymax > 0.)
			ymax = log10 (p_ymax)
		    else
			ymax = YMAX_LOG_PROFILE
		    if (p_ymin > 0.)
			ymin = log10 (p_ymin)
		    else
			ymin = YMIN_LOG_PROFILE

		    call ee_log10 (Memr[profile], nrmax, nimages, Memi[npts])
		    if (plot_stddev) {
			call ee_log10 (Memr[profile_u], nrmax, nimages,
				Memi[npts])
			call ee_log10 (Memr[profile_l], nrmax, nimages,
				Memi[npts])
		    }
		} else {
		    call strcpy ("Relative Intensity", Memc[ytitle],
				SZ_FNAME)
		    ymax = p_ymax
		    ymin = p_ymin
		}
		call ee_plot (Memc[device], Memc[title],
		    Memc[xtitle], Memc[ytitle], nimages,
		    Memr[radius], Memr[profile],
		    Memr[profile_u], Memr[profile_l], plot_stddev,
		    nrmax, Memi[npts], ymin, ymax)
	    }

	    # Plot encircled energy.  Don't plot standard deviations.
	    if (plot_energy) {
		call strcpy ("Encircled Energy", Memc[title], SZ_FNAME)
		call strcpy ("Fraction (%)", Memc[ytitle], SZ_FNAME)
		if (camera[1] != EOS) {
		    call strcat (" - ", Memc[title], SZ_FNAME)
		    call strcat (camera, Memc[title], SZ_FNAME)
		}
		ymax = e_ymax
		ymin = YMIN_ENERGY
		call ee_plot (Memc[device], Memc[title],
		    Memc[xtitle], Memc[ytitle], nimages,
		    Memr[radius], Memr[energy],
		    Memr[profile_u], Memr[profile_l], false,
		    nrmax, Memi[npts_ee], ymin, ymax)
	    }

	    # If there's just one input image, give the user the option
	    # to change the background & target counts and replot.
	    done = true
	    if (nimages == 1) {
		if (clgetb ("chg_back")) {
		    done = false
		    # Note that we get "bck" & "tgt"; their mode is "q".
		    call clputr ("bck", background)	# set the defaults
		    call clputr ("tgt", target)
		    background = clgetr ("bck")		# get new values
		    target = clgetr ("tgt")
		    # Note that we assume here that nimages=1.
		    call encirc (Memr[pix], nx, ny, xcent, ycent, pixsize,
			nrmax, background, target, plot_stddev,
			Memr[radius], Memr[profile], Memr[sigma],
			Memr[profile_u], Memr[profile_l],
			Memr[energy], Memi[npts], Memi[npts_ee], prof1)

		    if (ofd != NULL) {
			call ee_print (Memc[input], xc, yc,
				background, target, prof1, ofd,
				Memr[radius], Memr[profile],
				Memr[sigma], Memr[energy], 
				Memi[npts], Memi[npts_ee], plot_stddev)
		    }
		}
	    }
	}

	# Close the last image.
	if (im != NULL)
	    call imunmap (im)

	if (ofd != NULL)
	    call close (ofd)

	call mfree (radius, TY_REAL)
	call mfree (profile, TY_REAL)
	call mfree (energy, TY_REAL)
	call mfree (npts, TY_INT)

	call sfree (sp)

	call eprintf ("Bob's your uncle\n")
end

# ee_next_i -- get name of next image
# This routine gets the input image name, position of the PSF, background
# to be subtracted, and target value for encircled energy.  These are
# either taken from a text file (if infile is @filename) or gotten from
# cl parameters.  If the input is a text file we will open it if init=true.

procedure ee_next_i (infile, input, maxch, xc, yc,
			background, target, done, init)

char	infile[ARB]	# i: image name or @filename
char	input[ARB]	# o: name of current image
int	maxch		# i: maximum size of 'input'
real	xc, yc		# o: center of PSF, in pixels
real	background	# o: background to be subtracted
real	target		# o: expected total number of counts in PSF
bool	done		# o: set to true if no more images
bool	init		# i: if infile is @filename, open file if init=true
#--
pointer sp
pointer buf		# buffer for reading from input file
int	fd		# fd for input file
int	start		# infile[start] is the first letter of the file name
int	ip		# for ctowrd, ctor
bool	got_a_line	# have we gotten a non-comment line from input?
real	clgetr()
int	open(), getline(), fstati(), ctowrd(), ctor()
bool	streq()

begin
	start = 2
	if (streq (infile, "STDIN"))
	    start = 1

	if (infile[1] == '@' || streq (infile, "STDIN")) {

	    # We have a file containing image names.

	    # If we're reading from the standard input, and it has not been
	    # redirected, print a prompt message.
	    if (init) {
		if (streq (infile[start], "STDIN")) {
		    if (fstati (STDIN, F_REDIR) == NO) {
			call printf (
	    "Give image name, pixel location in X and Y, background, target:\n")
			call flush (STDOUT)
		    }
		}
	    }

	    call smark (sp)
	    call salloc (buf, SZ_LINE, TY_CHAR)

	    done = false

	    if (init)
		fd = open (infile[start], READ_ONLY, TEXT_FILE)

	    got_a_line = false

	    while (!got_a_line) {
		if (getline (fd, Memc[buf]) == EOF) {
		    call close (fd)
		    got_a_line = true		# to get out of while loop
		    done = true

		} else if (Memc[buf] != '#') {

		    ip = 1
		    if (ctowrd (Memc[buf], ip, input, maxch) > 0) {
			got_a_line = true
			if (ctor (Memc[buf], ip, xc) < 1)
			    xc = INDEFR
			if (ctor (Memc[buf], ip, yc) < 1)
			    yc = INDEFR
			if (ctor (Memc[buf], ip, background) < 1)
			    background = INDEFR
			if (ctor (Memc[buf], ip, target) < 1)
			    target = INDEFR
		    }
		}
	    }
	    call sfree (sp)

	} else if (init) {

	    # We have a single image name.
	    call strcpy (infile, input, maxch)
	    xc = clgetr ("xc")
	    yc = clgetr ("yc")
	    background = clgetr ("background")
	    target = clgetr ("target")

	    done = false

	} else {

	    done = true
	}
end

# ee_fratio -- get focal ratio of current image
# This routine assigns a value to the camera name (e.g. "f/96") and
# to the image scale in arcseconds per pixel.  If the ixunits argument
# is PIXELS, the image scale will be set to one.  If ixunits is ARCSEC
# then the scale is either gotten from the imscale argument (if it has
# a definite value) or from the focal ratio, as given by the F_RATIO
# keyword.  If F_RATIO is missing from the image header or is not 48,
# 75, 96, 151 or 288, a null string will be returned, and the pixsize
# will be set to one.
# This routine is FOC-specific.

procedure ee_fratio (im, ixunits, imscale, camera, maxch, pixsize)

pointer im		# i: pointer to imhdr struct
int	ixunits		# i: pixels or arcsec
real	imscale		# i: image scale or INDEF
char	camera[ARB]	# o: the focal ratio, e.g. "f/96"
int	maxch		# i: max size of camera string
real	pixsize		# o: pixel size in arcseconds per pixel
#--
int	f_ratio		# focal ratio
int	imaccf(), imgeti()

begin
	if (imaccf (im, "f_ratio") == YES) {
	    f_ratio = imgeti (im, "f_ratio")
	} else {
	    f_ratio = -1
	}

	if (f_ratio == 48) {

	    call strcpy ("f/48", camera, maxch)
	    pixsize = F48_PIXSIZE

	} else if (f_ratio == 75) {

	    call strcpy ("f/75", camera, maxch)
	    pixsize = F75_PIXSIZE

	} else if (f_ratio == 96) {

	    call strcpy ("f/96", camera, maxch)
	    pixsize = F96_PIXSIZE

	} else if (f_ratio == 151) {

	    call strcpy ("f/151", camera, maxch)
	    pixsize = F151_PIXSIZE

	} else if (f_ratio == 288) {

	    call strcpy ("f/288", camera, maxch)
	    pixsize = F288_PIXSIZE

	} else {

	    call strcpy ("", camera, maxch)
	    pixsize = UNIT_PIXSIZE

	}

	# Override with user-specified value?
	if (ixunits == PIXELS)
	    pixsize = 1.
	else if (!IS_INDEF(imscale))
	    pixsize = imscale
end

# ee_read_section -- get pointer to section of image

procedure ee_read_section (im, xc, yc, nrmax,
			pix, imin, imax, jmin, jmax)

pointer im		# i: pointer to imhdr struct
real	xc, yc		# i: pixel coordinates of feature
int	nrmax		# i: max radius
pointer pix		# o: pointer to image data
int	imin, imax	# o: limits of section on first axis
int	jmin, jmax	# o: limits of section on second axis
#--
pointer imgs2r()

begin
	imin = max (int(xc-nrmax), 1)
	imax = min (int(xc+nrmax+1.), IM_LEN(im,1))
	jmin = max (int(yc-nrmax), 1)
	jmax = min (int(yc+nrmax+1.), IM_LEN(im,2))

	pix = imgs2r (im, imin, imax, jmin, jmax)
end

# ee_check_input -- check for INDEF values
# If either background or target is INDEF, we get the value from the cl
# parameter.  If the position (either xc or yc) is INDEF, the position
# is taken to be the location of the maximum data value in the image.

procedure ee_check_input (im, xc, yc, background, target)

pointer im		# i: pointer to imhdr struct
real	xc, yc		# io: pixel coordinates of center of PSF
real	background	# io: background value to subtract
real	target		# io: target value for encircled energy
#--
pointer v		# points to data for a line
real	maxval		# maximum data value
int	ixc, iyc	# current estimate of location of maximum
int	i, j
pointer imgl2r()
real	clgetr()

begin
	if (IS_INDEFR(background))
	    background = clgetr ("background")

	if (IS_INDEFR(target))
	    target = clgetr ("target")

	# If the position was not specified, find the location of maximum.
	if (IS_INDEFR(xc) || IS_INDEFR(yc)) {
	    v = imgl2r (im, 1)
	    maxval = Memr[v]
	    ixc = 1
	    iyc = 1
	    do j = 1, IM_LEN(im,2) {
		v = imgl2r (im, j)
		do i = 0, IM_LEN(im,1)-1 {		# zero indexed
		    if (Memr[v+i] > maxval) {
			maxval = Memr[v+i]
			ixc = i + 1
			iyc = j
		    }
		}
	    }
	    xc = ixc
	    yc = iyc
	}
end

# ee_plot -- plot a curve

procedure ee_plot (device, title, xlabel, ylabel, nimages,
		x, y, y_u, y_l, plot_stddev, nrmax, npts, ymin, ymax)

char	device[ARB]		# i: graphics device
char	title[ARB]		# i: title for plot
char	xlabel[ARB]		# i: label for X axis
char	ylabel[ARB]		# i: label for Y axis
int	nimages			# i: number of images to plot
real	x[nrmax,ARB]		# i: X values to be plotted
real	y[nrmax,ARB]		# i: Y values to be plotted
real	y_u[nrmax,ARB]		# i: Y values plus one sigma
real	y_l[nrmax,ARB]		# i: Y values minus one sigma
bool	plot_stddev		# i: also plot curves at + and - one sigma?
int	nrmax			# i: allocated size of x,y arrays
int	npts[ARB]		# i: actual size of arrays to plot
real	ymin, ymax		# i: plot limits in Y
#--
pointer gp
real	xmin, xmax
real	extend
real	xstart, xstop		# actual plot limits (i.e. incl border)
real	ystart, ystop
int	start			# starting index for std dev curves
int	i
pointer gopen()

# Graphics cursor stuff:
pointer sp
pointer strval			# text (if any) associated with cursor
real	wx, wy			# cursor coordinates
int	wcs			# wcs to which coordinates belong
int	key			# keystroke value of cursor event
int	junk			# value of clgcur
int	clgcur(), fstati()
bool	streq()

begin
	xmin = 0.
	xmax = x[npts[1],1]
	do i = 2, nimages
	    xmax = max (xmax, x[npts[i],i])

	extend = (xmax - xmin) * X_EXTEND_BORDER
	xstart = xmin - extend
	xstop = xmax + extend

	extend = (ymax - ymin) * Y_EXTEND_BORDER
	ystart = ymin - extend
	ystop = ymax + extend

	gp = gopen (device, NEW_FILE, STDGRAPH)

	call gswind (gp, xstart, xstop, ystart, ystop)

	call glabax (gp, title, xlabel, ylabel)

	do i = 1, nimages {

	    start = min (2, npts[i])

	    call gseti (gp, G_PLTYPE, mod(i-1,4)+1)
#	    call gseti (gp, G_PLCOLOR, 1)			# black
	    call gpline (gp, x[1,i], y[1,i], npts[i])
	    # Plot upper & lower lines for standard deviation.
	    if (plot_stddev) {
#		call gseti (gp, G_PLCOLOR, 5)			# blue
		call gpline (gp, x[start,i], y_u[start,i], npts[i]-start+1)
		call gpline (gp, x[start,i], y_l[start,i], npts[i]-start+1)
	    }
	}

	call gclose (gp)

	# Bring up the graphics cursor if we're running this interactively.
	if (streq (device, "stdgraph") && fstati (STDGRAPH, F_REDIR) == NO) {
	    call smark (sp)
	    call salloc (strval, SZ_FNAME, TY_CHAR)
	    junk = clgcur ("cursor", wx, wy, wcs, key, Memc[strval], SZ_FNAME)
	    call sfree (sp)
	}
end

# ee_log10 -- take log10 of profile

procedure ee_log10 (profile, nrmax, nimages, npts)

real	profile[nrmax,nimages]	# io: input profile, output log10 (profile)
int	nrmax			# i: allocated array size
int	nimages			# i: upper limit to second index of profile
int	npts[nimages]		# i: actual array size for each image
#--
int	i, j

begin
	do j = 1, nimages {
	    do i = 1, npts[j] {
		if (profile[i,j] > 0.0)
		    profile[i,j] = log10 (profile[i,j])
		else
		    profile[i,j] = LOG_NEGATIVE
	    }
	}
end

# ee_print -- print profile and encircled energy

procedure ee_print (image, xc, yc,
		background, target, prof1, fd,
		radius, profile, sigma, energy, npts, npts_ee, stddev)

char	image[ARB]		# i: name of input image
real	xc, yc			# i: pixel coordinates of feature
real	background		# i: background value that was subtracted
real	target			# i: expected total counts
real	prof1			# i: non-normalized profile[1]
int	fd			# i: fd for output text file
real	radius[ARB]		# i: array of radii
real	profile[ARB]		# i: array of profile values
real	sigma[ARB]		# i: array of standard deviations of profile
real	energy[ARB]		# i: array of encircled energy values
int	npts			# i: size of arrays
int	npts_ee			# i: size of array for encircled energy
bool	stddev			# i: print standard deviation?
#--
int	i

begin
	call fprintf (fd, "# %s, center = [%0.3f,%0.3f]\n")
	    call pargstr (image)
	    call pargr (xc)
	    call pargr (yc)
	call fprintf (fd,
		"# value = %0.6g, background = %0.6g, target = %0.6g\n")
	    call pargr (prof1)
	    call pargr (background)
	    call pargr (target)

	if (stddev) {

	    call fprintf (fd,
	"#       radius        profile         energy    sig_profile\n")

	    do i = 1, npts_ee {
		call fprintf (fd, "%14.6g %14.6g %14.6g %14.6g\n")
		    call pargr (radius[i])
		    call pargr (profile[i])
		    call pargr (energy[i])
		    call pargr (sigma[i])
	    }
	    do i = npts_ee+1, npts {
		call fprintf (fd, "%14.6g %14.6g                %14.6g\n")
		    call pargr (radius[i])
		    call pargr (profile[i])
		    call pargr (sigma[i])
	    }

	} else {

	    call fprintf (fd,
		"#       radius        profile  encircled energy\n")

	    do i = 1, npts_ee {
		call fprintf (fd, "%14.6g %14.6g %14.6g\n")
		    call pargr (radius[i])
		    call pargr (profile[i])
		    call pargr (energy[i])
	    }
	    do i = npts_ee+1, npts {
		call fprintf (fd, "%14.6g %14.6g\n")
		    call pargr (radius[i])
		    call pargr (profile[i])
	    }
	}
end
