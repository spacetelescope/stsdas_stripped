include	<imhdr.h>
include	<time.h>

define	EPS		1.e-10
define	LP_TYPES	"|none|circular|square"
define	NO_FILTER	1
define	CIRCUL_FILTER	2
define	SQUARE_FILTER	3
define	NOISE_TYPES	"|independent|Poisson"
define	INDEPENDENT	1
define	POISSON		2
define	SM_TYPES	"|input|Gaussian|psf|Markov|white"
define	SM_INPUT	1
define	SM_GAUSSIAN	2
define	SM_PSF		3
define	SM_MARKOV	4
define	SM_WHITE	5
define	SM_IMAGE	6
define	FILTER_TYPES	"|Wiener|geometric|inverse|parametric"
define	W_WIENER	1
define	W_GEOMETRIC	2
define	W_INVERSE	3
define	W_PARAMETRIC	4

define	AR	Memr[$1 + ($3-1)*sizex   + $2-1]    # real array element
define	AX	Memx[$1 + ($3-1)*sizex   + $2-1]    # complex array element
define	AR2	Memr[$1 + ($3-1)*sizex/2 + $2-1]    # real array element

# WIENER  --   Fourier non-iterative deconvolution of 2-D images.
#
#
# 12 Dec 90     I. Busko  -  Task created.
# 19 Dec 90        "      -  Non-square arrays, Wiener filter added.
# 21 Dec 90        "      -  Noiseless PSF supported.
# 17 Jan 91        "      -  Added file name template/list support.
# 14 May 91        "      -  Poisson noise model.
# 23 May 91        "      -  Prunning filter.
# 20 Jun 91        "      -  Signal model, correlation factor.
# 25 Jul 91        "      -  Geometric mean filter.
# 23 Feb 93        "      -  Psets.
# 25 Mar 93        "      -  Power spectrum windowing.
# 26 Mar 93        "      -  HISTORY records appended in output header.


procedure t_wiener()

char	imlisti[SZ_LINE]		# input image list
char	imlisto[SZ_LINE]		# output image list/directory
char	inpsf[SZ_PATHNAME]		# psf image
char	filter[SZ_PATHNAME]		# optional filter image template
int	low_pass			# filter type
int	signal_model			# signal model
int	wiener_type			# type of Wiener filter
int	statistic			# noise statistical model
real	px0, py0			# optional position of psf in psf image
real	fwhm				# smoothing radius
real	gamma				# Wiener parameter
real	mask				# star mask
real	fnoise				# noise power estimate
real	correl				# correlation factor
real	mfwhm				# Gaussian model fwhm
bool	nlpsf				# noise-less psf ?
bool	verb

pointer	pp				# pset pointer
char	input[SZ_PATHNAME]		# input image name
char	output[SZ_PATHNAME]		# output image name
char	dirnamei[SZ_PATHNAME]		# directory name
char	dirnameo[SZ_PATHNAME]		# directory name
char	model[SZ_LINE]			# image with signal model
char	str[SZ_LINE]
int	listi, listo, root_len

pointer	clopset()
int	strdic()
int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
real	clgpsetr()
bool	clgpsetb()
bool	clgetb()

begin
	# Read input parameters.
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("psf", inpsf, SZ_PATHNAME)
	call clgstr ("output", imlisto, SZ_LINE)
	call clgstr ("filter", filter, SZ_PATHNAME)

	# Now from appropriate psets.
	pp     = clopset ("psfpars")
	nlpsf  = clgpsetb (pp, "nlpsf")
	px0    = clgpsetr (pp, "px0")
	py0    = clgpsetr (pp, "py0")
	mask   = clgpsetr (pp, "mask")
	call clcpset (pp)

	pp = clopset ("filterpars")
	call clgpseta (pp, "ftype", str, SZ_LINE)
	wiener_type = strdic (str, str, SZ_LINE, FILTER_TYPES)
	if (wiener_type == 0)
	    wiener_type = W_WIENER
	gamma  = clgpsetr (pp, "gamma")
	call clcpset (pp)

	pp = clopset ("modelpars")
	call clgpseta (pp, "signalm", str, SZ_LINE)
	signal_model = strdic (str, str, SZ_LINE, SM_TYPES)
	if (signal_model == 0) {
	    signal_model = SM_IMAGE
	    call strcpy (str, model, SZ_LINE)
	}
	correl = clgpsetr (pp, "correl")
	mfwhm  = clgpsetr (pp, "mfwhm")
	call clcpset (pp)

	pp = clopset ("noisepars")
	call clgpseta (pp, "statistic", str, SZ_LINE)
	statistic = strdic (str, str, SZ_LINE, NOISE_TYPES)
	if (statistic == 0)
	    statistic = INDEPENDENT
	fnoise = clgpsetr (pp, "fnoise")
	call clcpset (pp)

	pp = clopset ("lowpars")
	call clgpseta (pp, "lowpass", str, SZ_LINE)
	low_pass = strdic (str, str, SZ_LINE, LP_TYPES)
	if (low_pass == 0)
	    low_pass = NO_FILTER	
	fwhm   = clgpsetr (pp, "fwhm")

	verb   = clgetb ("verbose")

	# If the output string is a directory, generate names for
	# the new images accordingly.

	if (isdirectory (imlisto, dirnameo, SZ_PATHNAME) > 0) {
	    listi = imtopen (imlisti)
	    while (imtgetim (listi, input, SZ_PATHNAME) != EOF) {

		# Place the input image name, without a directory prefix, 
		# in string dirnamei.
		root_len = fnldir (input, dirnamei, SZ_PATHNAME)
		call strcpy (input[root_len + 1], dirnamei, SZ_PATHNAME)

		# Assemble output image name.
		call strcpy (dirnameo, output, SZ_PATHNAME)
		call strcat (dirnamei, output, SZ_PATHNAME)

		# Do it.
	        call w_wiener (input, inpsf, output, filter, low_pass, 
				px0, py0, fwhm, gamma, mask, fnoise, nlpsf, 
	                        statistic, wiener_type, signal_model, 
	                        model, correl, mfwhm, verb)
	    }
	    call imtclose (listi)

	} else {

	    # Expand the input and output image lists.
	    listi = imtopen (imlisti)
	    listo = imtopen (imlisto)
	    if (imtlen (listi) != imtlen (listo)) {
	        call imtclose (listi)
	        call imtclose (listo)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (listi, input,  SZ_PATHNAME) != EOF) &&
		   (imtgetim (listo, output, SZ_PATHNAME) != EOF)) {
	        call w_wiener (input, inpsf, output, filter, low_pass, 
				px0, py0, fwhm, gamma, mask, fnoise, nlpsf, 
	                        statistic, wiener_type, signal_model, 
	                        model, correl, mfwhm, verb)

	    }

	    call imtclose (listi)
	    call imtclose (listo)
	}


end


# W_WIENER  --  Wiener-deconvolve one image.

procedure w_wiener (input, inpsf, output, filter, low_pass, px0, py0,
                    fwhm, gamma, mask, fnoise, nlpsf, statistic, wiener_type,
                    signal_model, model, correl, mfwhm, verb)

char	input[ARB]			# input image
char	inpsf[ARB]			# psf image
char	output[ARB]			# output image
char	filter[ARB]			# optional filter image
char	model[ARB]			# image with signal model
int	wiener_type			# type of Wiener filter
int	low_pass			# filter type
int	signal_model			# signal model
int	statistic			# noise statistical model
real	px0, py0			# optional position of psf in psf image
real	fwhm				# smoothing radius
real	gamma				# Wiener parameter
real	mask				# star mask
real	fnoise				# noise power estimate
real	correl				# correlation factor
real	mfwhm				# Gaussian model fwhm
bool	nlpsf				# noise-less psf ?
bool	verb

pointer	imin, impsf, imout, imfilt	# IMIO pointers
pointer	immodel
pointer	obj, cobj			# real and complex input buffers
pointer	psf, cpsf			# real and complex psf bufers
pointer	sig_ps				# buffer for signal model ps
pointer	filt				# real output filter buffer
pointer	imod				# buffer for model image
pointer	out				# output image buffer
pointer	wk				# complex workspace for FT routine
pointer	rtemp, ctemp			# temporary real and complex arrays
char	str[SZ_LINE]
int	sizex,sizey			# output and filter image size
int	iszpsf, jszpsf			# input psf image size
int	nn[2], ndim, iform, isign	# FFT routine control parameters
int	isz1x,isz1y,isz2x,isz2y
int	i,j,i1,j1,i2,j2
long	jj, jj1
long	cpu, clock			# time variables
real	aux1, aux2, s1, s2
real	noise_ps			# noise power spectrum integral
real	noise_power			# noise spectral power
real	norm				# normalization constant
bool	deconv_flag, filter_flag	# output control

pointer	immap(), imgs2r(), imps2r()
int	strlen()
real	w_noise(), asumr()
long	cputime(), clktime()

begin
	if (verb) {
	    call printf ("%s -> %s      ")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}
	deconv_flag = false
	filter_flag = false
	if (strlen(output) > 0)
	    deconv_flag = true		# Deconvolution enabled
	if (strlen(filter) > 0)
	    filter_flag = true		# Filter output enabled

	cpu   = cputime (0)
	clock = clktime (0)

	# Open input image and get its size.
	imin = immap (input, READ_ONLY, 0)
	if (IM_NDIM(imin) != 2)
	    call error (0, "Input image section is not 2-dimensional.")
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)
	# If size not even, discard one column/line from input image.
	if (mod (sizex,2) != 0)
	    sizex = sizex - 1
	if (mod (sizey,2) != 0)
	    sizey = sizey - 1
	isz2x = sizex / 2
	isz2y = sizey / 2
	isz1x = sizex - 1
	isz1y = sizey - 1

	# Alloc complex workspace and set up parameters for FFT routine.
	call malloc (wk, 2*max(sizex,sizey), TY_COMPLEX)
	ndim  = 2
	iform = 1
	nn[1] = sizex
	nn[2] = sizey
	isign = -1

	# Read input image.
	obj = imgs2r (imin, 1, sizex, 1, sizey)

	# Normalize input image to unit integral flux. 
	# norm will be used at end to recover input image 
	# original scale.
	norm = asumr (AR(obj,1,1), sizex*sizey)
	if (norm != 0.)
	    call adivkr (AR(obj,1,1), norm, AR(obj,1,1), sizex*sizey)
	else
	    call error (0, "Input file has no valid image.")

	# Apply spectral window (Welch-type). Image will be read
	# again down below, since it can't be used for restoration
	# anymore after windowing.
	call w_welch (AR(obj,1,1), sizex, sizey, isz2x, isz2y)

	# Copy normalized input image to complex array and FT it.
	call malloc (cobj, sizex*sizey, TY_COMPLEX)
	call achtrx (AR(obj,1,1), AX(cobj,1,1), sizex*sizey)
	call fourt (AX(cobj,1,1), nn, ndim, isign, iform, Memx[wk])

	# Compute noise power from input image. noise_power will be
	# used in the Wiener filter computation. aux1 may be used next
	# for Poisson statistics.
	aux1 = w_noise (AX(cobj,1,1), sizex, sizey, fnoise, noise_power)

	# Processing of Poisson statistics. noise_ps will contain
	# integral of square-rooted image power spectrum.
	if ((statistic == POISSON) && (wiener_type != W_INVERSE)) {

	    #First, apply pruning filter to remove noise plateau from 
	    # input image power spectrum.
	    call malloc (ctemp, sizex*sizey, TY_COMPLEX)
	    call amovx (AX(cobj,1,1), AX(ctemp,1,1), sizex*sizey)
	    call w_prune (AX(ctemp,1,1), sizex, sizey, aux1)
	    # Transform back to data space.
	    call w_shift (AX(ctemp,1,1), sizex, sizey)
	    call fourt (AX(ctemp,1,1), nn, ndim, -isign, iform, Memx[wk])
	    # Normalize result to original intensity scale.
	    call achtxr (AX(ctemp,1,1), AR(obj,1,1), sizex*sizey)
	    aux1 = asumr (AR(obj,1,1), sizex*sizey)
	    aux1 = norm / aux1
	    call amulkr (AR(obj,1,1), aux1, AR(obj,1,1), sizex*sizey)
	    # Compute square root of image.
	    do j = 1, sizey {
	        do i = 1, sizex {
	            if (AR(obj,i,j) > 0.) 
	                AR(obj,i,j) = sqrt(AR(obj,i,j))
	            else 
	                AR(obj,i,j) = 0.
	        }
	    }
	    # Normalize it to unit integral flux.
	    aux1 = asumr (AR(obj,1,1), sizex*sizey)
	    call adivkr (AR(obj,1,1), aux1, AR(obj,1,1), sizex*sizey)
	    # Copy it to complex temporary array and FT it.
	    call achtrx (AR(obj,1,1), AX(ctemp,1,1), sizex*sizey)
	    call fourt (AX(ctemp,1,1),nn, ndim, isign, iform, Memx[wk])
	    # Compute power spectrum.
	    call malloc (rtemp, isz2x*isz2y, TY_REAL)
	    call w_ps (AX(ctemp,1,1), rtemp, sizex,sizey)
	    call mfree (ctemp, TY_COMPLEX)
	    # Compute power spectrum integral.
	    noise_ps = asumr (AR(rtemp,1,1), isz2x*isz2y)
	    call mfree (rtemp, TY_REAL)
	}

	# Open input psf and read it.
	impsf = immap (inpsf, READ_ONLY, 0)
	if (IM_NDIM(impsf) != 2)
	    call error (0, "Input psf section is not 2-dimensional.")
	iszpsf = IM_LEN(impsf,1)
	jszpsf = IM_LEN(impsf,2)
	psf = imgs2r (impsf, 1, iszpsf, 1, jszpsf)

	# If not supplied by the user, find center of psf as maximum
	# in psf image.
	if ((IS_INDEFR (px0)) || (IS_INDEFR (py0))) {
	    aux1 = -1. / EPS
	    do j = 0, jszpsf-1 {
	        do i = 0, iszpsf-1 {
	            aux2 = Memr[psf+j*iszpsf+i]
	            if (aux2 > aux1) {
	                aux1 = aux2
	                px0  = real(i+1)
	                py0  = real(j+1)
	            }
	        }
	    }
	}

	# Centralize psf in temporary real array.
	if ((px0 < 1) || (px0 > iszpsf) ||
	    (py0 < 1) || (py0 > jszpsf))
	    call error (0, "Psf center is outside psf image section.")
	call calloc (rtemp, sizex*sizey, TY_REAL)
	i2    = sizex / 2 - int(px0) + 1
	j2    = sizey / 2 - int(py0) + 1
	do j = 0, jszpsf - 1 {
	    j1 = j2 + j
	    if ((j1 >= 0) && (j1 <= isz1y)) {
	        jj  = psf   + long(j)  * iszpsf
	        jj1 = rtemp + long(j1) * sizex
	        do i = 0, iszpsf - 1 {
	            i1 = i2 + i 
	            if ((i1 >= 0) && (i1 <= isz1x))
	                Memr[jj1 + i1] = Memr[jj + i]
	        }
	    }
	}

	# Close psf image
	call imunmap (impsf)
 
	# Multiply psf by star mask.
	if (!IS_INDEFR (mask))
	    call w_mask (AR(rtemp,1,1), sizex, sizey, mask)
	else 
	    call w_mask (AR(rtemp,1,1), sizex, sizey,
	                 real(min(iszpsf,jszpsf)))

	# Normalize psf to unit integral flux.
	aux1 = asumr (AR(rtemp,1,1), sizex*sizey)
	if (aux1 != 0.)
	    call adivkr (AR(rtemp,1,1), aux1, AR(rtemp,1,1), sizex*sizey)
	else
	    call error (0, "Input psf file has no valid image.")

	# Alloc complex array and copy centralized, masked and 
	# normalized psf to it.
	call calloc (cpsf, sizex*sizey, TY_COMPLEX)
	call achtrx (AR(rtemp,1,1), AX(cpsf,1,1), sizex*sizey)
	call mfree (rtemp, TY_REAL)

	# Fourier transform psf. No need to prune, if it is noiseless.
	call fourt (AX(cpsf,1,1), nn, ndim, isign, iform, Memx[wk])
	if (!nlpsf) {
	    aux1 = w_noise (AX(cpsf,1,1), sizex, sizey, fnoise, aux2)
	    call w_prune (AX(cpsf,1,1), sizex, sizey, aux1)
	}

	# Open output image with same header as input image.
	if (deconv_flag) {
	    imout = immap (output, NEW_COPY, imin)
	    IM_LEN(imout,1) = sizex
	    IM_LEN(imout,2) = sizey
	    call sprintf (IM_TITLE(imout), SZ_IMTITLE, 
		"Wiener restoration of %s")
	        call pargstr (input)
	}

	# Open output filter image with same header as input image.
	if (filter_flag) {
	    imfilt = immap (filter, NEW_COPY, imin)
	    IM_LEN(imfilt,1) = sizex
	    IM_LEN(imfilt,2) = sizey
	    call sprintf (IM_TITLE(imfilt), SZ_IMTITLE, 
		"Wiener filter from %s")
	        call pargstr (inpsf)
	}

	# Build signal model power spectrum.

	if (wiener_type != W_INVERSE) {

	    call malloc (sig_ps, isz2x*isz2y, TY_REAL)
	    switch (signal_model) {

	    case SM_PSF:
	       # PSF FT already computed.
	        call w_ps  (AX(cpsf,1,1), sig_ps, sizex,sizey)

	    case SM_GAUSSIAN:
	        # Signal model is a Gaussian with user-provided FWHM.
	        # Power spectrum is simply Gaussian with correct FWHM.
	        # Remeber to take care of non-square arrays.
	        s1 = sizex / 6.28 / mfwhm
	        s2 = sizey / 6.28 / mfwhm
	        s1 = s1 * s1
	        s2 = s2 * s2
	        do j = 1, isz2y {
	            aux2 = real (j - 1) ** 2
	            if (aux2 > 0.0)
	                aux2 = exp (-(aux2 / s2 / 2.))
	            else
	                aux2 = 1.
	            do i = 1, isz2x {
	                aux1 = real (i - 1) ** 2
	                if (aux1 > 0.0) 
	                    aux1 = exp (-(aux1 / s1 / 2.))
	                else
	                    aux1 = 1.
	                AR2(sig_ps,i,j) = aux1 * aux2
	            }
	        }

	    case SM_INPUT,SM_WHITE:
	        # Signal model is input degraded image. cobj already
	        # contains its FT. Apply pruning, compute power spectrum
	        # and smooth it.
	        call malloc (ctemp, sizex*sizey, TY_COMPLEX)
	        call amovx (AX(cobj,1,1), AX(ctemp,1,1), sizex*sizey)
	        aux1 = w_noise (AX(ctemp,1,1), sizex, sizey, fnoise, aux2)
	        call w_prune (AX(ctemp,1,1), sizex, sizey, aux1)
	        call w_ps  (AX(ctemp,1,1), sig_ps, sizex,sizey)
	        call w_smps (AR2(sig_ps,1,1), isz2x, isz2y)
	        # If provided, use correlation factor. Otherwise,
	        # estimate signal power spectrum from degraded image
	        # power spectrum and psf squared FT amplitude. 
	        # Smooth psf squared FT amplitude.
	        if (IS_INDEFR (correl)) {
	            call malloc (rtemp, isz2x*isz2y, TY_REAL)
	            call w_ps  (AX(cpsf,1,1), rtemp, sizex,sizey)
	            call w_smps (AR2(rtemp,1,1), isz2x, isz2y)
	        }
	        # Build signal model.
	        do j = 1, isz2y {
	            do i = 1, isz2x {
	                if (IS_INDEFR (correl)) {
	                    if (AR2(rtemp,i,j) > 0.0) {
	                        AR2(sig_ps,i,j) = AR2(sig_ps,i,j) /
	                                          AR2(rtemp,i,j)
	                    } else {
	                        AR2(sig_ps,i,j) = 0.
	                    }
	                } else {
	                    AR2(sig_ps,i,j) = AR2(sig_ps,i,j) * correl
	                }
	            }
	        }
	        if (IS_INDEFR (correl)) 
	            call mfree (rtemp, TY_REAL)
	        if (signal_model == SM_WHITE) {
	            aux2 = asumr (AR2(sig_ps,1,1), isz2x*isz2y) /
	                   (isz2x*isz2y)
	            call amovkr (aux2, AR2(sig_ps,1,1), isz2x*isz2y)
	        }	        call mfree (ctemp, TY_COMPLEX)

	    case SM_IMAGE:
	        # Signal model comes from external image. Read
	        # and normalize it. To simplify handling of frequencies,
	        # only images whith same size as input are accepted.
	        # Apply pruning.
	        immodel = immap (model, READ_ONLY, 0)
	        if (IM_NDIM(immodel) != 2)
	            call error (0, "Model image is not 2-dimensional.")
	        i = IM_LEN(immodel,1)
	        j = IM_LEN(immodel,2)
	        if (mod (i,2) != 0)
	            i = i - 1
	        if (mod (j,2) != 0)
	            j = j - 1
	        if ((i != sizex) || (j != sizey))
	            call error (0, 
	            "Input and model images have different sizes.")
	        imod = imgs2r (immodel, 1, i, 1, j)
	        aux1 = asumr (AR(imod,1,1), sizex*sizey)
	        if (aux1 != 0.)
	            call adivkr (AR(imod,1,1), aux1, AR(imod,1,1),
	                         sizex*sizey)
	        else
	            call error (0, "Input model file has no valid image.")
	        call w_welch (AR(imod,1,1), sizex, sizey, isz2x, isz2y)
	        call malloc (ctemp, sizex*sizey, TY_COMPLEX)
	        call achtrx (AR(imod,1,1), AX(ctemp,1,1), sizex*sizey)
	        call imunmap (immodel)
	        call fourt (AX(ctemp,1,1), nn, ndim, isign, iform, Memx[wk])
	        aux1 = w_noise (AX(ctemp,1,1), sizex, sizey, fnoise, aux2)
	        call w_prune (AX(ctemp,1,1), sizex, sizey, aux1)
	        call w_ps  (AX(ctemp,1,1), sig_ps, sizex,sizey)
	        call mfree (ctemp, TY_COMPLEX)

 	    case SM_MARKOV:
	        # Markov circularly symetric  correlation factor is
	        # computed from input degraded image. Condition
	        # imposed is to equate average power.
	        # First, compute its power spectrum.
	        call malloc (ctemp, sizex*sizey, TY_COMPLEX)
	        call amovx (AX(cobj,1,1), AX(ctemp,1,1), sizex*sizey)
	        aux1 = w_noise (AX(ctemp,1,1), sizex, sizey, fnoise, aux2)
	        call w_prune (AX(ctemp,1,1), sizex, sizey, aux1)
	        call w_ps  (AX(ctemp,1,1), sig_ps, sizex,sizey)
	        call mfree (ctemp, TY_COMPLEX)
	        # Compute Markov correlation factor.
	        s1 = 0.
	        s2 = 0.
	        do j = 1, isz2y {
	            do i = 1, isz2x {
	                aux1 = real(i-1)*(i-1) + real(j-1)*(j-1)
	                if (aux1 > 0.) {
	                    s1 = s1 + (AR2(sig_ps,i,j) * aux1) /
	                              (1. - AR2(sig_ps,i,j))
	                    s2 = s2 + 1.
	                }
	            }
	        }
	        s1 = s1 / s2
	        # Generate Markov power spectrum.
	        do j = 1, isz2y {
	            do i = 1, isz2x {
	                aux1 = real(i-1)*(i-1) + real(j-1)*(j-1)
	                AR2(sig_ps,i,j) = s1 / (s1 + aux1)
	            }
	        }

	    default:

	    }
	}

	# Compute filter function.
	call w_filter (AX(cpsf,1,1), sig_ps, noise_ps, sizex, sizey, 
	               noise_power, gamma, wiener_type, statistic)

	# Free power spectrum memory.
	if (wiener_type != W_INVERSE)
	    call mfree (sig_ps, TY_REAL)

	# Read input image again, normalize and FT it.
	# This is because version already in memory was windowed 
	# by Welch window.
	obj = imgs2r (imin, 1, sizex, 1, sizey)
	call adivkr (AR(obj,1,1), norm, AR(obj,1,1), sizex*sizey)
	call achtrx (AR(obj,1,1), AX(cobj,1,1), sizex*sizey)
	isign = -1
	call fourt (AX(cobj,1,1), nn, ndim, isign, iform, Memx[wk])

	# Multiply object FT by filter.
	if (deconv_flag)
	    call amulx (AX(cobj,1,1), AX(cpsf,1,1), AX(cobj,1,1), 
			sizex*sizey) 

	# Apply low pass filter.
	if (deconv_flag)
	    call w_low_pass (AX(cobj,1,1), sizex, sizey, fwhm, low_pass)
	if (filter_flag)
	    call w_low_pass (AX(cpsf,1,1), sizex, sizey, fwhm, low_pass)

	if (deconv_flag) {
	    # Inverse transform back to image space.
	    isign = 1
	    call w_shift (AX(cobj,1,1), sizex, sizey)
	    call fourt (AX(cobj,1,1), nn, ndim, isign, iform, Memx[wk])

	    # Get output image buffer.
	    out = imps2r (imout, 1, IM_LEN(imout,1), 1, IM_LEN(imout,2))

	    # Copy result to output buffer, normalizing it to same
	    # scale as input image.
	    call achtxr (AX(cobj,1,1), AR(out,1,1), sizex*sizey)
	    aux1 = asumr (AR(out,1,1), sizex*sizey)
	    aux1 = norm / aux1
	    call amulkr (AR(out,1,1), aux1, AR(out,1,1), sizex*sizey)

	    # Append HISTORY header records to output image.
	    call sprintf (str, SZ_LINE, " - WIENER restoration of `%s' image. ")
	        call pargstr (input)
	    call w_timelog (str, SZ_LINE)
	    call imputh (imout, "HISTORY", str)
	    call sprintf (str, SZ_LINE, "PSF file: %s ")
	        call pargstr (inpsf)
	    call imputh (imout, "HISTORY", str)
	    switch (wiener_type) {
	    case W_INVERSE:
	        call sprintf (str, SZ_LINE, "Filter type: inverse. ")
	        call imputh (imout, "HISTORY", str)
	    case W_PARAMETRIC:
	        call sprintf (str, SZ_LINE, "Filter type: parametric.  Parameter = %g")
	            call pargr (gamma)
	        call imputh (imout, "HISTORY", str)
	    case W_WIENER:
	        call sprintf (str, SZ_LINE, "Filter type: Wiener. ")
	        call imputh (imout, "HISTORY", str)
	    case W_GEOMETRIC:
	        call sprintf (str, SZ_LINE, "Filter type: geometric mean. ")
	        call imputh (imout, "HISTORY", str)
	    }
	    switch (wiener_type) {
	    case W_WIENER,W_GEOMETRIC:
	        switch (signal_model) {
	        case SM_INPUT:
	            call sprintf (str, SZ_LINE, "Signal model: input image.  Correlation = %g")
	                call pargr (correl)
	            call imputh (imout, "HISTORY", str)
	        case SM_GAUSSIAN:
	            call sprintf (str, SZ_LINE, "Signal model: Gaussian.  FWHM = %g")
	                call pargr (mfwhm)
	            call imputh (imout, "HISTORY", str)
	        case SM_PSF:
	            call sprintf (str, SZ_LINE, "Signal model: PSF.")
	            call imputh (imout, "HISTORY", str)
	        case SM_MARKOV:
	            call sprintf (str, SZ_LINE, "Signal model: Markov.")
	            call imputh (imout, "HISTORY", str)
	        case SM_WHITE:
	            call sprintf (str, SZ_LINE, "Signal model: White.")
	            call imputh (imout, "HISTORY", str)
	        case SM_IMAGE:
	            call sprintf (str, SZ_LINE, "Signal model: prototype image `%s'.")
	                call pargstr (model)
	            call imputh (imout, "HISTORY", str)
	        }
	    default:
	    }
	    switch (low_pass) {
	    case CIRCUL_FILTER:
	        call sprintf (str, SZ_LINE, "Circular low-pass filter.  FWHM = %g")
	            call pargr (fwhm)
	        call imputh (imout, "HISTORY", str)
	    case SQUARE_FILTER:
	        call sprintf (str, SZ_LINE, "Square low-pass filter.  FWHM = %g")
	            call pargr (fwhm)
	        call imputh (imout, "HISTORY", str)
	    default:
	    }

	    # Close output image and work areas.
	    call imunmap (imout)
	    call mfree (wk, TY_COMPLEX)
	}

	if (filter_flag) {

	    # Get output filter buffer.
	    filt = imps2r (imfilt, 1, IM_LEN(imfilt,1), 1, IM_LEN(imfilt,2))

	    # Select amplitude of complex filter function for output. 
	    # Data is re-centered to put low frequencies at frame center.
	    do j = 1, isz2y {
	        do i = 1, isz2x {
	            AR(filt,i+isz2x,j+isz2y) = AX(cpsf,i,j) * 
					       conjg (AX(cpsf,i,j))
	        }
	    }
	    do j = isz2y+1, isz1y+1 {
	        do i = 1, isz2x {
	            AR(filt,i+isz2x,j-isz2y) = AX(cpsf,i,j) * 
					       conjg (AX(cpsf,i,j))
	        }
	    }
	    do j = 1, isz2y {
	        do i = isz2x+1, isz1x+1 {
	            AR(filt,i-isz2x,j+isz2y) = AX(cpsf,i,j) * 
					       conjg (AX(cpsf,i,j))
	        }
	    }
	    do j = isz2y+1, isz1y+1 {
	        do i = isz2x+1, isz1x+1 {
	            AR(filt,i-isz2x,j-isz2y) = AX(cpsf,i,j) * 
					       conjg (AX(cpsf,i,j))
	        }
	    }

	    # Close filter image.
	    call imunmap (imfilt)
	}

	# Close input image.
	call imunmap (imin)

	# De-alloc mempry.
	call mfree (cobj, TY_COMPLEX)
	call mfree (cpsf, TY_COMPLEX)

	if (verb) {
	    call printf ("%7.2f CPU seconds,  %5.2f elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)
	    call flush (STDOUT)
	}

	# Null filter name string. This is to avoid re-opening of the
	# same filter image on next call to w_wiener.
	call strcpy ("", filter, SZ_PATHNAME)
end



# W_FILTER  --  Build Wiener filter. The output is returned in the same
# array used for psf ft input. Format is given by Eq. 8.13 and Table 8.1
# in Andrews & Hunt (1977). 
#
# Computation assume that noise power was already removed from psf FT,
# by a suitable pruning filter. Spectral power should be  given in 
# 1-sided, bias-compensated units.

procedure w_filter (psf_ft, signal_ps, noise_ps, sizex, sizey,
                    noise_power, gamma, filtype, statistic)

complex psf_ft[sizex,sizey]		# psf FT
pointer	signal_ps			# signal model power spectrum
real	noise_ps			# noise model power spectrum integral
int	sizex, sizey			# complex array size
int	filtype				# Wiener filter type
real	noise_power			# measured noise spectral power
real	gamma				# Wiener parameter
int	statistic			# noise statistical model

complex	hconj, wien, inv
real	h				# modulus squared of psf FT
real	alpha				# geometric mean parameter
real	phi_noise			# theoretical noise spectral power
real	spsd				# signal spectral power
real	denom				# denominator in Wiener filter
real	damp				# damping term
real	w_get_ps()
int	i, j

begin
	if (IS_INDEFR (gamma))
	    gamma = 1.

	# Filter settings.
	switch (filtype) {
	case W_WIENER:
	    gamma = 1.
	    alpha = 0.
	case W_GEOMETRIC:
	    gamma = 1.
	    alpha = 0.5
	case W_INVERSE:
	    alpha = 1.
	case W_PARAMETRIC:
	    alpha = 0.
	}

	# In case of Poisson statistics, get additional term
	# in Wiener denominator. 
	switch (statistic) {
	case POISSON:
	    phi_noise = noise_ps
	default:
	    phi_noise = 1.
	}

	# Damping factor.
	damp = gamma * noise_power * phi_noise

	# Do it.
	do j = 1, sizey {
	    do i = 1, sizex {

	        # Get conjugate of psf FT and its amplitude squared.
	        hconj = conjg (psf_ft[i,j])
	        h     = real ((abs (psf_ft[i,j]))**2)

	        if (h > 0.) {

	            if (filtype != W_INVERSE) {

	                # Get signal power spectral density.
	                spsd = w_get_ps (signal_ps,i,j,sizex,sizey)

	                # Wiener denominator.
	                denom = h * spsd + damp

	                # Wiener term.
	                if (denom != 0.) {
	                    wien = hconj * spsd / denom
	                    if ((real(wien)!=0.) || (aimag(wien)!=0.))
	                        wien = wien ** (1.-alpha)
	                    else
	                        wien = complex (0.,0.)
	                } else
	                    wien = complex (0.,0.)
	            } else {
	                if (h != 0.) {
	                    wien = (hconj / h)
	                    if ((real(wien)!=0.) || (aimag(wien)!=0.))
	                        wien = wien ** (1.-alpha)
	                    else
	                        wien =  complex (0.,0.)
	                } else
	                    wien = complex (0.,0.)
	            }

	            # Inverse term.
	            if (h != 0.)
	                inv = (hconj / h) ** alpha
	            else
	                inv = complex (0.,0.)

	            # Filter
	            psf_ft[i,j] = inv * wien

	        } else
	            psf_ft[i,j] = complex (0.,0.)
	    }
	}
end




# W_PS  --  Given a Fourier transform array, compute power spectrum.
# Format is one-sided, bias-compensated. 

procedure w_ps (ft, ps, sizex, sizey)

complex	ft[sizex,sizey]			# FT input array
pointer	ps				# power spectrum output array
int	sizex, sizey			# input array size

int	isz1x,isz1y,isz2x,isz2y
int	i, j

begin
	isz1x = sizex + 1
	isz1y = sizey + 1
	isz2x = sizex / 2
	isz2y = sizey / 2
	do j = 1, isz2y {
	    do i = 1, isz2x {
	        AR2(ps,i,j) = (real (abs (ft[i,j])))**2
	        AR2(ps,i,j) = AR2(ps,i,j) +
	                      (real (abs (ft[i,isz1y-j])))**2
	        AR2(ps,i,j) = AR2(ps,i,j) +
	                      (real (abs (ft[isz1x-i,j])))**2
	        AR2(ps,i,j) = AR2(ps,i,j) +
	                      (real (abs (ft[isz1x-i,isz1y-j])))**2
	    }
	}
	call adivkr (AR2(ps,2,1), AR2(ps,1,1), AR2(ps,2,1), isz2x*isz2y-1)
	AR2(ps,1,1) = 1.0
end




# W_GET_PS  --  Given a one-sided power spectrum array, get power
# at frequency i,j.

real procedure w_get_ps (ps, i, j, sizex, sizey)

pointer	ps				# power spectrum input array
int	i, j				# frequency index
int	sizex, sizey			# original array size

int	i1, j1

begin
	if (i <= (sizex/2))
	    i1 = i
	else
	    i1 = sizex + 1 - i
	if (j <= (sizey/2))
	    j1 = j
	else
	    j1 = sizey + 1 - j
	return (AR2(ps,i1,j1))
end




# W_NOISE  --  Compute noise power by sampling FT array at specific
# "distance" frequency and "directionally" averaging the power there. 
# If freq = INDEF, the largest available frequency will be used.
# Returned value is directly in units of the squared Fourier transform.
# Parameter 'noise' contains 1-sided, bias-compensated noise power.

real procedure w_noise (ft, sizex, sizey, freq, noise)

complex ft[sizex,sizey]			# image FT
int	sizex,sizey			# FT size
real	freq				# frequency (in pixels) were to
					# sample FT
real	noise				# 1-sided, bias-compensated 
					# noise power

int	isf, nn
int	isz1x,isz1y,isz2x,isz2y
int	i, j, id1, id2, id3, id4
real	wt,ri, rj, fi, fj

begin
	isz1x = sizex + 1
	isz1y = sizey + 1
	isz2x = sizex / 2
	isz2y = sizey / 2
	if ((IS_INDEFR (freq)) || (freq > real(min(isz2x, isz2y))))
	    isf = max(isz2x, isz2y)
	else
	    isf = int (freq)
	wt = 0.0
	nn = 0
	# take care of non-square arrays
	if (sizex > sizey) {
	    fi = 1.
	    fj = real(sizey) / real(sizex)
	} else if (sizex < sizey) {
	    fi = real(sizex) / real(sizey)
	    fj = 1.
	} else {
	    fi = 1.
	    fj = 1.
	}
	do j = 1, isz2y {
	    do i = 1, isz2x {
	        ri  = real (i) * fi
	        rj  = real (j) * fj
	        id1 = int (sqrt ((ri - 1.)**2 + (rj - 1.)**2) + 0.5)
	        id2 = int (sqrt ((ri - 1.)**2 + (rj)**2)      + 0.5)
	        id3 = int (sqrt ((ri)**2      + (rj - 1.)**2) + 0.5)
	        id4 = int (sqrt ((ri)**2      + (rj)**2)      + 0.5)
	        if (id1 == isf) {
                    wt = wt + (real (abs (ft[i,j])))**2
	            nn = nn + 1
	        }
	        if(id2 == isf) {
	            wt = wt + (real (abs (ft[i,isz1y-j])))**2
	            nn = nn + 1
	        }
	        if(id3 == isf) {
	            wt = wt + (real (abs (ft[isz1x-i,j])))**2
	            nn = nn + 1
	       }
	       if(id4 == isf) {
	            wt = wt + (real (abs (ft[isz1x-i,isz1y-j])))**2
	            nn = nn + 1
	        }
	    }
	}
	wt = wt / real(nn) 
	noise = 4. * wt / (real (abs (ft[1,1])))**2
	return (wt)
end




# W_PRUNE  --  Prune noise plateau in FT, taking care for not distorting
# phase information. Noise level is assumed to be in FT units.

procedure w_prune (ft, sizex, sizey, noise)

complex	ft[sizex,sizey]		# aray whith FT to be pruned
int	sizex, sizey		# its size
real	noise			# noise level.

int	i, j, signa, signb
real	a, b, a1, b1, r

begin
	do j = 1, sizey {
	    do i = 1, sizex {
	        a = real  (ft[i,j])
	        b = aimag (ft[i,j])
	        if (abs(a) > 0.)
	            signa = a / abs(a)
	        else
	            signa = 1.
	        if (abs(b) > 0.)
	            signb = b / abs(b)
	        else
	            signb = 1.
	        a = a * a
	        b = b * b
	        # Optimal pruning level, taken from experiments by
	        # Wahl, F.M., 1987, Digital Image Signal Processing,
	        # Artech House, Inc.
	        r = (noise * 2.) ** 2 
	        if ((a + b) > r) {
	            if (a > 0.) {
	                a1 = (a + b - r) / (1 + b/a)
	                b1 = (a - a1 + b - r)
	            } else {
	                a1 = 0.
	                b1 = b - r
	            }
	            if (a1 > 0.)
	                a1 = sqrt (a1)
	            else
	                a1 = 0.
	            if (b1 > 0.)
	                b1 = sqrt (b1)
	            else
	                b1 = 0.
	            a1 = abs(a1) * signa
	            b1 = abs(b1) * signb
	            ft[i,j] = complex (a1,b1)
	        } else {
	           ft[i,j] = complex (0.,0.)
	        }
	    }
	}
end




# W_LOW_PASS -- Low pass filter.

procedure w_low_pass (image_ft, sizex, sizey, fwhm, low_pass)

complex	image_ft[sizex,sizey]	# image FT
int	sizex,sizey		# image FT size
int	low_pass		# filter type
real	fwhm			# smoothing radius

int	i,j,isz2x,isz2y
real	cutoffx, cutoffy
real	ri, rj
real	d1, d2, d3, d4
real	d1x, d2x, d3x, d4x
real	d1y, d2y, d3y, d4y
real	ax, ay

real	w_cwind()

begin
	if ((!IS_INDEFR (fwhm)) && (fwhm > 0.) &&
	    (fwhm <  sizex) && (fwhm < sizey)) {
	    # take care of non-square arrays
	    cutoffx = sizex / 2. / fwhm
	    cutoffy = sizey / 2. / fwhm
	    isz2x = sizex / 2
	    isz2y = sizey / 2

	    switch (low_pass) {
	    case NO_FILTER:

	    case CIRCUL_FILTER:
	        ax = cutoffx ** 2
	        ay = cutoffy ** 2
	        do j = 1, isz2y {
	            do i = 1, isz2x {
	                ri = real (i)
	                rj = real (j)
	                d1 = sqrt ((ri-1.)**2/ax + (rj-1.)**2/ay)
	                d2 = sqrt ((ri-1.)**2/ax + rj**2/ay)
	                d3 = sqrt (ri**2/ax + (rj-1.)**2/ay)
	                d4 = sqrt (ri**2/ax + rj**2/ay)
	                image_ft[i,j] = w_cwind (d1) * 
					image_ft[i,j]
	                image_ft[i,sizey+1-j] = w_cwind (d2) * 
						image_ft[i,sizey+1-j]
	                image_ft[sizex+1-i,j] = w_cwind (d3) * 
						image_ft[sizex+1-i,j]
	                image_ft[sizex+1-i,sizey+1-j] = w_cwind (d4) * 
					image_ft[sizex+1-i,sizey+1-j]
	            }
	        }
	    case SQUARE_FILTER:
	        do j = 1, isz2y {
	            do i = 1, isz2x {
	                ri  = real (i)
	                rj  = real (j)
	                d1x = (ri - 1.) / cutoffx
	                d1y = (rj - 1.) / cutoffy
	                d2x = (ri - 1.) / cutoffx
	                d2y = rj / cutoffy
	                d3x = ri / cutoffx
	                d3y = (rj - 1.) / cutoffy
	                d4x = ri / cutoffx
	                d4y = rj / cutoffy
	                image_ft[i,j] = w_cwind (d1x) * 
					w_cwind (d1y) * 
					image_ft[i,j]
	                image_ft[i,sizey+1-j] = w_cwind (d2x) * 
						w_cwind (d2y) * 
						image_ft[i,sizey+1-j]
	                image_ft[sizex+1-i,j] = w_cwind (d3x) * 
						w_cwind (d3y) * 
						image_ft[sizex+1-i,j]
	                image_ft[sizex+1-i,sizey+1-j] = w_cwind (d4x) * 
	                                                w_cwind (d4y) *
	                                  image_ft[sizex+1-i,sizey+1-j]
	            }
	        }
	    default:

	    }
	}
end




# W_MASK  -  Multiply real array by star mask. If mask is INDEF or zero,
# no masking takes place.

procedure w_mask (image, sizex, sizey, mask)

real	image[sizex,sizey]	# image array
int	sizex, sizey		# array size
real	mask			# mask size in pixels

int	isz2x, isz2y, i, j
real	r, aux, w_cwind()

begin
	if ((!IS_INDEFR (mask)) && (mask > 0.)) {
	    isz2x = sizex / 2
	    isz2y = sizey / 2
	    do j = 1, sizey {
	        aux = (real (isz2y-j))**2
	        do i = 1, sizex {
	            r = sqrt ((real(isz2x-i))**2 + aux) / mask
	            image[i,j]= image[i,j] * w_cwind(r)
	        }
	    }
	}
end




# W_CWIND  --  A window for FTs that has low sidelobes.

real procedure w_cwind (relr)

real	relr

real	c[4], r, r2, r3, aux

data c / .074, .302, .233, .390 /

begin
	if ((relr < 0.) || (relr > 1.5)) {
	    return (0.)
	} else {
	    # (1-R**2), R=0... 1.0
	    r = 1. - (relr * relr)
	    r2 = r * r
	    r3 = r2 * r
	    aux = c[1] + r * c[2] + r2 * c[3] + r3 * c[4]
	    # Let go all the way to zero
	    if (aux < 0.) aux = 0.
	    return (aux)
	}
end



# W_SHIFT  --  To be used before transforming FT back to data space,
# to shift image to center of field. Works for arrays with even 
# dimensions.

procedure w_shift (array, sizex, sizey)

complex	array[sizex, sizey]
int	sizex, sizey

int	i,j,

begin
	do j = 2, sizey, 2 {
	    do i = 1, sizex-1, 2 {
	        array[i,j] = -array[i,j]
	    }
	}
	do j = 1, sizey-1, 2 {
	    do i = 2, sizex, 2 {
	        array[i,j] = - array[i,j]
	    }
	}
end



# W_SMPS  --  Smooth power spectrum by a Gaussian kernel.
#
#  KS = 4.3 * SIGMA  for a 6*SIGMA diagonal.

define	KS	11				# kernel size
define	KS2	121				# its square
define	KERNEL	Memr[kern+($2-1)*KS   +$1-1]
define	TEMP	Memr[temp+($2-1)*isz2x+$1-1]

procedure w_smps (ps, isz2x, isz2y)

real	ps[isz2x, isz2y]
int	isz2x, isz2y

pointer	kern
pointer	temp
int	i, j, i1, j1, i2, j2
int	ks2
real	w

begin
	ks2 = (KS - 1) / 2
	call malloc (temp, isz2x*isz2y, TY_REAL)
	call amovr (ps, TEMP(1,1), isz2x*isz2y)
	call malloc (kern, KS2, TY_REAL)

	# Build kernel.
	do j = 1, KS {
	    do i = 1, KS {
	        KERNEL(i,j) = exp(-(real(i-ks2-1)**2+real(j-ks2-1)**2) / 
	                           (2.*(real(KS) / 4.29)**2))
	    }
	}

	# Do it
	do j = 1, isz2y {
	    do i = 1, isz2x {
	        ps[i,j] = 0.
	        w       = 0.
	        j2 = 0
	        do j1 = max (1, j-ks2), min (isz2y, j+ks2) {
	            j2 = j2 + 1
	            i2 = 0
	            do i1 = max (1, i-ks2), min (isz2x, i+ks2) {
	                i2 = i2 + 1
	                ps[i,j] = ps[i,j] + TEMP(i1,j1) * KERNEL(i2,j2)
	                w = w + KERNEL(i2,j2)
	            }
	        }
	        ps[i,j] = ps[i,j] / w
	    }
	}
	call mfree (temp, TY_REAL)
	call mfree (kern, TY_REAL)
end



# W_WELCH  --  Applies a Welch spectral window to input array.

procedure w_welch (array, sizex, sizey, isz2x, isz2y)

real	array[sizex,sizey]
int	sizex, sizey, isz2x, isz2y

int	i,j
real	aux1, aux2

begin
	do j = 1, isz2y {
	    do i = 1, isz2x {
	        aux1 = ((j - 1. - 0.5 * (sizey-2)) / (0.5 * sizey)) ** 2
	        aux2 = ((i - 1. - 0.5 * (sizex-2)) / (0.5 * sizex)) ** 2
	        aux1 = 1. - aux1 * aux2
	        array[i,j]                 = array[i,j] * aux1
	        array[sizex+1-i,j]         = array[sizex+1-i,j] * aux1
	        array[i,sizey+1-j]         = array[i,sizey+1-j] * aux1
	        array[sizex+1-i,sizey+1-j] = array[sizex+1-i,sizey+1-j] * aux1
	    }
	}
end



# W_TIMELOG -- Prepend a time stamp to the given string.
#
# For the purpose of a history logging prepend a short time stamp to the
# given string.  Note that the input string is modified.

procedure w_timelog (str, max_char)

char	str[max_char]		# String to be time stamped
int	max_char		# Maximum characters in string

pointer	sp, time, temp
long	clktime()

begin
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (temp, max_char, TY_CHAR)

	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	call sprintf (Memc[temp], max_char, "%s %s")
	    call pargstr (Memc[time])
	    call pargstr (str)
	call strcpy (Memc[temp], str, max_char)

	call sfree (sp)
end



