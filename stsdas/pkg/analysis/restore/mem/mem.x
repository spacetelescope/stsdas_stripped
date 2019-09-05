# Copyright(c) 1994 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<fset.h>
include	<imhdr.h>
include	<mach.h>
include	<math.h>

# MEM -- Perform MEM deconvolution on a 1-D or 2-D degraded image.
# Nailong Wu, 06-Sept-1994, STScI.

define	H2	(-$1*(log($1/$2)-1.0)-$2)  # A component of the entropy function
define	DH2	(-log($1/$2))	           # A component of the entropy gradient
define	CLIP	(max($1,real(EPSILOND)))   # Clip a zero or -ve value of image


procedure t_mem ()

include	"mem.com"

# This is Version D, featured by 
# 1. The preconditioned conjugate method (method=2), and the accurate
#    Newton method (method=3). 
# 2. The accurate 1-D search (opt=3) in maximization. 
# 3. Poisson noise only case (poisson=yes) handling.
# 4. Subpixelization technique (nsub>1).
# 5. Model updating technique (m_update).
 
# Filename string pointers

pointer	sp		# Memory stack pointer
pointer	in_deg		# Input degraded image 
pointer in_psf		# Input point spread function 
pointer	in_mod		# Input model (prior estimated) image 
pointer in_ges		# Input first guess image 
pointer in_icf		# Input Intrinsic Correlation Function 
pointer out_rest	# Output restored image

# File descriptor pointers

pointer	degrade		# Degraded image 
pointer psf		# PSF file 
pointer	model		# Model image
pointer	guess		# First guess image
pointer restore		# Output restored image 

# Dynamic memory pointers for image data and working space

pointer	pt_deg		# Degraded image
pointer	pt_psfft	# FFT of PSF combined with ICF or ICF alone 
pointer	pt_mod		# Model image
pointer	pt_current	# Current image in iteration
pointer	pt_conv		# Current image convolved with the PSF and ICF
pointer	pt_new		# Next image in iteration
pointer	pt_newconv	# Next image convolved with the PSF and ICF
pointer	pt_gradE	# Gradient of half chi-sq 
pointer	pt_NgradE	# New gradient of half chi-sq 
pointer	pt_2psfft	# Conj. of FT of (PSF*ICF)**2, for "Poisson noise only"
pointer	pt_hess		# Diagonal elements of Hessian of half chi-sq, or 
			# Hessian of half log Poisson likelihood
pointer	pt_imio		# Image to be read in or written out 
pointer	pt_cwkspace	# Complex working space 

pointer	pt_carray, work	# Complex array and working space for FFT

# Image and array sizes

int	n1deg, n2deg	# Degraded image size
int	n1deg_sub 	# nsub * degraded image size 
int	n2deg_sub	#
int	n1psf, n2psf	# PSF file size
int	n1mod, n2mod	# Model image size
int	n1ges, n2ges	# First guess image size
int	n1max, n2max	# Array size
int	n1lim, n2lim	# Read-in area size of model or guess image

# Parameters for deconvolution

real	noise		# Readout noise in electrons
real	adu		# A/D conversion constant, electrons/DN
real	vc[2]		# Coeffs. for calc. noise var. of the degraded image
bool	poisson		# Poisson noise only (or plus Gaussian noise)?
int	nsub		# Subpixelization factor
bool	blksum		# Block sum nsub**2 subpixels to get a normal pixel? 
int	nblk		# Size in subpixels (nsub or 1) for blocksum/replicate 
real	sigma[2]	# Sigmas of Gaussian fn as ICF 
real	fwhm[2]		# Full widths at half max of Gaussian fn as ICF
bool	hidden		# Output a hidden (or visible) image?
real	a_sp, b_sp	# Speed factors for renewing alpha and beta
real	a_rate  	# Decrease rate of a_sp (user input)
real	aim		# Factor for setting actual target chi-sq
int	maxiter		# Max total number of iterations
int	m_update	# Model update interval (in numbers of outer iterations)
int	method		# Method determining search direction
int	cj_on		# Inner iter. No. after using new alpha to turn on conj.
int	opt		# Optimal one-dim. search in the search direction 
real	tol[4]		# Converg. tolerances. for ME solution, chi-sq, tp, and
			# solution of linear eqs. (acc. Newton).
int	message		# Verboseness of output messages, 1 (least) - 3 (most)

# Other variables in iteration

bool	mod_file	# Input model image file exists?
bool	ges_file	# Input guess image file exists?
bool	icf_file	# Input ICF data file exists?
bool	useicf		# Use ICF?
int	niter		# Counter of the total iteration number	
int	n_conv		# Number of convolution/correlations
bool	me_image	# Is the output image an ME image?
bool	converge	# Is the output image a converged one?

int	narr		# Total number of points in array

real	pval		# Peak value of array
int	ploc[2]		# Peak location
int	sh[2]		# Amount of array shift 
bool	center		# Center PSF, ICF?
char	norm[SZ_LINE]	# Normalize PSF, ICF; "no", "peak", or "volume"
real	v_on_m		# Vol/max of ACF of combinatn of PSF and ICF 
real	scale		# Scale factor accounting for subpixelization

int	fstati(), clgeti(), imaccf()
real	clgetr(), imgetr()
bool	clgetb()

pointer	immap(), impl2r(), imgs2r(), imps2r()

begin
	# For properly output messages 

	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize the dynamic memory stack for image name strings

	call smark (sp)
	call salloc (in_deg, SZ_FNAME, TY_CHAR)
	call salloc (in_psf, SZ_FNAME, TY_CHAR)
	call salloc (in_mod, SZ_FNAME, TY_CHAR)
	call salloc (in_ges, SZ_FNAME, TY_CHAR)
	call salloc (in_icf, SZ_FNAME, TY_CHAR)
	call salloc (out_rest, SZ_FNAME, TY_CHAR)

	# Get input and output file names

	call clgstr ("input", Memc[in_deg], SZ_FNAME)
	call clgstr ("psf", Memc[in_psf], SZ_FNAME)
	call clgstr ("model", Memc[in_mod], SZ_FNAME)
	call clgstr ("guess", Memc[in_ges], SZ_FNAME)
	call clgstr ("icf", Memc[in_icf], SZ_FNAME)
	call clgstr ("output", Memc[out_rest], SZ_FNAME)

	# Get parameters for deconvolution

	noise = clgetr ("noise")		
	adu = clgetr ("adu")		
	vc[1] = (noise / adu) ** 2 
	vc[2] = 1.0 / adu 
	poisson = clgetb ("poisson")
	if (poisson)
	    vc[1] = 0.0

	tp = clgetr ("tp")		# Total power (flux) of image
	nsub = clgeti ("nsub")
	blksum = clgetb ("blksum")	# To get a normal pixel, blksum =
	if (blksum)			#   yes: block sum nsub**2 subpixels, 
	    nblk = nsub			#   no:  resample every nsub's subpixel.
	else				# Get nsub**2 subpixels from a normal
	    nblk = 1			# one accordingly.

	sigma[1] =  clgetr ("sigma[1]")
	if (sigma[1] <= EPSILONR) {
	    fwhm[1] =  clgetr ("fwhm[1]")
	    sigma[1] = fwhm[1] / sqrt (8.0 * log (2.0))
	} else
	    fwhm[1] = sigma[1] * sqrt (8.0 * log (2.0))
	
	sigma[2] =  clgetr ("sigma[2]")
	if (sigma[2] <= EPSILONR) {
	    fwhm[2] =  clgetr ("fwhm[2]")
	    sigma[2] = fwhm[2] / sqrt (8.0 * log (2.0))
	} else
	    fwhm[2] = sigma[2] * sqrt (8.0 * log (2.0))
	hidden= clgetb ("hidden")
	
	aim = clgetr ("aim")
	maxiter = clgeti ("maxiter")
	m_update = clgeti ("m_update")
	damping = clgetr ("damping")	# Normalized damping factor 
	method = clgeti ("method")

	if (method == 0) 
	    if (poisson)
		method = 3
	    else
		method = 2

	cj_on = clgeti ("cj_on")
	opt = clgeti ("opt")

	a_sp = clgetr ("a_sp")
	a_rate = clgetr ("a_rate")
	b_sp = clgetr ("b_sp")
	tol[1] = clgetr ("tol[1]")
	tol[2] = clgetr ("tol[2]")
	tol[3] = clgetr ("tol[3]")
	tol[4] = clgetr ("tol[4]")

	message = clgeti ("message")

	# Output the input parameters for keeping a record

	call printf ("\nInput summary:\n\n")
	call printf ("Input degraded image = %s\n") 
	    call pargstr(Memc[in_deg])
	call printf ("Input PSF = %s\n") 
	    call pargstr(Memc[in_psf])
	call printf ("Input model image = %s\n") 
	    call pargstr(Memc[in_mod])
	call printf ("Input first guess image = %s\n") 
	    call pargstr(Memc[in_ges])
	call printf ("Input ICF = %s\n") 
	    call pargstr(Memc[in_icf])
	call printf ("Output restored image = %s\n") 
	    call pargstr(Memc[out_rest])
	call printf ("noise = %g  adu = %g  poisson = %b  tp = %g  aim = %g")
	    call pargr(noise)
	    call pargr(adu)
	    call pargb(poisson)
	    call pargr(tp)
	    call pargr(aim)
	call printf ("  maxiter = %d\n")
	    call pargi(maxiter)
	call printf ("sigma1-2 =  %g, %g   fwhm1-2 = %g, %g   ")
	    call pargr(sigma[1])
	    call pargr(sigma[2])
	    call pargr(fwhm[1])
	    call pargr(fwhm[2])
	call printf ("hidden = %b\n")
	    call pargb(hidden)
	call printf ("nsub = %d  blksum = %b  method = %d  cj_on = %d  ")
	    call pargi(nsub)
	    call pargb(blksum)
	    call pargi(method)
	    call pargi(cj_on)
	call printf ("opt = %d  m_update = %d\n")
	    call pargi(opt)
	    call pargi(m_update)
	call printf ("a_sp = %g   a_rate = %g   b_sp = %g   damping = %g\n")
	    call pargr(a_sp)
	    call pargr(a_rate)
	    call pargr(b_sp)
	    call pargr(damping)
	call printf ("tol1-4 = %g, %g, %g, %g   message = %d\n")
	    call pargr(tol[1])
	    call pargr(tol[2])
	    call pargr(tol[3])
	    call pargr(tol[4])
	    call pargi(message)

	# Open the degraded image and get its size, and determine the size
	# after subpixelization.

	degrade = immap (Memc[in_deg], READ_ONLY, 0)
	n1deg = IM_LEN(degrade,1)	
	n2deg = IM_LEN(degrade,2)	
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub

	# Open the PSF file and get its size

	psf = immap (Memc[in_psf], READ_ONLY, 0)
	n1psf = IM_LEN(psf,1)	
	n2psf = IM_LEN(psf,2)	

	# Open the output deconvolved image having the same size as the
	# subpixelized input degraded image, then close it to reserve
	# disk space. N.B. the output image pixel value datatype is
	# forced to be real.

	restore = immap (Memc[out_rest], NEW_COPY, degrade)
	IM_LEN(restore,1) = n1deg_sub
	IM_LEN(restore,2) = n2deg_sub
	IM_PIXTYPE(restore) = TY_REAL
	pt_imio = impl2r (restore, 1)
	Memr[pt_imio] = 1.0
	call imunmap (restore)

	# Array size = max of subpixelized deg. image and PSF sizes

	n1max = max (n1deg_sub, n1psf)
	n2max = max (n2deg_sub, n2psf)

	# Dynamic memory allocation for input images and part of working space

	narr = n1max * n2max 
	call malloc (pt_deg, n1deg*n2deg, TY_REAL)
	call malloc (pt_psfft, narr, TY_COMPLEX)

	call malloc (pt_current, narr, TY_REAL)
	call malloc (pt_conv, narr, TY_REAL)
	call malloc (pt_new, narr, TY_REAL)

	if (poisson)		# Additional memory
	    call malloc (pt_2psfft, narr, TY_COMPLEX)
	else			# Virtually no additional memory 
	    call malloc (pt_2psfft, 1, TY_COMPLEX)

	call malloc (pt_hess, narr, TY_REAL)

	call malloc (pt_cwkspace, narr, TY_COMPLEX)

	# Initialize FFT

	call fft_b_ma (pt_carray, n1max, n2max, work)

	# Read in degraded image and move it to real array, starting at [1,1]

	pt_imio = imgs2r (degrade, 1, n1deg, 1, n2deg)
	call move_array (Memr[pt_imio], n1deg, n2deg, Memr[pt_deg],
	    n1deg, n2deg)
	call imunmap (degrade)

	# Combine PSF and ICF. Arrays "current" and "conv" hold PSF and ICF,
	# respectively, "new" is working space. Array "psfft" holds the result.
	# N.B. The input PSF and ICF must be subpixelized if subpixelization
	# is used.

	# Initialize counter of No. of convolutions

	n_conv = 0

	# Read in PSF, center and normalize its peak.
	pt_imio = imgs2r (psf, 1, n1psf, 1, n2psf)
	call move_array (Memr[pt_imio], n1psf, n2psf, Memr[pt_current],
	    n1max, n2max)
	call imunmap (psf)

	call arrpeak (Memr[pt_current], n1max, n2max, pval, ploc)
	sh[1] = -ploc[1] + 1
	sh[2] = -ploc[2] + 1
	center = true
	call strcpy ("peak", norm, SZ_LINE)
	call standard (Memr[pt_current], n1max, n2max, center, norm,
	    pval, sh, Memr[pt_new])

	# Check existence of optional input files
	mod_file = Memc[in_mod] != EOS && !IS_WHITE(Memc[in_mod])
	ges_file = Memc[in_ges] != EOS && !IS_WHITE(Memc[in_ges])
	icf_file = Memc[in_icf] != EOS && !IS_WHITE(Memc[in_icf])

	# Read in ICF if supplied, otherwise generate an elliptic Gaussian
	# function as ICF.
	useicf = false
	call get_icf (Memc[in_icf], Memr[pt_conv], n1max, n2max, icf_file,
	    sigma, useicf)

	# Center and normalize ICF's volume if use it
	if (useicf) {
	    call arrpeak (Memr[pt_conv], n1max, n2max, pval, ploc)
 	    sh[1] = -ploc[1] + 1
	    sh[2] = -ploc[2] + 1
	    center = true
	    call strcpy ("volume", norm, SZ_LINE)
	    call standard (Memr[pt_conv], n1max, n2max, center, norm,
	        pval, sh, Memr[pt_new])
	}

	# Perform FFT on PSF and ICF, then combine them. The combined PSF and
	# ICF is normalized so its volume=1. Also calculate some parameters.
	call trans_psf_c (Memr[pt_current], Memr[pt_conv], Memx[pt_psfft],
	    n1max, n2max, useicf, v_on_m, n_conv, Memx[pt_carray], work)

	# Set the volume of (PSF conv. ICF) to 1.0*nsub**2 if simply resampling
	# every nsub's subpixel in each dimension to get normal pixels. 

	if (!blksum) {
	    scale = nsub ** 2
	    call altmx (Memx[pt_psfft], Memx[pt_psfft], narr, scale, 0.0) 
	}

	# Calculate the diagonal elements of Hessian of half chi-sq, or cal. 
	# conjugate of FFT of (PSF*ICF)**2 in the case of Poisson noise only. 
	
	if (poisson)
	    call cal_psf2ft (Memx[pt_psfft], Memx[pt_2psfft],
	        n1max, n2max, Memr[pt_hess], n_conv, Memx[pt_carray], work)
	else
	    call cal_hess_g (Memr[pt_deg], Memx[pt_psfft],
	        Memr[pt_hess], n1max, n2max, n1deg, n2deg, nsub, nblk, 
	        vc, n_conv, Memx[pt_cwkspace], Memx[pt_carray], work)

	# Dynamic memory deallocation, and allocation for model image

	call mfree (pt_cwkspace, TY_COMPLEX)
	call malloc (pt_mod, narr, TY_REAL)

	# Output some parameters 
	
	if (message == 3) {
	    call printf ("\nOther parameters:\n\n")
	    call printf ("Vol/max of ACF of combination of PSF and ICF ")
	    call printf ("= %g\n\n")
	        call pargr (v_on_m)
	}

	# Read in the model image, if supplied, and input tp from its header
	# if the keyword ME_TP exists in the case where tp is not supplied
	# by user; otherwise generate a flat model from the tp supplied by 
	# user or estimated from the first guess image (if exists) or
	# finally from degraded image.
	# Note that the model image outside the degraded image area, if any,
	# will be ignored.
	# N.B. The input model and guess images must be subpixelized if
	# subpixelization is used. 

	if (mod_file) {
	    model = immap (Memc[in_mod], READ_ONLY, 0)
	    # Attempt to get parameter a_sp to replace the input
	    if (imaccf (model, "ME_A_SP") == YES) {
	        a_sp = imgetr (model, "ME_A_SP")
	        if (message == 3) {
		    call printf ("From model's header   a_sp = %g\n")
			call pargr (a_sp)
		}
	    }
	    n1mod = IM_LEN(model,1)	
	    n2mod = IM_LEN(model,2)	
	    n1lim = min (n1deg_sub, n1mod)
	    n2lim = min (n2deg_sub, n2mod)
	    pt_imio = imgs2r (model, 1, n1lim, 1, n2lim)
	    call move_array (Memr[pt_imio], n1lim, n2lim, Memr[pt_mod],
	        n1max, n2max)

	    if (tp <= EPSILONR)
	        # No input tp, attempt to get it from model's header. 
	        if (imaccf (model, "ME_TP") == YES) { 
	            tp = imgetr (model, "ME_TP")
		    if (message == 3) {
  	                call printf ("From model's header     TP = %g\n")
	                    call pargr (tp)
		    }
	        } 
	    call imunmap (model)
	} 

	if (ges_file) {
	    guess = immap (Memc[in_ges], READ_ONLY, 0)
	    if (tp <= EPSILONR) 
	        # No tp obtained so far, attempt to get it from guess' header. 
	        if (imaccf (guess, "ME_TP") == YES) { 
	            tp = imgetr (guess, "ME_TP")
		    if (message == 3) {
  	                call printf ("From guess' header     TP = %g\n")
	                    call pargr (tp)
		    }
		}
	}

	if (tp <= EPSILONR) {
	    # No tp obtained so far, calculate it from input deg. image. 
	    call def_tp (Memr[pt_deg], n1deg, n2deg, n1deg, n2deg)
	    if (message == 3) {
	        call printf ("From input image  TP = %g\n")
	            call pargr (tp)
	    }
	}

	if (!mod_file)
	    # No input model file, generate a flat one using tp. 
	    call def_model (Memr[pt_mod], n1max, n2max, n1deg_sub, n2deg_sub)

	# Initialize the current iterate. If the first guess image 
	# is provided, then it will be used; otherwise the model will
	# be used.
	# Note that the guess image outside the degraded image area, 
	# if any, will be ignored.

	alpha = 0.0
	beta = 0.0

	if (ges_file) {
	    # Attempt to get alpha, a_sp and beta from guess.
            if (imaccf (guess, "ME_ALPHA") == YES) {
	        alpha = imgetr (guess, "ME_ALPHA")
	        if (message == 3) {
		    call printf ("From guess' header  alpha = %g\n")
			call pargr (alpha)
		}
	    } else {
		call printf ("\nInput guess' header has no keyword")
		call printf (" ME_ALPHA. Set  alpha = 1.0\n")
		alpha = 1.0
	    } 

	    if (imaccf (guess, "ME_A_SP") == YES) {
	        a_sp = imgetr (guess, "ME_A_SP")
	        if (message == 3) {
		    call printf ("From guess' header   a_sp = %g\n")
			call pargr (a_sp)
		}
	    }

            if (imaccf (guess, "ME_BETA") == YES) { 
	        beta = imgetr (guess, "ME_BETA")
	        if (message == 3) {
		    call printf ("From guess' header   beta = %g\n")
			call pargr (beta)
		}
	    } else {
		call printf ("Input guess' header has no keyword")
		call printf (" ME_BETA. Set  beta = 0.0\n")
		beta = 0.0
	    } 

	    n1ges = IM_LEN(guess,1)	
	    n2ges = IM_LEN(guess,2)	
	    n1lim = min (n1deg_sub, n1ges)
	    n2lim = min (n2deg_sub, n2ges)
	    pt_imio = imgs2r (guess, 1, n1lim, 1, n2lim)
	    call move_array (Memr[pt_imio], n1lim, n2lim, Memr[pt_current],
	        n1max, n2max)
	    call imunmap (guess)
	} else
	    call amovr (Memr[pt_mod], Memr[pt_current], narr)

	# Dynamic memory allocation for more image and working space

	call malloc (pt_newconv, narr, TY_REAL)
	call malloc (pt_gradE, narr, TY_REAL)
	call malloc (pt_NgradE, narr, TY_REAL)

	# Seek the MEM solution (hidden image). 
	
	call irme_zero_d (Memr[pt_deg], Memx[pt_psfft], Memr[pt_mod],
	    Memx[pt_2psfft], Memr[pt_hess], Memr[pt_current], Memr[pt_conv], 
	    Memr[pt_new], Memr[pt_newconv], Memr[pt_gradE], Memr[pt_NgradE], 
	    n1max, n2max, n1deg, n2deg, poisson, nsub, nblk, vc, tol, 
	    a_sp, a_rate, b_sp, aim, maxiter, m_update, method, cj_on, opt, 
	    message, niter, n_conv, Memx[pt_carray], work) 

	# Output good news

	me_image = false	
	converge = false

	if (alpha < EPSILONR)
	    me_image = true

	if (gJ_on_gF <= tol[1]) {
	    call printf ("An ME image obtained.\n")
	    me_image = true	
	    if (xchisq >= chisq1 && xchisq <= chisq2 &&
	        abs(tp - xtp) <= tol[3] * tp) {
	        call printf ("Congratulations for convergence !!\n")
	        converge = true
	    }
	}

	# Free memory of some working space

	call mfree (pt_newconv, TY_REAL)
	call mfree (pt_mod, TY_REAL)
	call mfree (pt_deg, TY_REAL)	

	# Open the output deconvolved image again for writing

	restore = immap (Memc[out_rest], READ_WRITE, 0)
	pt_imio = imps2r (restore, 1, n1deg_sub, 1, n2deg_sub)
	
	if (hidden || !useicf)
	    # Output hidden image, same as visible image if no ICF is used
	    call move_array (Memr[pt_current], n1max, n2max, Memr[pt_imio],
	        n1deg_sub, n2deg_sub)
        else {
	    # Convolve the hidden image with ICF and output

	    # Get ICF again, held in array "conv".
	    call get_icf (Memc[in_icf], Memr[pt_conv], n1max, n2max, icf_file,
		sigma, useicf)

	    # Center and normalize ICF's volume 
	    call arrpeak (Memr[pt_conv], n1max, n2max, pval, ploc)
 	    sh[1] = -ploc[1] + 1
	    sh[2] = -ploc[2] + 1
	    center = true
	    call strcpy ("volume", norm, SZ_LINE)
	    call standard (Memr[pt_conv], n1max, n2max, center, norm,
	        pval, sh, Memr[pt_new])
	
	    # Do convolution, the result is held in array "conv".
	    call ffft_b (Memr[pt_conv], Memx[pt_psfft], n1max, n2max, work)
	    call convolution_c (Memx[pt_psfft], Memr[pt_current], 
	        Memr[pt_conv], n1max, n2max, n_conv, Memx[pt_carray], work)

	    call move_array (Memr[pt_conv], n1max, n2max, Memr[pt_imio],
	        n1deg_sub, n2deg_sub)
	}

	# Write ME image header keywords and close the restored image

	call me_header_d (restore, nsub, blksum, noise, adu, sigma, fwhm, a_sp,
	    hidden, me_image, converge, niter, n_conv)

	call imunmap (restore)

	# Output no. of convolution/correlations

	call printf ("Total number of convolutions =  %d.\n")
	    call pargi (n_conv)

	# Free dynamic memories 

	call sfree (sp)

	call mfree (pt_hess, TY_REAL)
	call mfree (pt_2psfft, TY_COMPLEX)

	call mfree (pt_NgradE, TY_REAL)
	call mfree (pt_gradE, TY_REAL)
	call mfree (pt_new, TY_REAL)
	call mfree (pt_conv, TY_REAL)
	call mfree (pt_current, TY_REAL)
	call mfree (pt_psfft, TY_COMPLEX)
	
	call fft_b_mf (pt_carray, work)
end

# Read in the ICF file if supplied, otherwise generate an elliptic Gaussian
# function as ICF.

procedure get_icf (icfname, icfarr, n1max, n2max, icf_file, sigma, useicf)

char	icfname[SZ_FNAME]	# Input ICF file name
real	icfarr[n1max,n2max]	# Array holding ICF data
int	n1max, n2max		# Array size
bool	icf_file		# Input ICF data file exists?
real	sigma[2]		# Sigmas of the Gaussian fn (ICF)
bool	useicf			# Use ICF?

pointer	icf			# ICF file descriptor
int	n1icf, n2icf		# ICF file size
int	n1lim, n2lim		# Read-in area size of ICF
pointer	pt_imi			# ICF file to be read in

pointer	immap(), imgs2r()

begin

	if (icf_file) {
	    # ICF file is supplied, so open it, get its size, and read in.

	    icf = immap (icfname, READ_ONLY, 0)
	    n1icf = IM_LEN(icf,1)	
	    n2icf = IM_LEN(icf,2)	

	    # ICF file read-in area size = min of array and ICF file sizes
	    n1lim = min (n1max, n1icf)
	    n2lim = min (n2max, n2icf)
	    pt_imi = imgs2r (icf, 1, n1lim, 1, n2lim)
	    call move_array (Memr[pt_imi], n1lim, n2lim, icfarr, n1max, n2max)
	    
	    call imunmap (icf)
	    useicf = true
	} else
	    if (sigma[1] > EPSILONR || sigma[2] > EPSILONR) {
	        # Generate an ICF of Gaussian type

	        call gaussfn (icfarr, n1max, n2max, sigma)
	        useicf = true
	    }
end

# Generate a volume normalized elliptic Gaussian fn centered at [n1/2+1,n2/2+1]

procedure gaussfn (icf, n1, n2, sigma)

real	icf[n1,n2]
int	n1, n2
real	sigma[2]	# Sigmas of Gaussian function

int	narr, nc1, nc2, k, l
real	gl, scale

begin
        nc1 = n1 / 2 + 1
        nc2 = n2 / 2 + 1
	narr = n1 * n2

	scale = 0.0
	do l = 1, n2 {
	    gl = exp (-0.5 * ((l - nc2) / sigma[2]) ** 2)
	    do k = 1, n1 { 
	        icf[k,l] = gl * exp (-0.5 * ((k - nc1) / sigma[1]) ** 2)
	        scale = scale + icf[k,l]
	    }
	}

	call adivkr (icf, scale, icf, narr) 
end

# Perform FFT on PSF, then multiplied by the FFT of ICF if use ICF. Output to
# complex array "psfft". The PSF and ICF have been centered. The combined
# PSF and ICF is normalized so that volume=1.
# Also calculate some parameters.

procedure trans_psf_c (psf, icf, psfft, n1, n2, useicf, v_on_m, n_conv,
	      carray, work)

include	"mem.com"

define	DAMP_MIN	0.1		# Lower limit of damping factor

real	psf[n1,n2], icf[n1,n2]		# Input PSF and ICF
complex	psfft[n1,n2]			# Output FFT of PSF with ICF
int	n1, n2
bool	useicf		# Use ICF? 
real	v_on_m		# Vol/max of ACF of vol-norm. PSF combined with ICF
int	n_conv		# Number of convolution/correlations
complex	carray		# Complex array for FFT
pointer	work		# Working space for FFT

int	narr		# Total number of points in array
real	scale
real 	macfq, vacfq	# Max & vol of ACF of q (vol-norm. PSF with ICF)

real	asumr()

begin 
	n_conv = n_conv + 1

	# Combine the FFTs of PSF and ICF

	call ffft_b (psf, psfft, n1, n2, work)

	narr = n1 * n2
	if (useicf) {
	    call ffft_b (icf, carray, n1, n2, work)
	    call amulx (psfft, carray, psfft, narr)
	}

	# Normalize the volume of PSF with ICF
 
	scale = 1.0 / real (psfft[1,1])
	call altmx (psfft, psfft, narr, scale, 0.0) 

	# Cal. macfq and vacfq, and their ratio v_on_m. Array "icf"
	# is working space. 

	call amovx (psfft, carray, narr)
	call acjgx (carray, carray, narr)
	call amulx (psfft, carray, carray, narr)
	call ifft_b (carray, icf, n1, n2, work)
	
	macfq = icf[1,1]
	vacfq = asumr (icf, narr)
	v_on_m = vacfq / macfq

	# Convert the input normalized (x - 1.0 - 100.0) damping factor to
	# actual one (DAMP_MIN - 1.0 - v_on_m). 
	
	damping = max (((v_on_m - 1.0) * damping + (100.0 - v_on_m)) / 99.0,
	    DAMP_MIN)
end

# Cal. conjugate of FT of (PSF*ICF)**2 in the case of Poisson noise only

procedure cal_psf2ft (psfft, psf2ft, n1, n2, hess, n_conv, carray, work)

include	"mem.com"

complex	psfft[n1,n2], psf2ft[n1,n2]
real	hess[n1,n2]		# Working space
int	n_conv			# Number of convolution/correlations
int	n1, n2			# Array size
complex	carray[n1,n2]
pointer	work

int	narr

begin
	n_conv = n_conv + 1
	narr = n1 * n2

	# Array "psfft" must be moved to "carray" and then perform FFT on
	# "carray"! 

	call amovx (psfft, carray, narr)
	call ifft_b (carray, hess, n1, n2, work)
	call amulr (hess, hess, hess, narr)

	call ffft_b (hess, psf2ft, n1, n2, work)
	call acjgx (psf2ft, psf2ft, narr)
	    
end

# Cal. the diagonal elements of Hessian of half chi-sq

procedure cal_hess_g (degrade, psfft, hess, n1, n2, n1deg, n2deg, 
	      nsub, nblk, vc, n_conv, cwkspace, carray, work)

include	"mem.com"

real	degrade[n1deg,n2deg]
complex	psfft[n1,n2]
real	hess[n1,n2]
int	n1, n2, n1deg, n2deg	# Array and degraded image sizes
int	nsub			# Subpixelization factor
int	nblk			# Block size
real	vc[2]
int	n_conv			# Number of convolution/correlations
complex	cwkspace[n1,n2]
complex	carray[n1,n2]
pointer	work

real 	var, temp
int	narr, k, l, kk, ll, knsub, lnsub

begin
	n_conv = n_conv + 2
	narr = n1 * n2

	# Array "psfft" must be moved to "carray" and then perform FFT on
	# "carray"! 

	call amovx (psfft, carray, narr)
	call ifft_b (carray, hess, n1, n2, work)
	call amulr (hess, hess, hess, narr)

	# Using the FFT tech. to calculate cross correlation between
	# (PSF with ICF)**2 and 1/var, i.e., the diagonal elements required. 

	call ffft_b (hess, cwkspace, n1, n2, work)
	call acjgx (cwkspace, cwkspace, narr)

	call aclrr (hess, narr)
	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub
	        var = vc[1] + vc[2] * abs(degrade[k,l]) 
	        if (var <= EPSILONR)
	            var = MAX_REAL

	        temp = 1.0 / var
		do ll = 1, nblk
	            do kk = 1, nblk
			    hess[knsub+kk,lnsub+ll] = temp
	    }
	}	
	call ffft_b (hess, carray, n1, n2, work)

	call amulx (carray, cwkspace, carray, narr)
	call ifft_b (carray, hess, n1, n2, work)    
end

# Calculate the total power from the input image.
# Note that only the area defined by the degraded image is effective.

procedure def_tp (image, n1, n2, n1deg, n2deg) 

include	"mem.com"

real	image[n1,n2]
int	n1, n2
int	n1deg, n2deg	# Degraded image size

int	k, l

begin
	tp = 0.0
	do l = 1, n2deg
	    do k = 1, n1deg
	        tp = tp + image[k,l]
end

# Generate a flat model image from the total power.
# Note that only the area defined by the degraded image is effective.

procedure def_model (model, n1, n2, n1deg_sub, n2deg_sub)

include	"mem.com"

real	model[n1,n2]
int	n1, n2
int	n1deg_sub, n2deg_sub	# nsub * degraded image size

real 	intensity
int	k, l

begin
	intensity = tp / (n1deg_sub * n2deg_sub)

	do l = 1, n2deg_sub {
	    do k = 1, n1deg_sub
		model[k,l] = intensity
	    do k = n1deg_sub + 1, n1
	        model[k,l] = 0.0
	}
	do l = n2deg_sub + 1, n2
	    do k = 1, n1
	        model[k,l] = 0.0
end

# This is a procedure to implement methods for MEM deconvolution, version D.
# 1. The zeroth-order approximate Newton method.
# 2. The preconditioned conjugate method.
# 3. The accurate Newton method.

procedure irme_zero_d (degrade, psfft, model, psf2ft, hess, current, conv, 
	      new, newconv, gradE, NgradE, n1, n2, n1deg, n2deg, poisson, 
	      nsub, nblk, vc, tol, a_sp, a_rate, b_sp, aim, maxiter, m_update,
	      method, cj_on, opt, message, niter, n_conv, carray, work)

include	"mem.com"

# Arrays containing images and working space in iteration

real 	degrade[n1deg,n2deg]	# Degraded image
complex	psfft[n1,n2]	# FFT of the PSF combined with ICF
real	model[n1,n2]	# Model image
complex	psf2ft[n1,n2]	# Conjugate of FT[(PSF*ICF)**2] ("Poisson noise only")
real 	hess[n1,n2]	# Diagonal elements of Hessian of half chi-sq, or half
			# of log Poisson likelihood.
real 	current[n1,n2]	# Current image in iteration
real 	conv[n1,n2]	# Current image convolved with the PSF and ICF
real 	new[n1,n2]	# Next image in iteration
real 	newconv[n1,n2]	# Next image convolved with the PSF and ICF

real	gradE[n1,n2]	# Gradient of half chi-sq, or half log P. likelihood
real	NgradE[n1,n2]	# New gradient of ......

int	n1, n2		# Array size
int	n1deg, n2deg	# Degraded image size 
bool	poisson		# Poisson noise only (or plus Gaussian noise)?
int	nsub		# Subpixelization factor
int	nblk		# Block size

real	vc[2]		# Coeffs. for calc. noise var. of the degraded image
real	tol[4]		# Converg. tols. for ME soln, chi-sq, tp, soln of l.eqs.
real	a_sp, b_sp	# Speed factors for renewing alpha and beta
real	a_rate  	# Decrease rate of a_sp (user input)
real	aim		# Factor for setting actual target chi-sq
int	maxiter		# Max total number of iterations
int	m_update	# Model updating control
int	method		# Method determining search direction
int	cj_on		# Inner iter. No. after using new alpha to turn on conj.
int	opt		# Optimal one-dim. search in the search direction 
int	message		# Verboseness of output message, 1 (least) - 3 (most)

int	niter		# Counter of the total iteration number	
int	n_conv		# Number of convolution/correlations

complex	carray[n1,n2]	# Complex array for FFT
pointer	work		# Working space for FFT

# Local variables

int	narr		# The total number of points in array
int	n1deg_sub	# nsub * degraded image size 
int	n2deg_sub	# 
int	ndata		# The total No. of pixels having non-zero noise var
int	k, l
real	var		# The total noise variance
int	in_iter[5]	# Counter of inner iterations for fixed alpha & beta	
bool	a_isnew		# Has alpha been renewed?
bool	b_isnew		# Has beta been renewed?
real	d_alpha		# Increment of alpha in its renewing
real	d_beta		# Increment of beta in its renewing
real	a_sp_old	# a_sp before increase
real	alpha_tmp	# Temporary copy of alpha
real	beta_tmp	# Temporary copy of beta
real	b_sp_tmp	# Temporary copy of b_sp

real	a_rate1		# In/decrease rate of a_sp, auto adjust initial |gradJ|
bool	a_reduce	# Reduce alpha due to too many inner iterations?

int	n_conv1		# Number of convol./correlations in the current iter.
int	outer_iter	# No. of outer iterations since last updating model
int	nloop		# Counter of loops

real	step		# Step in optimal one-dim. search
real	norm_xchisq	# Normalized (by ndata) current chi-sq

real	p_lhood		# -2 * log likelihood of Poisson distribution
real	p_lhood0	# = 2 * sum(log d!) = -2 * sum(-log d!) 
real	p_lhood1	# = 2 * sum(+d'_n - d_n log d'_n) = -2 * sum(-... + ...)
real	norm_p_lhood	# p_lhood / ndata

int	fstati()

begin
	# Initialize some parameters in common /me_com/ for iteration

	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	narr = n1 * n2
	ndata = 0 
	do l = 1, n2deg
	    do k = 1, n1deg {
	        var = vc[1] + vc[2] * abs(degrade[k,l]) 
	        if (var > EPSILONR)
	            ndata = ndata + 1
	    }

	chisq = ndata * aim			# Target chi-sq
	chisq1 = chisq * (1.0 - 2.0 * tol[2])	# Its lower limit for converg.
	chisq2 = chisq * (1.0 + tol[2])		#     upper limit for converg.
	tol1sq = tol[1] ** 2
	step = 1.0

	if (poisson) {
	    p_lhood0 = 0.0
	    do l = 1, n2deg
	        do k = 1, n1deg
		    if (degrade[k,l] >= 2.0)
		        do nloop = 2, int(degrade[k,l])
			    p_lhood0 = p_lhood0 + log (real (nloop))

	    p_lhood0 = p_lhood0 * 2.0
	    
	    if (message == 3) {
	        call printf ("|P>> From input image  2 * sum(log d!) = %g\n")
	            call pargr (p_lhood0)
	    }
	}

	# Initialize counters etc. 

	niter = 0
	do l = 1, 5 
	    in_iter[l] = 0	

	outer_iter = 0

	call printf ("\nIteration summary:\n\n")

	# Output target parameters for iteration

	if (poisson)
	    call printf ("|P>> Target error = %g\n")
	else
	    call printf ("Target chi-square = %g\n")
	        call pargr (chisq)
	call printf ("Target total power = %g\n\n")
	    call pargr (tp)

	# Convolve the current estimate with the PSF and ICF, and calculate 
	# gradients, vector products, statistics etc. 

	call convolution_c (psfft, current, conv, n1, n2, n_conv, carray, work)

	if (poisson)
	    # Cal. diagonal elements of Hessian of half log Poisson likelihood
	    call cal_hess_p (degrade, psf2ft, conv, hess, n1, n2, n1deg, n2deg,
		nsub, nblk, vc, n_conv, carray, work)

	call cal_gradE_d (degrade, conv, psfft, gradE, n1, n2, n1deg, n2deg, 
	    poisson, nsub, nblk, vc, n_conv, carray, work)
	call calculate_d (degrade, model, current, conv, gradE, hess, n1, n2,
	    n1deg, n2deg, poisson, nsub, nblk, vc)

	# Iterate until convergence or the prescribed max number of iterations 
	# is reached. Maximally 21 extra iterations may be continued to seek 
	# ME solution.

	if (message == 3) {
	    call printf ("No. convolutions before iteration = %d\n\n")
	        call pargi (n_conv)
	}

	while (niter + 1 <= maxiter && 
            (gJ_on_gF > tol[1] || xchisq < chisq1 || xchisq > chisq2 ||
	    abs(tp - xtp) > tol[3] * tp) 
	    || 
	    niter + 1 > maxiter && gJ_on_gF > tol[1] && niter <= maxiter + 20) {

	    # Update alpha and/or beta, and calculate the new gradients etc.
	    # The current alpha and its renewing rate a_sp will be auto
	    # adjusted according to the magnitude of the initial gradient of 
	    # the objective function, and the convergence speed of inner 
	    # iteration for fixed alpha and beta.
	
	    # Update counters and output iteration summary

	    n_conv1 = n_conv 
	    niter = niter + 1
	    call printf ("*** Iteration %d  (max = %d)  ")
	        call pargi (niter)
	        call pargi (maxiter)

	    if (gJ_on_gF <= tol[1] || niter == 1) {
		do l = 1, 4
		    in_iter[l] = in_iter[l+1]
                in_iter[5] = 0
	    }
	    in_iter[5] = in_iter[5] + 1

	    if (message == 3) {
		call printf ("inner_iter = (")
	        do l = 1, 3 {
		    call printf ("%d  ")
	                call pargi (in_iter[l])
		}
	        call printf ("%d)  %d\n")
	            call pargi (in_iter[4])
	            call pargi (in_iter[5])
	    } else
		call printf ("\n")

	    a_isnew = false
	    b_isnew = false

	    if (gJ_on_gF <= tol[1] || niter == 1) {
		if (gJ_on_gF <= tol[1] && niter == 1)
		    call printf ("## model/guess is an ME image.\n")
                if (xchisq < chisq1 || xchisq > chisq2) {
	            # Renew alpha

		    # Model is updated every m_update'th outer iteration
		    # when the current chi-sq is greater than its target
		    # by more than the tolerance value.
		    if (outer_iter == m_update && xchisq > chisq2) {
			call printf ("--Update model. ")
			call amovr (current, model, narr)
			alpha= 0.0
			beta = 0.0
	                call calculate_d (degrade, model, current, conv, gradE,
			    hess, n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)
			outer_iter = 0
		    }
		    outer_iter = outer_iter + 1

	            call printf("--Renew alpha. ")

	            call new_alpha_d (a_sp, a_rate, d_alpha)
		    a_isnew = true
		}

	        if (abs(tp - xtp) > tol[3] * tp) {
		    # Renew beta

	            call printf("--Renew beta.\n")

	            call new_beta_d (b_sp, d_beta)
		    b_isnew = true
		} else 
	            call printf("\n")

	        call calculate_d (degrade, model, current, conv, gradE, hess, 
	            n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)

		# If initial gJ_on_gF for new alpha and/or beta is too
		# small/large, then in/decrease alpha and/or beta (dec. only)
		# until gJ_on_gF is greater/less than or equal to the 
		# prescribed values. a_sp is also changed accordingly. 
		# Note the other 2 conditions.

		a_sp_old = a_sp
		alpha_tmp = alpha
		beta_tmp = beta
		b_sp_tmp = b_sp

		nloop = 1
	        if (a_isnew) {
		    while (gJ_on_gF < 0.20 && xchisq > chisq2 && nloop <= 10) {
		        # 0.20 = 4.0 * 0.05, 0.35 = (0.20 + 0.50) / 2.0
		        if (message == 3) {
		       	    call printf ("Initial |gradJ|/|1| = %g  ")
			        call pargr (gJ_on_gF)
			    call printf ("to be increased\n")
		        }

			a_rate1 = gJ_on_gF / 0.35
			a_sp = a_sp / a_rate1
		        alpha = alpha_tmp + d_alpha * (a_sp - a_sp_old)
		   
	                call calculate_d (degrade, model, current, conv, gradE,
		            hess, n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)
		        nloop = nloop + 1
		    }
		}   
 
		nloop = 1
		while (gJ_on_gF > 0.51 && nloop <= 20) {
		    # 0.5 = 10.0 * 0.05
		    if (message == 3) {
			call printf ("Initial |gradJ|/|1| = %g  ")
			    call pargr (gJ_on_gF)
			call printf ("to be decreased\n")
		    }

		    if (a_isnew) {
			a_rate1 = 0.35 / gJ_on_gF
		        alpha = alpha - d_alpha * a_sp * (1.0 - a_rate1)
			a_sp = a_sp * a_rate1
		    }
		    if (b_isnew) {
			beta = beta - d_beta * b_sp_tmp * (1.0 - a_rate1)
			b_sp_tmp = b_sp_tmp * a_rate1
		    }
	            call calculate_d (degrade, model, current, conv, gradE,
		        hess, n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)
		    nloop = nloop + 1
		}

		if (message == 3) {
		    call printf ("Initial |gradJ|/|1| = %g\n")
		        call pargr (gJ_on_gF)
		}
	    }

	    # Raise a_sp if it is too small

	    a_sp = max (a_sp, 0.01)

	    # Exit from iteration due presumably to numerical ill-condition

	    if (niter > 1 && alpha < EPSILONR) {
		call printf ("\nIteration cannot go any further. ")
		call printf ("But you may have the last ME image.\n")
		break
	    }

	    # If there are too many inner iterations before convergence,
	    # reduce the current alpha and a_sp for use in the future.
	    # The same measure is taken if step = 0.0.

	    a_reduce = false
	    if (mod (in_iter[5], 8) == 0 || step < EPSILONR) {
	        alpha = alpha - d_alpha * a_sp * (1.0 - a_rate)
	        call calculate_d (degrade, model, current, conv, gradE, hess,
	            n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)
	        a_sp = a_sp * a_rate
		a_reduce = true
            }

	    # To get the next iterate, take a full step in the direction 
	    # determined by one of the methods: 1. Approx. Newton.
	    # 2. Conjugate. 3. Accurate Newton. Then calculate next blurred 
	    # image and new gradient.

	    if (method <= 2)
	        # Approx. Newton 
	        call nr_step_appx (degrade, model, current, conv, gradE, 
		    hess, new, n1, n2, n1deg, n2deg, nsub, nblk, vc)

	    if (method == 3) 
	        # Accurate. Newton. "NgradE" and "newconv" are working space. 
	        call nr_step_acc1 (degrade, model, psfft, current, conv, 
		    gradE, hess, new, n1, n2, n1deg, n2deg, poisson, 
		    nsub, nblk, vc, tol[4], n_conv, message, NgradE, newconv,
		    carray, work)

	    if (method == 2 && in_iter[5] >= cj_on && niter > 1 &&
		!a_reduce) 
		# Conjugate method.  "NgradE" and "newconv" are change
		# and its blurred in the previous iteration.
	        call nr_conj (degrade, psfft, NgradE, newconv, current, conv, 
		    new, n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc, n_conv,
		    message, carray, work)
		# Now "newconv" is the blurred new iterate
	    else
	        call convolution_c (psfft, new, newconv, n1, n2, n_conv,
		    carray, work)

	    call cal_gradE_d (degrade, newconv, psfft, NgradE, n1, n2,
	        n1deg, n2deg, poisson, nsub, nblk, vc, n_conv, carray, work)

	    # If required (opt=2,3), calculate optimal step length by one-dim.
	    # search, and then move to the optimal image by inter/extrapolation.

	    step = 1.0
	    if (opt >= 2) 
		if (poisson)
	            call opt_step_p (degrade, model, current, conv, new, 
			newconv, gradE, NgradE, n1, n2, n1deg, n2deg, nsub, 
			nblk, vc, opt, step, message)
		else
	            call opt_step_g (degrade, model, current, conv, new, 
			newconv, gradE, NgradE, n1, n2, n1deg, n2deg, nsub, 
			nblk, vc, opt, step, message)

	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub {
	            new[k,l] = new[k,l] - current[k,l]
		    newconv[k,l] = newconv[k,l] - conv[k,l]
		    NgradE[k,l] = NgradE[k,l] - gradE[k,l]

                    current[k,l] = CLIP(current[k,l] + step * new[k,l])
	            conv[k,l] = conv[k,l] + step * newconv[k,l]
	            gradE[k,l] = gradE[k,l] + step * NgradE[k,l]
		    NgradE[k,l] = new[k,l]	# Temp. copy of the change
	        }

	    # Calculate new vector products, statistics etc.

	    if (poisson) {
	        call cal_gradE_d (degrade, conv, psfft, gradE, n1, n2,
	            n1deg, n2deg, poisson, nsub, nblk, vc, n_conv, carray, work)

	        call cal_hess_p (degrade, psf2ft, conv, hess, n1, n2, 
		    n1deg, n2deg, nsub, nblk, vc, n_conv, carray, work)

		p_lhood1 = 0.0
		do l = 1, n2deg
		    do k = 1, n1deg
		        if (conv[k,l] > 0.0)
		            p_lhood1 = p_lhood1 + conv[k,l] - degrade[k,l] * 
			        log (conv[k,l])

		p_lhood = p_lhood0 + 2.0 * p_lhood1
		norm_p_lhood = p_lhood / ndata
	    }

	    call calculate_d (degrade, model, current, conv, gradE, hess,
		n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)

	    norm_xchisq = xchisq / ndata

	    # Output iteration summary 

	    if (gJ_on_gF <= tol[1] || message >= 2) {
	        if (gJ_on_gF <= tol[1])
	            call printf("## ME solution found.\n")

		if (poisson) 
	            call printf ("|P>> Error = %g")
	        else
		    call printf ("Chi-sq = %g")
	        call pargr (xchisq)

	        call printf ("  (Target = %g, %g)  ")
	            call pargr (chisq)
	            call pargr (aim)
	        call printf ("Normalized = %g\n")
	            call pargr (norm_xchisq)

		if (poisson && message == 3) {
	            call printf ("|P>> -2*log likelihood = %g   norm = %g\n")
	                call pargr (p_lhood)
	                call pargr (norm_p_lhood)
		}
	        call printf ("Hidden image total power = %g   (Target = %g)\n")
	    	    call pargr (xtp)
	            call pargr (tp)
	        call printf ("Hidden image max = %g   min = %g\n")
		    call pargr (immax)
	            call pargr (immin)
	        call printf ("Step = %g\n")
	            call pargr (step)
	    }

	    n_conv1 = n_conv - n_conv1
	    if (message < 3) {
	        call printf ("|gradJ|/|1| = %g  (tol1 = %g)\n\n")
		    call pargr (gJ_on_gF)
		    call pargr (tol[1])
	    } else {
	        call printf ("|gradJ|/|1| = %g  (tol1 = %g)  test = %g\n")
		    call pargr (gJ_on_gF)
		    call pargr (tol[1])
		    call pargr (test)
	        call printf ("alpha = %g    a_sp = %g    beta = %g    ")
	            call pargr (alpha)
	            call pargr (a_sp)
	            call pargr (beta)
	        call printf ("b_sp = %g\n")
	            call pargr (b_sp)
	        call printf ("No. convlns. so far = %d,  ")
	            call pargi (n_conv)
	        call printf ("in this iter. = %d\n\n")
	            call pargi (n_conv1)
	    }
	    # For properly output messages when output is redirected.

	    if (fstati (STDOUT, F_REDIR) == YES)
	       call flush (STDOUT)
	}
end

# Convolve an image with the PSF (FFTed already). Note that the PSF may be a
# combination of PSF and ICF, or ICF only. 

procedure convolution_c (psfft, image, conv, n1, n2, n_conv, carray, work)

complex psfft[n1,n2]		# FFT of PSF
real	image[n1,n2]		# Image to be convolved
real	conv[n1,n2]		# Output image from convolution
int	n1, n2
int	n_conv			# Number of convolution/correlations
complex	carray[n1,n2]		# Complex array for FFT	
pointer	work			# Working space for FFT	

int	narr			# Total number of points in array

begin
	n_conv = n_conv + 1
	call ffft_b (image, carray, n1, n2, work)

	narr = n1 * n2
	call amulx (carray, psfft, carray, narr)  
	call ifft_b (carray, conv, n1, n2, work)
end

# Calculate cross correlation between (PSF*ICF) (FFTed) and an image.

procedure correlation (psfft, image, corr, n1, n2, n_conv, carray, work)

complex psfft[n1,n2]		# FFT of PSF
real	image[n1,n2]		# Image to be convolved
real	corr[n1,n2]		# Output image from convolution
int	n1, n2
int	n_conv			# No. of convolution/correlations
complex	carray[n1,n2]		# Complex array for FFT	
pointer	work			# Working space for FFT	

int	narr			# Total number of points in array

begin
	n_conv = n_conv + 1
	narr = n1 * n2

	call ffft_b (image, carray, n1, n2, work)
	call acjgx (carray, carray, narr)

	call amulx (carray, psfft, carray, narr)
	call acjgx (carray, carray, narr)
	call ifft_b (carray, corr, n1, n2, work)    
end

# Cal. the diagonal elements of Hessian of half log Poisson likelihood 

procedure cal_hess_p (degrade, psf2ft, conv, hess, n1, n2, n1deg, n2deg,
		nsub, nblk, vc, n_conv, carray, work)

include	"mem.com"

real	degrade[n1deg,n2deg]
complex	psf2ft[n1,n2]		# Conjugate of FT of (PSF*ICF)**2
real	conv[n1,n2], hess[n1,n2]
int	n1, n2, n1deg, n2deg	# Array and degraded image sizes
int	nsub			# Subpixelization factor
int	nblk			# Block size
real	vc[2]
int	n_conv			# Number of convolution/correlations
complex	carray[n1,n2]
pointer	work

real 	temp
int	narr, k, l, kk, ll, knsub, lnsub

begin
	n_conv = n_conv + 1
	narr = n1 * n2

	# Using the FFT tech. to calculate half of cross correlation between
	# (PSF with ICF)**2 and (data/mock data), i.e., the diagonal 
	# elements required. 

	call aclrr (hess, narr)

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub

	        temp = 0.0
	        do ll = 1, nblk
	            do kk = 1, nblk
			temp = temp + conv[knsub+kk,lnsub+ll]

	        if (temp <= 0.0)
		    temp = 1.0
		
		temp = 0.5 * degrade[k,l] / temp ** 2

	        do ll = 1, nblk
	            do kk = 1, nblk
			hess[knsub+kk,lnsub+ll] = temp
	    }
	}
	    call ffft_b (hess, carray, n1, n2, work)

	    call amulx (psf2ft, carray, carray, narr)
	    call ifft_b (carray, hess, n1, n2, work)    
end

# Calculate cross correlation between PSF and residual, which is gradient of
# half chi-sq; or between PSF and (minus) half residual, which is the gradiet 
# of (minus) half log likelihood if "Poisson noise only".

procedure cal_gradE_d (degrade, conv, psfft, gradE, n1, n2, n1deg, n2deg, 
	      poisson, nsub, nblk, vc, n_conv, carray, work)

include	"mem.com"

real	degrade[n1deg,n2deg], conv[n1,n2] # Input and "current" blurred images
complex	psfft[n1,n2]			  # FFT of PSF (with ICF)
real	gradE[n1,n2]			  # Gradient reqired
int	n1, n2, n1deg, n2deg		  # Array and degraded image sizes
bool	poisson				  # P. noise only (or plus Gau. nse)?
int	nsub				  # Subpixelization factor
int	nblk				  # Block size
real	vc[2]
int	n_conv			 	  # Number of convolution/correlations
complex	carray[n1,n2]
pointer	work

real 	var				  # The total noise variance
int	narr, k, l, kk, ll, knsub, lnsub
real	temp

begin
	narr = n1 * n2
	call aclrr (gradE, narr)

	# Calculate (minus, half if "Poisson noise only") residual 

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub
	        var = vc[1] + vc[2] * abs(degrade[k,l]) 
	        if (var <= EPSILONR)
	            var = MAX_REAL

	        temp = 0.0
		do ll = 1, nblk
		    do kk = 1, nblk
			temp = temp + conv[knsub+kk,lnsub+ll]

		if (poisson) { 
		    if (temp > 0.0)
	                temp = 0.5 * (temp - degrade[k,l]) / temp 
		} else
		    temp = (temp - degrade[k,l]) / var

		do ll = 1, nblk
		    do kk = 1, nblk
			gradE[knsub+kk,lnsub+ll] = temp
	    }
	}
	# Cal. cross correlation

	call correlation (psfft, gradE, gradE, n1, n2, n_conv, carray, work)
end

# Calculate gradients, vector products, statistics etc. about the "current"
# iterate. In the case of Poisson noise only, "chi-sq" is the sum of
# relevant error squared (output as "Error").

procedure calculate_d (degrade, model, current, conv, gradE, hess,
	      n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc)

include	"mem.com"

real	degrade[n1deg,n2deg], model[n1,n2], current[n1,n2], conv[n1,n2]
real	gradE[n1,n2]		# Gradient of half ...
real	hess[n1,n2]		# Diagonal elements of Hessian
int	n1, n2, n1deg, n2deg	# Array and degraded image sizes
bool	poisson			# Poisson noise only (or plus Gaussian noise)?
int	nsub			# Subpixelization factor
int	nblk			# Block size
real	vc[2]

real	var			# Noise variance = vc[1] + vc[2]*abs(deg) 
real 	grad[4], diagonal	                 
real	factor1, temp

int	n1deg_sub, n2deg_sub	# nsub * degraded image size
int	k, l, kk, ll, knsub, lnsub, m, n

begin
	# Initialize

	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub

	xtp = 0.0			# Current tp
	xchisq = 0.0			# Current chi-sq
	immax = current[1,1]
	immin = immax
	do n = 1, 4
	    do m = n, 4
	        graddotgrad[m,n] = 0.0

	factor1 = alpha * damping 

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
	        grad[E] = 2.0 * gradE[k,l]
	        grad[F] = 1.0
	        grad[H] = DH2(current[k,l],model[k,l])
	        grad[J] = grad[H] - alpha * gradE[k,l] - beta

	        diagonal = current[k,l] / 
	            (1.0 + factor1 * hess[k,l] * current[k,l]) 

	        xtp = xtp + current[k,l]
	        immin = min (immin, current[k,l])
	        immax = max (immax, current[k,l])
	
	        # Calculate vector products

		do n = 1, 4
	    	    do m = n, 4
	                graddotgrad[m,n] = graddotgrad[m,n] + 
	                    grad[m] * diagonal * grad[n]
	    }

	# Cal. chi-sq

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub

		if (poisson)
		    var = conv[k,l]
	 	else 
	            var = vc[1] + vc[2] * abs(degrade[k,l]) 

	        if (var <= EPSILONR)
	            var = MAX_REAL

		temp = 0.0
		do ll = 1, nblk
		    do kk = 1, nblk
			temp = temp + conv[knsub+kk,lnsub+ll]

	        xchisq = xchisq + (temp - degrade[k,l]) ** 2 / var
	    }
	}
	# Calculate |gradJ|/|gradF|

	gJ_on_gF = sqrt (graddotgrad[J,J] / graddotgrad[F,F])

	# Calculate the parallelism between (gradH - beta * gradF) and 
	# alpha * gradE, 1.0 - cos <(...), (...)>.

	if (graddotgrad[H,H] <= EPSILONR)
	    test = 0.0 
	else {
	    test = 1.0 -
	        alpha * (graddotgrad[H,E] - beta * graddotgrad[F,E]) / 
	        sqrt (max (EPSILONR, 
		(graddotgrad[H,H] + beta ** 2 * graddotgrad[F,F] - 
	        2.0 * beta * graddotgrad[H,F]) * alpha ** 2 * graddotgrad[E,E]))
	}
end

# Calculate a new value of alpha. Parameter a_sp is used to control the
# renew rate of alpha.

procedure new_alpha_d (a_sp, a_rate, d_alpha)

include	"mem.com"

real	a_sp, a_rate		# Speed factor and its reduction rate 
real	d_alpha			# Increment of alpha

real 	delta, ubound, lbound

begin
	if (xchisq > chisq) {
	    # Calculate a new d_alpha, and use the old a_sp.

	    delta = graddotgrad[J,E] ** 2 - graddotgrad[E,E] *
	        (graddotgrad[J,J] - tol1sq * graddotgrad[F,F])

	    if (delta > 0.0) {
	        delta = sqrt (delta)
	        ubound = (graddotgrad[J,E] + delta) / graddotgrad[E,E]
	        lbound = (graddotgrad[J,E] - delta) / graddotgrad[E,E]

	        d_alpha = (xchisq - chisq) / graddotgrad[E,E]
	        d_alpha = min (d_alpha, ubound)
	        d_alpha = max (d_alpha, lbound)
	        d_alpha = 2.0 * d_alpha
	    } else
		d_alpha = 0.2 * alpha / a_sp

	    alpha = alpha + a_sp * d_alpha
	} else {
	    # Use the old d_alpha, and reduce a_sp.

    	    alpha = alpha - a_sp * d_alpha * (1.0 - a_rate)
	    a_sp = a_sp * a_rate
	}
end

# Calculate a new value of beta. Parameter b_sp is used to control the
# renew rate of beta.

procedure new_beta_d (b_sp, d_beta)

include	"mem.com"

real	b_sp, d_beta		# Speed factor and increment of beta

real 	delta, ubound, lbound

begin
	# Calculate a new beta

	delta = graddotgrad[J,F] ** 2 - graddotgrad[F,F] *
	    (graddotgrad[J,J] - tol1sq * graddotgrad[F,F])

	if (delta > 0.0) {
	    delta = sqrt (delta)
	    ubound = (graddotgrad[J,F] + delta) / graddotgrad[F,F]
	    lbound = (graddotgrad[J,F] - delta) / graddotgrad[F,F]

	    d_beta = (xtp - tp) / graddotgrad[F,F]
	    d_beta = min (d_beta, ubound)
	    d_beta= max (d_beta, lbound)
	} else {
	    if (xtp > tp)
	        d_beta = 0.2 * beta / b_sp
	    if (xtp < tp)
	        d_beta = -0.2 * beta / b_sp
	}

	beta = beta + b_sp * d_beta
end

# Take a full step in the direction determined by the approximate 
# Newton method.

procedure nr_step_appx (degrade, model, current, conv, gradE, hess, new,
	      n1, n2, n1deg, n2deg, nsub, nblk, vc)

include	"mem.com"

real 	degrade[n1deg,n2deg], model[n1,n2], current[n1,n2], conv[n1,n2]
real 	new[n1,n2]		# Next iterate
real	gradE[n1,n2]		# Gradient of half ... 
real	hess[n1,n2]		# Diagonal elements of Hessian of half ... 
int	n1, n2, n1deg, n2deg	# Array and degraded image sizes
int	nsub			# Subpixelization factor
int	nblk			# Block size
real	vc[2]

int	n1deg_sub, n2deg_sub	# nsub * degraded image size
int	k, l
real 	diagonal, gradJ, factor1

begin
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	factor1 = alpha * damping 

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
	        gradJ = DH2(current[k,l],model[k,l]) - alpha * gradE[k,l] - beta
	        diagonal = current[k,l] / 
	            (1.0 + factor1 * hess[k,l] * current[k,l])

	        new[k,l] = CLIP(current[k,l] + diagonal * gradJ)
	    }
end

# Take a full step in the direction determined by the accurate 
# Newton method.

procedure nr_step_acc1 (degrade, model, psfft, current, conv, gradE, hess, new,
	      n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc, tol4, n_conv,
	      message, rwk1, rwk2, carray, work)

include	"mem.com"

real 	degrade[n1deg,n2deg], model[n1,n2], current[n1,n2], conv[n1,n2]
complex	psfft[n1,n2]
real 	new[n1,n2]		# Next iterate
real	gradE[n1,n2]		# Gradient of half ...
real	hess[n1,n2]		# Diagonal elements of Hessian of half ...  
int	n1, n2, n1deg, n2deg	# Array and degraded image sizes
bool	poisson			# P. noise only (or plus Gau. noise)?
int	nsub			# Subpixelization factor
int	nblk			# Block size
real	vc[2]
real	tol4			# Converg. tolerance for solutn of linear eqs.  
int	n_conv			# No. convolution/correlations
int	message	

real	rwk1[n1,n2], rwk2[n1,n2]	# Real arrays as working space
complex	carray[n1,n2]			# Complex array and working space
pointer	work				# for FFT

int	n1deg_sub, n2deg_sub		# nsub * degraded image size
int	narr, k, l, kk, ll, knsub, lnsub, nn
real 	diagonal, gradJ, factor1

real 	var, temp1, temp1_old, temp2, aa, ba, cc

begin
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	narr = n1 * n2
	factor1 = alpha * damping 
	temp1_old = 1.0e30
	
	call aclrr (new, narr)
	call aclrr (rwk1, narr)
	call aclrr (rwk2, narr)

	# Solve for the change iteratively

	do nn = 1, 11 {

	    temp1 = 0.0
	    temp2 = 0.0
	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub {
	       	    gradJ = DH2(current[k,l],model[k,l]) - 
			alpha * gradE[k,l] - beta
		    rwk1[k,l] = gradJ - rwk1[k,l]

	            diagonal = current[k,l] / 
	                (1.0 + factor1 * hess[k,l] * current[k,l])

		    rwk1[k,l] = rwk1[k,l] * diagonal	
	     	    temp1 = temp1 + rwk1[k,l] ** 2

		    temp2 =temp2 + diagonal ** 2
		}

	    temp1 = sqrt (temp1 / temp2)

	    if (message == 3) {
	        call printf ("|A>> Error = %g  (tol4 = %g)")
	    	    call pargr (temp1)
		    call pargr (tol4)
		if (nn == 1)
	            call printf ("  in solution of linear eqs.\n")
		else
	            call printf ("\n")
	    }

	    if (temp1 <= tol4 || abs (temp1 - temp1_old) <= tol4 * 1.0e-2 ||
		nn == 11) 
		break
	    temp1_old = temp1

	    call convolution_c (psfft, rwk1, rwk2, n1, n2, n_conv, carray, work)

	    do l = 1, n2deg {
	        lnsub = (l - 1) * nsub
	        do k = 1, n1deg {
		    knsub = (k - 1) * nsub
		    if (poisson) {
	                temp1 = 0.0
		        do ll = 1, nblk
		            do kk = 1, nblk
				temp1 = temp1 + conv[knsub+kk,lnsub+ll]
	                if (temp1 <= 0.0)
		            temp1 = 1.0
		        var = 0.5 * degrade[k,l] / temp1 ** 2
		    } else {
	                var = vc[1] + vc[2] * abs(degrade[k,l]) 
	                if (var <= EPSILONR)
	                    var = MAX_REAL
		        var = 1.0 / var
		    }

	            temp2 = 0.0
		    do ll = 1, nblk
		        do kk = 1, nblk
			    temp2 = temp2 + rwk2[knsub+kk,lnsub+ll]

		    temp1 = var * temp2

		    do ll = 1, nblk
		        do kk = 1, nblk
		    	    rwk2[knsub+kk,lnsub+ll] = temp1
		}
	    }

	    call correlation (psfft, rwk2, rwk2, n1, n2, n_conv, carray, work)

	    call amulkr (rwk2, alpha, rwk2, narr)

	    aa = 0.0
	    ba = 0.0
	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub {
		    rwk2[k,l] = rwk1[k,l] / current[k,l] + rwk2[k,l]

	            diagonal = current[k,l] / 
	                (1.0 + factor1 * hess[k,l] * current[k,l])

		    aa = aa + (rwk2[k,l] * diagonal) ** 2
		    ba = ba + rwk1[k,l] * rwk2[k,l] * diagonal
		}

	    cc = ba / aa
	    if (nn == 1)
		cc = 1.0

	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub {
		    new[k,l] = new[k,l] + cc * rwk1[k,l]

	       	    gradJ = DH2(current[k,l],model[k,l]) - 
			alpha * gradE[k,l] - beta
		    rwk1[k,l] = rwk2[k,l] * cc + gradJ - rwk1[k,l] *
		 	(1.0 / current[k,l] + factor1 * hess[k,l])
		}
	}

        if (nn == 1)
	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub
	            new[k,l] = CLIP(current[k,l] + rwk1[k,l])
	else
	    do l = 1, n2deg_sub
	        do k = 1, n1deg_sub 
	            new[k,l] = CLIP(current[k,l] + new[k,l])
end

# Determine conjugate direction based on the approx. Newton direction
# (preconditioned conjugate method).

procedure nr_conj (degrade, psfft, pre_ch, prech_conv, current, conv, new, 
	      n1, n2, n1deg, n2deg, poisson, nsub, nblk, vc, n_conv,
	      message, carray, work)

include	"mem.com"

real 	degrade[n1deg,n2deg], current[n1,n2], conv[n1,n2], new[n1,n2]
complex	psfft[n1,n2]
real	pre_ch[n1,n2], prech_conv[n1,n2]    # Previous change and its blurred
int	n1, n2, n1deg, n2deg		    # Array and degraded image sizes
bool	poisson				    # P. noise only (or plus Gau. nse)?
int	nsub				    # Subpixelization factor
int	nblk				    # Block size
real	vc[2]
int	n_conv				    # Number of convolution/correlations
int	message				
complex carray[n1,n2]
pointer	work

int	n1deg_sub, n2deg_sub		     # nsub * degraded image size
int	narr, k, l, kk, ll, knsub, lnsub
real 	var, aa, aa1, bb, bb1, cc, temp1, temp2

begin
	n_conv = n_conv + 1
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	narr = n1 * n2
	aa = 0.0
	aa1 = 0.0
	bb = 0.0
	bb1 = 0.0

	# The convolution result  (blurred new) is in the real part of carray. 

	call ffft_b (new, carray, n1, n2, work)
	call amulx (carray, psfft, carray, narr)
	call ifft_d (carray, n1, n2, work)

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
		if (current[k,l] > 0.0) { 
		    aa = aa + pre_ch[k,l] * (new[k,l] - current[k,l]) /
			current[k,l]
		    bb = bb + pre_ch[k,l] ** 2 / current[k,l]
		}
	    }

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
		knsub = (k - 1) * nsub
		if (poisson) {
	            temp1 = 0.0
		    do ll = 1, nblk
		        do kk = 1, nblk
		    	    temp1 = temp1 + conv[knsub+kk,lnsub+ll]
	            if (temp1 <= 0.0)
		        temp1 = 1.0
		    var = 0.5 * degrade[k,l] / temp1 ** 2
		} else {
	            var = vc[1] + vc[2] * abs(degrade[k,l]) 
	            if (var <= EPSILONR)
	                var = MAX_REAL
		    var = 1.0 / var
		}

        	temp1 = 0.0
		temp2 = 0.0
		do ll = 1, nblk
	    	    do kk = 1, nblk {
			temp1 = temp1 + prech_conv[knsub+kk,lnsub+ll]
			temp2 = temp2 + real(carray[knsub+kk,lnsub+ll]) - 
		    	    conv[knsub+kk,lnsub+ll]
	    	    }

		aa1 = aa1 + temp1 * temp2 * var
		bb1 = bb1 + temp1 ** 2 * var 
	    }
	}
	aa = aa + alpha * aa1
	bb = bb + alpha * bb1
	cc = aa / bb

	# Change new by conjugate method, and cal. its blurred.

	if (message == 3) {
	    call printf ("|C>> Conjugacy = %g\n")
	        call pargr (cc)
	}

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
		new[k,l] = CLIP(new[k,l] - cc * pre_ch[k,l])
		prech_conv[k,l] = real(carray[k,l]) - cc * prech_conv[k,l]
	    }
end

# For "Poisson noise only".
# One-dim. search for max J point along the direction determined before. 
# opt=1: no; 2: quadratic extrapolation and cubic interpolation;
# 3: quadratic extrapolation and accurate interpolation.

procedure opt_step_p (degrade, model, current, conv, new, newconv, gradE,
	      NgradE, n1, n2, n1deg, n2deg, nsub, nblk, vc, opt, step,
	      message)

include	"mem.com"

define	STEP_MIN	0.0
define	STEP_MAX	2.0

real	degrade[n1deg,n2deg], model[n1,n2]
real	current[n1,n2], conv[n1,n2]	# Current iterate and its blurred
real	new[n1,n2], newconv[n1,n2]	# Next iterate and its blurred
real	gradE[n1,n2], NgradE[n1,n2] 	# Old and new gradients of half ...
int	n1, n2, n1deg, n2deg		# Array and degraded image sizes
int	nsub				# Subpixelization factor
int	nblk				# Block size
real	vc[2]
int	opt			# 1-D search methods, 1 - 3
real	step			# Optimal step, limited on [STEP_MIN, _MAX] 
int	message

real	h_alpha, uu, uuconv, newtp
real 	gradJ, gradJdotd_b0, gradJdotd_b1, J0, J1, z, w

real	bb, cc, dd, dd1, step_appx, step_tmp, bc, df, df1, dstep, temp1, temp2
real	temp3

int	n1deg_sub, n2deg_sub	# nsub * degraded image size
int	k, l, kk, ll, knsub, lnsub, nn

real	lhood0, lhood1

begin
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	
	h_alpha = 0.5 * alpha
	newtp = 0.0
	gradJdotd_b0 = 0.0
	gradJdotd_b1 = 0.0
	lhood0 = 0.0
	lhood1 = 0.0
	J0 = -beta * xtp
	J1 = 0.0
	bb = 0.0

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub
		
		temp1 = 0.0
		temp2 = 0.0
		do ll = 1, nblk
		    do kk = 1, nblk {
			temp1 = temp1 + conv[knsub+kk,lnsub+ll]
			temp2 = temp2 + newconv[knsub+kk,lnsub+ll] 
		    }

		if (temp1 > 0.0)
		    lhood0 = lhood0 + temp1 - degrade[k,l] * log (temp1)
		if (temp2 > 0.0)
		    lhood1 = lhood1 + temp2 - degrade[k,l] * log (temp2)
	    }
	}

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
		uu = new[k,l] - current[k,l]
		uuconv = newconv[k,l] - conv[k,l]
		newtp = newtp + new[k,l]
		bb = bb + uuconv

	        gradJ = DH2(current[k,l],model[k,l]) - alpha* gradE[k,l] - beta
	        gradJdotd_b0 = gradJdotd_b0 +  gradJ * uu 

	        gradJ = DH2(new[k,l],model[k,l]) - alpha * NgradE[k,l] - beta
	        gradJdotd_b1 = gradJdotd_b1 +  gradJ * uu

	        J0 = J0 + H2(current[k,l],model[k,l])
	        J1 = J1 + H2(new[k,l],model[k,l])
	    }

	J0 = J0 - h_alpha * lhood0 
	J1 = J1 - h_alpha * lhood1 - beta * newtp

	if (gradJdotd_b0 * gradJdotd_b1 >= -EPSILONR) { 
	    # Search for max J point by quadratic extrapolation 

	    step = min (gradJdotd_b0 / (gradJdotd_b0 - gradJdotd_b1), 
	        STEP_MAX)
	    step = max (step, STEP_MIN)
	} else {
	    # Search for max J point by cubic interpolation 

	    z = 3.0 * (J1 - J0) - gradJdotd_b0 - gradJdotd_b1
	    w = sqrt (z * z - gradJdotd_b0 * gradJdotd_b1)
	    step = gradJdotd_b0 / (gradJdotd_b0 - z + w)  
	}

	if (step < 0.0)
	    step = 0.0

	if (message == 3) {
	    call printf ("o  step = %g\n")
    	        call pargr (step)
	}

	if (opt == 3 && step < 1.0 && step > EPSILONR) {
	    # Accurate interpolation

	    bb = h_alpha * bb
	    cc = beta * (newtp - xtp)
	    bc = bb + cc
            step_appx = step

	    # Use Newton method to find accurate max J point, starting with
	    # cubic approx. point.

	    do nn = 1, 20 {
		step_tmp = step
	        dd = 0.0
	        dd1 = 0.0
	        df = 0.0
	        df1 = 0.0

	        do l = 1, n2deg {
	            lnsub = (l - 1) * nsub
	            do k = 1, n1deg {
	                knsub = (k - 1) * nsub
		
		        temp1 = 0.0
		        temp2 = 0.0
		        do ll = 1, nblk
		            do kk = 1, nblk {
		    	        temp1 = temp1 + conv[knsub+kk,lnsub+ll]
			        temp2 = temp2 + newconv[knsub+kk,lnsub+ll] -
			            conv[knsub+kk,lnsub+ll] 
		            }
			temp3 = temp1 + step * temp2
			if (temp3 > 0.0) {
		 	    dd1 = dd1 + degrade[k,l] * temp2 / temp3
		 	    df1 = df1 + degrade[k,l] * temp2 ** 2  / temp3 ** 2 
			}
	            }
	        }
		dd1 = -h_alpha * dd1
		df1 = h_alpha * df1

	        do l = 1, n2deg_sub
	            do k = 1, n1deg_sub {
		        uu = new[k,l] - current[k,l]
		        dd = dd + uu * 
		            log (CLIP(current[k,l] + step * uu) / model[k,l]) 
		        df = df + uu ** 2 / (current[k,l] + step * uu)
		    }

	        dstep = (dd + dd1 + bc) / (df + df1)
 	        step =  step - dstep

		if (step < EPSILONR) {
		    step = STEP_MIN
	            call printf ("v  step = %g\n")
	                call pargr (step)
		    break
		}

	        if (message == 3) {
		    call printf ("v  step = %g\n")
	                call pargr (step)
		}

	        if (abs (step - step_tmp) < 1.0e-5)
		break
	    }

	    if (step >= 1.0)
		step = step_appx
	}
end

# One-dim. search for max J point along the direction determined before. 
# opt=1: no; 2: quadratic extrapolation and cubic interpolation;
# 3: quadratic extrapolation and accurate interpolation.

procedure opt_step_g (degrade, model, current, conv, new, newconv, gradE,
	      NgradE, n1, n2, n1deg, n2deg, nsub, nblk, vc, opt, step,
	      message)

include	"mem.com"

define	STEP_MIN	0.0
define	STEP_MAX	4.0

real	degrade[n1deg,n2deg], model[n1,n2]
real	current[n1,n2], conv[n1,n2]	# Current iterate and its blurred
real	new[n1,n2], newconv[n1,n2]	# Next iterate and its blurred
real	gradE[n1,n2], NgradE[n1,n2] 	# Old and new gradients of half chi-sq
int	n1, n2, n1deg, n2deg		# Array and degraded image sizes
int	nsub				# Subpixelization factor
int	nblk				# Block size
real	vc[2]
int	opt			# 1-D search methods, 1 - 3
real	step			# Optimal step, limited on [STEP_MIN, _MAX] 
int	message

real	h_alpha, var, uu, newchisq, newtp
real 	gradJ, gradJdotd_b0, gradJdotd_b1, J0, J1, z, w

real	aa, bb, cc, dd, step_appx, step_tmp, bc, df, dstep, temp1, temp2

int	n1deg_sub, n2deg_sub	# nsub * degraded image size
int	k, l, kk, ll, knsub, lnsub

begin
	n1deg_sub = n1deg * nsub
	n2deg_sub = n2deg * nsub
	
	h_alpha = 0.5 * alpha
	newchisq = 0.0
	newtp = 0.0
	gradJdotd_b0 = 0.0
	gradJdotd_b1 = 0.0
	J0 = -h_alpha * xchisq - beta * xtp
	J1 = 0.0
	aa = 0.0
	bb = 0.0

	do l = 1, n2deg {
	    lnsub = (l - 1) * nsub
	    do k = 1, n1deg {
	        knsub = (k - 1) * nsub
	        var = vc[1] + vc[2] * abs(degrade[k,l]) 
	        if (var <= EPSILONR)
	            var = MAX_REAL
		
		temp1 = 0.0
		temp2 = 0.0
		do ll = 1, nblk
		    do kk = 1, nblk {
			temp1 = temp1 + newconv[knsub+kk,lnsub+ll]
			temp2 = temp2 + newconv[knsub+kk,lnsub+ll] -
			    conv[knsub+kk,lnsub+ll]
		    }

	        newchisq = newchisq + (temp1 - degrade[k,l]) ** 2 / var
		aa = aa + temp2 ** 2 / var
	    }
	}

	do l = 1, n2deg_sub
	    do k = 1, n1deg_sub {
		uu = new[k,l] - current[k,l]
		newtp = newtp + new[k,l]

	        gradJ = DH2(current[k,l],model[k,l]) - beta
	        gradJdotd_b0 = gradJdotd_b0 +  gradJ * uu 

	        gradJ = DH2(new[k,l],model[k,l]) - alpha * NgradE[k,l] - beta
	        gradJdotd_b1 = gradJdotd_b1 +  gradJ * uu

	        J0 = J0 + H2(current[k,l],model[k,l])
	        J1 = J1 + H2(new[k,l],model[k,l])

		bb = bb + uu * gradE[k,l]
	    }

	gradJdotd_b0 = gradJdotd_b0 - alpha * bb 
	J1 = J1 - h_alpha * newchisq - beta * newtp

	if (gradJdotd_b0 * gradJdotd_b1 >= -EPSILONR) { 
	    # Search for max J point by quadratic extrapolation 

	    step = min (gradJdotd_b0 / (gradJdotd_b0 - gradJdotd_b1), 
	        STEP_MAX)
	    step = max (step, STEP_MIN)
	} else {
	    # Search for max J point by cubic interpolation 

	    z = 3.0 * (J1 - J0) - gradJdotd_b0 - gradJdotd_b1
	    w = sqrt (z * z - gradJdotd_b0 * gradJdotd_b1)
	    step = gradJdotd_b0 / (gradJdotd_b0 - z + w)  
	}

	if (step < 0.0)
	    step = 0.0

	if (message == 3) {
	    call printf ("o  step = %g\n")
    	        call pargr (step)
	}

	if (opt == 3 && step < 1.0 && step > EPSILONR) {
	    # Accurate interpolation

	    aa = alpha * aa
	    bb = alpha * bb
	    cc = beta * (newtp - xtp)
	    bc = bb + cc
            step_appx = step

	    # Use Newton method to find accurate max J point, starting with
	    # cubic approx. point.

	    do kk = 1, 20 {
		step_tmp = step
	        dd = 0.0
	        df = 0.0

	        do l = 1, n2deg_sub
	            do k = 1, n1deg_sub {
		        uu = new[k,l] - current[k,l]
		        dd = dd + uu * 
		            log (CLIP(current[k,l] + step * uu) / model[k,l]) 
		        df = df + uu ** 2 / (current[k,l] + step * uu)
		    }

	        dstep = (aa * step + dd + bc) / (aa + df)
 	        step =  step - dstep

		if (step < EPSILONR) {
		    step = STEP_MIN
	            call printf ("v  step = %g\n")
	                call pargr (step)
		    break
		}

	        if (message == 3) {
		    call printf ("v  step = %g\n")
	                call pargr (step)
		}

	        if (abs (step - step_tmp) < 1.0e-5)
		break
	    }

	    if (step >= 1.0)
		step = step_appx
	}
end

# This is to write header keywords of the restored image, recording a few
# iteration parameters and variables, some of which are from common /me_com/.

procedure me_header_d (restore, nsub, blksum, noise, adu, sigma, fwhm, a_sp, 
	      hidden, me_image, converge, niter, n_conv)

include	"mem.com"

pointer restore		# Pointer of output restored image descriptor 
int	nsub		# Subpixelization factor
bool	blksum		# Block sum nsub**2 subpixels to get a normal pixel? 
real	noise		# Readout noise in electrons
real	adu		# A/D conversion constant, electrons/DN
real	sigma[2]	# Sigmas of Gaussian fn (ICF)
real	fwhm[2]		# Full widths at half max of Gaussian fn (ICF)
real	a_sp		# Speed factor for renewing alpha
bool	hidden		# Is the output image a hidden (or visible) image?
bool	me_image	# Is the output image an ME image?
bool	converge	# Is the output image a converged one?
int	niter		# The total number of iterations
int	n_conv		# Number of convolution/correlations

begin
	call imastr (restore, "  ", "  mem records:  ")
	call imaddi (restore, "ME_NSUB", nsub) 
	call imaddb (restore, "ME_BSUM", blksum) 
	call imaddr (restore, "ME_NOISE", noise) 
	call imaddr (restore, "ME_ADU", adu) 
	call imaddr (restore, "ME_TP", tp)           	# Total power of image 
	call imaddr (restore, "ME_SIGM1", sigma[1]) 
	call imaddr (restore, "ME_SIGM2", sigma[2]) 
	call imaddr (restore, "ME_FWHM1", fwhm[1]) 
	call imaddr (restore, "ME_FWHM2", fwhm[2]) 

	call imaddr (restore, "ME_ALPHA", alpha) 	# Multiplier alpha 
	call imaddr (restore, "ME_A_SP", a_sp)	 	 
	call imaddr (restore, "ME_BETA", beta)	 	# Multiplier beta 

	call imaddb (restore, "ME_HIDDN", hidden) 
	call imaddb (restore, "ME_MEIMG", me_image) 
	call imaddb (restore, "ME_CONVG", converge) 
	call imaddi (restore, "ME_NITER", niter) 
	call imaddi (restore, "ME_NCONV", n_conv) 

	call imaddr (restore, "ME_MAX", immax)		# Max of hidden image 
	call imaddr (restore, "ME_MIN", immin) 		# Min of hidden image
end

# The following 5 procedures are shared by the task source programs in
# the mem0 package of IRAF.

# Move real ain[n1in,n2in] to aout[n1out,n2out]. ain[1,1] => aout[1,1]

procedure move_array (ain, n1in, n2in, aout, n1out, n2out)

real	ain[n1in,n2in], aout[n1out,n2out]  # Input and output real arrays
int	n1in, n2in, n1out, n2out	   # Array sizes

int	n1min, n2min, i, j

begin
	n1min = min (n1in, n1out)
	n2min = min (n2in, n2out)

	# Clear the out array

	do j = 1, n2out
	    do i = 1, n1out
	        aout[i,j] = 0.0 

	# Copy from input to out array

	do j = 1, n2min
	    do i = 1, n1min
	        aout[i,j] = ain[i,j] 
end

# Find the peak value and its location of a real array

procedure arrpeak (a, n1, n2, pval, ploc)

real	a[n1,n2]
int	n1, n2
real	pval		# Peak value
int	ploc[2]		# Peak location

real	ptmp	
int	i, j 

real	ahivr()

begin

	    pval = ahivr (a[1,1], n1)
	    ploc[2] = 1
	    do j = 2, n2 {
	        ptmp = ahivr (a[1,j], n1)
	        if (ptmp > pval) {
	            pval = ptmp
	            ploc[2] = j
	        }
	    }
	    ploc[1] = 1
	    do i = 1, n1 {
	        if (a[i,ploc[2]] == pval) {
	            ploc[1] = i
	            break
	        }
	    }
end

# Shift array a[n1,n2] by (sh[1],sh[2]) so that its peak at [1,1] if center=yes,
# then set the peak value=1.0 if norm="peak", or volume=1.0 if norm="volume".
# Real array atmp is working space.

procedure standard (a, n1, n2, center, norm, pval, sh, atmp)

real	a[n1,n2]
int	n1, n2
bool	center		# Move PSF peak to the DFT center?
char	norm[SZ_LINE]	# Normalization control
real	pval		# Peak value of array
int	sh[2]		# Amount of shift
real	atmp[n1,n2]	# Working space

int	narr		# Total number of points in array
int	inln, outln, i, j
real	scale

int	strncmp ()

begin
	narr = n1 * n2

	if (center) {
	    outln = 1 + sh[2]
	    outln = mod (outln, n2)
	    do inln = 1, n2 {
	        if (outln < 1)
	            outln = outln + n2
	        if (outln > n2)
                   outln = outln - n2
                call lnshift (a[1,inln], atmp[1,outln], n1, sh[1]) 
                outln = outln + 1
	    }
	    do j = 1, n2
	        do i = 1, n1 
	            a[i,j] = atmp[i,j]
	}
	
	if (strncmp (norm, "p", 1) == 0) {	# For peak normalization
	    scale =  pval
	    call adivkr (a, scale, a, narr)
	}
	if (strncmp (norm, "v", 1) == 0) { 	# For volume normalization
	    scale = 0.0
	    do j = 1, n2
	        do i = 1, n1 
	            scale = scale + a[i,j]
	    call adivkr (a, scale, a, narr)
	}
end

# Shift cyclically 1-D array ain[n1] by sh1, resulting aout[n1]. 
# ain and aout must be two distinct arrays.

procedure lnshift (ain, aout, n1, sh1)

real	ain[n1], aout[n1]	# Input and output arrays
int	n1, sh1

int	shabs			# abs (sh1)
int	nx			# n1 - abs(sh1)

begin
 
	if (sh1 > 0) {
	    nx = n1 - sh1
	    call amovr (ain, aout[sh1+1], nx)
	    call amovr (ain[nx+1], aout, sh1)
	} else if (sh1 < 0) {
	    shabs = abs (sh1)
	    nx = n1 - shabs
	    call amovr (ain, aout[nx+1], shabs)
	    call amovr (ain[shabs+1], aout, nx)
	} else 
	    call amovr (ain, aout, n1)
end

# The following are interface procedures to the FFT subroutine in ncarfft.

# Interface procedures for using subs in the NCARFFT library for FFT.
# (1) fft_b_ma (pt_carray, n1, n2, work) 	# Initialize  
# (2) fft_b_mf (pt_carray, work)		# Memory free 
# (3) ffft_b (rarray, carray, n1, n2, work)	# Forward FFT
# (4) ifft_b (carray, rarray, n1, n2, work)	# Inverse FFT
# (5) ifft_d (carray, n1, n2, work)		# Inverse FFT

define	LEN_WORK	3
define	TRIGTAB1	Memi[$1]
define	TRIGTAB2	Memi[$1+1]
define	XWORK		Memi[$1+2]

# Memory allocation for complex data array and working space for FFT

procedure fft_b_ma (pt_carray, n1, n2, work)

pointer	pt_carray	# Pointer of complex array 
int	n1, n2		# Array dimensions
pointer	work		# Pointer of fft work structure

int	narr		# Total number of points in array
int	wsiz		# Size of trigonometrical fn table 

begin
	narr = n1 * n2
	call malloc (pt_carray, narr, TY_COMPLEX)

	call malloc (work, LEN_WORK, TY_STRUCT)
	wsiz = n1 * 4 + 15
	call calloc (TRIGTAB1(work), wsiz, TY_REAL)
	call cffti (n1, Memr[TRIGTAB1(work)])

	wsiz = n2 * 4 + 15
	call calloc (TRIGTAB2(work), wsiz, TY_REAL)
	call cffti (n2, Memr[TRIGTAB2(work)])

	call malloc (XWORK(work), n2, TY_COMPLEX)
end

# Memory deallocation for complex data array and working space for FFT

procedure fft_b_mf (pt_carray, work)

pointer	pt_carray	# Pointer of complex array 
pointer	work		# Pointer of fft work structure

begin
	call mfree (pt_carray, TY_COMPLEX)

	call mfree (TRIGTAB1(work), TY_REAL)
	call mfree (TRIGTAB2(work), TY_REAL)
	call mfree (XWORK(work), TY_COMPLEX)
	call mfree (work, TY_STRUCT)
end

# 2-D forward FFT based on NCARFFT. FFT in NCAR is done for complex
# array. So the real input array is moved to the complex array, which
# is then FFTed and output. The scale factor=1.0 (not scaled).
# The input real array, rinput, remains intack after FFT.

procedure ffft_b (rinput, coutput, n1, n2, work)

real	rinput[n1,n2]		# Input real array
complex	coutput[n1,n2]		# Output complex array, allocated in fft_b_ma
int	n1, n2			# Array dimensions
pointer	work			# FFT work structure, allocated in fft_b_ma 

int	i, j

begin
	# Move and transform row by row 
	do i = 1, n2 {
	    call achtrx (rinput[1,i], coutput[1,i], n1)
	    call cfftf (n1, coutput[1,i], Memr[TRIGTAB1(work)])
        }

	# Transform column by column
	do i = 1, n1 {
	    do j = 1, n2 {
	        Memx[XWORK(work)+j-1] = coutput[i,j]
	    }
	    call cfftf (n2, Memx[XWORK(work)], Memr[TRIGTAB2(work)])
	    do j = 1, n2 {
	        coutput[i,j] = Memx[XWORK(work)+j-1]
	    }
	}
end

# 2-D inverse FFT based on NCARFFT. FFT in NCAR is done for complex
# array. So the complex array after FFT is moved to a real array before
# output. The scale factor=1.0/(n1*n2).
# N.B. The input complex array, cinput, is changed after FFT !!

procedure ifft_b (cinput, routput, n1, n2, work)

complex	cinput[n1,n2]		# Input complex array, allocated in fft_b_ma
real	routput[n1,n2]		# Output real array
int	n1, n2			# Array dimensions
pointer	work			# FFT work structure,allocated in fft_b_ma 

real	scale			# Scale factor
int	i, j

begin
	scale = 1.0 / (n1 * n2)

	# Transform row by row
	do i = 1, n2 {
	    call cfftb (n1, cinput[1,i], Memr[TRIGTAB1(work)])
	}

	# Transform column by column
	do i = 1, n1 {
	    do j = 1, n2 {
	        Memx[XWORK(work)+j-1] = cinput[i,j]
	    }
	    call cfftb (n2, Memx[XWORK(work)], Memr[TRIGTAB2(work)])
	    do j = 1, n2 {
	        cinput[i,j] = Memx[XWORK(work)+j-1]
	    }
	}

	# Extract real part of complex array and scale it
	do i = 1, n2 {
	    call achtxr (cinput[1,i], routput[1,i], n1)
	    do j = 1, n1
	        routput[j,i] = routput[j,i] * scale    
	}
end

# 2-D inverse FFT based on NCARFFT. FFT in NCAR is done for complex
# array. The complex array after FFT is scaled by the factor=1.0/(n1*n2),
# and output. 
# N.B. The input complex array, cinput, is changed after FFT !!

procedure ifft_d (cinput, n1, n2, work)

complex	cinput[n1,n2]		# Input complex array, allocated in fft_b_ma
int	n1, n2			# Array dimensions
pointer	work			# FFT work structure,allocated in fft_b_ma 

real	scale			# Scale factor
int	i, j

begin
	scale = 1.0 / (n1 * n2)

	# Transform row by row
	do i = 1, n2 {
	    call cfftb (n1, cinput[1,i], Memr[TRIGTAB1(work)])
	}

	# Transform column by column
	do i = 1, n1 {
	    do j = 1, n2 {
	        Memx[XWORK(work)+j-1] = cinput[i,j]
	    }
	    call cfftb (n2, Memx[XWORK(work)], Memr[TRIGTAB2(work)])
	    do j = 1, n2 {
	        cinput[i,j] = Memx[XWORK(work)+j-1]
	    }
	}

	# Scale the complex array 
	do i = 1, n2 
	    do j = 1, n1
	        cinput[j,i] = cinput[j,i] * scale    
end
