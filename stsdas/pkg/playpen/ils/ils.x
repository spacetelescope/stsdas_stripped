include	<imhdr.h>
include <ctype.h>

define	SA_TYPES	"|none|fixed|update"
define	SA_NONE		1
define	SA_FIXED	2
define	SA_UPDATE	3

define	VAR_SIZE	3                              # variance window size
define	NIMP		10			       # max. # of images


# 2-D array addressing macros. Assumes line size given by
# IL_XSIZEOUT() macro, and structure pointer is `ilp' variable.

define	BR  Memr[$1] 				   # real array base element
define	BX  Memx[$1] 				   # complex array base element
define	AR  Memr[$1+($3-1)*IL_XSIZEOUT(ilp)+ $2-1] # real array element
define	AX  Memx[$1+($3-1)*IL_XSIZEOUT(ilp)+ $2-1] # complex array element
define	MB  Memi[$1+$2-1]			   # multi-array base address


# Data storage structure

define	LEN_ILSTRUCT	30

define	IL_BETA		Memr[$1+0]	# Iteration gain
define	IL_ALPHA	Memr[$1+1]	# Regularization constant
define	IL_NORM		Memr[$1+2]	# Gradient norm
define	IL_ATUNE	Memr[$1+3]	# Spatial adaptivity tuning parameter
define	IL_CCDGAIN	Memr[$1+4]	# Detector gain
define	IL_RNOISE	Memr[$1+5]	# Readout noise
define	IL_ITER		Memi[$1+6]	# Current iteration
define	IL_VERB		Memi[$1+7]	# Verbosity level
define	IL_XSIZEOUT	Memi[$1+8]	# X size of output image
define	IL_YSIZEOUT	Memi[$1+9]	# Y size of output image
define	IL_NP		Memi[$1+10]	# Total # of pixels in output image
define	IL_ADAP		Memi[$1+11]	# Spatial adaptivity control type
define	IL_NIMAGE	Memi[$1+12]	# Number of input images
define	IL_NPSF		Memi[$1+13]	# Number of PSF images
define	IL_NMASK	Memi[$1+14]	# Number of mask images
define	IL_POSITIVITY	Memb[$1+15]	# Turn on positivity constraint ?
define	IL_USEMASK	Memb[$1+16]	# Use mask images ?
define	IL_AUTO		Memb[$1+17]	# Automatic gain ?
define	IL_CHISQ	Memr[$1+18]	# Current chi-square

# Array pointers

define	IL_CIMAGE	Memi[$1+19]	# Complex input image buffers
define	IL_CPSF		Memi[$1+20]	# Complex PSF buffers
define	IL_WMASK	Memi[$1+21]	# Bad pixel mask matrices
define	IL_REGUL	Memi[$1+22]	# Regularization operator
define	IL_OBJ		Memi[$1+23]	# k-th estimate
define	IL_FOBJ		Memi[$1+24]	# k-th estimate's Fourier transform
define	IL_CD		Memi[$1+25]	# Conjugate direction
define	IL_SADAPT	Memi[$1+26]	# Spatial adaptation matrix
define	IL_TEMPR	Memi[$1+27]	# Real scratch array
define	IL_TEMPC	Memi[$1+28]	# Complex scratch array
define	IL_WK		Memi[$1+29]	# Complex workspace for FT routine




# ILS  --   Iterative least squares image restoration.
#
# This task restores images by the least squares (minimum noise norm) plus
# smoothness constraint criterion. Noise regularization is achieved by a 
# 2-D Laplacian smoothness operator, and/or a positivity constraint.
#
# Task features:
# - Simultaneous restoration of multiple distorted versions of the same scene,
#   including multiple psfs, can be performed. 
# - Optional spatial adaptivity, "eye-model" type, is achieved by estimating 
#   the local variance either from an external, "activity" image, or from
#   the restored estimate at the current iteration. 
# - Optional bad pixel masking is implemented thru "mask" images. 
# - Intermediate results may be written to disk along the iteration sequence. 
# - Optional edge processing.
#
#
# The basic conjugate-gradients iteration is performed in data space, however
# the gradient itself is computed in Fourier domain. Uses Norman Brenner's
# fourt FFT routine.
#
#
# 30 Jun 92  -  Task created.
# 10 Feb 93  -  Added positivity constraint.
# 18 Feb 93  -  Model image.
# 25 May 93  -  Smoothness regularization, multiple images.
# 20 Jun 93  -  Spatial adaptivity.
# 12 Jul 93  -  Bad pixel / weight mask.
# 23 Aug 93  -  Update spatial adaptivity array.
# 05 Sep 93  -  Write intermediate images.
# 08 Sep 93  -  Multiple bad pixel / weight masks.
# 12 Sep 93  -  Edge processing.
# 17 Sep 93  -  Conjugate gradients iteration (fixed step).
# 02 Feb 94  -  Conjugate gradients with full line search.
# 01 May 95  -  Chi-square computation.
#
#
# Author:  I. Busko


procedure t_ils()

char	imlisti[SZ_LINE]		# input image list
char	imlistp[SZ_LINE]		# input psf list
char	imlistm[SZ_LINE]		# input bad pixel masks list
char	output[SZ_PATHNAME]		# output image name
char	model[SZ_PATHNAME]		# model image
char	activ[SZ_PATHNAME]		# "activity" image
int	niter				# number of iterations
int	nsave				# output result every nsave iteration
real	px0, py0			# optional psf position in psf image
real	mask				# star mask
bool	nlpsf				# noise-less psf ?
bool	center				# center input data in output image ?

char	image[SZ_PATHNAME]		# image name
char	str[SZ_LINE]			# work area
pointer	ilp				# pointer to data storage structure
pointer	imin, imout, imodel, imask	# IMIO pointers
pointer	pmod				# model image buffer
pointer	act				# activity image
pointer	temp				# temporary pointer
pointer	pp				# pset pointer
int	listi, listp, listm		# list pointers
int	sizex,sizey			# input image(s) size
int	nn[2], ndim, iform, isign	# FFT routine control parameters
int	i,j,i1,j1, iter
int	isz2x,isz2y
long	cpu,clock,tc,te			# timing variables
real	etc, ete, aux
real	limchi
bool	ptime

pointer	immap(), imgs2r(), imps2r()
pointer	il_imread(), il_readp()
pointer	clopset()
int	imtopen(), imtgetim(), imtlen()
int	clgeti()
int	strlen(), strdic()
real	clgpsetr(), clgetr()
long	cputime(), clktime()
bool	clgetb(), clgpsetb()

begin
	# Create structure.
	call malloc (ilp, LEN_ILSTRUCT, TY_STRUCT)

	# Input parameters.
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("psf", imlistp, SZ_LINE)
	call clgstr ("output", output, SZ_PATHNAME)
	call clgstr ("model", model, SZ_PATHNAME)
	call clgstr ("wmask", imlistm, SZ_PATHNAME)
	call il_bclean (imlisti, SZ_LINE)      # Clean string from
	call il_bclean (imlistp, SZ_LINE)      # leading blanks.
	call il_bclean (output, SZ_PATHNAME)
	call il_bclean (model, SZ_PATHNAME)
	call il_bclean (imlistm, SZ_PATHNAME)
	niter            = clgeti ("niter")
	limchi           = clgetr ("limchisq")
	IL_XSIZEOUT(ilp) = clgeti ("xsizeout")
	IL_YSIZEOUT(ilp) = clgeti ("ysizeout")
	center           = clgetb ("center")
	nsave            = clgeti ("nsave")
	IL_VERB(ilp)     = clgeti ("verbosity")

	# Now from pset.
	pp                 = clopset ("ilspars")
	IL_AUTO(ilp)       = clgpsetb (pp, "auto")
	IL_BETA(ilp)       = clgpsetr (pp, "beta")
	IL_ALPHA(ilp)      = clgpsetr (pp, "alpha")
	IL_POSITIVITY(ilp) = clgpsetb (pp, "positivity")
	call clgpseta (pp, "adap", str, SZ_LINE)
	IL_ADAP(ilp) = strdic (str, str, SZ_LINE, SA_TYPES)
	if (IL_ADAP(ilp) != SA_NONE) {
	    IL_ATUNE(ilp)   = clgpsetr (pp, "atune")
	    IL_CCDGAIN(ilp) = clgpsetr (pp, "adu")
	    IL_RNOISE(ilp)  = clgpsetr (pp, "noise")
	}
	call clgpseta (pp, "activity", activ, SZ_PATHNAME)
	call il_bclean (activ, SZ_PATHNAME)
	px0   = clgpsetr (pp, "px0")
	py0   = clgpsetr (pp, "py0")
	mask  = clgpsetr (pp, "mask")
	nlpsf = clgpsetb (pp, "nlpsf")
	call clcpset (pp)

	if (nsave == 0)		# do not write intermediate results
	    nsave = niter + 1

	if (IL_AUTO(ilp))
	    IL_BETA(ilp) = 1.0	# starting step size

	IL_CHISQ(ilp) = 1.E30

	# Expand input image lists.
	listi = imtopen (imlisti)
	listp = imtopen (imlistp)
	IL_NIMAGE(ilp) = imtlen (listi)
	IL_NPSF(ilp)   = imtlen (listp)
	if (IL_NIMAGE(ilp) > NIMP)
	    call error (0, "Too many input images.")
	if ((IL_NPSF(ilp) > 1) && (IL_NIMAGE(ilp) != IL_NPSF(ilp))) {
	    call imtclose (listi)
	    call imtclose (listp)
	    call imtclose (listm)
	    call error (0, "Number of input and psf images not the same.")
	}
	IL_USEMASK(ilp) = false
	if ((strlen(imlistm) > 0) && (!IS_WHITE(imlistm[1]))) {
	    listm = imtopen (imlistm)
	    IL_NMASK(ilp) = imtlen (listm)
	    if ((IL_NMASK(ilp) > 1) && (IL_NIMAGE(ilp) != IL_NMASK(ilp))) {
	        call imtclose (listi)
	        call imtclose (listp)
	        call imtclose (listm)
	        call error (0, "Number of input and mask images not the same.")
	    }
	    IL_USEMASK(ilp) = true
	}

	# If requested output size not even, discard one column/line.
	if ((IL_XSIZEOUT(ilp) > 0) && (IL_YSIZEOUT(ilp) > 0)) {
	    if (mod (IL_XSIZEOUT(ilp),2) != 0)
	        IL_XSIZEOUT(ilp) = IL_XSIZEOUT(ilp) - 1
	    if (mod (IL_YSIZEOUT(ilp),2) != 0)
	        IL_YSIZEOUT(ilp) = IL_YSIZEOUT(ilp) - 1
	}

	# Alloc pointer vectors for multi-array storage.
	call malloc (IL_CIMAGE(ilp), IL_NIMAGE(ilp), TY_INT)
	call malloc (IL_CPSF(ilp),   IL_NPSF(ilp),   TY_INT)
	call malloc (IL_WMASK(ilp),  IL_NMASK(ilp),  TY_INT)

	# Open input image list, read first input image and compute its FT.
	# Also, create work space IL_WK for FFT routine.
	j = imtgetim (listi, image, SZ_PATHNAME)
	IL_WK(ilp) = NULL
	MB(IL_CIMAGE(ilp),1) = il_imread (ilp, image, imin, sizex, sizey)

	# Set parameters for FFT routine.
	ndim  = 2
	iform = 1
	nn[1] = IL_XSIZEOUT(ilp)
	nn[2] = IL_YSIZEOUT(ilp)

	# Print memory info.
	if (IL_VERB(ilp) > 0) {
	    aux = 4. + 3. * 2.			# 4 real + 3 complex buffers
	    aux = aux + real(IL_NIMAGE(ilp) + 	# image and psf complex buffers
	                     IL_NPSF(ilp)) * 2.
	    if (IL_ALPHA(ilp) > 0.)
	        aux = aux + 1. * 2.		# regularization complex oper.
	    if ((IL_ADAP(ilp) != SA_NONE) && (IL_ALPHA(ilp) > 0.))
	        aux = aux + 1.			# adaptation weights
	    if (IL_USEMASK(ilp))
	        aux = aux + IL_NMASK(ilp)	# pixel masks 
	    else if ((IL_XSIZEOUT(ilp) > sizex) || (IL_YSIZEOUT(ilp) > sizey))
	        aux = aux + 1.			# edge extension
	    if (nsave < (niter + 1))
	        aux = aux + 1.			# write intermediate result
	    aux = aux * 4. * IL_NP(ilp) / 1.E6	# 4 bytes / real pixel
	    call printf ("Peak data memory needed = %5.2g Mb.\n")
	        call pargr (aux)
            call flush (STDOUT)
	}

	# Read subsequent input images and compute their FTs.
	if (IL_NIMAGE(ilp) > 1) {
	    do i = 2, IL_NIMAGE(ilp) { 
	        j = imtgetim (listi, image,  SZ_PATHNAME)
	        MB(IL_CIMAGE(ilp),i) = il_imread (ilp, image, temp, i1, j1)
	        call imunmap (temp)
	        if ((i1 != sizex) || (j1 != sizey))
	            call error (0, "Input images have not the same size.")
	    }
	}

	# Read psf image(s) and compute its (their) FT(s).
	do i = 1, IL_NPSF(ilp) { 
	    j = imtgetim (listp, image, SZ_PATHNAME)
	    MB(IL_CPSF(ilp),i) = il_readp (ilp, image, px0,py0, mask, nlpsf)
	}

	# Open and read mask image(s). Must have same size as input.
	# For now, mask images must be comprised of 1s for no masking and
	# 0s for masking. Maybe this should be modified to comply to
	# data quality file pixel definitions.
	if (IL_USEMASK(ilp)) {
	    do i = 1, IL_NMASK(ilp) { 
	        j = imtgetim (listm, image, SZ_PATHNAME)
	        imask = immap (image, READ_ONLY, 0)
	        if (IM_NDIM(imask) != 2)
	            call error (0, "Mask image section is not 2-dimensional.")
	        if ((IM_LEN(imask,1) != sizex) || 
	            (IM_LEN(imask,2) != sizey))
	            call error (0, 
	                       "Mask and input images have different sizes.")
	        temp = imgs2r (imask, 1, sizex, 1, sizey)
	        call calloc (MB(IL_WMASK(ilp),i), IL_NP(ilp), TY_REAL) 
	        call il_center (ilp, temp, MB(IL_WMASK(ilp),i), sizex, sizey)
	        call imunmap (imask)
	    }

	# Otherwise, check if edge extension has been requested, and
	# act accordingly: activate masking and build one edge mask.
	} else {
	    if ((IL_XSIZEOUT(ilp) > sizex) || (IL_YSIZEOUT(ilp) > sizey)) {
	        IL_USEMASK(ilp) = true
	        IL_NMASK(ilp) = 1
	        call calloc (MB(IL_WMASK(ilp),1), IL_NP(ilp), TY_REAL)
	        call amovkr (1., BR(MB(IL_WMASK(ilp),1)), IL_NP(ilp))
	        call il_zedge (ilp, sizex, sizey)
	    }
	}

	# Open model image. Must have same size as input (if edge extension
	# disabled) or output (if enabled).
	if ((strlen(model) > 0) && (!IS_WHITE(model[1]))) {
	    imodel = immap (model, READ_ONLY, 0)
	    if (IM_NDIM(imodel) != 2)
	        call error (0, "Model image section is not 2-dimensional.")
	    if (IL_USEMASK(ilp)) {
	        if ((IM_LEN(imodel,1) != IL_XSIZEOUT(ilp)) || 
	            (IM_LEN(imodel,2) != IL_YSIZEOUT(ilp)))
	            call error (0, 
	            "Model and output images have different sizes.")
	    } else {
	        if ((IM_LEN(imodel,1) != sizex) || 
	            (IM_LEN(imodel,2) != sizey))
	            call error (0, 
	            "Model and input images have different sizes.")
	    }
	}

	# Open output image with same header as input image.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = IL_XSIZEOUT(ilp)
	IM_LEN(imout,2) = IL_YSIZEOUT(ilp)
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, 
	        "Least Squares restoration of %s")
	    call pargstr (imlisti)

	# Get output buffer.
	IL_OBJ(ilp) = imps2r (imout, 1, IM_LEN(imout,1), 1, IM_LEN(imout,2))
	call amovkr (0., BR(IL_OBJ(ilp)), IL_NP(ilp))

	# If requested, create regularization operator (2-D Laplacian) 
	# and compute its FT.
	if (IL_ALPHA(ilp) > 0.) {
	    call calloc (IL_REGUL(ilp), IL_NP(ilp), TY_COMPLEX)
	    isz2x = IL_XSIZEOUT(ilp) / 2
	    isz2y = IL_YSIZEOUT(ilp) / 2
	    AX(IL_REGUL(ilp), isz2x,   isz2y)   = complex(1.,0.)
	    AX(IL_REGUL(ilp), isz2x-1, isz2y)   = complex(-0.25,0.)
	    AX(IL_REGUL(ilp), isz2x+1, isz2y)   = complex(-0.25,0.)
	    AX(IL_REGUL(ilp), isz2x,   isz2y-1) = complex(-0.25,0.)
	    AX(IL_REGUL(ilp), isz2x,   isz2y+1) = complex(-0.25,0.)
	    isign = -1
	    call fourt (BX(IL_REGUL(ilp)), nn, ndim, isign, iform,
	                Memx[IL_WK(ilp)])
	}

	# Alloc space for estimate's FT, and for conjugate direction.
	call calloc (IL_FOBJ(ilp), IL_NP(ilp), TY_COMPLEX)
	call calloc (IL_CD(ilp),  IL_NP(ilp), TY_REAL)
	IL_NORM(ilp) = 1.d0

	# If supplied, use model image to initialize.
	if ((strlen(model) > 0) && (!IS_WHITE(model[1]))) {
	    pmod = imgs2r (imodel, 1, IL_XSIZEOUT(ilp), 1, IL_YSIZEOUT(ilp))
	    call amovr  (BR(pmod), BR(IL_OBJ(ilp)),   IL_NP(ilp))
	    call achtrx (BR(pmod), BX(IL_FOBJ(ilp)), IL_NP(ilp))
	    call imunmap (imodel)
	    isign = -1
	    call fourt (BX(IL_FOBJ(ilp)), nn, ndim, isign, iform, 
	                Memx[IL_WK(ilp)])
	}

	# Compute regularization weights from either first input 
	# image or activity image.
	if ((IL_ALPHA(ilp) > 0.) &&
	    (IL_ADAP(ilp) != SA_NONE)) {
	    call malloc (IL_SADAPT(ilp), IL_NP(ilp), TY_REAL)
	    if (strlen(activ) != 0) {
	        act  = immap (activ, READ_ONLY, 0)
	        pmod = imgs2r (act, 1, sizex, 1, sizey)
	    } else
	        pmod = imgs2r (imin, 1, sizex, 1, sizey)
	    call calloc (temp, IL_NP(ilp), TY_REAL)
	    call il_center (ilp, pmod, temp, sizex, sizey)
	    call il_weight (ilp, temp)
	    call mfree (temp, TY_REAL)
	    if (strlen(activ) != 0)
	        call imunmap (act)
	}

	# First input image is not necessary anymore; can be closed.
	call imunmap (imin)

	# Alloc scratch space.
	call malloc (IL_TEMPC(ilp), IL_NP(ilp), TY_COMPLEX)
	call malloc (IL_TEMPR(ilp), IL_NP(ilp), TY_REAL)

	# Timing variables.
	cpu   = cputime (0)
	clock = clktime (0)
	ptime = true

	# Main loop.
	iter = 0
	while ((iter < niter) && (IL_CHISQ(ilp) > limchi)) {
	    iter = iter + 1
	    IL_ITER(ilp) = iter

	    if (IL_VERB(ilp) == 1) {
	        call printf ("Iteration %d   ")
	            call pargi (IL_ITER(ilp))
	        call flush (STDOUT)
	    } else if (IL_VERB(ilp) >= 2) {
	        call printf ("--- Begin iteration %d ---\n")
	            call pargi (IL_ITER(ilp))
	        call flush (STDOUT)
	    }

	    # Update estimate by one conjugate gradient iteration.
	    call il_cg (ilp)

	    # Print chi-square.
	    if (IL_AUTO(ilp)) {
	        call printf ("Chi-square = %g\n")
	            call pargr (IL_CHISQ(ilp))
	        call flush (STDOUT)
	    }

	    # Guess and print total execution time.
	    if (IL_VERB(ilp) > 0) {
	        tc  = cputime (cpu)
	        te  = clktime (clock)
	        etc = real(tc) / 1000. / iter * niter
	        ete = real(te) /   60. / iter * niter
	        if (ptime && (te > long(40))) {
	            call printf ("\nEstimated %6.0f CPU seconds ")
	                call pargr (etc)
	            call printf ("or %5.1f elapsed minutes to finish.\n\n")
	                call pargr (ete)
	            call flush (STDOUT)
	            ptime = false
	        }
	    }

	    # Re-compute regularization weights. 
	    if ((IL_ALPHA(ilp) > 0.) &&
	        (IL_ADAP(ilp) == SA_UPDATE))
	        call il_weight (ilp, IL_OBJ(ilp))

	    # Write intermediate result.
	    if (mod(IL_ITER(ilp),nsave) == 0) {
	        call malloc (temp, IL_NP(ilp), TY_REAL)
	        call amovr (BR(IL_OBJ(ilp)), BR(temp), IL_NP(ilp))
	        if (((IL_XSIZEOUT(ilp) > sizex)  || 
	             (IL_YSIZEOUT(ilp) > sizey)) && 
	             !center)
	             call il_decenter (ilp, sizex, sizey)
	        call imunmap (imout)
	        imout = immap (output, READ_WRITE, 0)
	        IL_OBJ(ilp) = imps2r (imout, 1, IL_XSIZEOUT(ilp), 
	                                     1, IL_YSIZEOUT(ilp))
	        call amovr (BR(temp), BR(IL_OBJ(ilp)), IL_NP(ilp))
	        call mfree (temp, TY_REAL)
	    }
	}

	# De-center output image.
	if (((IL_XSIZEOUT(ilp) > sizex)  || 
	     (IL_YSIZEOUT(ilp) > sizey)) && 
	      !center)
	    call il_decenter (ilp, sizex, sizey)

	# Print total execution time.
	if (IL_VERB(ilp) > 0) {
	    cpu   = cputime (cpu)
	    clock = clktime (clock)
	    call printf ("\n%7.2f CPU seconds,  %5.2f elapsed minutes.\n")
	        call pargr (real (cpu) / 1000.)
	        call pargr (real (clock) / 60.)
	}

	# Close images and work areas.
	call mfree (IL_TEMPR(ilp), TY_REAL)
	call mfree (IL_TEMPC(ilp), TY_COMPLEX)
	if ((IL_ADAP(ilp) != SA_NONE) && (IL_ALPHA(ilp) > 0.))
	    call mfree (IL_SADAPT(ilp), TY_REAL)
	call mfree (IL_CD(ilp), TY_REAL)
	call mfree (IL_FOBJ(ilp), TY_COMPLEX)
	if (IL_ALPHA(ilp) > 0.)
	    call mfree (IL_REGUL(ilp), TY_COMPLEX)
	call imunmap (imout)
	if (IL_USEMASK(ilp)) {
	    do i = 1, IL_NMASK(ilp)
	        call mfree (MB(IL_WMASK(ilp),i), TY_REAL)
	} 
	do i = 1, IL_NPSF(ilp)
	    call mfree (MB(IL_CPSF(ilp),i), TY_COMPLEX)
	do i = 1, IL_NIMAGE(ilp)
	    call mfree (MB(IL_CIMAGE(ilp),i), TY_COMPLEX)
	call mfree (IL_WK(ilp), TY_COMPLEX)

	call mfree (IL_WMASK(ilp), TY_INT)
	call mfree (IL_CPSF(ilp),  TY_INT)
	call mfree (IL_CIMAGE(ilp),  TY_INT)

	call mfree (ilp, TY_STRUCT)
end




# IL_CG  --  Conjugate gradient iteration.
#
# Routine updates current estimate stored in array fk by the conjugate
# gradients algorithm. Computations are performed in data space, however,
# the gradient computation itself is performed in Fourier space. All
# Fourier transforms must be performed prior to calling this routine.
# Multiple input images, as well as multiple PSFs, are supported.

procedure il_cg (ilp)

pointer	ilp			# pointer to data storage structure

pointer	gradient		# real gradient
pointer	accum			# complex accumulator
int	nn[2],ndim,iform,isign	# FFT routine control parameters
int	i, j, k, kp, km
real	gamma, norm
complex	hconj

begin
	# Initialization.
	call calloc (accum,    IL_NP(ilp), TY_COMPLEX)
	call malloc (gradient, IL_NP(ilp), TY_REAL)
	ndim  = 2
	iform = 1
	nn[1] = IL_XSIZEOUT(ilp)
	nn[2] = IL_YSIZEOUT(ilp)

	# Compute current estimate's FT.
	call achtrx (BR(IL_OBJ(ilp)), BX(IL_FOBJ(ilp)), IL_NP(ilp))
	isign = -1
	call fourt (BX(IL_FOBJ(ilp)), nn, ndim, isign, iform, Memx[IL_WK(ilp)])

	# Gradient computation. Loop thru input, psf and mask images.
	if (IL_VERB(ilp) >= 2) {
	    call printf ("Computing gradient...  ")
	    call flush (STDOUT)
	}
	do k = 1, IL_NIMAGE(ilp) {

	    kp = k
	    if (IL_NPSF(ilp) == 1)
	        kp = 1
	    km = k
	    if (IL_NMASK(ilp) == 1)
	        km = 1

	    # Multiply previous estimate by psf FT (convolve).
	    call amulx (BX(IL_FOBJ(ilp)), BX(MB(IL_CPSF(ilp),kp)), 
	                BX(IL_TEMPC(ilp)), IL_NP(ilp)) 

	    # Subtract convolved estimate from input image.
	    call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp), 
	                                      IL_YSIZEOUT(ilp))
	    call asubx (BX(MB(IL_CIMAGE(ilp),k)), BX(IL_TEMPC(ilp)), 
	                BX(IL_TEMPC(ilp)), IL_NP(ilp))

	    # Pixel masking at restoration step.
	    if (IL_USEMASK(ilp)) {
	        # Inverse transform back to image space.
	        isign = 1
	        call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform,
	                    Memx[IL_WK(ilp)])
	        call achtxr (BX(IL_TEMPC(ilp)), BR(IL_TEMPR(ilp)),IL_NP(ilp))
	        call adivkr (BR(IL_TEMPR(ilp)), real(IL_NP(ilp)), 
	                     BR(IL_TEMPR(ilp)), IL_NP(ilp))
	        # Do it.
	        call amulr (BR(IL_TEMPR(ilp)), BR(MB(IL_WMASK(ilp),km)), 
	                    BR(IL_TEMPR(ilp)), IL_NP(ilp))
	        # Fourier transform again...
	        call achtrx (BR(IL_TEMPR(ilp)), BX(IL_TEMPC(ilp)), IL_NP(ilp))
	        isign = -1
	        call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform,
	                    Memx[IL_WK(ilp)])
	    }

	    # Multiply by PSF conjugate FT (reblur with transposed PSF).
	    do j = 1, IL_YSIZEOUT(ilp) {
	        do i = 1, IL_XSIZEOUT(ilp) {
	            hconj = conjg (AX(MB(IL_CPSF(ilp),kp),i,j))
	            AX(IL_TEMPC(ilp),i,j) = hconj * AX(IL_TEMPC(ilp),i,j)
	        }
	    }
	    call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp), 
	                                      IL_YSIZEOUT(ilp))

	    # Accumulate.
	    call amulkx (BX(IL_TEMPC(ilp)), complex(1./IL_NIMAGE(ilp),0.), 
	                 BX(IL_TEMPC(ilp)), IL_NP(ilp))
	    call aaddx (BX(accum), BX(IL_TEMPC(ilp)), BX(accum), IL_NP(ilp))
	}

	# Regularize.
	if (IL_ALPHA(ilp) > 0.) {

	    # Multiply by regularization operator.
	    call amulx (BX(IL_FOBJ(ilp)), BX(IL_REGUL(ilp)), BX(IL_TEMPC(ilp)), 
	                IL_NP(ilp))

	    # Regularization weighting.
	    if (IL_ADAP(ilp) != SA_NONE) {
	        # Inverse transform back to image space.
	        call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp),
	                                          IL_YSIZEOUT(ilp))
	        isign = 1
	        call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform,
	                    Memx[IL_WK(ilp)])
	        call achtxr (BX(IL_TEMPC(ilp)), BR(IL_TEMPR(ilp)), IL_NP(ilp))
	        call adivkr (BR(IL_TEMPR(ilp)), real(IL_NP(ilp)), 
	                     BR(IL_TEMPR(ilp)), IL_NP(ilp))
	        # Apply weights.
	        do j = 2, IL_YSIZEOUT(ilp)-1 {
	            do i = 2, IL_XSIZEOUT(ilp)-1 {
	                AR(IL_TEMPR(ilp),i,j) = AR(IL_TEMPR(ilp),i,j) * 
	                                        AR(IL_SADAPT(ilp),i,j)
	            }
	        }
	        # Fourier transform again...
	        call achtrx (BR(IL_TEMPR(ilp)), BX(IL_TEMPC(ilp)), IL_NP(ilp))
	        isign = -1
	        call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform,
	                    Memx[IL_WK(ilp)])
	        call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp), 
	                                          IL_YSIZEOUT(ilp))
	    }

	    # Multiply by regularization operator complex conjugate.
	    do j = 1, IL_YSIZEOUT(ilp) {
	        do i = 1, IL_XSIZEOUT(ilp) {
	            AX(IL_TEMPC(ilp),i,j) = AX(IL_TEMPC(ilp),i,j) * 
	                                   conjg(AX(IL_REGUL(ilp),i,j))
	        }
	    }

	    # Multiply by regularization scalar parameter.
	    call amulkx (BX(IL_TEMPC(ilp)), complex(IL_ALPHA(ilp),0.), 
	                 BX(IL_TEMPC(ilp)), IL_NP(ilp))

	    # Subtract from reblured VanCittert term.
	    call asubx (BX(accum), BX(IL_TEMPC(ilp)), BX(accum), IL_NP(ilp))
	}

	# Accumulator now contains the gradient FT. 
	# Inverse-FT it and compute its squared norm.
	isign = 1
	call amovx (BX(accum), BX(IL_TEMPC(ilp)), IL_NP(ilp))
	call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform, 
	            Memx[IL_WK(ilp)])
	call achtxr (BX(IL_TEMPC(ilp)), BR(gradient), IL_NP(ilp))
	call adivkr (BR(gradient), real(IL_NP(ilp)), BR(gradient), IL_NP(ilp))
	norm = 0.0
	do j = 1, IL_YSIZEOUT(ilp) {
	    do i = 1, IL_XSIZEOUT(ilp) {
	        norm = norm + AR(gradient,i,j) * AR(gradient,i,j)
	    }
	}

	# Conjugacy factor.
	if (IL_ITER(ilp) == 1)
	    gamma = 0.0
	else
	    gamma = real(norm / IL_NORM(ilp))

	# Avoid divergency when in no-auto mode.
	if (!IL_AUTO(ilp)) {
	    if (gamma > 1.0)
	        gamma = 0.9
	}

	if (IL_VERB(ilp) >= 2) {
	    call printf ("done.  Conjugacy factor =%6.4g\n")
	        call pargr (gamma)
	        call flush (STDOUT)
	}

	# Store norm for next iteration.
	IL_NORM(ilp) = norm

	# Compute new direction.
	call amulkr (BR(IL_CD(ilp)), gamma, BR(IL_TEMPR(ilp)), IL_NP(ilp))
	call aaddr (BR(gradient), BR(IL_TEMPR(ilp)), BR(IL_CD(ilp)), IL_NP(ilp))

	# Compute step along new direction (line minimization).
	if (IL_AUTO(ilp))
	    call il_minimize (ilp)

	# Update estimate.
	call amulkr (BR(IL_CD(ilp)), IL_BETA(ilp), BR(IL_TEMPR(ilp)), 
	             IL_NP(ilp))
	call aaddr (BR(IL_OBJ(ilp)), BR(IL_TEMPR(ilp)), BR(IL_OBJ(ilp)), 
	            IL_NP(ilp))

	# Apply positivity constraint.
	if (IL_POSITIVITY(ilp))
	    call arltr (BR(IL_OBJ(ilp)), IL_NP(ilp), 0., 0.)

	# Free work areas.
	call mfree (gradient, TY_REAL)
	call mfree (accum,    TY_COMPLEX)
end




# IL_MINIMIZE  --  Minimize function by golden search. Routine computes
#                  optimum `beta' (IL_BETA(ilp)) for a previously computed
#                  conjugate direction. Starting guess for `beta' comes 
#                  from former iteration.

procedure il_minimize (ilp)

pointer	ilp			# pointer to data storage structure

real	ax, bx, cx
real	fa, fb, fc, fu, f0, f1, f2, f3
real	dum, r, q, u
real	x0, x1, x2, x3
real	gold, glimit, ulim
real	tiny, tol, gr, gc

real	il_objective()

begin
	gold   = 1.618034
	glimit = 100.
	tiny   = 1.E-20
	gr     = 0.61803399
	gc     = 0.38196602
	tol    = 0.1

	# Bracket the minimum.
	if (IL_VERB(ilp) >= 2) {
	    call printf ("Bracketing minimum: \n")
	    call flush (STDOUT)
	}
	ax = IL_BETA(ilp) * 0.8
	bx = IL_BETA(ilp) * 1.3
	fa = il_objective (ax, ilp)
	fb = il_objective (bx, ilp)
	if (fb > fa) {
	    dum = ax
	    ax  = bx
	    bx  = dum
	    dum = fb
	    fb  = fa
	    fa  = dum
	}
	cx = bx + gold * (bx - ax)
	fc = il_objective (cx, ilp)
10
	if (IL_VERB(ilp) >= 2) {
	    call printf ("%6.3g %6.3g %6.3g\n")
	        call pargr (ax)
	        call pargr (bx)
	        call pargr (cx)
	    call flush (STDOUT)
	}
	if (fb > fc) {
	    r = (bx - ax) * (fb - fc)
	    q = (bx - cx) * (fb - fa)
	    u = bx - ((bx - cx) * q - (bx - ax) * r) /
	        (2. * sign(max(abs(q - r), tiny), q - r))
	    ulim = bx + glimit * (cx - bx)
	    if ((bx - u) * (u - cx) > 0.) {
	        fu = il_objective (u, ilp)
	        if (fu < fc) {
	            ax = bx
	            bx = u
	            goto 10
	        } else if (fu > fb) {
	            cx = u
	            goto 10
	        }
	        u = cx + gold * (cx - bx)
	        fu = il_objective (u, ilp)
	    } else if ((cx - u) * (u - ulim) > 0.) {
	        fu = il_objective(u, ilp)
	        if (fu < fc) {
	            bx = cx
	            cx = u
	            u  = cx + gold * (cx - bx)
	            fb = fc
	            fc = fu
	            fu = il_objective (u, ilp)
	       }
	    } else if ((u - ulim) * (ulim - cx) > 0.) {
	        u = ulim
	        fu = il_objective (u, ilp)
	    } else {
	        u = cx + gold * (cx - bx)
	        fu = il_objective (u, ilp)
	    }
	    ax = bx
	    bx = cx
	    cx = u
	    fa = fb
	    fb = fc
	    fc = fu
	    goto 10
	}

	if (IL_VERB(ilp) >= 2) {
	    call printf ("Found ! Begin golden search...\n")
	    call flush (STDOUT)
	}

	# Golden search.
	x0 = ax
	x3 = cx
	if (abs(cx - bx) > abs(bx - ax)) {
	    x1 = bx
	    x2 = bx + gc * (cx - bx)
	} else {
	    x2 = bx
	    x1 = bx - gc * (bx - ax)
	}
	f1 = il_objective (x1, ilp)
	f2 = il_objective (x2, ilp)
	while (abs(x3 - x0) > tol * (abs(x1) + abs(x2))) {

	    if (IL_VERB(ilp) >= 2) {
	        call printf ("Beta =%6.3g,  F = %g     Beta =%6.3g,  F = %g\n")
	            call pargr (x1)
	            call pargr (f1 / real(IL_NP(ilp)))
	            call pargr (x2)
	            call pargr (f2 / real(IL_NP(ilp)))
	        call flush (STDOUT)
	    }

	    if (f2 < f1) {
	        x0 = x1
	        x1 = x2
	        x2 = gr * x1 + gc * x3
	        f0 = f1
	        f1 = f2
	        f2 = il_objective (x2, ilp)
	    } else {
	        x3 = x2
	        x2 = x1
	        x1 = gr * x2 + gc * x0
	        f3 = f2
	        f2 = f1
	        f1 = il_objective (x1, ilp)
	    }
	}
	if (f1 < f2)
	    IL_BETA(ilp) = x1
	else
	    IL_BETA(ilp) = x2

	if (IL_VERB(ilp) >= 2) {
	    call printf ("End search.   Adopted beta = %7.3g     ")
	        call pargr (IL_BETA(ilp))
	    call flush (STDOUT)
	}
end




# IL_OBJECTIVE  --  Compute objective function value for a given beta. 
#                   Chi-square is computed as bonus, but is NOT used
#                   at any step in the algorithm.

real procedure il_objective (beta, ilp)

real	beta			# gain
pointer	ilp			# pointer to data storage structure

int	nn[2],ndim,iform,isign	# FFT routine control parameters
int	i,j, k, kp, km
real	resid, noise, weight, sum_weight
double	sum_norm, sum_chi

real	il_nmodel()

begin
	ndim  = 2
	iform = 1
	nn[1] = IL_XSIZEOUT(ilp)
	nn[2] = IL_YSIZEOUT(ilp)

	# Compute objective function's argument. This is the estimate at a
	# point along `direction', with step `beta'.
	call amulkr (BR(IL_CD(ilp)), beta, BR(IL_TEMPR(ilp)), IL_NP(ilp)) 
	call aaddr (BR(IL_OBJ(ilp)), BR(IL_TEMPR(ilp)), BR(IL_TEMPR(ilp)), 
	            IL_NP(ilp))
	if (IL_POSITIVITY(ilp))
	    call arltr (BR(IL_TEMPR(ilp)), IL_NP(ilp), 0., 0.)

	# Now its FT.
	call achtrx (BR(IL_TEMPR(ilp)), BX(IL_FOBJ(ilp)), IL_NP(ilp))
	isign = -1
	call fourt (BX(IL_FOBJ(ilp)), nn, ndim, isign, iform, Memx[IL_WK(ilp)])

	# Residual norm and chi-squared computation. 
	# Loop thru input, psf and mask images.
	sum_norm   = 0.D0
	sum_chi    = 0.D0
	sum_weight = 0.0
	do k = 1, IL_NIMAGE(ilp) {

	    kp = k
	    if (IL_NPSF(ilp) == 1)
	        kp = 1
	    km = k
	    if (IL_NMASK(ilp) == 1)
	        km = 1

	    # Convolve argument with PSF and subtract from input image.
	    call amulx (BX(IL_FOBJ(ilp)), BX(MB(IL_CPSF(ilp),kp)), 
	                BX(IL_TEMPC(ilp)), IL_NP(ilp)) 
	    call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp), 
	                                      IL_YSIZEOUT(ilp))
	    call asubx (BX(MB(IL_CIMAGE(ilp),k)), BX(IL_TEMPC(ilp)), 
	                BX(IL_TEMPC(ilp)), IL_NP(ilp))

	    # Inverse transform back to image space.
	    isign = 1
	    call fourt  (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform,
	                 Memx[IL_WK(ilp)])
	    call achtxr (BX(IL_TEMPC(ilp)), BR(IL_TEMPR(ilp)), IL_NP(ilp))
	    call adivkr (BR(IL_TEMPR(ilp)), real(IL_NP(ilp)), 
	                 BR(IL_TEMPR(ilp)), IL_NP(ilp))

	    # Update weighted squared norm and chi-square.
	    do j = 1, IL_YSIZEOUT(ilp) {
	        do i = 1, IL_XSIZEOUT(ilp) {
	            resid = AR(IL_TEMPR(ilp),i,j)
	            if (IL_USEMASK(ilp))
	                weight = AR(MB(IL_WMASK(ilp),km),i,j)
	            else
	                weight = 1.
	            sum_norm = sum_norm + resid**2 * weight
	            if (IL_CCDGAIN(ilp) > 0.)
	                resid = resid * IL_CCDGAIN(ilp)
	            noise = il_nmodel (ilp, AR(IL_OBJ(ilp),i,j))
	            if (noise == 0.)
	                noise = 1.
	            sum_chi = sum_chi + resid**2 / noise * weight
	            sum_weight = sum_weight + weight
	        }
	    }
	}
	sum_norm = sum_norm / IL_NIMAGE(ilp)
	IL_CHISQ(ilp) = real(sum_chi / sum_weight / IL_NIMAGE(ilp))

	# Now add regularization term's contribution to norm.
	if (IL_ALPHA(ilp) > 0.) {

	    # Multiply by regularization operator.
	    call amulx (BX(IL_FOBJ(ilp)), BX(IL_REGUL(ilp)), 
	                BX(IL_TEMPC(ilp)), IL_NP(ilp))

	    # Inverse transform back to image space.
	    call il_shift (BX(IL_TEMPC(ilp)), IL_XSIZEOUT(ilp), 
	                                      IL_YSIZEOUT(ilp))
	    isign = 1
	    call fourt (BX(IL_TEMPC(ilp)), nn, ndim, isign, iform, 
	                Memx[IL_WK(ilp)])
	    call achtxr (BX(IL_TEMPC(ilp)), BR(IL_TEMPR(ilp)), IL_NP(ilp))
	    call adivkr (BR(IL_TEMPR(ilp)), real(IL_NP(ilp)), 
	                 BR(IL_TEMPR(ilp)), IL_NP(ilp))

	    # Update weighted squared norm.
	    do j = 2, IL_YSIZEOUT(ilp)-1 {
	        do i = 2, IL_XSIZEOUT(ilp)-1 {
	            if (IL_ADAP(ilp) != SA_NONE)
	                weight = AR(IL_SADAPT(ilp),i,j)
	            else
	                weight = 1.
	            sum_norm = sum_norm + AR(IL_TEMPR(ilp),i,j) * 
	                                  AR(IL_TEMPR(ilp),i,j) * 
	                                  weight * IL_ALPHA(ilp)
	        }
	    }
	}

	return (real(sum_norm))
end




# IL_WEIGHT  --  Compute regularization weight matrix for spatial adaptivity. 
#                Weights based on "eye model", noise model includes Poisson
#                and signal-independent components.

procedure il_weight (ilp, image)

pointer	ilp			# pointer to data storage structure
pointer	image			# input image buffer pointer


int	i,j
real	noise

real	il_nmodel()

begin
	# Compute variance.
	call il_variance (ilp, image)

	# Compute regularization weights.
	do j = 2, IL_YSIZEOUT(ilp)-1 {
	    do i = 2, IL_XSIZEOUT(ilp)-1 {
	        noise = il_nmodel (ilp, AR(image,i,j))
	        AR(IL_SADAPT(ilp),i,j) = IL_ATUNE(ilp) * 
	                                 max (0.,AR(IL_SADAPT(ilp),i,j)-noise)
	        AR(IL_SADAPT(ilp),i,j) = 1. / (1. + 
	                                 AR(IL_SADAPT(ilp),i,j))
	    }
	}
end




# IL_VARIANCE  --  Compute variance in square window about each image pixel.
#                  Window size is defined by compilation constant VAR_SIZE.
#                  Pixels at the image edges are set to zero variance.
#                  If routine is called prior to first iteration is complete, 
#                  and pixel masking is active, masking will be performed based 
#                  on first input image's mask.

procedure il_variance (ilp, image)

pointer	ilp			# pointer to data storage structure
pointer	image			# input data array

int	i, j, ii, jj
int	w2
real	nn, pixel
double	sum, sum2

begin
	w2 = VAR_SIZE / 2                # assume odd-sized window !
	do j = w2+1, IL_YSIZEOUT(ilp)-w2 {
	    do i = w2+1, IL_XSIZEOUT(ilp)-w2 {
	        sum  = double(0.)
	        sum2 = double(0.)
	        nn   = 0.
	        do ii = i-w2, i+w2 {
	            do jj = j-w2, j+w2 {
	                if (!IL_USEMASK(ilp)   || 
	                    (IL_ITER(ilp) > 2) ||
	                    (IL_USEMASK(ilp)   && 
	                    (AR(MB(IL_WMASK(ilp),1),ii,jj) > 0.))) {
	                    pixel = AR(image,ii,jj)
	                    sum  = sum  + double(pixel)
	                    sum2 = sum2 + double(pixel)**2
	                    nn   = nn   + 1.
	                }
	            }
	        }
	        if (nn > 1.) 
	            AR(IL_SADAPT(ilp),i,j) = (sum2 - sum*sum/nn) / (nn-1.)
	        else
	            AR(IL_SADAPT(ilp),i,j) = 0.
	    }
	}

	# Edges.
	do i = 1, w2 {
	    do j = 1, IL_YSIZEOUT(ilp) { 
	        AR(IL_SADAPT(ilp),i,j) = 0.
	        AR(IL_SADAPT(ilp),IL_XSIZEOUT(ilp)-i+1,j) = 0.
	    }
	}
	do j = 1, w2 {
	    do i = 1, IL_XSIZEOUT(ilp) { 
	        AR(IL_SADAPT(ilp),i,j) = 0.
	        AR(IL_SADAPT(ilp),i,IL_YSIZEOUT(ilp)-1+j) = 0.
	    }
	}
end




# IL_IMREAD  --  Read one input image and create its Fourier transform.
#
# Routine returns pointer to FT complex array; this has to be freed by
# caller after use. Routine also returns input image size, as well as
# IMIO pointer to it. This pointer has to be unmap-ed by caller. 
# If output image size is larger than image size, the pixel array will
# be copied to the center of the larger array before FTing. If a zero-sized
# output array is specified, routine sets it to input image size.
#
# On first call, this routine also creates the work space needed by 
# FFT FOURT routine.

pointer procedure il_imread (ilp, input, imin, sizex, sizey)

pointer	ilp			# pointer to data storage structure
char	input[ARB]		# input image name
pointer	imin			# IMIO pointer
int	sizex,sizey		# original size of input image.

pointer	ima, cima,  temp
int	nn[2], ndim, iform, isign

pointer	immap(),imgs2r()

begin
	# Open input image and get its size.
	imin = immap (input, READ_ONLY, 0)
	if (IM_NDIM(imin) != 2)
	    call error (0, "Input image section is not 2-dimensional.")
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)

	# If size not even, discard one column/line.
	if (mod (sizex,2) != 0)
	    sizex = sizex - 1
	if (mod (sizey,2) != 0)
	    sizey = sizey - 1

	# If not defined, set size of output array accordingly.
	if (IL_XSIZEOUT(ilp) == 0)
	    IL_XSIZEOUT(ilp) = sizex
	if (IL_YSIZEOUT(ilp) == 0)
	    IL_YSIZEOUT(ilp) = sizey

	# Total # of pixels.
	IL_NP(ilp) = IL_XSIZEOUT(ilp) * IL_YSIZEOUT(ilp)

	# Read input image and centralize it in larger array.
	ima = imgs2r (imin, 1, sizex, 1, sizey)
	call calloc (temp, IL_NP(ilp), TY_REAL)
	call il_center (ilp, ima, temp, sizex, sizey)

	# Copy input image to complex array and FT it.
	call malloc (cima, IL_NP(ilp), TY_COMPLEX)
	call achtrx (BR(temp), BX(cima), IL_NP(ilp))
	call mfree (temp, TY_REAL)
	# If not yet created, prepare work space needed by FFT routine.
	if (IL_WK(ilp) == NULL)
	    call malloc (IL_WK(ilp), 2*max(IL_XSIZEOUT(ilp), IL_YSIZEOUT(ilp)), 
	                 TY_COMPLEX)
	ndim  = 2
	iform = 1
	nn[1] = IL_XSIZEOUT(ilp)
	nn[2] = IL_YSIZEOUT(ilp)
	isign = -1
	call fourt  (BX(cima), nn, ndim, isign, iform, Memx[IL_WK(ilp)])

	return (cima)
end




# IL_READP  --  Read one psf image and create its Fourier transform.
#
# Routine returns pointer to FT complex array; this has to be freed by
# caller after use. Psf is centered before FTing, using either provided
# position or maximum in image. Psf is masked and normalized to unit
# integral, FT-ed, and optionally prunned from noise. 

pointer procedure il_readp (ilp, input, px0,py0, mask, nlpsf)

pointer	ilp			# pointer to data storage structure
char	input[ARB]		# input image name
real	px0, py0		# psf position in psf image
real	mask			# star mask
bool	nlpsf			# noise-less psf ?

pointer	impsf, psf, cpsf, temp
int	nn[2], ndim, iform, isign
int	iszpsf, jszpsf			# psf image size
int	i,j,i1,j1,i2,j2
int	isz1x,isz1y,isz2x,isz2y
long	jj, jj1
real	aux1, aux2

pointer	immap(),imgs2r()
real	il_noise(),asumr()

begin
	isz2x = IL_XSIZEOUT(ilp) / 2
	isz2y = IL_YSIZEOUT(ilp) / 2
	isz1x = IL_XSIZEOUT(ilp) - 1
	isz1y = IL_YSIZEOUT(ilp) - 1

	# Open input psf and read it.
	impsf = immap (input, READ_ONLY, 0)
	if (IM_NDIM(impsf) != 2)
	    call error (0, "Input psf section is not 2-dimensional.")
	iszpsf = IM_LEN(impsf,1)
	jszpsf = IM_LEN(impsf,2)
	psf = imgs2r (impsf, 1, iszpsf, 1, jszpsf)

	# If not supplied by user, find psf center as
	# maximum in psf image.
	if ((px0 == INDEF) || (py0 == INDEF)) {
	    aux1 = -1. / 1.E-10
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
	call calloc (temp, IL_NP(ilp), TY_REAL)
	i2 = isz2x - int(px0) + 1
	j2 = isz2y - int(py0) + 1
	do j = 0, jszpsf - 1 {
	    j1 = j2 + j
	    if ((j1 >= 0) && (j1 <= isz1y)) {
	        jj  = psf  + long(j)  * iszpsf
	        jj1 = temp + long(j1) * IL_XSIZEOUT(ilp)
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
	if (mask != INDEF)
	    call il_mask (BR(temp), IL_XSIZEOUT(ilp), IL_YSIZEOUT(ilp), mask)
	else 
	    call il_mask (BR(temp), IL_XSIZEOUT(ilp), IL_YSIZEOUT(ilp),
	                 real(min(iszpsf,jszpsf)))

	# Normalize psf to unit integral flux.
	aux1 = asumr (BR(temp), IL_NP(ilp))
	if (aux1 != 0.)
	    call adivkr (BR(temp), aux1, BR(temp), IL_NP(ilp))
	else
	    call error (0, "Input psf file has no valid image.")

	# Alloc complex array and copy centralized, masked and 
	# normalized psf to it.
	call calloc (cpsf, IL_NP(ilp), TY_COMPLEX)
	call achtrx (BR(temp), BX(cpsf), IL_NP(ilp))
	call mfree (temp, TY_REAL)

	# Fourier transform psf. No need to prune, if it is noiseless.
	ndim  = 2
	iform = 1
	nn[1] = IL_XSIZEOUT(ilp)
	nn[2] = IL_YSIZEOUT(ilp)
	isign = -1
	call fourt (BX(cpsf), nn, ndim, isign, iform, Memx[IL_WK(ilp)])
	if (!nlpsf) {
	    aux1 = il_noise (BX(cpsf), IL_XSIZEOUT(ilp), IL_YSIZEOUT(ilp),
	                    INDEFR, aux2)
	    call il_prune (BX(cpsf), IL_XSIZEOUT(ilp), IL_YSIZEOUT(ilp), aux1)
	}

	return (cpsf)
end




# IL_NMODEL  --  Compute noise variance from Poisson + readout noise model.
#
# CCD gain is used to tell if Poisson noise is present. When set to zero,
# readout-only noise model is used, and must be defined in DN instead of
# electrons.

real procedure il_nmodel (ilp, data)

pointer	ilp
real	data

real	noise

begin
	# Readout (signal-independent) noise.
	noise = IL_RNOISE(ilp) ** 2

	# Poisson noise contribution.
	noise = noise + max(0., data * IL_CCDGAIN(ilp))

	return (noise)
end




# IL_CENTER  --  Centralize smaller image in larger array.
#
# This procedure assumes that all dimensions are even.

procedure il_center (ilp, in, out, sizex, sizey)

pointer	ilp		# pointer to data storage structure
pointer	in		# input (smaller) array
pointer	out		# output (larger) array
int	sizex		# input array X size
int	sizey		# input array Y size

int	line
long	offx
long	p_in, p_out

begin
	if ((sizex > IL_XSIZEOUT(ilp)) || (sizey > IL_YSIZEOUT(ilp)))
	    call error (0,"Requested output size too small.")

	offx  = (IL_XSIZEOUT(ilp) - sizex) / 2
	p_out = (IL_YSIZEOUT(ilp) - sizey) / 2 * IL_XSIZEOUT(ilp) + offx
	p_in  = 0
	line  = 1

	while (p_in < (sizex*sizey)) {
	    Memr[out+p_out] = Memr[in+p_in]
	    p_in  = p_in  + 1
	    p_out = p_out + 1
	    if (p_in >= (sizex*line)) {
	        p_out = p_out + 2 * offx
	        line  = line + 1
	    }
	}
end




# IL_ZEDGE  --  Fill mask edge with zeros. This routine only called when
#               no external masks are beign read, but edge extension is
#               selected. Only mask associated with first input image is
#               processed. Procedure assumes that all dimensions are even.

procedure il_zedge (ilp, sizex, sizey)

pointer	ilp		# pointer to data storage structure
int	sizex		# input array X size
int	sizey		# input array Y size

int	i,j
int	edgex, edgey

begin
	edgex = (IL_XSIZEOUT(ilp) - sizex) / 2
	edgey = (IL_YSIZEOUT(ilp) - sizey) / 2
	do j = 1, IL_YSIZEOUT(ilp) {
	    do i = 1, edgex {
	        AR(MB(IL_WMASK(ilp),1),i,j)            = 0.
	        AR(MB(IL_WMASK(ilp),1),IL_XSIZEOUT(ilp)-i+1,j) = 0.
	    }
	}
	do j = 1, edgey {
	    do i = 1, IL_XSIZEOUT(ilp) {
	        AR(MB(IL_WMASK(ilp),1),i,j)            = 0.
	        AR(MB(IL_WMASK(ilp),1),i,IL_YSIZEOUT(ilp)-j+1) = 0.
	    }
	}
end




# IL_DECENTER  --  De-center image before output.

procedure il_decenter (ilp, sizex, sizey)

pointer	ilp		# pointer to data storage structure
int	sizex, sizey

int	i,j, i1,j1

begin
	i1 = (IL_XSIZEOUT(ilp) - sizex) / 2
	j1 = (IL_YSIZEOUT(ilp) - sizey) / 2
	do j = j1+1, IL_YSIZEOUT(ilp) 
	    call amovr (AR(IL_OBJ(ilp),1,j), AR(IL_TEMPR(ilp),1,j-j1), 
	                IL_XSIZEOUT(ilp))
	do j = 1, j1 {
	    do i = 1, IL_XSIZEOUT(ilp) {
	        AR(IL_TEMPR(ilp),i,sizey+j1+j) = AR(IL_OBJ(ilp),i,j)
	    }
	}
	do j = 1, IL_YSIZEOUT(ilp) {
	    do i = i1+1, IL_XSIZEOUT(ilp)
	        AR(IL_OBJ(ilp),i-i1,j) = AR(IL_TEMPR(ilp),i,j)
	    do i = 1, i1
	        AR(IL_OBJ(ilp),sizex+i1+i,j) = AR(IL_TEMPR(ilp),i,j)
	}
end




# IL_SHIFT  --  To be used before transforming FT back to data space, to shift
#               image to center of field. Works for arrays with even dimensions.

procedure il_shift (array, sizex, sizey)

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



# IL_NOISE  --  Compute noise power by sampling FT array at specific
# "distance" frequency and "directionally" averaging the power there. 
# If freq = INDEF, the largest available frequency will be used.
# Returned value is directly in units of the squared Fourier transform.
# Parameter 'noise' contains 1-sided, bias-compensated noise power.

real procedure il_noise (ft, sizex, sizey, freq, noise)

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




# IL_PRUNE  --  Prune noise plateau in FT, taking care for not distorting
# phase information. Noise level is assumed to be in FT units.

procedure il_prune (ft, sizex, sizey, noise)

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





# IL_MASK  -  Multiply real array by star mask. If mask is INDEF or zero,
# no masking takes place.

procedure il_mask (image, sizex, sizey, mask)

real	image[sizex,sizey]	# image array
int	sizex, sizey		# array size
real	mask			# mask size in pixels

int	isz2x, isz2y, i, j
real	r, aux, il_cwind()

begin
	if ((!IS_INDEFR (mask)) && (mask > 0.)) {
	    isz2x = sizex / 2
	    isz2y = sizey / 2
	    do j = 1, sizey {
	        aux = (real (isz2y-j))**2
	        do i = 1, sizex {
	            r = sqrt ((real(isz2x-i))**2 + aux) / mask
	            image[i,j]= image[i,j] * il_cwind(r)
	        }
	    }
	}
end




# IL_CWIND  --  A window for FTs that has low sidelobes.

real procedure il_cwind (relr)

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




# IL_BCLEAN  --  Clean string from leading blanks.

procedure il_bclean (str, size)

char	str[ARB]
int	size

char	tempst[SZ_FNAME]
int	i, j

begin
	j = 1
	i = 1
	while (IS_WHITE(str[i]))
	    i = i + 1
	while ((str[i]!=EOS) && (i<=size)) {
	    tempst[j] = str[i]
	    i = i + 1
	    j = j + 1
	}
	tempst[j] = EOS
	call strcpy (tempst, str, size)
end



