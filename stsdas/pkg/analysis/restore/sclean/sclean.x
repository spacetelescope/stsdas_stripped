include	<imhdr.h>

define	LARGE	1.e10
define	H_LEN	5				# history vector length
define	AR	Memr[$1+($3-1)*isize+$2-1]	# real data array element
define	AX	Memx[$1+($3-1)*isize+$2-1]	# complex data array element
define	ARP	Memr[$1+($3-1)*iszpsf+$2-1]	# real psf array element


# SCLEAN  --   Keel's sigma-CLEAN deconvolution of 2-D images.
#
#      From a FORTRAN code in Keel, W.C., 1991, PASP 103,723
#
#      Features added in the SPP translation:
#      - Optional convolution with restored beam, either direct or
#        in Fourier space.
#      - Optional addition of residuals to CLEANed map.
#      - Modified convergence criterion.
#      - Input image list processing.
#
#
# 18 Jul 92    I. Busko   -   SPP task created.
# 23 Aug 92      "        -   More verbose option.
# 14 Sep 92      "        -   Update S/N locally.
# 22 Nov 92      "        -   Processes two peaks in each loop.
# 02 Feb 93      "        -   Still more verbose.


procedure t_sclean()

char	imlisti[SZ_LINE]		# input image list
char	imlisto[SZ_LINE]		# output image list/directory
char	inpsf[SZ_PATHNAME]		# psf image
char	resid[SZ_PATHNAME]		# residual image template
int	maxloop				# maximum number of iterations
real	cgain				# CLEAN loop gain
int	verb				# verbose options
real    noise				# readout noise 
real	adu				# gain (electrons/ADU)
real	crit				# stopping criterion
real	fwhm				# restored beam FWHM
bool	fft				# perform convolution in Fourier space ?
bool	addres				# add residual map ?

char	input[SZ_PATHNAME]		# input image name
char	output[SZ_PATHNAME]		# output image name
char	dirnamei[SZ_PATHNAME]		# directory name
char	dirnameo[SZ_PATHNAME]		# directory name
int	listi, listo, root_len
int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
int	clgeti()
real	clgetr()
bool	clgetb()

begin
	# Input parameters.
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("psf", inpsf, SZ_PATHNAME)
	call clgstr ("output", imlisto, SZ_LINE)
	call clgstr ("residual", resid, SZ_PATHNAME)
	noise     = clgetr ("noise")
	adu    = clgetr ("adu")
	cgain   = clgetr ("cgain")
	crit    = clgetr ("crit")
	fwhm    = clgetr ("fwhm")
	fft     = clgetb ("fft")
	maxloop = clgeti ("maxloop")
	addres  = clgetb ("addres")
	verb    = clgeti ("verbose")

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
	        call s_clean (input, inpsf, output, resid,  
			      noise, adu, crit, cgain, fwhm, fft,
	                      maxloop, addres, verb)
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
	        call s_clean (input, inpsf, output, resid,  
			      noise, adu, crit, cgain, fwhm, fft,
	                      maxloop, addres, verb)
	    }

	    call imtclose (listi)
	    call imtclose (listo)
	}


end


# S_CLEAN  --  Deconvolve one image by sigma-CLEAN algorithm.

procedure s_clean (input, inpsf, output, resid, noise, adu, crit,
                   cgain, fwhm, fft, maxloop, addres, verb)

char	input[ARB]			# input image
char	inpsf[ARB]			# psf image
char	output[ARB]			# output image
char	resid[ARB]			# residual image
int	maxloop				# maximum number of iterations
real	cgain				# CLEAN loop gain
real	crit				# stopping criterion
real	noise				# readout noise
real	adu				# gain in electrons/ADU
real	fwhm				# restored beam FWHM
bool	fft				# perform convolution in Fourier space ?
bool	addres				# add residual map ?
int	verb

pointer	imin, impsf, imout, imres	# IMIO pointers
pointer	obj				# input image buffer
pointer	psf				# psf bufer
pointer	res				# residual image buffer
pointer	out				# output image buffer
pointer	sigma				# sigma array
pointer	snr				# signal-to-noise array
pointer	cout				# convolved output image
pointer	wk				# complex workspace for FT routine
pointer	kernel				# direct convolution kernel
char	str[SZ_LINE]
int	isize,jsize			# output and residual image size
int	iszpsf, jszpsf			# input psf image size
int	i,j,i1,j1,i2,j2
int	iter				# iteration number
int	ipk1, jpk1			# largest peak
int	ipk2, jpk2			# second largest peak
int	ipk3, jpk3			# largest peak in updated S/N region.
int	nn[2], ndim, iform, isign	# FFT routine control parameters
int	ksize				# direct convolution kernel size
int	px0, py0			# psf peak
int	h_last				# significance history buffer pointer
long	cpu, clock, tc, te		# time variables
real	ccflux, sflux, ccmin
real	psum				# peak-normalized psf flux
real	vmax				# data value at peak S/N
real	snmax1				# largest S/N peak  
real	snmax2				# second largest S/N peak  
real	snmax3				# largest S/N peak in updated region. 
real	aux, ax, ay, sig, norm
real	history[H_LEN]			# significance history circular buffer
bool	resid_flag			# output control
bool	do_flag				# stopping control

pointer	immap(), imgs2r(), imps2r()
int	strlen()
real	asumr()
long	cputime(), clktime()

begin
	if (verb > 0) {
	    call printf ("%s -> %s      \n")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}

	do_flag    = true
	resid_flag = false
	if (strlen(resid) > 0)
	    resid_flag = true		# Enable residual image output
	ccmin      = LARGE

	# Initialize history buffer.
	h_last = 1
	do i = 1, H_LEN
	    history[i] = LARGE

	# Open input image and get its size. Read it.
	imin = immap (input, READ_ONLY, 0)
	if (IM_NDIM(imin) != 2)
	    call error (0, "Input image section is not 2-dimensional.")
	isize = IM_LEN(imin,1)
	jsize = IM_LEN(imin,2)
	obj = imgs2r (imin, 1, isize, 1, jsize)

	# Open output image with same header as input image. 
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = isize
	IM_LEN(imout,2) = jsize
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, 
	             "Sigma-CLEAN restoration of %s")
	    call pargstr (input)

	# Get output buffer and zero it.
	out = imps2r (imout, 1, IM_LEN(imout,1), 1, IM_LEN(imout,2))
	call amovkr (0., AR(out,1,1), IM_LEN(imout,1)*IM_LEN(imout,2))

	# Open input psf and get its size. Read it.
	impsf = immap (inpsf, READ_ONLY, 0)
	if (IM_NDIM(impsf) != 2)
	    call error (0, "Input psf section is not 2-dimensional.")
	iszpsf = IM_LEN(impsf,1)
	jszpsf = IM_LEN(impsf,2)
	psf = imgs2r (impsf, 1, iszpsf, 1, jszpsf)

	# Locate psf peak as maximum in psf array. 
	vmax = 0.
	do j = 1, jszpsf {
	    do i = 1, iszpsf {
	        if (ARP(psf,i,j) > vmax) {
	            vmax = ARP(psf,i,j)
	            px0  = i
	            py0  = j
	        }
	    }
	}

	# Normalize psf to unit peak and compute normalized flux.
	if (vmax != 0.)
	    call adivkr (ARP(psf,1,1), vmax, ARP(psf,1,1), iszpsf*jszpsf)
	else
	    call error (0, "Input psf file has no valid image.")
	psum = asumr (ARP(psf,1,1), iszpsf*jszpsf)

	# Set up sigma image.
	call malloc (sigma, isize*jsize, TY_REAL)
	do j = 1, jsize {
	    do i = 1, isize {
	        aux = noise*noise + AR(obj,i,j) / adu 
	        if (aux > 0. )
	            AR(sigma,i,j) = sqrt(aux)
	        else
	            AR(sigma,i,j) = 0.
	    }
	}

	# Compute S/N image.
	call malloc (snr, isize*jsize, TY_REAL)
	do j1 = 1, jsize {
	    do i1 = 1, isize {
	        if (AR(sigma,i1,j1) > 0.)
	            AR(snr,i1,j1) = AR(obj,i1,j1) / AR(sigma,i1,j1)
	        else
	            AR(snr,i1,j1) = 0.
	    }
	}

	if (verb == 2) {
	    call printf ("Iteration   Peak data     \n")
	    call flush (STDOUT)
	}

	# Timing variables.
	cpu   = cputime (0)
	clock = clktime (0)
	tc    = cputime (0)
	te    = clktime (0)

	# Main CLEANing loop.
	iter = 0
	while ((iter < maxloop) && do_flag) {

	    # Guesses total execution time.
	    if (verb > 0) {
	        if (iter == 50) {
	            tc = cputime (tc)
	            te = clktime (te)
	            call printf ("Estimated maximum %7.1f CPU seconds \
or %5.1f elapsed minutes to finish.\n")
	                call pargr (real(tc)/1000./50.*real(maxloop-50)+10.)
	                call pargr (real(te)/60./50.*real(maxloop-50)+.25)
	            call flush (STDOUT)
	        }
	    }

	    # Locate S/N two largest peaks. This is to avoid
	    # searching the full S/N map in each iteration.
	    # One CLEANing loop will process two peaks at once,
	    # speeding up the algorithm by a factor ~2.
	    call s_peak (AR(snr,1,1), isize, jsize, iszpsf, jszpsf,
	                 ipk1, jpk1, snmax1, ipk2, jpk2, snmax2)

	    # Process first largest peak.
	    iter = iter + 1
	    snmax1 = abs (snmax1)
	    snmax2 = abs (snmax2)
	    vmax   = AR(obj,ipk1,jpk1)

	    # Update CLEAN map at position of largest peak.
	    sflux  = cgain * vmax 
	    ccflux = sflux * psum
	    AR(out,ipk1,jpk1) = AR(out,ipk1,jpk1) + ccflux

	    # ipk1,jpk1,ccflux is largest peak's CLEAN component.
	    # Subtract scaled psf from residual map and update S/N array 
	    # accordingly. Also, locate largest peak among updated 
	    # S/N array elements.
	    snmax3 = 0.
	    do j1 = 1, jszpsf {
	        j2 = jpk1 - py0 + j1
 	        do i1 = 1, iszpsf {
	            i2 = ipk1 - px0 + i1
	            if ((i2 > 0) && (i2 <= isize) &&
	                (j2 > 0) && (j2 <= jsize)) {
	                AR(obj,i2,j2) = AR(obj,i2,j2) - sflux * ARP(psf,i1,j1)
	                if (AR(sigma,i2,j2) > 0.)
	                    AR(snr,i2,j2) = AR(obj,i2,j2) / AR(sigma,i2,j2)
	                else
	                    AR(snr,i2,j2) = 0.
	                if (abs(AR(snr,i2,j2)) > snmax3) {
	                    snmax3 = abs(AR(snr,i2,j2))
	                    ipk3 = i2
	                    jpk3 = j2
	                }
	            }
	        }
	    }

	    if (verb == 2) {
	        call printf ("%d          %d %d    %g  %g  %g\n")
	            call pargi (iter)
	            call pargi (ipk1)
	            call pargi (jpk1)
	            call pargr (vmax)
	            call pargr (sflux)
	            call pargr (snmax1)
	            call flush (STDOUT)
	    }

	    # Test to see if second peak was found in previous s_peak call. 

	    if ((ipk2 != 0 ) && (jpk2 != 0)) {

	        # Yes; choose next peak to process among (ipk2,jpk2) 
	        # and (ipk3,jpk3).
	        if (snmax3 > snmax2) {
	            snmax1 = snmax3
	            ipk1   = ipk3
	            jpk1   = jpk3
	        } else {
	            snmax1 = snmax2
	            ipk1   = ipk2
	            jpk1   = jpk2
	        }

	        # Now process second peak.
	        iter = iter + 1
	        vmax = AR(obj,ipk1,jpk1)

	        # Update CLEAN map at position of second peak.
	        sflux  = cgain * vmax
	        ccflux = sflux * psum
	        AR(out,ipk1,jpk1) = AR(out,ipk1,jpk1) + ccflux

	        # ipk1,jpk1,ccflux is second peak's CLEAN component.
	        # Subtract scaled psf from residual map and update S/N array 
	        # accordingly. 
	        do j1 = 1, jszpsf {
	            j2 = jpk1 - py0 + j1
 	            do i1 = 1, iszpsf {
	                i2 = ipk1 - px0 + i1
	                if ((i2 > 0) && (i2 <= isize) &&
	                    (j2 > 0) && (j2 <= jsize)) {
	                    AR(obj,i2,j2) = AR(obj,i2,j2) - 
	                                    sflux * ARP(psf,i1,j1)
	                    if (AR(sigma,i2,j2) > 0.)
	                        AR(snr,i2,j2) = AR(obj,i2,j2) / AR(sigma,i2,j2)
	                    else
	                        AR(snr,i2,j2) = 0.
	                }
	            }
	        }

	        # This works only on even iter !
	        if ((verb == 1) && (mod(iter,100) == 0)) {
	            call printf ("\rIteration %d, current S/N = %g")
	                call pargi (iter)
	                call pargr (snmax1)
	                call flush (STDOUT)
	        }

	        if (verb == 2) {
	             call printf ("%d          %d %d    %g  %g  %g\n")
	                call pargi (iter)
	                call pargi (ipk1)
	                call pargi (jpk1)
	                call pargr (vmax)
	                call pargr (sflux)
	                call pargr (snmax1)
	                call flush (STDOUT)
	        }
	    }

	    # Processing of stopping conditions.

#           THIS IS HOW THE ORIGINAL CODE WAS WRITTEN.
#           HOWEVER, DOESN'T WORK AS EXPECTED (TERMINATES TOO QUICKLY).
#
#	    # If (abs(ccflux)) is going up, terminate CLEAN.
#	    if (abs(ccflux) > (3. * ccmin))
#	         do_flag = false
#	    if (abs(ccflux) < ccmin)
#	        ccmin = abs(ccflux)
#
#
#           THIS IS THE ALTERNATE STOPPING PROCEDURE.

	    #  Update S/N history. This is actually beign done
	    # for every other peak only, but works as well.
	    history[h_last] = snmax1
	    if (h_last < H_LEN)
	        h_last = h_last + 1
	    else
	        h_last = 1

	    # Checks H_LEN last updated S/N values. 
	    # If all of then are below `crit', set stopping flag.
	    i = 0
	    do j = 1, H_LEN {
	        if (history[j] < crit) {
	            i = i + 1
	        }
	    }
	    if (i == H_LEN)
	        do_flag = false
	}

	if (verb > 0) {
	    call printf ("\nCLEAN loop ended after %d iterations; last peak \
had S/N = %g\n")
	        call pargi (iter)
	        call pargr (snmax1)
	    call flush (STDOUT)
	}

	call mfree (snr, TY_REAL)
	call mfree (sigma, TY_REAL)
	call imunmap (impsf)

	# Create optional residual image file with same header as input image.
	if (resid_flag) {
	    imres = immap (resid, NEW_COPY, imin)
	    IM_LEN(imres,1) = isize
	    IM_LEN(imres,2) = jsize
	    call sprintf (IM_TITLE(imres), SZ_IMTITLE, 
		"Sigma-CLEAN residual from %s")
	        call pargstr (inpsf)
	    res = imps2r (imres, 1, IM_LEN(imres,1), 1, IM_LEN(imres,2))
	    call amovr (AR(obj,1,1), AR(res,1,1), isize*jsize)
	    call imunmap (imres)
	}

	# Convolve CLEAN map with restored beam.
	if (fwhm > 0.) {

	    if (verb > 0) {
	        call printf ("Convolving with restored beam ...")
	        call flush (STDOUT)
	    }

	    # Fourier convolution. Uses Norman Brenner's FOURT FFT routine.
	    # Routine s_fourier actually implements a low-pass filter, which
	    # is analogous to a convolution with a flat-topped kernel.
	    if (fft) {
	        # Alloc complex workspace and set up parameters for FFT routine.
	        call malloc (cout, isize*jsize,        TY_COMPLEX)
	        call malloc (wk,   2*max(isize,jsize), TY_COMPLEX)
	        ndim  = 2
	        iform = 1
	        nn[1] = isize
	        nn[2] = jsize
	        isign = -1

	        # Copy output image to complex array and FT it.
	        call achtrx (AR(out,1,1), AX(cout,1,1), isize*jsize)
	        call fourt (AX(cout,1,1), nn, ndim, isign, iform, Memx[wk])

	        # Convolve
	        call s_fourier (AX(cout,1,1), isize, jsize, fwhm)

	        # Go back to data space.
	        isign = 1
	        call fourt (AX(cout,1,1), nn, ndim, isign, iform, Memx[wk])
	        call achtxr (AX(cout,1,1), AR(out,1,1), isize*jsize)
	        aux = 1. / (real(isize) * real(jsize))
	        call amulkr (AR(out,1,1), aux, AR(out,1,1), isize*jsize)

	        call mfree (wk,   TY_COMPLEX)
	        call mfree (cout, TY_COMPLEX)

	    # Direct convolution.
	    } else {
	        # Compute symmetric, normalized gaussian kernel
	        sig = 0.4296 * fwhm
	        ksize = 2 * ((int (6.*sig)) / 2) + 1
	        if (ksize < 3)
	            ksize = 3
	        i1 = ksize /2 + 1
	        norm = 0.0
	        call malloc (kernel, ksize*ksize, TY_REAL)
	        do j = 1, ksize {
	            ay = j - i1
	            do i = 1 , ksize {
	                ax = i - i1
	                aux = 0.5 * (ax * ax + ay * ay) / sig
	                if (aux < 1.e8) {
	                    Memr[kernel+(j-1)*ksize+i-1] = exp (-aux)
	                    norm = norm + Memr[kernel+(j-1)*ksize+i-1]
	                } else
	                    Memr[kernel+(j-1)*ksize+i-1] = 0.
	            }
	        }
	        if (norm > 0.0)
	            call adivkr (Memr[kernel], norm, Memr[kernel], ksize*ksize)

	        # Perform convolution.
	        call malloc (cout, isize*jsize, TY_REAL)
	        call s_direct (AR(out,1,1), Memr[cout], isize, jsize, 
	                       Memr[kernel], ksize)

	        call mfree (cout,   TY_REAL)
	        call mfree (kernel, TY_REAL)
	    }
	}

	# Add residual map to CLEANed, convolved image.
	if (addres) {
	    if (verb > 0) {
	        call printf ("\nAdding residual map to CLEAN map ...")
	        call flush (STDOUT)
	    }
	    call aaddr (AR(out,1,1), AR(obj,1,1), AR(out,1,1), isize*jsize)
	}

	# Append HISTORY header records to output image.
	call sprintf (str, SZ_LINE, " Sigma-CLEAN: gain = %f, niter = %d ")
	    call pargr (cgain)
	    call pargi (iter)
	call imputh (imout, "HISTORY", str)
	if (fwhm > 0.) {
	    call sprintf (str, SZ_LINE, " Sigma-CLEAN restored beam fwhm = %f ")
	        call pargr (fwhm)
	    call imputh (imout, "HISTORY", str)
	}
	if (addres) {
	    call sprintf (str, SZ_LINE, " Sigma-CLEAN residual map added. ")
	    call imputh (imout, "HISTORY", str)
	}

	call imunmap (imout)
 	call imunmap (imin)

	if (verb > 0) {
	    call printf ("\n%7.2f CPU seconds,  %5.2f elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)
	    call flush (STDOUT)
	}

	# Null residual image name string. This is to avoid re-opening the
	# same residual image on next call to s_clean.
	call strcpy ("", resid, SZ_PATHNAME)
end



# S_PEAK  --  Finds the two largest peaks (absolute value) in image
#             array. The second peak must be outside the region 
#             centered on first peak and with size given by
#             iszpsf,jszpsf. If search for second peak fails,
#             routine returns ipk2 = jpk2 = vmax2 = 0.
#
# 1-D addressing was atempted, but is avoided in this implementation
# because vector indices of type long result in slower code (SUN4 compiler).

procedure s_peak (array, isize, jsize, iszpsf, jszpsf,
	          ipk1, jpk1, vmax1, ipk2, jpk2, vmax2)

real	array[isize,jsize]		# input array
int	isize, jsize			# array size
int	iszpsf, jszpsf			# psf array size
int	ipk1, jpk1			# largest peak position
int	ipk2, jpk2			# second largest peak position
real	vmax1				# largest peak value
real	vmax2				# second largest peak value

real	avmax1, avmax2
real	datum, adatum
int	ip2, jp2			# minimum distance between peaks.
int	i, j


begin
	# Initialize variables
	ip2 = iszpsf / 2 + 2
	jp2 = jszpsf / 2 + 2
	vmax1  = array[1,1]
	avmax1 = abs(vmax1)
	vmax2  = 0.
	avmax2 = 0.
	ipk2   = 0
	jpk2   = 0

	# Main loop.
	do j = 1, jsize {
	    do i = 2, isize {

	        # Get datum from array.
	        datum  = array[i,j]
	        adatum = abs(datum)

	        # Rank it (in absolute value).
	        if (adatum < avmax2) {
	            # Datum is smaller than second peak: do nothing.

	        } else if (adatum > avmax1) {
	            # Datum is larger than first peak: move former first
	            # peak to second rank; add new datum as first peak.
	            vmax2  = vmax1
	            avmax2 = avmax1
	            ipk2   = ipk1
	            jpk2   = jpk1
	            vmax1  = datum
 	            avmax1 = adatum
	            ipk1   = i
	            jpk1   = j
	            # Invalidate second peak if distance is below minimum.
	            if ((abs(ipk2 - ipk1) < ip2) &&
	                (abs(jpk2 - jpk1) < jp2)) {
	                vmax2  = 0.
	                avmax2 = 0.
	                ipk2   = 0
	                jpk2   = 0
	            }
	        } else {
	            # Datum fits in between first and second peaks: move
	            # it in place of former second peak, only if it is 
	            # beyond minimum distance.
	            if ((abs(i - ipk1) > ip2) ||
	                (abs(j - jpk1) > jp2)) {
	                vmax2  = datum
 	                avmax2 = adatum
	                ipk2   = i
	                jpk2   = j
	            }
	        }
	    }
	}
end



# S_DIRECT  --  Convolution in data space with square kernel.

procedure s_direct (data, work, isize, jsize, kernel, ksize)

real	data[isize,jsize]	# input:  array to be convolved
				# output: convolved array
real	work[isize,jsize]	# work area
int	isize,jsize		# data and work array size
real	kernel[ksize,ksize]	# kernel 
int	ksize			# kernel size (must be odd)

int	i,j,i1,j1,ik,jk
int	k2

begin
	k2 = ksize /2 + 1

	# Scan data array. Borders are left out.
	call amovr (data, work, isize*jsize)
	do j = k2+1, jsize-k2 {
	    do i = k2+1, isize-k2 {
	        work[i,j] = 0.0
	        jk = 0
	        # Scan kernel.
	        do j1 = j-k2+1, j+k2-1 {
	            jk = jk + 1
	            ik = 0
	            do i1 = i-k2+1, i+k2-1 {
	                ik = ik + 1
	                work[i,j] = work[i,j] + data[i1,j1] * kernel[ik,jk]
	            }
	        }
	    }
	}
	call amovr (work, data, isize*jsize)
end



# S_FOURIER  --  Low-pass circular filter in Fourier space.

procedure s_fourier (image_ft, sizex, sizey, fwhm)

complex	image_ft[sizex,sizey]	# image FT
int	sizex,sizey		# image FT size
real	fwhm			# smoothing radius

int	i,j,isz2x,isz2y
real	cutoffx, cutoffy
real	ri, rj
real	d1, d2, d3, d4
real	ax, ay

real	s_wind()

begin
	if ((!IS_INDEFR(fwhm)) && (fwhm > 0.) &&
	    (fwhm <  sizex) && (fwhm < sizey)) {
	    # take care of non-square arrays
	    cutoffx = sizex / 1.15 / fwhm
	    cutoffy = sizey / 1.15 / fwhm
	    isz2x = sizex / 2
	    isz2y = sizey / 2
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
	            image_ft[i,j] = s_wind (d1) * image_ft[i,j]
	            image_ft[i,sizey+1-j] = s_wind (d2) * 
	                                    image_ft[i,sizey+1-j]
	            image_ft[sizex+1-i,j] = s_wind (d3) * 
                                            image_ft[sizex+1-i,j]
	            image_ft[sizex+1-i,sizey+1-j] = s_wind (d4) * 
	                                    image_ft[sizex+1-i,sizey+1-j]
	        }
	    }
	}
end



# S_WIND  --  A window for FTs that has low sidelobes.

real procedure s_wind (relr)

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



