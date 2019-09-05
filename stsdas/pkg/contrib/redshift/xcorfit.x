###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure xcorfit(spectrum, tempspec, peak)
#		real	spectrum[ARB], tempspec[ARB]
#		real	peak

#  Description:	XCORFIT is the heart of the XCOR task.  For the given
#		spectrum, cross correlations with each template are
#		performed and redshifts and dispersions are calculated.

#  Arguments:	real	spectrum[ARB]	Object spectrum
#		real	tempspec[ARB]	Array of pointers to template spectra

#  Returns:	real	peak		Height of cross correlation peak

#  Notes:	Much data passed in common through "fquot.com"

#  History:	June	1987	Gerard Kriss
#		June 2, 1989	Lucy Willard
#			Comment out unused variable declarations
#			nparam, kmin, t2, and chiper

###########################################################################

include	"fquot.h"

procedure xcorfit(spectrum, tempspec, peak)

real	spectrum[ARB], tempspec[ARB]
real	peak

int	i, j, lup, nhalf
int	ishift
int	dbin
#int	nparam
int	newpts, log2
int	iwk[20]
int	numpts, ncoef
#real	kmin
real	specavg, tempavg, cxnorm
#real	t1, t2
real	t1
real	x1, x2
real	err, width, w, delta, b
real	xb[25], h[25], co[3]
real	pow[MAXPTS]
real	xcor[MAXPTS]
complex	specft[MAXPTS], tempft[MAXPTS], cxcor[MAXPTS]
complex	gamn

complex	conjg(), cmplx()
real	real()
#real	chiper(), alog()
real	alog()

include	"fquot.com"

begin

# Zero the FFT arrays

	call aclrx(specft,MAXPTS2)
	call aclrx(tempft,MAXPTS2)

# If npts not a power of two, determine next largest power of two:

	newpts = npts
	if ( npts != 256 && npts != 512 && npts != 1024 && npts != 2048 &&
	     npts != 4096 && npts != 8192 )
	{
		newpts = alog(real(npts)) / LN2 +  1.
		newpts = 2**newpts
	}

# Set initial values for parameters

	nhalf = newpts / 2

# Window the object spectrum and get its transform
#call printf("Windowing and FFTing the object\n")

	call windat(spectrum, specft, npts, -ishift, order, han, specavg)
# If pltwin, show windowed spectrum
	if ( pltwin ) {
		x1 = 1.
		x2 = newpts
		call plotspec(newpts, specft, specname, x1, x2)
	}

# Same for template

	call windat(tempspec, tempft, npts, ishift, order, han, tempavg)

# Compute the Fourier transforms

	call fftr(specft, gamn, newpts, iwk)
	specft[nhalf+1] = gamn
	call fftr(tempft, gamn, newpts, iwk)
	tempft[nhalf+1] = gamn
	for ( i = 2; i <= nhalf; i = i + 1)
	{
		j = newpts + 2 - i
		specft[j] = conjg(specft[i])
		tempft[j] = conjg(tempft[i])
	}

# If pltfft, show fft power spectrum
	if ( pltfft ) {
		x1 = 1.
		x2 = newpts/2.
		for ( i = 1; i <= newpts/2; i = i + 1)
		{
			pow[i] = real(specft[i])**2 + aimag(specft[i])**2
		}
		call plotspec(newpts/2, pow, specname, x1, x2)
	}

# Filter the transforms to remove low and high frequency noise.

	for ( i = 1; i <= lo; i = i + 1)
	{
		specft[i] = specft[i] * (0., 0.)
		tempft[i] = tempft[i] * (0., 0.)
	}
	for ( i = 2 * nrun; i <= newpts; i = i + 1)
	{
		specft[i] = specft[i] * (0., 0.)
		tempft[i] = tempft[i] * (0., 0.)
	}
	for ( i = lo + 1; i <= 2 * lo; i = i + 1)
	{
		t1 = (i - lo) / lo
		specft[i] = specft[i] * cmplx(t1, 0.)
		tempft[i] = tempft[i] * cmplx(t1, 0.)
	}
	for ( i = nrun; i <= 2 * nrun - 1; i = i + 1)
	{
		t1 = (2 * nrun - i) / nrun
		specft[i] = specft[i] * cmplx(t1, 0.)
		tempft[i] = tempft[i] * cmplx(t1, 0.)
	}

# Compute the transform of the cross-correlation function and transform it back.
# At the same time, find the normalization.
#call printf("Computing the cross-correlation.\n")

	specavg = 0.
	tempavg = 0.
	for ( i = 1; i <= newpts; i = i + 1)
	{
		specavg = specavg + real(specft[i])**2 + aimag(specft[i])**2
		tempavg = tempavg + real(tempft[i])**2 + aimag(tempft[i])**2
		cxcor[i] = conjg(specft[i]) * tempft[i]
	}
	specavg = sqrt(specavg)
	tempavg = sqrt(tempavg)
	cxnorm = specavg * tempavg

# Transform the cross-correlation function back to real space.
# Note that fft2 leaves the coefficients in reverse order. ffrdr2 reorders them.
# Note also that cxcor on return contains only half of the coefficients.  The
# others are obtained by symmetry.

	log2 = alog(real(newpts)) / LN2 + 0.001
	call fft2(cxcor, log2, iwk)
	call ffrdr2(cxcor, log2, iwk)
	for ( i = 1; i <= nhalf; i = i + 1)
	{
		xcor[nhalf + i] = real(cxcor[i]) / cxnorm
		xcor[i] = real(cxcor[nhalf + i]) / cxnorm
	}

# Show the cross-correlation function if requested

	x1 = -nhalf
	x2 = nhalf - 1
# Find the peak in the cross-correlation function.
# The peak found here can be altered in plotspec with the 'p' keystroke.

	peak = 0.
	for ( i = 1; i <= newpts; i = i + 1)
	{
		if ( xcor[i] >= peak )
		{
			peak = xcor[i]
			z[1] = i - nhalf - 1
		}
	}

	if ( pltfit ) {
		call plotspec(newpts, xcor, specname, x1, x2)
	}
	dbin = z[1]

# Fit a parabola to the peak
	for ( i = 1; i <= 11; i = i + 1)
	{
	b = (sig1 + 2.* sig2 * w + sig3 * w**2) * ze[1] / 2.3548 / deltav
		xb[i] = -6 + i
		h[i] = xcor[dbin + nhalf - 5 + i]
	}
	numpts = 11
	ncoef = 3
	call earlin(xb,h,numpts,co,-ncoef)
	z[1] = -co[2] /  (2. * co[3])
	peak = co[1] - co[2]**2 / (4. * co[3])
	width = sqrt( - peak / (2. * co[3]) )
	z[1] = z[1] + dbin
	w = deltav * width / 2.3548
	z[2] = (sig0 + sig1 * w + sig2 * w**2 + sig3 * w**3) / deltav
	if ( z[2] <= 0. )
		z[2] = 0.01

# Get the error in the redshift estimate.

	err = 0.
	lup = nhalf - abs(dbin) - 1
	for ( i = 0; i <= lup; i = i + 1)
	{
		delta = xcor[nhalf + dbin + i] - xcor[nhalf + dbin - i]
		err = err + delta**2
	}
	err = err / newpts / 2.
	ze[1] = 6.2832 * width / 8. / (1. + peak/(1.414*sqrt(err)))

# Get the error in the velocity dispersion

	b = (sig1 + 2.* sig2 * w + 3. * sig3 * w**2) * ze[1] / 2.3548
	b = sqrt( sqrt(1. + b**2 / z[2]**2) - 1.)
	ze[2] = 1.4142135 * z[2] * b

# Record intermediate results in the log files

	if ( debug ) {
	    for ( i = 1; i <= nlogfd; i = i + 1)
	    {
		call fprintf(logfd[i],"Maximum of cross-correlation is in bin %d.\n")
			call pargi(dbin)
		call fprintf(logfd[i],"Parabolic fit to peak gives: %e %e %e\n")
			call pargr(co[1])
			call pargr(co[2])
			call pargr(co[3])
		call fprintf(logfd[i], " Variance of cross-correlation is %e\n")
			call pargr(err)
	    }
	}
end
