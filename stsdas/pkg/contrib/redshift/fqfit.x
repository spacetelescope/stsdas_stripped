###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure fqfit(spectrum, tempspec, chisq)
#		real	spectrum[ARB], tempspec[ARB]
#		real	chisq

#  Description:	FQFIT is the heart of the FQUOT task.  It fits each template
#		spectrum in tempspec to the given galaxy spectrum using the
#		FORTRAN routines originally written by Paul Schechter.
#		

#  Arguments:	real	spectrum[ARB]	Spetrum of object to be fit
#		real	tempspec[ARB]	Pointers to template spectra

#  Returns:	real	chisq		Reduced Chi-square for the fit

#  Notes:	Data shared in common defined in "fquot.com"

#  History:	June	1987	Gerard Kriss

###########################################################################

include	"fquot.h"

procedure fqfit(spectrum, tempspec, chisq)

real	spectrum[ARB], tempspec[ARB]
real	chisq

int	i, lup, nhalf
int	ishift
int	nparam
int	newpts
int	iwk[20]
real	kmin
real	specavg, tempavg
real	t1, t2
real	x1, x2
real	x[MAXPTS2], ye[MAXPTS2]
real	pow[MAXPTS2]
complex	specft[MAXPTS2], tempft[MAXPTS2], quot[MAXPTS2]
complex	gamn

real	chiper(), alog()

include	"fquot.com"

begin

# Zero the FFT arrays

	call aclrx(specft,MAXPTS2)
	call aclrx(tempft,MAXPTS2)

# If plot enabled, show the object spectrum.
	if ( pltspec ) {
		x1 = 1.
		x2 = npts
		call plotspec(npts, spectrum, specname, x1, x2)
	}

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
	ishift = alog10(1. + z0 / C) / dlogw
	z[1] =  - ishift
	z[2] = alog ( sig0 / deltav )
	z[3] = gam0

# Window the object spectrum and get its transform
#call printf("Windowing and FFTing the object\n")

	call windat(spectrum, specft, npts, -ishift, order, han, specavg)
# If pltwin, show windowed spectrum
	if ( pltwin ) {
		x1 = 1.
		x2 = newpts
		call plotspec(newpts, specft, specname, x1, x2)
	}
	call fftr(specft, gamn, newpts, iwk)

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

# Same for template
#call printf("Windowing and FFTing the template\n")

	call windat(tempspec, tempft, npts, ishift, order, han, tempavg)
	call fftr(tempft, gamn, newpts, iwk)

# Compute preliminary error array and determine high frequency cutoff
#call printf("Computing preliminary error array.\n")
#call printf("specavg, tempavg = %e %e\n")
#	call pargr(specavg)
#	call pargr(tempavg)

	t1 = cpf * npts / specavg
	t2 = cpf * npts / tempavg
	for ( i = 1; i <= nhalf; i = i + 1)
	{
		ye[i] = t1 / (real(specft[i])**2 + aimag(specft[i])**2) +
			t2 / (real(tempft[i])**2 + aimag(tempft[i])**2)
	}
	call runsum(ye, newpts, lup, nrun, chi0)
	if ( lup < lo + 20 )
		lo = max0(1,lup - 20)

# Compute the Fourier Quotient and fit the Gaussian
#call printf("Computing the Fourier Quotient.\n")

	kmin = TWOPI / newpts
	for ( i = 1; i <= lup; i = i + 1)
	{
		x[i] = kmin * (i - 1)
		quot[i] = specft[i] / tempft[i]
		ye[i] = sqrt( ye[i] * (real(quot[i])**2 + aimag(quot[i])**2) )
	}
	nparam = 3
#call printf("Fitting the Gaussian. lo = %d, lup = %d.\n")
#    call pargi(lo)
#    call pargi(lup)
	chisq = chiper(x, quot, ye, unc, lo, lup, z, ze, nparam, niter)

# Show the fit if requested

	if ( pltfit ) {
		call plotfit(x, quot, specname, lo, lup, z)
	}

end
