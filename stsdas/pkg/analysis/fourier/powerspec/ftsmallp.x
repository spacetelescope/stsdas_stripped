include <imhdr.h>
include "../fourier.h"
include "powerspec.h"

# ft_small_p -- power spectrum for small images
# This routine gets pointers to input & output image data and calls
# a routine to do the power spectrum (or autocorrelation) in-memory.
#
# Phil Hodge,  1-Jul-1993  Subroutine created based on ft_small_f.
# Phil Hodge, 23-Jul-1993  Remove section that modified FT_CRPIX.

procedure ft_small_p (fti, fto, option, logpower, center)

pointer fti		# i: pointer to FT structure for input image
pointer fto		# i: pointer to FT structure for output image
int	option		# i: autocorrelation or power spectrum
bool	logpower	# i: take log10 of power spectrum?
bool	center		# i: center output?
#--
pointer sp
pointer ximg		# scratch for complex copy of image
real	realpt, imagpt	# real & imaginary parts of a complex number
real	xnpix		# naxis1 * naxis2, converted to real
real	log_xnpix	# log10 of xnpix
int	npix1, npix2	# lengths of first & second axes
int	i		# loop index
bool	fwd		# forward transform?
bool	decenter	# decenter the input?  (no)
errchk	salloc, ft_ix_copy

begin
	call smark (sp)

	npix1 = IM_LEN(FT_IMAGE(fti),1)
	if (IM_NDIM(FT_IMAGE(fti)) > 1)
	    npix2 = IM_LEN(FT_IMAGE(fti),2)
	else
	    npix2 = 1

	# Allocate space for a complex array to contain the entire image.
	call salloc (ximg, npix1 * npix2, TY_COMPLEX)

	# Get the input data and convert to complex.
	decenter = false
	call ft_ix_copy (fti, Memx[ximg], npix1, npix2, decenter)

	# Do the forward Fourier transform in-place in the complex array.
	fwd = true
	call ft_cmplx (Memx[ximg], npix1, npix2, fwd)

	# Compute power spectrum and normalize.
	# If we are to take the logarithm of the result, we can do it here
	# for a power spectrum but not for an autocorrelation, because in
	# the latter case we still have to take the inverse Fourier transform.
	xnpix = npix1 * npix2
	log_xnpix = log10 (xnpix)
	do i = 0, npix1*npix2-1 {
	    realpt =  real (Memx[ximg+i])
	    imagpt = aimag (Memx[ximg+i])
	    realpt = realpt**2 + imagpt**2
	    if (logpower && option == POWERSPEC) {
		if (realpt > 0.)
		    realpt = log10 (realpt) - log_xnpix
		else
		    realpt = LOG_ZERO
	    } else {
		realpt = realpt / xnpix
	    }
	    Memx[ximg+i] = realpt
	}

	if (option == AUTOCORR) {
	    # Take the inverse Fourier transform in-place in ximg.
	    fwd = false
	    call ft_cmplx (Memx[ximg], npix1, npix2, fwd)

	    # take log10
	    if (logpower) {
		do i = 0, npix1*npix2-1 {
		    realpt = real (Memx[ximg+i])
		    if (realpt > 0.)
			Memx[ximg+i] = log10 (realpt)
		    else
			Memx[ximg+i] = LOG_ZERO
		}
	    }
	}

	# Convert from complex, and write the data to output.
	call ft_xo_copy (fto, Memx[ximg], npix1, npix2, center)

	call sfree (sp)
end
