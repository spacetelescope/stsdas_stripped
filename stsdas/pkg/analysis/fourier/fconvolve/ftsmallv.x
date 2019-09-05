include <imhdr.h>
include "../fourier.h"

# ft_small_v -- convolution for small images
# This routine reads two images into memory, takes the Fourier transform
# of each, multiplies their transforms, takes the inverse Fourier transform,
# and writes the inverse transform to an output image.

procedure ft_small_v (fti1, fti2, fto, pad)

pointer fti1		# i: pointer to FT structure for input image
pointer fti2		# i: pointer to FT structure for PSF image
pointer fto		# i: pointer to FT structure for output image
bool	pad		# i: use sum of sizes of input images?
#--
pointer sp
pointer ximg1		# scratch for complex copy of image
pointer ximg2		# scratch for complex copy of PSF image
complex	xnpix		# naxis1 * naxis2, converted to complex
int	ndim		# dimension of image
int	npix1, npix2	# lengths of first & second axes of image
int	ppix1, ppix2	# lengths of first & second axes of PSF
int	naxis1, naxis2	# size of array used for Fourier transform
bool	fwd		# forward transform?
bool	center		# center output?  (no)
bool	decenter	# decenter the input?

begin
	call smark (sp)

	ndim = IM_NDIM(FT_IMAGE(fti1))

	npix1 = IM_LEN(FT_IMAGE(fti1),1)
	ppix1 = IM_LEN(FT_IMAGE(fti2),1)
	if (ndim > 1) {
	    npix2 = IM_LEN(FT_IMAGE(fti1),2)
	    ppix2 = IM_LEN(FT_IMAGE(fti2),2)
	} else {
	    npix2 = 1
	    ppix2 = 1
	}

	if (pad) {
	    naxis1 = npix1 + ppix1
	    if (ndim > 1)
		naxis2 = npix2 + ppix2
	    else
		naxis2 = 1
	} else {
	    naxis1 = max (npix1, ppix1)
	    naxis2 = max (npix2, ppix2)
	}

	# Allocate space for complex arrays to contain the images.
	call salloc (ximg1, naxis1 * naxis2, TY_COMPLEX)
	call salloc (ximg2, naxis1 * naxis2, TY_COMPLEX)

	# Get the input data and convert to complex.  Shift the PSF to
	# put the maximum at [1,1].
	decenter = false
	call ft_vx_copy (fti1, Memx[ximg1], naxis1, naxis2, decenter)
	decenter = true
	call ft_vx_copy (fti2, Memx[ximg2], naxis1, naxis2, decenter)

	# Do the forward Fourier transform in-place in the complex arrays.
	fwd = true
	call ft_cmplx (Memx[ximg1], naxis1, naxis2, fwd)
	call ft_cmplx (Memx[ximg2], naxis1, naxis2, fwd)

	# Multiply the complex arrays, writing back to ximg1.
	call amulx (Memx[ximg1], Memx[ximg2], Memx[ximg1], naxis1*naxis2)

	# Normalize, working in-place in ximg1.
	xnpix = complex (naxis1*naxis2, 0)	# both arguments are integer
	call adivkx (Memx[ximg1], xnpix, Memx[ximg1], naxis1*naxis2)

	# Take the inverse Fourier transform in-place in ximg1.
	fwd = false
	call ft_cmplx (Memx[ximg1], naxis1, naxis2, fwd)

	# Convert from complex, and write the data to output.
	center = false
	call ft_xo_copy (fto, Memx[ximg1], naxis1, naxis2, center)

	call sfree (sp)
end
