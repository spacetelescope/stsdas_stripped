# This file contains ft_vx_copy and ft_find_max.
# The first copies from input images to a complex array, and the second
# finds the location of the maximum absolute value, allowing for the
# input to include either real or imaginary parts, or both.
# The decenter option shifts the maximum to pixel [1,1].
# The "v" in the name "ft_vx_copy" refers to "convolve", as these
# routines are used in the fconvolve task.
#
# Phil Hodge, 28-Jun-1993  Subroutine created based on ft_ix_copy.

include <imhdr.h>
include "../fourier.h"

# ft_vx_copy -- copy input to a complex array
# This routine reads data from the input images, the real and/or imaginary
# parts, and copies to a complex array.
#
# In contrast to ft_ix_copy, if decenter is true the pixel with maximum
# absolute value is shifted to [1,1].  This is roughly the same thing
# because we use this option for a PSF, and we expect the maximum to be
# near the center of the image.
#
# The input image and the output array need not be the same size, but
# the array must be at least as large as the image.

procedure ft_vx_copy (fti, ximg, nx, ny, decenter)

pointer fti			# i: pointer to FT structure for input image
complex ximg[nx,ny]		# o: array to contain image data
int	nx, ny			# i: array size (may be larger than image)
bool	decenter		# i: decenter input?
#--
pointer sp
pointer xline			# scratch for one line of image(s)
pointer iimRe, iimIm		# real & imaginary input image header pointers
pointer iRe, iIm		# pointers to real & imaginary input data
complex zero			# zero
int	ndim			# dimension of image
int	npix1, npix2		# size of image
int	mx, my			# pixel coordinates of maximum
int	shift[2]		# amount to shift
int	i, j
int	out			# if decenter=true, move line j to line out
pointer imgl2r()

begin
	zero = (0.0, 0.0)

	iimRe = FT_REPT(fti)
	iimIm = FT_IMPT(fti)

	ndim = IM_NDIM(FT_IMAGE(fti))
	npix1 = IM_LEN(FT_IMAGE(fti),1)
	if (ndim > 1)
	    npix2 = IM_LEN(FT_IMAGE(fti),2)
	else
	    npix2 = 1

	# Shift the image so the maximum value is at the first pixel.
	if (decenter) {

	    # First read the image in order to locate the brightest pixel.
	    call ft_find_max (fti, mx, my)
	    shift[1] = 1 - mx
	    shift[2] = 1 - my

	    call smark (sp)
	    # We will copy an image line into this array, converting to
	    # complex, and then decenter this array by shift[1] and put
	    # the output into the appropriate line of ximg.
	    call salloc (xline, nx, TY_COMPLEX)
	    do i = 0, nx-1
		Memx[xline+i] = zero

	    # We will move line 'j' of the image to line 'out' of ximg.
	    # This initial value of 'out' corresponds to j = 1.
	    if (ndim == 1) {
		out = 1
	    } else {
		out = 1 + shift[2] + ny
		out = mod (out, ny)
	    }

	    # First copy the lower portion of the image to the upper part
	    # of ximg, and copy the upper portion of the image to the lower
	    # part of ximg.

	    if (FT_REAL(fti) == YES && FT_IMAG(fti) == YES) {

		# top and bottom parts of ximg
		do j = 1, npix2 {
		    iRe = imgl2r (iimRe, j)
		    iIm = imgl2r (iimIm, j)
		    # Note that we use npix1 for apkxr but nx for ft_shift_x.
		    call apkxr (Memr[iRe], Memr[iIm], Memx[xline], npix1)
		    call ft_shift_x (Memx[xline], shift[1], ximg[1,out], nx)
		    out = out + 1
		    if (out > ny)
			out = out - ny
		}

	    } else if (FT_REAL(fti) == YES) {

		do j = 1, npix2 {
		    iRe = imgl2r (iimRe, j)
		    # Note that we use npix1 for achtrx but nx for ft_shift_x.
		    call achtrx (Memr[iRe], Memx[xline], npix1)
		    call ft_shift_x (Memx[xline], shift[1], ximg[1,out], nx)
		    out = out + 1
		    if (out > ny)
			out = out - ny
		}

	    } else if (FT_IMAG(fti) == YES) {

		do j = 1, npix2 {
		    iIm = imgl2r (iimIm, j)
		    do i = 0, npix1-1
			Memx[xline+i] = complex (0.0, Memr[iIm+i])
		    call ft_shift_x (Memx[xline], shift[1], ximg[1,out], nx)
		    out = out + 1
		    if (out > ny)
			out = out - ny
		}
	    }

	    # Now fill the middle part of ximg with zero.  This is the part
	    # that does not correspond to any portion of the input image.
	    if (ndim > 1) {
		do j = 1, ny-npix2 {
		    call amovkx (zero, ximg[1,out], nx)
		    out = out + 1
		}
	    }

	    call sfree (sp)

	} else {				# just copy without decentering

	    # Get the input data and convert to complex.
	    if (FT_REAL(fti) == YES && FT_IMAG(fti) == YES) {
		do j = 1, npix2 {
		    iRe = imgl2r (iimRe, j)
		    iIm = imgl2r (iimIm, j)
		    call apkxr (Memr[iRe], Memr[iIm], ximg[1,j], npix1)
		    do i = npix1+1, nx
			ximg[i,j] = zero
		}
	    } else if (FT_REAL(fti) == YES) {
		do j = 1, npix2 {
		    iRe = imgl2r (iimRe, j)
		    call achtrx (Memr[iRe], ximg[1,j], npix1)
		    do i = npix1+1, nx
			ximg[i,j] = zero
		}
	    } else if (FT_IMAG(fti) == YES) {
		do j = 1, npix2 {
		    iIm = imgl2r (iimIm, j)
		    do i = 1, npix1
			ximg[i,j] = complex (0.0, Memr[iIm+i-1])
		    do i = npix1+1, nx
			ximg[i,j] = zero
		}
	    }
	    if (ndim > 1) {
		do j = npix2+1, ny
		    call amovkx (zero, ximg[1,j], nx)
	    }
	}
end

# ft_find_max -- find the location of the maximum
# This routine reads the images containing the real and imaginary parts
# (whichever we have) to find the location of the pixel with the maximum
# absolute value.

procedure ft_find_max (fti, mx, my)

pointer fti		# i: pointer to FT structure for input image
int	mx, my		# o: pixel coordinates of maximum absolute value
#--
pointer sp
pointer line		# scratch for one line of image
pointer iimRe, iimIm	# real & imaginary input image header pointers
pointer iRe, iIm	# pointers to real & imaginary input data
real	maxval		# maximum absolute value
int	npix1, npix2	# size of image
int	i, j
pointer imgl2r()

begin
	call smark (sp)

	npix1 = IM_LEN(FT_IMAGE(fti),1)
	npix2 = IM_LEN(FT_IMAGE(fti),2)

	iimRe = FT_REPT(fti)
	iimIm = FT_IMPT(fti)

	call salloc (line, npix1, TY_REAL)

	mx = 1				# initial values
	my = 1

	if (FT_REAL(fti) == YES && FT_IMAG(fti) == YES) {

	    # In this section we look for the maximum of the square
	    # of the absolute value.

	    do j = 1, npix2 {
		iRe = imgl2r (iimRe, j)
		iIm = imgl2r (iimIm, j)
		# Compute the sum of squares of real and imaginary parts.
		call amgsr (Memr[iRe], Memr[iIm], Memr[line], npix1)
		if (j == 1)
		    maxval = Memr[line]			# initial value
		# Look for maximum.
		do i = 0, npix1-1 {
		    if (Memr[line+i] > maxval) {
			maxval = Memr[line+i]
			mx = i + 1
			my = j
		    }
		}
	    }

	} else if (FT_REAL(fti) == YES) {

	    do j = 1, npix2 {
		iRe = imgl2r (iimRe, j)
		# Compute absolute value at each pixel.
		call aabsr (Memr[iRe], Memr[line], npix1)
		if (j == 1)
		    maxval = Memr[line]			# initial value
		# Look for maximum.
		do i = 0, npix1-1 {
		    if (Memr[line+i] > maxval) {
			maxval = Memr[line+i]
			mx = i + 1
			my = j
		    }
		}
	    }


	} else if (FT_IMAG(fti) == YES) {

	    do j = 1, npix2 {
		iIm = imgl2r (iimIm, j)
		# Compute absolute value at each pixel.
		call aabsr (Memr[iIm], Memr[line], npix1)
		if (j == 1)
		    maxval = Memr[line]			# initial value
		# Look for maximum.
		do i = 0, npix1-1 {
		    if (Memr[line+i] > maxval) {
			maxval = Memr[line+i]
			mx = i + 1
			my = j
		    }
		}
	    }
	}

	call sfree (sp)
end
