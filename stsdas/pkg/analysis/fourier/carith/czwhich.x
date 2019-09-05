include <imhdr.h>
include "../fourier.h"
include "ftarith.h"		# defines values for eflag

# cz_which -- which files exist
# This routine gets information about the input & output images, in
# particular, which files exist or should be created, and how large
# they are.

procedure cz_which (fti1, fti2, fto,
		eflag, crereal, creimag, npix)

pointer fti1, fti2, fto		# i: pointers to ft struct
int	eflag			# o: specifies which input files exist
bool	crereal			# o: create real part of output image?
bool	creimag			# o: create imaginary part of output image?
int	npix			# o: length of first axis of an image
#--
int	ndim			# dimension of images
int	k

begin
	if (FT_REAL(fti1) == YES) {

	    if (FT_IMAG(fti1) == YES) {

		if (FT_REAL(fti2) == YES) {
		    if (FT_IMAG(fti2) == YES)
			eflag = FT1r1i2r2i
		    else
			eflag = FT1r1i2r__
		} else {
		    eflag = FT1r1i__2i
		}

	    } else {		# FT_IMAG(fti1) == NO

		if (FT_REAL(fti2) == YES) {
		    if (FT_IMAG(fti2) == YES)
			eflag = FT1r__2r2i
		    else
			eflag = FT1r__2r__
		} else {
		    eflag = FT1r____2i
		}
	    }

	} else {		# FT_REAL(fti1) == NO

	    if (FT_IMAG(fti1) == YES) {

		if (FT_REAL(fti2) == YES) {
		    if (FT_IMAG(fti2) == YES)
			eflag = FT__1i2r2i
		    else
			eflag = FT__1i2r__
		} else {
		    eflag = FT__1i__2i
		}

	    } else {		# FT_IMAG(fti1) == NO

		if (FT_REAL(fti2) == YES) {
		    if (FT_IMAG(fti2) == YES)
			eflag = FT____2r2i
		    else
			eflag = FT____2r__
		} else {
		    eflag = FT______2i
		}
	    }
	}

	# Check the dimension and size.
	ndim = IM_NDIM(FT_IMAGE(fti2))
	npix = IM_LEN(FT_IMAGE(fti2),1)

	if (FT_REAL(fti1) == YES || FT_IMAG(fti1) == YES) {
	    if (IM_NDIM(FT_IMAGE(fti1)) != ndim)
		call error (1, "input images are not the same dimension")

	    do k = 1, ndim
		if (IM_LEN(FT_IMAGE(fti1),k) != IM_LEN(FT_IMAGE(fti2),k))
		    call error (1, "input images are not the same size")
	}

	# Check which output files need to be created.
	switch (eflag) {
	case FT1r1i2r2i, FT1r__2r2i, FT__1i2r2i, FT1r1i2r__, FT1r1i__2i:
	    crereal = true
	    creimag = true
	case FT____2r2i:
	    crereal = true
	    creimag = true
	case FT1r__2r__, FT__1i__2i, FT____2r__:
	    crereal = true
	    creimag = false
	case FT1r____2i, FT__1i2r__, FT______2i:
	    crereal = false
	    creimag = true
	}
end
