include <imhdr.h>
include "../fourier.h"

# cz_cutoff -- get lower limit for division
# The lower limit for division is the cutoff multiplied by the maximum
# modulus squared.  If cutoff is negative, the value at the first pixel
# is taken; if cutoff is positive, the entire image is searched for the
# maximum modulus; if cutoff is zero, lowlim is set to zero.
# The first element of lowlim is the value, and the second element is
# the value squared.  If the divisor consists of only one part (real
# or imaginary) then the value will be used by czdivr; otherwise, the
# value squared will be used.

procedure cz_cutoff (fti, cutoff, lowlim)

pointer fti		# i: pointer to ft struct for an input image
real	cutoff		# i: lowlim is this fraction of maximum value
real	lowlim[2]	# o: lower limit for division (value, value ** 2)
#--
pointer xr, xi		# pointers to data for real & imaginary parts
real	valr, vali	# real & imaginary parts of the value at a pixel
real	value		# a value to be compared with the maximum
real	maxval		# maximum absolute value in image
long	vr[IM_MAXDIM]	# for getting next line
long	vi[IM_MAXDIM]	# for imaginary part
int	npix		# length of first axis
int	k
int	dummy
bool	done
int	imgnlr()

begin
	if (FT_REAL(fti) == NO && FT_IMAG(fti) == NO)
	    call error (1, "cz_cutoff:  no image")

	do k = 1, IM_MAXDIM {
	    vr[k] = 1
	    vi[k] = 1
	}

	if (cutoff < 0.) {

	    # A negative value of cutoff means that we should take lowlim
	    # equal to abs(cutoff) * the value at [1,1].

	    valr = 0.
	    vali = 0.
	    if (FT_REAL(fti) == YES) {
		dummy = imgnlr (FT_REPT(fti), xr, vr)
		valr = Memr[xr]
	    }
	    if (FT_IMAG(fti) == YES) {
		dummy = imgnlr (FT_IMPT(fti), xi, vi)
		vali = Memr[xi]
	    }
	    lowlim[1] = abs (cutoff) * sqrt (valr ** 2 + vali ** 2)
	    lowlim[2] = lowlim[1] ** 2

	} else if (cutoff > 0.) {

	    # Search the entire image for the maximum value.
	    maxval = 0.
	    npix = IM_LEN(FT_IMAGE(fti),1)

	    if (FT_REAL(fti) == YES && FT_IMAG(fti) == YES) {

		done = false
		while ( ! done ) {
		    if (imgnlr (FT_REPT(fti), xr, vr) == EOF ||
			imgnlr (FT_IMPT(fti), xi, vi) == EOF) {
			done = true
		    } else {
			do k = 0, npix-1 {
			    value = Memr[xr+k] ** 2 + Memr[xi+k] ** 2
			    if (value > maxval)
				maxval = value
			}
		    }
		}
		lowlim[1] = cutoff * sqrt (maxval)
		lowlim[2] = lowlim[1] ** 2

	    } else {

		# Either real or imaginary part.
		done = false
		while ( ! done ) {
		    if (imgnlr (FT_IMAGE(fti), xr, vr) == EOF) {
			done = true
		    } else {
			do k = 0, npix-1 {
			    value = abs (Memr[xr+k])	# note:  not squared
			    if (value > maxval)
				maxval = value
			}
		    }
		}
		lowlim[1] = cutoff * maxval
		lowlim[2] = lowlim[1] ** 2
	    }

	} else {
	    lowlim[1] = 0.
	    lowlim[2] = 0.
	}
end
