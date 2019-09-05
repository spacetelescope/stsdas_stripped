include <imhdr.h>
include "../fourier.h"
include "ftarith.h"

define	DEF_LEN_BPL	10	# default length of list of bad pixel locations

# carith -- complex arithmetic
# This task will either multiply or divide two complex images, where the real
# and imaginary parts are stored as two separate images.  The file name for
# the real part ends in "r", and the name for the imaginary part ends in "i".
#
# Phil Hodge, 23-July-1990  Task created.
# Phil Hodge, 21-July-1993  Print statement modified for small divisors.
# Phil Hodge, 15-Jan-1995   Call ft_struct_open and ft_struct_close.

procedure carith()

pointer input1, input2		# scr for names of input images
pointer output			# scr for name of output images
pointer oper			# "*" or "/"
int	operation		# multiply or divide
real	cutoff			# specifies lower limit for division
bool	verbose
#--
pointer sp
pointer bp			# list of bad pixel locations
pointer pdummy			# dummy pointer
pointer fti1, fti2, fto		# pointers to ft struct
pointer x1r, x1i, x2r, x2i	# pointers to input data
pointer oxr, oxi		# pointers to output data
real	lowlim[2]		# lower limit for division (modulus, mod ** 2)
long	v[IM_MAXDIM,6]		# line index for 4 input & 2 output images
int	eflag			# existence flag for real & imag parts
int	npix			# length of a line in images
int	len_bp			# current allocated size of bad-pixel list
int	nbad			# number of pixels with small denominator
int	j, k
bool	num_exists		# is there a numerator?
real	clgetr()
int	cz_gline()
bool	clgetb()

begin
	call smark (sp)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (oper, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	call clgstr ("input1", Memc[input1], SZ_FNAME)
	call clgstr ("operation", Memc[oper], SZ_FNAME)
	if (Memc[oper] == '*')
	    operation = FT_MULTIPLY
	else if (Memc[oper] == '/')
	    operation = FT_DIVIDE
	else
	    call error (1, "operation must be * or /")
	call clgstr ("input2", Memc[input2], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	if (operation == FT_DIVIDE)
	    cutoff = clgetr ("cutoff")
	verbose = clgetb ("verbose")

	# Allocate Fourier structure for input & output images.
	call ft_struct_open (fti1)
	call ft_struct_open (fti2)
	call ft_struct_open (fto)

	# Allocate space for a list of bad pixel locations, i.e. points
	# where the denominator is small.
	if (operation == FT_DIVIDE) {
	    len_bp = DEF_LEN_BPL
	    call malloc (bp, len_bp, TY_INT)
	}

	# Allocate a dummy pointer so we can use Memr of something for
	# images that don't exist.  Initialize data pointers to pdummy.
	call salloc (pdummy, 1, TY_REAL)
	Memr[pdummy] = 1.
	x1r = pdummy
	x1i = pdummy
	x2r = pdummy
	x2i = pdummy
	oxr = pdummy
	oxi = pdummy

	# Initialize v for line by line I/O.
	do k = 1, 6			# up to 4 input & 2 output images
	    do j = 1, IM_MAXDIM
		v[j,k] = 1

	# Open input images, create output images, get length of a line.
	# Get the lower limit for division.  Set eflag depending on which
	# files exist.
	call cz_init (Memc[input1], Memc[input2], Memc[output],
		fti1, fti2, fto, operation,
		num_exists, eflag, cutoff, lowlim, npix)

	# Print file names.
	if (verbose) {
	    call ft_ri_print (fti1, "input1")
	    call ft_ri_print (fti2, "input2")
	    call ft_ri_print (fto, "output")
	    call flush (STDOUT)
	}

	# Get each line and operate.
	nbad = 0
	while (cz_gline (fti1, fti2, fto, v,
			x1r, x1i, x2r, x2i, oxr, oxi) != EOF) {

	    if (operation == FT_DIVIDE)
		call czdivr (Memr[x1r], Memr[x1i], Memr[x2r], Memr[x2i],
			Memr[oxr], Memr[oxi], npix, eflag,
			bp, len_bp, lowlim, nbad)
	    else			# multiply
		call czmultr (Memr[x1r], Memr[x1i], Memr[x2r], Memr[x2i],
			Memr[oxr], Memr[oxi], npix, eflag)
	}

	if (nbad > 0 && verbose) {
		call printf ("denominator small at %d pixel")
		    call pargi (nbad)
	    if (nbad > 1)
		call printf ("s\n")
	    else
		call printf ("\n")
	}

	if (num_exists)
	    call ft_close_files (fti1)
	call ft_close_files (fti2)
	call ft_close_files (fto)
	if (operation == FT_DIVIDE)
	    call mfree (bp, TY_INT)
	call ft_struct_close (fto)
	call ft_struct_close (fti2)
	call ft_struct_close (fti1)
	call sfree (sp)
end

# czmultr -- complex multiplication
# This routine multiplies complex arrays which are stored as separate real
# and imaginary parts.  At least one part (real or imaginary) of both the
# multiplicand and the multiplier must be present.

procedure czmultr (x1r, x1i, x2r, x2i, oxr, oxi, n, eflag)

real	x1r[n]		# i: multiplicand, real part
real	x1i[n]		# i: multiplicand, imaginary part
real	x2r[n]		# i: multiplier, real part
real	x2i[n]		# i: multiplier, imaginary part
real	oxr[n]		# o: product, real part
real	oxi[n]		# o: product, imaginary part
int	n		# i: size of arrays
int	eflag		# i: existence flag for real & imaginary parts
#--
int	j

begin
	switch (eflag) {

	case FT1r1i2r2i:
	    do j = 1, n {
		oxr[j] = x1r[j] * x2r[j] - x1i[j] * x2i[j]
		oxi[j] = x1r[j] * x2i[j] + x2r[j] * x1i[j]
	    }

	case FT1r__2r2i:
	    do j = 1, n {
		oxr[j] = x1r[j] * x2r[j]
		oxi[j] = x1r[j] * x2i[j]
	    }

	case FT__1i2r2i:
	    do j = 1, n {
		oxr[j] = -x1i[j] * x2i[j]
		oxi[j] =  x2r[j] * x1i[j]
	    }

	case FT1r1i2r__:
	    do j = 1, n {
		oxr[j] = x1r[j] * x2r[j]
		oxi[j] = x1i[j] * x2r[j]
	    }

	case FT1r1i__2i:
	    do j = 1, n {
		oxr[j] = -x1i[j] * x2i[j]
		oxi[j] =  x1r[j] * x2i[j]
	    }

	case FT1r__2r__:
	    do j = 1, n
		oxr[j] = x1r[j] * x2r[j]

	case FT1r____2i:
	    do j = 1, n
		oxi[j] = x1r[j] * x2i[j] 

	case FT__1i2r__:
	    do j = 1, n
		oxi[j] = x1i[j] * x2r[j]

	case FT__1i__2i:
	    do j = 1, n
		oxr[j] = -x1i[j] * x2i[j]
	}
end

# czdivr -- complex division
# This routine divides complex arrays which are stored as separate real and
# imaginary parts.  Any of the parts may be missing, except that at least one
# part of the denominator must be present.  If both parts of the numerator are
# missing, we just take the reciprocal of the denominator. 
# When the divisor is less than or equal to lowlim, the quotient is obtained
# by interpolating between neighboring pixels.  Lowlim is actually an array
# of two values; the first element is the lower limit itself, and the second
# element is the lower limit squared.  This is so we can use whichever is
# appropriate depending on whether the divisor consists of both real and
# imaginary parts (use lowlim[2]) or just one part (use lowlim[1]).

procedure czdivr (x1r, x1i, x2r, x2i, oxr, oxi, n, eflag,
		bp, len_bp, lowlim, nbad)

real	x1r[n]		# i: numerator, real part
real	x1i[n]		# i: numerator, imaginary part
real	x2r[n]		# i: divisor, real part
real	x2i[n]		# i: divisor, imaginary part
real	oxr[n]		# o: quotient, real part
real	oxi[n]		# o: quotient, imaginary part
int	n		# i: size of arrays
int	eflag		# i: existence flag for real & imaginary parts
pointer bp		# io: list of bad pixel locations
int	len_bp		# io: current allocated size of bad-pixel list
real	lowlim[2]	# i: lower limit for divisor
int	nbad		# io: number of pixels with small divisor
#--
real	temp		# divide by this when divisor includes both r & i
int	new_nbad	# additional number of bad points in current row
int	j

begin
	new_nbad = 0			# initial value

	switch (eflag) {

	case FT1r1i2r2i:
	    do j = 1, n {

		temp = x2r[j] ** 2 + x2i[j] ** 2

		if (temp > lowlim[2]) {
		    oxr[j] = (x1r[j] * x2r[j] + x1i[j] * x2i[j]) / temp
		    oxi[j] = (x2r[j] * x1i[j] - x1r[j] * x2i[j]) / temp
		} else {
		    # Save current pixel number in list of bad pixel locations.
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT1r__2r2i:
	    do j = 1, n {

		temp = x2r[j] ** 2 + x2i[j] ** 2

		if (temp > lowlim[2]) {
		    oxr[j] =  x1r[j] * x2r[j] / temp
		    oxi[j] = -x1r[j] * x2i[j] / temp
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT__1i2r2i:
	    do j = 1, n {

		temp = x2r[j] ** 2 + x2i[j] ** 2

		if (temp > lowlim[2]) {
		    oxr[j] = x1i[j] * x2i[j] / temp
		    oxi[j] = x2r[j] * x1i[j] / temp
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT1r1i2r__:
	    do j = 1, n {

		if (abs (x2r[j]) > lowlim[1]) {
		    oxr[j] = x1r[j] / x2r[j]
		    oxi[j] = x1i[j] / x2r[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT1r1i__2i:
	    do j = 1, n {

		if (abs (x2i[j]) > lowlim[1]) {
		    oxr[j] =  x1i[j] / x2i[j]
		    oxi[j] = -x1r[j] / x2i[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT1r__2r__:
	    do j = 1, n {

		if (abs (x2r[j]) > lowlim[1]) {
		    oxr[j] = x1r[j] / x2r[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxr, n, Memi[bp], new_nbad)

	case FT1r____2i:
	    do j = 1, n {

		if (abs (x2i[j]) > lowlim[1]) {
		    oxi[j] = -x1r[j] / x2i[j] 
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxi, n, Memi[bp], new_nbad)

	case FT__1i2r__:
	    do j = 1, n {

		if (abs (x2r[j]) > lowlim[1]) {
		    oxi[j] = x1i[j] / x2r[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxi, n, Memi[bp], new_nbad)

	case FT__1i__2i:
	    do j = 1, n {

		if (abs (x2i[j]) > lowlim[1]) {
		    oxr[j] = x1i[j] / x2i[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxr, n, Memi[bp], new_nbad)

	case FT____2r2i:
	    do j = 1, n {

		temp = x2r[j] ** 2 + x2i[j] ** 2

		if (temp > lowlim[2]) {
		    oxr[j] =  x2r[j] / temp
		    oxi[j] = -x2i[j] / temp
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp2 (oxr, oxi, n, Memi[bp], new_nbad)

	case FT____2r__:
	    do j = 1, n {

		if (abs (x2r[j]) > lowlim[1]) {
		    oxr[j] = 1. / x2r[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxr, n, Memi[bp], new_nbad)

	case FT______2i:

	    do j = 1, n {

		if (abs (x2i[j]) > lowlim[1]) {
		    oxi[j] = -1. / x2i[j]
		} else {
		    call cz_mark_bad (bp, len_bp, new_nbad, j)
		}
	    }
	    if (new_nbad > 0)
		call cz_interp1 (oxi, n, Memi[bp], new_nbad)
	}

	if (new_nbad > 0)
	    nbad = nbad + new_nbad
end

# cz_mark_bad -- add current pixel number to list of bad pixels

procedure cz_mark_bad (bp, len_bp, new_nbad, j)

pointer bp		# io: pointer to list of bad pixel locations
int	len_bp		# io: current allocated length of bad pixel list
int	new_nbad	# io: current number of bad pixels
int	j		# i: pixel number to be added to list
#--

begin
	new_nbad = new_nbad + 1

	# If the currently allocated size of bp is not large enough to
	# hold new_nbad elements, reallocate bp.
	if (new_nbad > len_bp) {
	    len_bp = len_bp + DEF_LEN_BPL
	    call realloc (bp, len_bp, TY_INT)
	}

	# Save current pixel number in list of bad pixel locations.
	Memi[bp+new_nbad-1] = j
end
