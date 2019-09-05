include	<math.h>

define	SZ_FUNCT	16

# TESTFUNC -- Create functional-form test data

procedure t_testfunc ()

double	incr, clgetd(), yval
int	npts, shft, clgeti(), i
pointer	sp, func, value

begin
	call smark (sp)
	call salloc (func, SZ_FUNCT, TY_CHAR)

	# Get user parameters.
	call clgstr ("function", Memc[func], SZ_FUNCT)
	yval = clgetd ("constant")
	incr = clgetd ("delta")
	npts = clgeti ("npoints")
	shft = clgeti ("oshift")
	shft = mod (shft, npts)

	# Allocate storage for function values.
	call malloc (value, npts, TY_DOUBLE)

	# Make the test function.
	call mk_test_func (Memc[func], yval, incr, npts, shft, Memd[value])

	# Print the values.
	do i = 1, npts {
	    call printf ("%g\n")
		call pargd (Memd[value+i-1])
	}

	call mfree (value, TY_DOUBLE)
end

# mk_test_func -- make a test function
# This routine evaluates the function over the specified range.
#
# Phil Hodge, 28-Jan-1989  Taken from Chris's t_testfunc; made into a
#			subroutine so it can be used by mkfunc as well.

procedure mk_test_func (fcn, yval, incr, npts, shft, y)

char	fcn[SZ_FUNCT]		# i: name of function
double	yval			# i: constant
double	incr			# i: increment
int	npts			# i: number of points to compute
int	shft			# i: amount of shift
double	y[npts]			# o: array of function values
#--
pointer sp
pointer fd			# scratch for function dictionary
int	strdic()

begin
	call smark (sp)
	call salloc (fd, SZ_LINE, TY_CHAR)

	# Load function dictionary.
	call strcpy ("|box|comb|constant|cosine", Memc[fd], SZ_LINE)
	call strcat ("|delta|gaussian", Memc[fd], SZ_LINE)
	call strcat ("|heaviside|line|pe|po", Memc[fd], SZ_LINE)
	call strcat ("|rectangle|shah|signum", Memc[fd], SZ_LINE)
	call strcat ("|sinc|sine|triangle|y", Memc[fd], SZ_LINE)
	call strcat ("|bessel", Memc[fd], SZ_LINE)

	switch (strdic (fcn, fcn, SZ_FUNCT, Memc[fd])) {
	case 1:
	    call rectangle (npts, incr, shft, y)
	case 2:
	    call shah (npts, incr, shft, y)
	case 3:
	    call constant (npts, yval, y)
	case 4:
	    call cosine (npts, incr, shft, y)
	case 5:
	    call delta_fcn (npts, incr, shft, y)
	case 6:
	    call gaussian (npts, incr, shft, y)
	case 7:
	    call heaviside (npts, shft, y)
	case 8:
	    call unit_line (npts, incr, shft, y)
	case 9:
	    call imp_pair (npts, incr, YES, shft, y)
	case 10:
	    call imp_pair (npts, incr, NO, shft, y)
	case 11:
	    call rectangle (npts, incr, shft, y)
	case 12:
	    call shah (npts, incr, shft, y)
	case 13:
	    call signum (npts, shft, y)
	case 14:
	    call sinc (npts, incr, shft, y)
	case 15:
	    call sine (npts, incr, shft, y)
	case 16:
	    call triangle (npts, incr, shft, y)
	case 17:
	    call unit_line (npts, incr, shft, y)
	case 18:
	    call bessel (npts, incr, shft, y)
	default:
	    call error (1, "Unknown function")
	}
end

procedure delta_fcn (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	i, off

begin
	do i = 1, npts
	    y[i] = 0.d0
	off = 1 + shft
	if (off > npts)
	    off = off - npts
	if (off < 1)
	    off = off + npts
	y[off] = 1.d0
end

procedure bessel (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	i
double	x0, x, bessj0()

begin
	x0 = -shft * incr

	x = x0
	do i = 1, npts/2+1 {
	    y[i] = bessj0 (x)
	    x = x + incr
	}

	x = x0 - incr
	do i = npts, npts/2+2, -1 {
	    y[i] = bessj0 (x)
	    x = x - incr
	}
end

procedure constant (npts, yval, y)

int	npts
double	yval, y[*]
int	i

begin
	do i = 1, npts
	    y[i] = yval
end

procedure cosine (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	i
double	x0, x

begin
	x0 = -shft * incr * PI

	x = x0
	do i = 1, npts/2+1 {
	    y[i] = cos (x)
	    x = x + incr * PI
	}

	x = x0 - incr * PI
	do i = npts, npts/2+2, -1 {
	    y[i] = cos (x)
	    x = x - incr * PI
	}
end

procedure sine (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	i
double	x0, x

begin
	x0 = -shft * incr * PI

	x = x0
	do i = 1, npts/2+1 {
	    y[i] = sin (x)
	    x = x + incr * PI
	}

	x = x0 - incr * PI
	do i = npts, npts/2+2, -1 {
	    y[i] = sin (x)
	    x = x - incr * PI
	}
end

procedure gaussian (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
#--
int	i
double	x0, x

begin
	x0 = -shft * incr

	x = x0
	do i = 1, npts/2+1 {
	    y[i] = exp (-x * x / 2)
	    x = x + incr
	}

	x = x0 - incr
	do i = npts, npts/2+2, -1 {
	    y[i] = exp (-x * x / 2)
	    x = x - incr
	}
end

procedure heaviside (npts, shft, y)

int	npts, shft
double	y[*]
int	hnpts, i

begin
	hnpts = (npts-1) / 2 + shft
	hnpts = mod (hnpts, npts)
	if (hnpts < 1)
	    hnpts = hnpts + npts

	do i = 1, hnpts
	    y[i] = 0.d0
	do i = hnpts+1, npts
	    y[i] = 1.d0
end

procedure imp_pair (npts, incr, even, shft, y)

int	npts, shft
double	incr, y[*]
int	even
int	hrwid, off, i

begin
	do i = 1, npts
	    y[i] = 0.d0

	hrwid = (1.d0/incr)/2.d0
	hrwid = mod (hrwid, npts)
	if (hrwid < 1)
	    hrwid = hrwid + npts

	off = hrwid + shft + 1
	if (off > npts)
	    off = off - npts
	if (off < 1)
	    off = off + npts

	if (even == YES)
	    y[off] = 0.5d0
	else
	    y[off] = -0.5d0

	off = npts - hrwid + shft + 1
	if (off > npts)
	    off = off - npts
	if (off < 1)
	    off = off + npts

	y[off] = 0.5d0
end

procedure rectangle (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
#--
int	hrwid, i

begin
	do i = 1, npts
	    y[i] = 0.d0

	hrwid = (1.d0/incr)/2.d0

	do i = 1, hrwid {
	    y[i] = 1.d0
	    y[npts-i+1] = 1.d0
	}

	y[hrwid+1] = 1.d0

	call shft_ord (npts, shft, y)	# shift the ordinate (in-place)
end

procedure shah (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	wid, i
int	off0, off

begin
	do i = 1, npts
	    y[i] = 0.d0

	wid = 1.d0/incr
	if (wid == 0)
	    call error (1, "delta must be < 1")

	off0 = 1 + shft
	off0 = mod (off0, npts)

	off = off0
	do i = 1, npts/2+1, wid {
	    if (off > npts)
		off = off - npts
	    if (off < 1)
		off = off + npts
	    y[off] = 1.d0
	    off = off + wid
	}

	off = off0 - wid
	do i = npts, npts/2+2, -wid {
	    if (off > npts)
		off = off - npts
	    if (off < 1)
		off = off + npts
	    y[off] = 1.d0
	    off = off - wid
	}
end

procedure signum (npts, shft, y)

int	npts, shft
double	y[*]
int	hnpts, i

begin
	hnpts = (npts-1) / 2 + shft
	hnpts = mod (hnpts, npts)
	if (hnpts < 1)
	    hnpts = hnpts + npts

	do i = 1, hnpts
	    y[i] = -1.d0

	do i = hnpts+1, npts
	    y[i] = 1.d0
end

procedure sinc (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
#--
double	x0, x, pi_x
int	i

begin
	x0 = -shft * incr

	x = x0
	do i = 1, npts/2+1 {
	    pi_x = PI * x
	    if (pi_x == 0.d0)
		y[i] = 1.d0
	    else
		y[i] = sin (pi_x) / pi_x
	    x = x + incr
	}

	x = x0 - incr
	do i = npts, npts/2+2, -1 {
	    pi_x = PI * x
	    if (pi_x == 0.d0)
		y[i] = 1.d0
	    else
		y[i] = sin (pi_x) / pi_x
	    x = x - incr
	}
end

procedure triangle (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	htwid, i
double	x

begin
	htwid = 1/incr - 1
	x = 1.d0

	y[1] = x
	x = x - incr

	do i = 1, htwid {
	    y[i+1] = x
	    y[npts-i+1] = x
	    x = x - incr
	}
	call shft_ord (npts, shft, y)		# shift the ordinate
end

procedure unit_line (npts, incr, shft, y)

int	npts, shft
double	incr, y[*]
int	i
double	x

begin
	x = 0.d0

	do i = 1, npts {
	    y[i] = x
	    x = x + incr
	}
	call shft_ord (npts, shft, y)		# shift the ordinate
end

# shft_ord -- shift the ordinate

procedure shft_ord (npts, shft, y)

int	shft		# i: amount of shift (toward right is positive)
int	npts		# i: size of arrays
double	y[npts]		# io: values; shifted in-place
#--
pointer sp
pointer v
int	ip, op		# indexes for input & output

begin
	if (shft == 0)
	    return

	call smark (sp)
	call salloc (v, npts, TY_DOUBLE)

	call amovd (y, Memd[v], npts)

	ip = -shft		# zero indexed
	ip = mod (ip, npts)

	do op = 1, npts {
	    if (ip >= npts)
		ip = ip - npts
	    if (ip < 0)
		ip = ip + npts
	    y[op] = Memd[v+ip]
	    ip = ip + 1
	}
	call sfree (sp)
end
