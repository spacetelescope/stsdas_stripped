include <imhdr.h>

# MPC2_CNTR -- Compute star center using MPC algorithm
# as outlined in Stellar Magnitudes from Digital Images.
#
# Taken from proto$t_imcntr.x; mpc2_getcenter modified to check for sum2 = 0.

procedure mpc2_cntr (im, xstart, ystart, boxsize, xcntr, ycntr)

pointer	im
real	xstart, ystart
int	boxsize
real	xcntr, ycntr

int	x1, x2, y1, y2, half_box
int	ncols, nrows, nx, ny, try
real	xinit, yinit
pointer	bufptr, sp, x_vect, y_vect
int	imgs2r()

begin
	half_box = (boxsize - 1) / 2
	xinit = xstart
	yinit = ystart

	# Mark region to extract - use box size
	ncols = IM_LEN (im, 1)
	nrows = IM_LEN (im, 2)
	try = 0

	repeat {
	    x1 = amax1 (xinit - half_box, 1.0) +0.5
	    x2 = amin1 (xinit + half_box, real(ncols)) +0.5
	    y1 = amax1 (yinit - half_box, 1.0) +0.5
	    y2 = amin1 (yinit + half_box, real(nrows)) +0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # Extract region around center
	    bufptr = imgs2r (im, x1, x2, y1, y2)

	    # Collapse to two 1-D arrays
	    call smark (sp)
	    call salloc (x_vect, nx, TY_REAL)
	    call salloc (y_vect, ny, TY_REAL)

	    call aclrr (Memr[x_vect], nx)
	    call aclrr (Memr[y_vect], ny)

	    # Sum all rows
	    call mpc2_rowsum (Memr[bufptr], Memr[x_vect], nx, ny)

	    # Sum all columns
	    call mpc2_colsum (Memr[bufptr], Memr[y_vect], nx, ny)

	    # Find centers
	    call mpc2_getcenter (Memr[x_vect], nx, xcntr)
	    call mpc2_getcenter (Memr[y_vect], ny, ycntr)

	    # Add in offsets
	    xcntr = xcntr + x1
	    ycntr = ycntr + y1

	    call sfree (sp)
	    try = try + 1
	    if (try == 1) {
		if ((abs(xcntr-xinit) > 1.0) || (abs(ycntr-yinit) > 1.0)) {
		    xinit = xcntr
		    yinit = ycntr
		}
	    } else
		break
	}
end

# MPC2_ROWSUM -- Sum all rows in a raster

procedure mpc2_rowsum (v, row, nx, ny)

int	nx, ny
real	v[nx,ny]
real	row[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		row[j] = row[j] + v[j,i]
end


# MPC2_COLSUM -- Sum all columns in a raster.

procedure mpc2_colsum (v, col, nx, ny)

int	nx, ny
real	v[nx,ny]
real	col[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		col[j] = col[j] + v[i,j]
end


# MPC2_GETCENTER -- Compute center of gravity of array.

procedure mpc2_getcenter (v, nv, vc)

real	v[ARB]
int	nv
real	vc

int	i
real	sum1, sum2, sigma, cont

begin
	# Assume continuum level is at endpoints
	# Compute first moment
	sum1 = 0.0
	sum2 = 0.0

	call aavgr (v, nv, cont, sigma)

	do i = 1, nv
	    if (v[i] > cont) {
	        sum1 = sum1 + (i-1) * (v[i] - cont)
	        sum2 = sum2 + (v[i] - cont)
	    }

	# Determine center
	if (sum2 > 0.)
	    vc = sum1 / sum2
	else
	    vc = nv / 2
end
