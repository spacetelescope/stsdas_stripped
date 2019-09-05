define 	X_CENTER	400.
define 	X_RANGE 	400.
define 	Y_CENTER	400.
define 	Y_RANGE 	400.

define	MAX_ORDER	8

#  legendre -- Perform the intra-chip geometric distortion correction
#
#  Description:
#  ------------
#  Evaluates two dimensional Legendre polynomials to perform the intra-chip 
#  geometric distortion correction
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Oct-1992  J.-C. Hsu		adapted from Shawn Ewald's FORTRAN code
#------------------------------------------------------------------------------

procedure legendre (x0, y0, dx, dy, ax, ay, order)

real	x0, y0		# input (raw) X and Y coordiantes
real	dx, dy		# output corrections of X and Y
real	ax[*]
real	ay[*]
int	order		# order of the maximum polynomial

real	x, y
real	z		# Legendre polynomials cross-product at successive 
			# orders
real	x2, y2
real	px[MAX_ORDER+1], py[MAX_ORDER+1]
int	i, j, k
#==============================================================================
begin

	# scale to the range of -1. and +1.
	x = (x0 - X_CENTER) / X_RANGE
	y = (y0 - Y_CENTER) / Y_RANGE

	x2 = x**2
	y2 = y**2

	# construct up to (MAX_ORDER + 1)th order Legendre polynomials
	if (order > MAX_ORDER) call error(1, "polynomial has too high order")

	px[1] = 1.
	px[2] = x
	px[3] = (3. * x2 - 1.) / 2.
	px[4] = (5. * x2 - 3.) * x / 2.
	px[5] = ((35. * x2 - 30.) * x2 + 3.) / 8.
	px[6] = ((63. * x2 - 70.) * x2 + 15.) * x / 8.
	px[7] = (((231. * x2 - 315.) * x2 + 105.) * x2 - 5.) / 16.
	px[8] = (((429. * x2 - 693.) * x2 + 315.) * x2 - 35.) * x / 16.
	px[9] = ((((6435. * x2 - 12012.) * x2 + 6930.) * x2 - 1260.) * x2 + 35.)		/ 128.

	py[1] = 1.
	py[2] = y
	py[3] = (3. * y2 - 1.) / 2.
	py[4] = (5. * y2 - 3.) * y / 2.
	py[5] = ((35. * y2 - 30.) * y2 + 3.) / 8.
	py[6] = ((63. * y2 - 70.) * y2 + 15.) * y / 8.
	py[7] = (((231. * y2 - 315.) * y2 + 105.) * y2 - 5.) / 16.
	py[8] = (((429. * y2 - 693.) * y2 + 315.) * y2 - 35.) * y / 16.
	py[9] = ((((6435. * y2 - 12012.) * y2 + 6930.) * y2 - 1260.) * y2 + 35.)		/ 128.

	# evaluate the polynomial cross products
	dx = 0.
	dy = 0.
	k = 1
	do i = 1, order+1 {
	    do j = 1, order+1 {
		z = px[j] * py[i]
		dx = dx + ax[k] * z
		dy = dy + ay[k] * z
		k = k + 1
	    }
	}
end
