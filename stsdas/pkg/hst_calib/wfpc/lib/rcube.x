define 	X_CENTER	400.
define 	Y_CENTER	400.

#  legendre -- Perform the intra-chip geometric distortion correction
#
#  Description:
#  ------------
#  Evaluates radius cube to perform the intra-chip 
#  geometric distortion correction
#	
#	delta(x) = ax[1] * x * (x**2 + y**2)
#	delta(y) = ax[1] * y * (x**2 + y**2)
#	where x = x offset from X_CENTER y = y offset from Y_CENTER	
#  
#  Date		Author		Description
#  ----		------		-----------
#  14-Jul-1994  J.-C. Hsu	temporary use of the pincushion distortion only
#------------------------------------------------------------------------------

procedure rcube (x0, y0, dx, dy, ax, ay, order)

real	x0, y0		# input (raw) X and Y coordiantes
real	dx, dy		# output corrections of X and Y
real	ax[*]
real	ay[*]
int	order		# order of the maximum polynomial

real	x, y
real	r2
#==============================================================================
begin
	x = (x0 - X_CENTER) 
	y = (y0 - Y_CENTER)

	r2 = x**2 + y**2
	dx = ax[1] * x * r2
	dy = ax[1] * y * r2
end
