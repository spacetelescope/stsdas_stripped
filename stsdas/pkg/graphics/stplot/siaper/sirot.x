include	<math.h>

#---------------------------------------------------------------------------
.help si_rot 24Jul95 source
.ih
NAME
si_rot -- Rotate with parity.
.endhelp
#---------------------------------------------------------------------------
procedure si_rot (x, y, n, a, p, rx, ry)

real	x[n], y[n]		# I:  Points to be rotated.
int	n			# I:  Number of points.
real	a			# I:  Angle from +v3 to +v2 in degrees
int	p			# I:  Parity.
real	rx[n], ry[n]		# O:  Rotated points.

# Declarations.
real	cosra, pcosra		# Cos, parity cos.
int	i			# Generic.
real	ra			# Angle in radians.
real	sinra, psinra		# Sin, parity sin.

begin
	ra = DEGTORAD(a)
	cosra = cos (ra)
	pcosra = p * cosra
	sinra = sin (ra)
	psinra = -1 * p * sinra
	do i = 1, n {
	    rx[i] = x[i] * pcosra + y[i] * sinra
	    ry[i] = x[i] * psinra + y[i] * cosra
	}
end
#---------------------------------------------------------------------------
# End of si_rot
#---------------------------------------------------------------------------
