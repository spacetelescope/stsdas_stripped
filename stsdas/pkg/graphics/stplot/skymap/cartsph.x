procedure cart_sph (x, y, ra, dec, pl_cnst)

include <math.h>
include	"skymap.h"

real	x, y
double	ra, dec
pointer	pl_cnst

double	psi, eta
double	div, tn, tm, tl, fl, fm, fn, a, b, t

begin
	psi = x * PLATE_SCALE(pl_cnst)
	eta = y * PLATE_SCALE(pl_cnst)
	div = sqrt (1.0 + psi*psi * eta*eta)

	tn  = 1.0 / div
	tm  = eta * tn
	tl  = psi * tn

	fl  = - tl * SIN_A(pl_cnst) -
	        tm * COSA_SIND(pl_cnst) +
	        tn * COSA_COSD(pl_cnst)
	fm  =   tl * COS_A(pl_cnst) -
	        tm * SINA_SIND(pl_cnst) +
	        tn * SINA_COSD(pl_cnst) 
	fn  =   tm * COS_D(pl_cnst) +
	        tn * SIN_D(pl_cnst)

	b   = TWOPI
	a   = atan2 (fm, fl) + b
	ra  = mod (a, b)
	t   = sqrt (fl*fl + fm*fm)
	dec = atan2 (fn, t)
end
