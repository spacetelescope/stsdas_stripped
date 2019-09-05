procedure sph_cart (ra, dec, x, y, pl_cnst)

# SPH_CART -- Convert equatorial celestial coordinates to cartesian 
# coordinates using gnomonic projection

include	"skymap.h"

double	ra, dec
real	x, y
pointer	pl_cnst

double	t, fl, fm, fn, tl, tm, tn

begin
	t  = cos (dec)

	fl = t * cos (ra)
	fm = t * sin (ra)
	fn = sin (dec)

	tl = - fl * SIN_A(pl_cnst) +
	       fm * COS_A(pl_cnst)
	tm = - fl * COSA_SIND(pl_cnst) -
	       fm * SINA_SIND(pl_cnst) +
	       fn * COS_D(pl_cnst)
	tn =   fl * COSA_COSD(pl_cnst) +
	       fm * SINA_COSD(pl_cnst) +
	       fn * SIN_D(pl_cnst)

	# Chart coordinates in mm
	x = (tl / tn) / PLATE_SCALE(pl_cnst)
	y = (tm / tn) / PLATE_SCALE(pl_cnst)
end
