procedure draw_connect (gp, pl_cnst, star_str, ra, dec, numstars)

include <gset.h>
include	"skymap.h"

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer
double	ra[ARB], dec[ARB]
int	numstars

int	row
real	wl, wr, wb, wt
real	x, y
double	r, d
double	dot
int	nvert
pointer	sp, xp, yp

begin
	call gseti  (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, wl, wr, wb, wt)

	call gseti (gp, G_PLTYPE, CONN_STYLE(star_str))

	nvert = 0
	call smark (sp)
	call salloc (xp, numstars, TY_REAL)
	call salloc (yp, numstars, TY_REAL)

	do row = 1, numstars {
	    # For each object in catalog
	    r = ra[row];  d = dec[row]

	    # Find dot product between chart center and position
	    dot = cos (r) * cos (d) * COSA_COSD(pl_cnst) + 
		  sin (r) * cos (d) * SINA_COSD(pl_cnst) +
		  sin (d) * SIN_D(pl_cnst)

	    if (dot < 0.5)
		# Too far from chart center
		next

	    call sph_cart (r, d, x, y, pl_cnst)

	    if (MIRROR_FLIP(pl_cnst) == YES) {
		if (x < wl || x > wr || y < wb || y > wt)
		    # Object is outside the chart borders
		    next
	    } else {
		if (x > wl || x < wr || y < wb || y > wt)
		    # Object is outside the chart borders
		    next
	    }

	    nvert = nvert + 1
	    Memr[xp+nvert-1] = x
	    Memr[yp+nvert-1] = y
	}

	call gpline (gp, Memr[xp], Memr[yp], nvert)

	call sfree (sp)
end
