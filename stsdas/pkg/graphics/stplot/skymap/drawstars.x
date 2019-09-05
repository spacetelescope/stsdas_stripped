procedure draw_stars (gp, pl_cnst, star_str, ra, dec, mag, colind, numstars)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst
pointer	star_str			# Stars structure
double	ra[ARB], dec[ARB]
real	mag[ARB]
int	colind[ARB]			# Color index array
int	numstars

int	row
real	wl, wr, wb, wt
real	x, y
double	r, d
real	m
real	faint, bright
double	dot
int	c

begin
	call gseti  (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, wl, wr, wb, wt)

	bright = BRIGHT_PLOT_MAG(star_str)
	faint  = FAINT_PLOT_MAG(star_str)

	call gseti (gp, G_CLIP, NO)

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

	    m = mag[row]
	    if (m > faint || m < bright)
		# Object is not in brightness range
		next

	    # Set the color index to the catalog "class" value
	    # Making sure we don't use colors < 1
	    c = max (1, colind[row])
	    c = min (16, c)

	    switch (SYMBOL_STYLE(star_str)) {
	    case FILLED_STAR:
		call filled_star (gp, star_str, x, y, m, c)

	    case ERASE_STAR:
		call erase_star (gp, star_str, x, y, m, c)

	    case EFILLED_STAR:
		call efilled_star (gp, star_str, x, y, m, c)

	    default:
		call open_star (gp, star_str, x, y, m, c,
		    SYMBOL_STYLE(star_str))
	    }
	}

	call gseti (gp, G_CLIP, YES)
end
