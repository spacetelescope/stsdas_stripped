procedure draw_cm (gp, star_str, pl_cnst, cmstyle, cmsize)

include <math.h>
include <gset.h>
include	"skymap.h"

pointer	gp				# Graphics descriptor pointer
pointer	star_str			# Stars markers structure pointer
pointer	pl_cnst				# Plate constants structure pointer
int	cmstyle				# Center marker style
real	cmsize				# Marker size (degrees or NDC)

real	xsize, ysize
int	style

bool	fp_equalr()

begin
	call gseti (gp, G_WCS, MM_R_WCS)
	call gseti (gp, G_PLWIDTH, 1)

	if (fp_equalr (cmsize, 0)) {
	    # Single point marker
	    style = GM_POINT
	    xsize = 1.0
	    ysize = 1.0

	} else {
	    switch (cmstyle+4) {
	    case PLUS_STAR:
		style = GM_PLUS
	    case CROSS_STAR:
		style = GM_CROSS
	    case SQUARE_STAR:
		style = GM_BOX
	    case DIAMOND_STAR:
		style = GM_DIAMOND
	    case CIRCLE_STAR:
		style = GM_CIRCLE
	    }
	}

	if (cmsize > 0.0) {
	    # Size in NDC
	    xsize = cmsize * DEV_ASPECT(star_str)
	    ysize = cmsize

	} else if (cmsize < 0.0) {
	    # Size in degrees
	    xsize = DEGTORAD(cmsize) / PLATE_SCALE(pl_cnst)
	    ysize = xsize
	}

	call gmark (gp, 0.0, 0.0, style, xsize, ysize)
end
