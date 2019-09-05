procedure open_star (gp, star_str, x, y, mag, color, style)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str		# Stars structure
real	x, y			# WC coordinates
real	mag			# Magnitude
int	color			# Color index
int	style			# Symbol style

real	size
real	xs, ys
real	wcs_ndc_x, wcs_ndc_y
real	xc, yc

begin
	# Dot size in NDC
	size = MAG_SIZE_SLOPE(star_str) * mag + MAG_SIZE_INTERC(star_str)
	xs   = size * DEV_ASPECT(star_str)
	ys   = size

	call gseti (gp, G_PMLTYPE, GL_SOLID)
	call gsetr (gp, G_PMWIDTH, 1.0)

	if (CLASS_COL_EX(star_str) == YES)
	    # Use individual color index
	    call gseti (gp, G_PLCOLOR, color)

	switch (style) {
	case OPEN_STAR:
	    call gmark (gp, x, y, GM_CIRCLE, xs, ys)
	case PLUS_STAR:
	    call gmark (gp, x, y, GM_PLUS, xs, ys)
	    return
	case CROSS_STAR:
	    call gmark (gp, x, y, GM_CROSS, xs, ys)
	    return
	case SQUARE_STAR:
	    call gmark (gp, x, y, GM_BOX, xs, ys)
	    return
	case DIAMOND_STAR:
	    call gmark (gp, x, y, GM_DIAMOND, xs, ys)
	    return
	case CIRCLE_STAR:
	    call gmark (gp, x, y, GM_CIRCLE, xs, ys)
	    return
	default:
	    call gmark (gp, x, y, GM_CIRCLE, xs, ys)
	}

	if (size < LINE_SEP(star_str) * 8.0)
	    # Symbol too small
	    return

	# Draw the sight lines
	# WC/NDC scale
	call ggscale (gp, x, y, wcs_ndc_x, wcs_ndc_y)

	xc = x + xs * wcs_ndc_x / 2.0
	yc = y
	call gmark (gp, xc, yc, GM_HLINE, xs/2.0, 1.0)
	xc = x - xs * wcs_ndc_x / 2.0
	call gmark (gp, xc, yc, GM_HLINE, xs/2.0, 1.0)

	xc = x
	yc = y + ys * wcs_ndc_y / 2.0
	call gmark (gp, xc, yc, GM_VLINE, 1.0, ys/2.0)
	yc = y - ys * wcs_ndc_y / 2.0
	call gmark (gp, xc, yc, GM_VLINE, 1.0, ys/2.0)
end
