procedure erase_star (gp, star_str, x, y, mag, color)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str		# Stars structure
real	x, y			# WC coordinates
real	mag			# Magnitude
int	color			# Color index

real	size
real	dvx, dvy
real	dwx, dwy
real	wcs_ndc_x, wcs_ndc_y
real	xs, ys

begin
	call gseti (gp, G_PLTYPE, GL_SOLID)
	call gsetr (gp, G_PLWIDTH, RASTER_WIDTH)

	if (CLASS_COL_EX(star_str) == YES)
	    # Use individual color index
	    call gseti (gp, G_PLCOLOR, color)

	# Dot size in NDC
	size = MAG_SIZE_SLOPE(star_str) * mag + MAG_SIZE_INTERC(star_str)
	xs   = size * DEV_ASPECT(star_str) + LINE_SEP(star_str)
	ys   = size + LINE_SEP(star_str)
	size = size / 2.0

	# WC/NDC scale
	call ggscale (gp, x, y, wcs_ndc_x, wcs_ndc_y)

	for (dvy = -size;  dvy <= size;  dvy = dvy + LINE_SEP(star_str)) {
	    # Each raster in dot offset in NDC 
	    # Half-width of raster in NDC
	    dvx = sqrt (size*size - dvy*dvy) * DEV_ASPECT(star_str)
	    # Half-width of raster in WC
	    dwx = dvx * wcs_ndc_x
	    # Offset from center in WC
	    dwy = dvy * wcs_ndc_y

	    # Draw the raster
	    call gline (gp, x-dwx, y+dwy, x+dwx, y+dwy)
	}

	# Erase the edge
	call gseti (gp, G_PMLTYPE, GL_CLEAR)
	call gsetr (gp, G_PMWIDTH, 1.0)
	call gmark (gp, x, y, GM_CIRCLE, xs, ys)
end
