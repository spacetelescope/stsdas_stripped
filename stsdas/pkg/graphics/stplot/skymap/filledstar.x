procedure filled_star (gp, star_str, x, y, mag, color)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str		# Stars structure
real	x, y			# WC coordinates
real	mag			# Magnitude
int	color			# Color index

real	size, ss
real	dvx, dvy
real	dwx, dwy
real	wcs_ndc_x, wcs_ndc_y
real	aspect

begin
	call gseti (gp, G_PLTYPE, GL_SOLID)
	call gsetr (gp, G_PLWIDTH, RASTER_WIDTH)

	if (CLASS_COL_EX(star_str) == YES)
	    # Use individual color index
	    call gseti (gp, G_PLCOLOR, color)

	# Dot size in NDC
	size = MAG_SIZE_SLOPE(star_str) * mag + MAG_SIZE_INTERC(star_str)
	size = size / 2.0
	ss   = size * size
	aspect = DEV_ASPECT(star_str)

	# WC/NDC scale
	call ggscale (gp, x, y, wcs_ndc_x, wcs_ndc_y)

	for (dvy = -size;  dvy <= size;  dvy = dvy + LINE_SEP(star_str)) {
	    # Each raster in dot offset in NDC 
	    # Half-width of raster in NDC
#	    call eprintf ("%f %f\n")
#		call pargr (size)
#		call pargr (dvy)
	    dvx = dvy*dvy
	    dvx = ss - dvx

	    # add abs() to prevent negative, JC Hsu 4/30/2004
	    dvx = sqrt (abs(dvx))
	    dvx = dvx * aspect
	    # Half-width of raster in WC
	    dwx = dvx * wcs_ndc_x
	    # Offset from center in WC
	    dwy = dvy * wcs_ndc_y

	    # Draw the raster
	    call gline (gp, x-dwx, y+dwy, x+dwx, y+dwy)
	}
end
