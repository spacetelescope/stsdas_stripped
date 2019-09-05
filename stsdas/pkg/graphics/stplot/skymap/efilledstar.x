procedure efilled_star (gp, star_str, x, y, mag, color)

#  Draw a "true" filled symbol, not implemented by all graphics kernels.
#  Actually, draw it twice, first slightly larger and "clear" or
#  drawing with the background color.  Then draw it normal size
#  (smaller).  This permits seeing overlapping stars, assuming they are
#  drawn in the order larger to smaller.

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str		# Stars structure
real	x, y			# WC coordinates
real	mag			# Magnitude
int	color			# Color index

real	size
real	xs, ys

begin
	# Dot size in NDC
	size = MAG_SIZE_SLOPE(star_str) * mag + MAG_SIZE_INTERC(star_str)
	xs   = size * DEV_ASPECT(star_str)
	ys   = size

	call gseti (gp, G_PMLTYPE, GL_SOLID)
	call gsetr (gp, G_PMWIDTH, 1.0)

	# Erase with the margin
	call gseti (gp, G_FASTYLE, GF_CLEAR)
	call gmark (gp, x, y, GM_CIRCLE+GM_FILL,
	    xs+CLEAR_MARGIN, ys+CLEAR_MARGIN)

	# Draw the (normal-sized) symbol solid
	call gseti (gp, G_FASTYLE, GF_SOLID)
	if (CLASS_COL_EX(star_str) == YES)
	    # Use individual color index
	    call gseti (gp, G_FACOLOR, color)

	call gmark (gp, x, y, GM_CIRCLE+GM_FILL, xs, ys)

	call gseti (gp, G_FASTYLE, GF_HOLLOW)
end
