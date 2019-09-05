procedure prt_near (gp, pl_cnst, star_str, wx, wy, box)

include <gset.h>
include	"skymap.h"

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer
real	wx, wy			# Cursor position in WCS (mm)
real	box			# Full width of search box (NDC)

real	wx1, wy1, wx2, wy2
real	dx, dy
real	ds
double	ra1, dec1, ra2, dec2
real	wl, wr, wb, wt

begin
	call gseti (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, wl, wr, wb, wt)
#	call gline (gp, wl+5.0, wb+5.0, wl+5.0, wt-5.0)

	# WCS/NDC
	call ggscale (gp, wx, wy, dx, dy)

	# Half-width of box
	ds = dy * box / 2.0

	# Edges of search box
	wx1 = wx - ds
	wx2 = wx + ds
	wy1 = wy - ds
	wy2 = wy + ds

	# Draw a box around the region of interest
	call gseti (gp, G_PLTYPE, GL_DASHED)
	call gline (gp, wx1, wy1, wx2, wy1)
	call gline (gp, wx2, wy1, wx2, wy2)
	call gline (gp, wx2, wy2, wx1, wy2)
	call gline (gp, wx1, wy2, wx1, wy1)

	# Celestial coords of region corners
	call cart_sph (wx1, wy1, ra1, dec1, pl_cnst)
	call cart_sph (wx2, wy2, ra2, dec2, pl_cnst)

	call gdeactivate (gp, 0)

	# Print the catalog entries of the stars in the box
	call prt_box (CAT_TBL_ROW(star_str), 
	    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
	    CAT_MAG_VAL(star_str), CAT_CLASS(star_str), 
	    CAT_NAME(star_str), CAT_NAME_SIZE(star_str), 
	    NUM_CAT_VALS(star_str),
	    ra1, dec1, ra2, dec2)

	call greactivate (gp, 0)
end
