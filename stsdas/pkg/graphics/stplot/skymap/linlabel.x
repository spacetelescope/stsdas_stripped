procedure lin_label (gp, pl_cnst, star_str, wcs)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer
int	wcs

begin
	# Start a new page
	call gframe (gp)

	call gseti  (gp, G_WCS, wcs)
	call gseti  (gp, G_DRAWGRID, YES)
	call gseti  (gp, G_TXQUALITY, GT_HIGH)

	call glabax (gp, EOS, EOS, EOS)

	# Draw a symbol for each star
	call draw_stars (gp, pl_cnst, star_str, 
	    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
	    CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
	    NUM_CAT_VALS(star_str))
end
