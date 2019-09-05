procedure sky_legend (gp, star_str, pl_cnst)

include	"skymap.h"

# Draw the scale, mag key and title

pointer	gp			# Graphics descriptor pointer
pointer	star_str		# Stars markers structure pointer
pointer	pl_cnst			# Plate constants structure pointer

begin
	# Draw the chart scale legend
	call ch_legend (gp, pl_cnst)

	# Write the catalog title
	call cat_title (gp, CAT_TITLE(star_str))

	# Draw the key to star symbol sizes
	call star_key (gp, star_str)
end
