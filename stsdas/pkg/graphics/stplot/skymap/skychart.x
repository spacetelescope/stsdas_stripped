procedure sky_chart (gp, pl_cnst, star_str, 
	racen, deccen, scale, size)

include	"skymap.h"

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer
double	racen, deccen		# Chart center coordinates in radians
real	scale			# Chart scale in radians/mm
real	size			# Width of chart in radians

begin
	# Start a new page
	call gframe (gp)

	# Scale the plot
	call grid_scale (gp, star_str, pl_cnst, racen, deccen,
	    scale, size)

	# Draw the grid and labels in celestial coordinates
	call chart_grid (gp, pl_cnst)

	# Draw a symbol for each star
	call draw_stars (gp, pl_cnst, star_str, 
	    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
	    CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
	    NUM_CAT_VALS(star_str))
end
