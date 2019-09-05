procedure cur_grid (gp, pl_cnst, star_str)

include <math.h>
include <gset.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer

real	wx, wy
int	wcs, key
pointer	sp, text, command
double	alpha, delta		# Chart center in radians
real	scale			# Chart scale in radians/mm
real	size			# Width of chart in radians
real	wl, wr, wb, wt		# WCS window
int	ip
real	magnify
real	m1, m2
pointer	labfmt			# Label text format
real	box, tbox		# Full width of search box (NDC)

data	box /MAX_SIZE/

string	keys_file	"stsdaslib$scr/skymap.key"
string	help_prompt	"sky map"
string	curpar		"coords"
string	def_fmt		"s:.75"

int	clgcur(), ctod(), ctowrd(), ctor()

begin
	call smark  (sp)
	call salloc (text,    SZ_LINE, TY_CHAR)
	call salloc (command, SZ_LINE, TY_CHAR)
	call salloc (labfmt,  SZ_LINE, TY_CHAR)

	call strcpy (def_fmt, Memc[labfmt], SZ_LINE]

	# Plate corners in mm
	call gseti  (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, wl, wr, wb, wt)

	# This is a kludge to force all polylines and polymarkers,
	# including text, to plot correctly
	call gline  (gp, wl, wb, wr, wb)

	while (clgcur (curpar, wx, wy, wcs, key, Memc[text], SZ_LINE) != EOF) {
	    if (key == 'q')
		break

	    if (wcs != MM_R_WCS)
		# Make sure we have the appropriate WCS
		call gctran (gp, wx, wy, wx, wy, wcs, MM_R_WCS)

	    # Find celestial coordinates of cursor position
	    call cart_sph (wx, wy, alpha, delta, pl_cnst)

	    # Plate corners in mm
	    call gseti  (gp, G_WCS, MM_R_WCS)
	    call ggwind (gp, wl, wr, wb, wt)
	    call gline  (gp, wl, wb, wr, wb)

	    # Chart scale in radians/mm
	    scale = PLATE_SCALE(pl_cnst)

	    # Chart size in radians
	    size = scale * (wt - wb)

	    switch (key) {
	    case 'c':
		# Label in celestial coordinates (keep the same scaling)
		# Draw the grid and labels in celestial coordinates

		# Start a new page
		call gframe (gp)

		# Draw the celestial coordinate grid
		call chart_grid (gp, pl_cnst)

		# Draw a symbol for each star
		call draw_stars (gp, pl_cnst, star_str, 
		    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
		    CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
		    NUM_CAT_VALS(star_str))

	    case 'f':
		# Flip chart horizontally
		if (MIRROR_FLIP(pl_cnst) == YES)
		    MIRROR_FLIP(pl_cnst) = NO
		else
		    MIRROR_FLIP(pl_cnst) = YES

		# Don't change the scale
		size = INDEFR

		# Don't move the center
		alpha = CEN_RA(pl_cnst)
		delta = CEN_DEC(pl_cnst)

		# Scale and draw the chart and stars
		call sky_chart (gp, pl_cnst, star_str, 
		    alpha, delta, scale, size)

	    case 'k':
		# Draw the scale, mag. key, and title
		call sky_legend (gp, star_str, pl_cnst)

	    case 'l':
		call draw_labels (gp, pl_cnst, star_str, 
		    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
		    CAT_MAG_VAL(star_str), CAT_NAME(star_str),
		    NUM_CAT_VALS(star_str),
		    CAT_NAME_SIZE(star_str), Memc[labfmt])

	    case 'm':
		# Move coordinate at cursor to plate center
		size = INDEFR

		# Scale and draw the chart and stars
		call sky_chart (gp, pl_cnst, star_str, 
		    alpha, delta, scale, size)
		
		# Warp the cursor to the reference coordinate (center)
		call gscur (gp, 0.0, 0.0)

	    case 'n':
		# Print catalog around cursor
		call prt_near (gp, pl_cnst, star_str, wx, wy, box)

	    case 'o':
		# Print center symbol
		call draw_cm (gp, star_str, pl_cnst,
		    CEN_MARK_SYMB(star_str), CEN_MARK_SIZE(star_str))

	    case 'p':
		# Zoom out:  replot at larger scale
		scale = 2.0 * scale
		size  = INDEFR

		# Scale and draw the chart and stars
		call sky_chart (gp, pl_cnst, star_str, 
		    alpha, delta, scale, size)
		
		# Warp the cursor to the reference coordinate (center)
		call gscur (gp, 0.0, 0.0)

	    case 'r':
		# Label the chart in mm from center (keep the same scaling)
		call lin_label (gp, pl_cnst, star_str, MM_F_WCS)

	    case 's':
		# Label the chart in "arc from center (keep the same scaling)
		call lin_label (gp, pl_cnst, star_str, SEC_WCS)

	    case 'v':
		# Connect the dots
		if (CONN_STYLE(star_str) != 0)
		    call draw_connect (gp, pl_cnst, star_str,
			CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
			NUM_CAT_VALS(star_str))

	    case 'z':
		# Zoom in:  replot at smaller scale
		scale = scale / 2.0
		size  = INDEFR

		# Scale and draw the chart and stars
		call sky_chart (gp, pl_cnst, star_str, 
		    alpha, delta, scale, size)
		
		# Warp the cursor to the reference coordinate (center)
		call gscur (gp, 0.0, 0.0)

	    case '?':
		# Page the cursor keys help text
		call gpagefile (gp, keys_file, help_prompt)

	    case ':':
		# Colon command
		ip = text
		if (ctowrd (Memc, ip, Memc[command], SZ_LINE) <= 0)
		    next

		switch (Memc[command]) {
		case 'b':
		    # Set or show the range of plotted brightness
		    if (ctor (Memc, ip, m1) <= 0) {
			# Show the current range
			call printf ("Brightness range:  %g -- %g mag\n")
			    call pargr (BRIGHT_PLOT_MAG(star_str))
			    call pargr (FAINT_PLOT_MAG(star_str))
			next
		    }

		    if (ctor (Memc, ip, m2) <= 0)
			next

		    BRIGHT_PLOT_MAG(star_str) = min (m1, m2)
		    FAINT_PLOT_MAG(star_str)  = max (m1, m2)

		    # Start a new page
		    call gframe (gp)

		    # Draw the celestial coordinate grid
		    call chart_grid (gp, pl_cnst)

		    # Draw a symbol for each star
		    call draw_stars (gp, pl_cnst, star_str, 
			CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
			CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
			NUM_CAT_VALS(star_str))

		case 'l':
		    # Label with the name column
		    # Get format string from command line
		    if (ctowrd (Memc, ip, Memc[labfmt], SZ_LINE) <= 0)
			# Null format, use default
			call strcpy (def_fmt, Memc[labfmt], SZ_LINE)

		    call draw_labels (gp, pl_cnst, star_str, 
			CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
			CAT_MAG_VAL(star_str), CAT_NAME(star_str),
			NUM_CAT_VALS(star_str),
			CAT_NAME_SIZE(star_str), Memc[labfmt])

		case 'm':
		    # Move to a different center altogether
		    if (ctod (Memc, ip, alpha) > 0)
			alpha = HRSTORAD(alpha)
			if (ctod (Memc, ip, delta) > 0)
			    delta = DEGTORAD(delta)
		    size = INDEFR

		    # Scale and draw the chart and stars
		    call sky_chart (gp, pl_cnst, star_str, 
			alpha, delta, scale, size)
		
		    # Warp the cursor to the reference coordinate (center)
		    call gscur (gp, 0.0, 0.0)

		case 'n':
		    # Set or show search box size
		    if (ctor (Memc, ip, tbox) > 0) {
			# Set the box size
			if (tbox > 0)
			    box = tbox * MAX_SIZE
		    } else {
			# Show the box size
			tbox = RADTOSA(size * box)
			call printf ("Search box width %g (%g arcsec)\n")
			    call pargr (box / MAX_SIZE)
			    call pargr (tbox)
		    }

		case 's':
		    # Change the plate scale (read "/mm)
		    # and move the center to the coordinates at the cursor

		    if (ctor (Memc, ip, scale) > 0) {
			scale = SATORAD(scale)	# Rad/mm
			size  = INDEFR

			# Scale and draw the chart and stars
			call sky_chart (gp, pl_cnst, star_str, 
			    alpha, delta, scale, size)
		    }
		
		    # Warp the cursor to the reference coordinate (center)
		    call gscur (gp, 0.0, 0.0)

		case 'w':
		    # Change the plate size (read degrees)
		    # and move the center to the coordinates at the cursor

		    if (ctor (Memc, ip, size) > 0) {
			scale = INDEFR
			size  = DEGTORAD(size)

			# Scale and draw the chart and stars
			call sky_chart (gp, pl_cnst, star_str, 
			    alpha, delta, scale, size)
		    }
		
		    # Warp the cursor to the reference coordinate (center)
		    call gscur (gp, 0.0, 0.0)

		case 'z':
		    # Zoom or unzoom (read magnification)

		    if (ctor (Memc, ip, magnify) > 0) {
			scale = scale / magnify
			size  = INDEFR

			# Scale and draw the chart and stars
			call sky_chart (gp, pl_cnst, star_str, 
			    alpha, delta, scale, size)
		    }
		
		    # Warp the cursor to the reference coordinate (center)
		    call gscur (gp, 0.0, 0.0)
		}

	    default:
		# Any other key;  Echo the coordinates
		call prt_coords (alpha, delta)
	    }
	}

	call sfree (sp)
end
