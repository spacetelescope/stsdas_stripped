# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 

include <gset.h>
include <tbset.h>
include	<psiescape.h>
include	"skymap.h"

#  29 April 1992 Add PostScript style filled-circle symbols with clear
#  border.  ZGL
#  30 April 1992  Add color capability.  Use color task parameter for
#  whole plot, class catalog column for individual symbols.  ZGL
#  5 May 1992  Add escapes for psikern to change text spacing.  ZGL
#  15 Oct 1992  Add escapes to explicitly specify the graphics colormap
#  to facilitate better color mapping for the classes.  ZGL
#  18 June 1993 Add new marker types for objects.
#               Add central coordinate mark
#               Add optional connection between objects


procedure t_skymap ()

# skymap -- Draw a chart, labeled in celestial coordinates, with symbols 
# representing objects of differing brightness.

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer

double	racen, deccen		# Chart center
real	scale, size		# Chart scale
pointer	sp
pointer	catalog			# Catalog table name
pointer	colnames		# Column names
pointer	format			# Text format
int	minrow, maxrow		# Range of table rows
real	faint, bright		# Magnitude limits
double	ramin, decmin	 	# Coordinate limits
double	ramax, decmax		# Coordinate limits
bool	append			# Append to existing plot?
bool	drawgrd			# Draw coordinage grid?
bool	drawkey			# Draw brightness scale?
bool	drawstr			# Draw stars?
bool	mirror			# Flip chart?
int	color			# Color index of plot
bool	center			# Draw central marker?
bool	connect			# Connect the dots?

short	psgrlutr[17]
short	psgrlutg[17]
short	psgrlutb[17]

#  The following is copied from "stplot$psikern/psiescape.h"
#  It's here explicitly because of Tables.

data	psgrlutr /  255, 0, 255, 255, 255, 255, 255, 255, 170,
                   85,   0,   0,   0,   0,   0,   0,   0 /
data	psgrlutg /  255, 0,   255, 0,   0,  85, 170, 255, 255,
                  255, 255, 255, 255, 255, 170,  85,   0 /
data	psgrlutb /  255, 0, 255, 255,   0,   0,   0,   0,   0,
                    0,   0,  85, 170, 255, 255, 255, 255 /

bool	clgetb(), sky_open()

begin
	call smark  (sp)
	call salloc (catalog,  SZ_FNAME, TY_CHAR)
	call salloc (colnames, SZ_COLNAME*NUM_COLS, TY_CHAR)
	call salloc (format,   SZ_LINE, TY_CHAR)

	# Allocate the structures
	append = sky_open (gp, pl_cnst, star_str)

	# Read the catalog parameters
	call cat_parms (Memc[catalog], Memc[colnames], SZ_FNAME, 
	    minrow, maxrow, faint, bright, ramin, ramax, decmin, decmax)

	BRIGHT_CAT_MAG(star_str) = bright
	FAINT_CAT_MAG(star_str)  = faint

	# Read the chart parameters
	call sky_parms (racen, deccen, scale, size, mirror, color,
	    CEN_MARK_SYMB(star_str), CEN_MARK_SIZE(star_str),
	    CONN_STYLE(star_str))

	if (mirror)
	    MIRROR_FLIP(pl_cnst) = YES
	else
	    MIRROR_FLIP(pl_cnst) = NO

	# Force color map (psikern only, of course)
	call gescape (gp, PS_GR_RED_LUT, psgrlutr, 17)
	call gescape (gp, PS_GR_GREEN_LUT, psgrlutg, 17)
	call gescape (gp, PS_GR_BLUE_LUT, psgrlutb, 17)

	# Color for entire plot
	COLOR_INDEX(star_str) = color
	call gseti (gp, G_PLCOLOR, color)
	call gseti (gp, G_PMCOLOR, color)
	call gseti (gp, G_FACOLOR, color)
	call gseti (gp, G_TXCOLOR, color)

#	call gseti (gp, G_TXSPACING, -0.25)

	call clgstr ("title", CAT_TITLE(star_str), SZ_FNAME)
	if (CAT_TITLE(star_str) == EOS)
	    call strcpy (Memc[catalog], CAT_TITLE(star_str), SZ_FNAME)

	drawstr = clgetb ("stars")
	drawgrd = clgetb ("grid")
	drawkey = clgetb ("key")
	center  = clgetb ("center")
	connect = clgetb ("connect")

	# Read the catalog data
	call chart_stars (star_str, Memc[catalog], Memc[colnames],
	    minrow, maxrow, faint, bright, ramin, decmin, ramax, decmax)

	# Find the transformation between magnitude and spot size
	call mag_scale (gp, star_str, CAT_MAG_VAL(star_str), 
	    NUM_CAT_VALS(star_str), faint, bright)

	# Scale the plot
	call grid_scale (gp, star_str, pl_cnst, racen, deccen,
	    scale, size)

	# Draw the grid and labels in celestial coordinates
	if (drawgrd)
	    call chart_grid (gp, pl_cnst)

	# Draw a symbol for each star
	if (drawstr)
	    call draw_stars (gp, pl_cnst, star_str, 
		CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
		CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
		NUM_CAT_VALS(star_str))

	call gseti (gp, G_PLCOLOR, color)
	call gseti (gp, G_PMCOLOR, color)
	call gseti (gp, G_FACOLOR, color)
	call gseti (gp, G_TXCOLOR, color)

	if (connect)
	    # Draw lines between objects
	    call draw_connect (gp, pl_cnst, star_str,
		CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
		NUM_CAT_VALS(star_str))

	if (center)
	    call draw_cm (gp, star_str, pl_cnst,
		CEN_MARK_SYMB(star_str), CEN_MARK_SIZE(star_str))

	# Draw the scale, mag. key, and title
	if (drawkey)
	    call sky_legend (gp, star_str, pl_cnst)

	# Draw name labels
	if (clgetb ("label")) {
	    call clgstr ("format", Memc[format], SZ_LINE)
	    call draw_labels (gp, pl_cnst, star_str, 
		CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
		CAT_MAG_VAL(star_str), CAT_NAME(star_str),
		NUM_CAT_VALS(star_str),
		CAT_NAME_SIZE(star_str), Memc[format])
	}

	# Interactive
	if (clgetb ("interactive"))
	    call cur_grid (gp, pl_cnst, star_str)

	call sfree (sp)

	# Deallocate the memory and structures
	call sky_close (gp, pl_cnst, star_str)
end






















