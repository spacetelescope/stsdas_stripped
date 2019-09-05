procedure ch_legend (gp, pl_cnst)

include <math.h>
include <gset.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst

double	scale
real	wl, wr, wb, wt
real	vl, vr, vb, vt
real	x, y
real	y1, y2
real	dx, dy
double	dec, ddec, dc_gap
double	dec_scale		# Size of scale marker in "arc
pointer	sp, label, units
real	width			# Chart size in radians
int	line
short	txtspc

# Declination intervals in seconds of arc
define	NDECGAP		17
double	decgap[NDECGAP]
#               1"   2"   5"   10"   15"   30"   1'    2'     5'     10'
#               15'    30'    1d     2d     5d     10d    20d
data	decgap /1.0, 2.0, 5.0, 10.0, 15.0, 30.0, 60.0, 120.0, 300.0, 600.0, 
                900.0, 1.8d3, 3.6d3, 7.2d3, 1.8d4, 3.6d4, 7.2d4/ 

string	text_format "h=l;v=c;q=h"

double	cha_near()

begin
	call gseti  (gp, G_WCS, MM_F_WCS)
	call ggview (gp, vl, vr, vb, vt)
	call ggwind (gp, wl, wr, wb, wt)

	scale     = PLATE_SCALE(pl_cnst) * double (wt - wb)	# Radians
	dc_gap    = RADTOSA(scale) / double (DEC_NUM_TRY + 2)
	dec_scale = cha_near (dc_gap, decgap, NDECGAP)		# "arc

	ddec = SATORAD(dec_scale) / 2
	dec  = CEN_DEC(pl_cnst)
	call sph_cart (CEN_RA(pl_cnst), dec-ddec, x, y1, pl_cnst)
	call sph_cart (CEN_RA(pl_cnst), dec+ddec, x, y2, pl_cnst)

	call ggscale (gp, x, y, dx, dy)
	x = wr + 0.02 * dx

	dy = abs (y2 - y1)
	y  = (y1 + y2) / 2.0

	call gsetr (gp, G_TXSIZE, 1.0)
	call gseti (gp, G_CLIP, NO)
	call gsetr (gp, G_PLWIDTH, 1.0)

	call gmark (gp, x, y, GM_VEBAR, 2.0, -dy)

	call smark  (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)

	# Force fixed text spacing (only works with psikern)
	txtspc = 0
	call gescape (gp, 12, txtspc, 1)

	if (dec_scale >= 3600.0) {
	    # Degrees
	    call sprintf (Memc[label], SZ_LINE, " %2d")
		call pargi (int (dec_scale / 3600.0d0))
	    call gtext (gp, x, y, "   o", "v=b;h=l")

	} else if (dec_scale >= 60.0) {
	    # Minutes
	    call sprintf (Memc[label], SZ_LINE, " %.0f\'")
		call pargd (dec_scale / 60.0d0)

	} else {
	    # Seconds
	    call sprintf (Memc[label], SZ_LINE, " %.0f\"")
		call pargd (dec_scale)
	}

	call gtext (gp, x, y, Memc[label], "v=c;h=l")

	call gseti  (gp, G_WCS, LEGEND_WCS)

	# Force variable text spacing (only works with psikern)
	txtspc = 1
	call gescape (gp, 12, txtspc, 1)

	# Chart center RA and Dec
	line = 1
	call chtext (gp, line, "Center:", NORMAL_SCRIPT)

	# Force fixed text spacing (only works with psikern)
	txtspc = 0
	call gescape (gp, 12, txtspc, 1)

	line = line + 1
	call rad_hms (CEN_RA(pl_cnst), Memc[label], Memc[units], SZ_LINE)
	call chtext (gp, line, Memc[label], NORMAL_SCRIPT)
	call chtext (gp, line, Memc[units], SUPER_SCRIPT)

	line = line + 1
	call rad_dms (CEN_DEC(pl_cnst), Memc[label], Memc[units], SZ_LINE)
	call chtext (gp, line, Memc[label], NORMAL_SCRIPT)
	call chtext (gp, line, Memc[units], SUPER_SCRIPT)

	# Force variable text spacing (only works with psikern)
	txtspc = 1
	call gescape (gp, 12, txtspc, 1)

	# Chart scale in "/mm
	line = line + 2
	call chtext (gp, line, "Scale:", NORMAL_SCRIPT)
	call sprintf (Memc[label], SZ_LINE, "%.4g\"/mm")
	    call pargd (RADTOSA(PLATE_SCALE(pl_cnst)))

	line = line + 1
	call chtext (gp, line, Memc[label], NORMAL_SCRIPT)

	# Chart width in degrees
	line = line + 2
	call chtext (gp, line, "Width:", NORMAL_SCRIPT)

	# Force fixed text spacing (only works with psikern)
	txtspc = 0
	call gescape (gp, 12, txtspc, 1)

	line = line + 1
	width = (wt - wb) * PLATE_SCALE(pl_cnst)	# Radians
	call sprintf (Memc[label], SZ_LINE, "%4.3g")
	    call pargr (RADTODEG(width))		# Degrees
	call chtext (gp, line, Memc[label], NORMAL_SCRIPT)
	call strcpy ("    o", Memc[units], SZ_LINE)
	call chtext (gp, line, Memc[units], SUPER_SCRIPT)

	call sfree (sp)
end
