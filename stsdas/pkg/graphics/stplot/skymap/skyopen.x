bool procedure sky_open (gp, pl_cnst, star_str)

include <gset.h>
include	"skymap.h"

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer

pointer	sp, device
real	vl, vr, vb, vt
real	wl, wr, wb, wt
int	mode
bool	append

bool	clgetb()
pointer	gopen()

begin
	call smark (sp)

	call salloc (device, SZ_LINE, TY_CHAR)
	call clgstr ("device", Memc[device], SZ_LINE)

	append = clgetb("append")
	if (append)
	    mode = APPEND
	else
	    mode = NEW_FILE

	# Open graphics
	gp = gopen (Memc[device], mode, STDGRAPH)

	call gseti (gp, G_TXQUALITY, GT_HIGH)

	# Set the plot viewport for the stars key
	call gseti (gp, G_WCS, STAR_KEY_WCS)
	vl = 0.815;  vr = 1.0
	vb = 0.05;   vt = 0.4
	call gsview (gp, vl, vr, vb, vt)
	wl =  0.0;  wr = 1.0
	wt = -0.5;  wb = real (NUM_STARS) + 0.5
	call gswind (gp, wl, wr, wb, wt)

	# Set the plot window/viewport for the legend
	call gseti (gp, G_WCS, LEGEND_WCS)
	vl = 0.815;  vr = 0.99
	vb = 0.5;    vt = 0.95
	call gsview (gp, vl, vr, vb, vt)
	wl = 0.0;  wr = 1.0
	wb = real (TEXT_LINES) + 0.5;  wt = 0.5
	call gswind (gp, wl, wr, wb, wt)

	# Allocate the catalog and star data structure
	call malloc (star_str, LEN_STAR_STR, TY_STRUCT)
	call malloc (CAT_TITLE_P(star_str), SZ_FNAME, TY_CHAR)

	# Allocate the chart (plate) constants structure
	call malloc (pl_cnst, LEN_PL_CNST, TY_STRUCT)

	call sfree (sp)

	return (append)
end
