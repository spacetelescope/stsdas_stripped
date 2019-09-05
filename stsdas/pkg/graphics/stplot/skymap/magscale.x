procedure mag_scale (gp, star_str magval, numvals, faint, bright)

include <mach.h>
include <math.h>
include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str			# Stars structure
real	magval[ARB]			# Input magnitudes
int	numvals
real	faint, bright			# Input magnitude limits

real	big, small
int	row
real	slope
pointer	sp, instyle, outstyle, stydic
real	xs, ys, yr

int	strdic()
real	ggetr(), clgetr()

begin
	call gseti (gp, G_WCS, MM_R_WCS)

	xs = ggetr (gp, "xs")
#	call eprintf ("%f\n")
#	    call pargr (xs)

	ys = ggetr (gp, "ys")
#	call eprintf ("%f\n")
#	    call pargr (ys)

	if (xs == 0 || ys == 0)
	    DEV_ASPECT(star_str) = 1
	else
	    DEV_ASPECT(star_str) = ys / xs

	yr = ggetr (gp, "yr")
#	call eprintf ("%f\n")
#	    call pargr (yr)

	# Separation between rasters
	LINE_SEP(star_str)   = 1.0 / yr / OVERLAP

	bright = clgetr ("brightstar")
	faint  = clgetr ("faintstar")

	if (IS_INDEFR(bright)) {
	    # Use brightest catalog object
	    bright = MAX_REAL
	    do row = 1, numvals
		bright = min (bright, magval[row])
	}

	if (IS_INDEFR(faint)) {
	    # Use faintest catalog object
	    faint = -MAX_REAL
	    do row = 1, numvals
		faint = max (faint, magval[row])
	}

	if (faint == bright)
	    call error (0, "Can't find brightness scale")

	if (faint < bright) {
	    # Switch faint and bright magnitude limits
	    BRIGHT_PLOT_MAG(star_str) = faint
	    FAINT_PLOT_MAG(star_str)  = bright
	    faint  = FAINT_PLOT_MAG(star_str)
	    bright = BRIGHT_PLOT_MAG(star_str)
	} else {
	    BRIGHT_PLOT_MAG(star_str) = bright
	    FAINT_PLOT_MAG(star_str)  = faint
	}

	small = clgetr ("smallspot")
	big   = clgetr ("bigspot")
	if (IS_INDEFR(small))
	    small = MIN_SIZE
	if (IS_INDEFR(big))
	    big = MAX_SIZE

	slope = (big - small) / (bright - faint)
	MAG_SIZE_SLOPE(star_str)  = slope
	MAG_SIZE_INTERC(star_str) = small - slope * faint

	call smark (sp)

	call salloc (stydic, SZ_LINE, TY_CHAR)
	call clgstr ("objstyle.p_min", Memc[stydic], SZ_LINE)
	call salloc (instyle, SZ_LINE, TY_CHAR)
	call clgstr ("objstyle", Memc[instyle], SZ_LINE)
	call salloc (outstyle, SZ_LINE, TY_CHAR)
	SYMBOL_STYLE(star_str) = strdic (Memc[instyle], 
	    Memc[outstyle], SZ_LINE, Memc[stydic])

	call sfree (sp)
end
