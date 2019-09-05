procedure grid_scale (gp, star_str, pl_cnst, racen, deccen,
	scale, size)

include <math.h>
include <gset.h>
include	"skymap.h"

pointer	gp			# Graphics descriptor
pointer	star_str		# Stars markers structure pointer
pointer	pl_cnst			# Plate constants structure pointer
double	racen, deccen		# Plate center in radians
real	scale			# Chart scale in radians/mm
real	size			# Width of chart in radians

real	widrad			# Width of chart in radians
real	widmm			# Width of chart in mm
real	vcx, vcy, dvx, dvy
real	vl, vr, vb, vt
real	wl, wr, wb, wt
real	ar, av
double	mindec, maxdec
double	minra,  maxra
real	xs, ys, xr, yr
int	mirror			# Flip chart?

real	ggetr()

begin
	call gseti (gp, G_WCS, MM_R_WCS)


	# Find the limits in declination
	call alimd (CAT_DEC_VAL(star_str), NUM_CAT_VALS(star_str),
		    mindec, maxdec)
	if (IS_INDEFD(deccen))
	    deccen = (mindec + maxdec) / 2

	# Find the limits in Right Ascension
	if (IS_INDEFD(racen)) {
	    call alimd (CAT_RA_VAL(star_str), NUM_CAT_VALS(star_str),
			minra, maxra)
	    if (abs(maxra-minra) > PI)
		minra = minra + TWOPI
	    racen = (minra + maxra) / 2.d0
	    if (racen > TWOPI)
		racen = racen - TWOPI
	}

	# Projection constants
	call prj_const (pl_cnst, racen, deccen)

	# Device aspect ratio (ar > 1 ==> portrait)
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")
	if (xs == 0)
	    xs = 1.
	if (ys == 0)
	    ys = 1.
	ar = ys / xs

	if (IS_INDEFR(scale) && IS_INDEFR(size)) {
	    # Scale on all data unless it's too wide
	    vl = 0.08;  vr = 0.80
	    vb = 0.05;  vt = 0.95
	    call gsview (gp, vl, vr, vb, vt)
	    widmm  = (vt - vb) * ys * 1000.0

	    if (IS_INDEFD(mindec))
		# Find the limits in declination
		call alimd (CAT_DEC_VAL(star_str), NUM_CAT_VALS(star_str),
		    mindec, maxdec)

	    # The width is the range in declination
	    widrad = maxdec - mindec

	    # Plus a 10% border
	    widrad = widrad + widrad * 0.1

	    if (RADTODEG(widrad) > 80.0)
		call error (0, "Chart is too wide")
	}

	if (IS_INDEFR(scale) && !IS_INDEFR(size)) {
	    # Set size to fill viewport and find chart scale ("/mm)
	    # Set the plot viewport (leave margins)
            vl = 0.08;  vr = 0.80
	    vb = 0.05;  vt = 0.95
	    call gsview (gp, vl, vr, vb, vt)
	    widmm  = (vt - vb) * ys * 1000.0
	    widrad = size
	}

	if (!IS_INDEFR(scale) && IS_INDEFR(size)) {
	    # Set size to fill viewport and find chart size (mm)
            vl = 0.08;  vr = 0.80
	    vb = 0.05;  vt = 0.95
	    call gsview (gp, vl, vr, vb, vt)
	    widmm  = (vt - vb) * ys * 1000.0
	    widrad = scale * widmm
	}

	if (!IS_INDEFR(scale) && !IS_INDEFR(size)) {
	    # Set plot size to chart scale and width
	    vcx = 0.44;  vcy = 0.50

	    widrad = size		# Height in radians
	    widmm  = size / scale	# Height in mm
	    yr = widmm / (ys * 1000.0)	# Height in NDC
	    xr = widmm / (xs * 1000.0)	# Width in NDC

	    dvx = xr / 2.0
	    dvy = yr / 2.0

	    vl = vcx - dvx;  vr = vcx + dvx
	    vb = vcy - dvy;  vt = vcy + dvy

	    if (vl < 0.0 || vr > 1.0 || vb < 0.0 || vt > 1.0)
		call error (0, "chart is too big at this scale")

	    call gsview (gp, vl, vr, vb, vt)
	}

	# Plate scale in radians/mm
	PLATE_SCALE(pl_cnst) = double (widrad / widmm)

	mirror = MIRROR_FLIP(pl_cnst)

	av = (vt - vb) / (vr - vl)
	wt = widmm / 2.0;   wb = -wt
	wl = wt / ar / av;  wr = -wl

	# Set the plot window for WC in mm same sense as R.A.
	if (mirror == YES)
	    call gswind (gp, wr, wl, wb, wt)
	else
	    call gswind (gp, wl, wr, wb, wt)

	# Set the plot window for WC in mm always increasing right
	call gseti  (gp, G_WCS, MM_F_WCS)
	call gsview (gp, vl, vr, vb, vt)
	call gswind (gp, wr, wl, wb, wt)

	# Set the plot window for WC in sec arc increasing right
	call gseti  (gp, G_WCS, SEC_WCS)
	call gsview (gp, vl, vr, vb, vt)
	wr = RADTOSA(widrad);  wl = -wr
	wb = wl;  wt = wr
	call gswind (gp, wl, wr, wb, wt)
end
