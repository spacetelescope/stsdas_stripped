procedure draw_labels (gp, pl_cnst, star_str, ra, dec, mag, name,
	numstars, namsiz, format)

include <ctype.h>
include <gset.h>
include	<psiescape.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst
pointer	star_str			# Stars structure
double	ra[ARB], dec[ARB]
real	mag[ARB]
char	name[namsiz,ARB]		# Labels (star names)
int	namsiz				# Size of strings
int	numstars
char	format[ARB]			# String format

int	row
real	wl, wr, wb, wt
double	r, d
real	m
double	dot
int	ip
real	size, xs, ys
real	xc, yc, x, y
real	wcs_ndc_x, wcs_ndc_y
short	txtspc

begin
	call gseti  (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, wl, wr, wb, wt)

	# Change the line width.  To force the change, draw a non-existant
	# line.
	call gsetr  (gp, G_PLWIDTH, 1.0)
	call gseti (gp, G_CLIP, NO)
	xs = abs (wl) + abs (wr)
	ys = abs (wb) + abs (wt)
	call gline  (gp, xs, ys, xs, ys)
	call gseti (gp, G_CLIP, YES)

	# Force proportional spacing (only works with psikern)
	txtspc = YES
	call gescape (gp, PS_VARIABLE_SPACE, txtspc, 1)	# PS_VARIABLE_SPACE

	do row = 1, numstars {
	    # For each object in catalog
	    r = ra[row];  d = dec[row];  m = mag[row]

	    # Find dot product between chart center and position
	    dot = cos (r) * cos (d) * COSA_COSD(pl_cnst) + 
		  sin (r) * cos (d) * SINA_COSD(pl_cnst) +
		  sin (d) * SIN_D(pl_cnst)

	    if (dot < 0.5)
		# Too far from chart center
		next

	    call sph_cart (r, d, xc, yc, pl_cnst)

	    if (MIRROR_FLIP(pl_cnst) == YES) {
		if (xc < wl || xc > wr || yc < wb || yc > wt)
		    # Object is outside the chart borders
		    next
	    } else {
		if (xc > wl || xc < wr || yc < wb || yc > wt)
		    # Object is outside the chart borders
		    next
	    }

	    # Strip leading blanks
	    for (ip = 1;  IS_WHITE(name[ip,row]);  ip = ip + 1)
		;

            # Dot size in NDC
            size = MAG_SIZE_SLOPE(star_str) * m + MAG_SIZE_INTERC(star_str)
            xs   = size * DEV_ASPECT(star_str)
            ys   = size
 
            # WC/NDC scale
            call ggscale (gp, xc, yc, wcs_ndc_x, wcs_ndc_y)
 
	    # Offset by size of marker
#            x = xc + xs * wcs_ndc_x / 2.0
            x = xc + 2 * xs * wcs_ndc_x
#            y = yc + ys * wcs_ndc_y / 2.0
	    y = yc

	    # Draw the text
	    call gtext (gp, x, y, name[ip,row], format)
	}
end
