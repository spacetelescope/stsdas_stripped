procedure chart_grid (gp, pl_cnst)

include <math.h>
include <gset.h>
include	<psiescape.h>
include	"skymap.h"

pointer	gp
pointer	pl_cnst

# Spherical celestial coordinates
double	racen, deccen			# Coordinates of chart center
double	nera,  nwra,  sera,  swra	# RA of plot Corners
double	nedec, nwdec, sedec, swdec	# Dec of plot corners
double	ra, dec
double	tcra, tcdec, bcra, bcdec	# Center coordinates
double	ramax,  ramin
double	decmax, decmin
double	raint,  raran,  rasec,  rasr,  rexgap,  rainc
double	decint, decran, decsec, decsr, dexgap
double	delra
double	sgn
double	temp

# Cartesian plate (plot) coordinates
real	x, x1, x2, xpole
real	y, y1, y2, ypole
real	left, right, bottom, top	# Plot corners

bool	polar, nearpole
bool	donext, alternate
double	decchk
pointer	sp, label, units

double	escale, nscale, sscale
real	xleast, xmost, xrange
real	yleast, ymost, yrange
int	linra, lindec
real	edge
bool	north

short	txtspc

# Declination intervals in seconds of arc

#define  NDECGAP         12
#double  decgap[NDECGAP]
#               1"   6"   12"   30"   1'    6'     12'    30'    1d
#               2d     5d     10d
#data    decgap /1.0, 6.0, 12.0, 30.0, 60.0, 360.0, 720.0, 1.8e3, 3.6e3,
#		7.2e3, 1.8e4, 3.6e4/

# Modified table of declination intervals, 3 Dec 90, Z.G. Levay
define	NDECGAP		16
double	decgap[NDECGAP]
#               1"   2"   5"   10"   20"   30"   1'    2'     5'
#               10'    20'    30'    1d     2d     5d     10d
data	decgap /1.0, 2.0, 5.0, 10.0, 20.0, 30.0, 60.0, 120.0, 300.0,
                600.0, 1.2e3, 1.8e3, 3.6e3, 7.2e3, 1.8e4, 3.6e4/

# R.A. intervals in seconds of time
define	NRAGAP		13
double	ragap[NRAGAP]
#              1s   2s   5s   10s   20s   30s   1m    2m     5m     10m
#	       20m    30m    1h
data	ragap /1.0, 2.0, 5.0, 10.0, 20.0, 30.0, 60.0, 120.0, 300.0, 600.0, 
	       1.2e3, 1.8e3, 3.6e3/

double	cha_near(), round_up()
bool	fp_equald()

begin 
	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)

	# RA, Dec of plot center in radians
	racen  = CEN_RA(pl_cnst)
	deccen = CEN_DEC(pl_cnst)

	north = deccen >= 0.0d0

	# Chart corners in mm
	call gseti  (gp, G_WCS, MM_R_WCS)
	call ggwind (gp, left, right, bottom, top)

	if (MIRROR_FLIP(pl_cnst) == YES) {
	    # Flip horizontally
	    temp  = left
	    left  = right
	    right = temp
	}

	# Draw edges of viewport/window
	call gseti  (gp, G_PLTYPE, GL_SOLID)
	call gsetr  (gp, G_PLWIDTH, 2.0)
	call gamove (gp, left,  bottom)
	call gadraw (gp, right, bottom)
	call gadraw (gp, right, top)
	call gadraw (gp, left,  top)
	call gadraw (gp, left,  bottom)

	# Force fixed text spacing (only works with psikern)
	txtspc = NO
	call gescape (gp, PS_VARIABLE_SPACE, txtspc, 1)

	# Is the pole on the chart?

	if (fp_equald (deccen, 0.0d0))
	    sgn = HALFPI
	else
	    sgn = HALFPI * deccen / abs (deccen)

	call sph_cart (racen, sgn, xpole, ypole, pl_cnst)

	polar = xpole <= left && 
		xpole >= right && 
		ypole >= bottom && 
		ypole <= top

	# RA, Dec of each corner 
	call cart_sph (left,  bottom, sera, sedec, pl_cnst)
	call cart_sph (right, bottom, swra, swdec, pl_cnst)
	call cart_sph (left,  top,    nera, nedec, pl_cnst)
	call cart_sph (right, top,    nwra, nwdec, pl_cnst)

	# Is it nearly polar?
	nearpole = max (abs (sedec), abs (nedec)) >= ALMOST_POLE || polar

	# Adjust for the Case where Chart Straddles 0H. RA
	if (nwra > nera)
	    nwra = nwra - TWOPI
	if (swra > sera)
	    swra = swra - TWOPI

	if (polar) {
	    # Pole is on the chart
	    call gmark (gp, xpole, ypole, GM_PLUS, 3.0, 3.0)

	    ramin = 0.0
	    ramax = TWOPI

	    if (north) {
		decmin = min (nwdec, swdec)
		decmax = HALFPI - (HALFPI - decmin) * 0.1d0
	    } else {
		# South
		decmax = max (nwdec, swdec)
		decmin = -HALFPI - (-HALFPI - decmax) * 0.1d0
	    }

	    raran  = ramax  - ramin
	    decran = decmax - decmin

	} else {
	    # Pole is outside the chart
	    # Find Extreme Dec. on Chart
	    call cart_sph (0.0, top, tcra, tcdec, pl_cnst)
	    call cart_sph (0.0, bottom, bcra, bcdec, pl_cnst)

	    # Range and extremes of RA in radians
	    # bearing in mind that the plot may be reversed
	    if (north) {
		ramax = nera
		ramin = nwra
	    } else {
		# South
		ramax = sera
		ramin = swra
	    }

	    raran = ramax - ramin

	    # Dec. Range and Extremes
	    if (tcdec < 0.0d0)
		# Top South
		decmax = nwdec
	    else
		# Top North
		decmax = tcdec

	    if (bcdec < 0.0d0)
		# Bottom South
		decmin = bcdec
	    else
		# Bottom North
		decmin = swdec

	    decran = decmax - decmin
	# Polar?
	}

	# Use dotted lines for grid
	call gseti (gp, G_PLTYPE, GL_DOTTED)
	call gseti (gp, G_CLIP, YES)
	call gsetr (gp, G_PLWIDTH, 1.0)
	call gsetr (gp, G_TXSIZE, 0.85)

	# Grid-Line Interval in RA (in Seconds of Time)
	rasr = RADTOST(raran)
	rexgap = rasr / RA_NUM_TRY

	# Rounded RA interval in seconds of time
	raint = cha_near (rexgap, ragap, NRAGAP)
	rasec = mod (round_up (RADTOST(ramin), raint), double (STPERDAY))

	repeat {
	    # Plot the Meridians of Equal RA
	    ra = STTORAD(rasec)
	    call sph_cart (ra, decmax, x1, y1, pl_cnst)
	    call gamove (gp, x1, y1)

	    call sph_cart (ra, decmin, x2, y2, pl_cnst)
	    call gadraw (gp, x2, y2)

	    rasec = rasec + raint

	} until (STTORAD(rasec) > ramax)

	# Choose Grid-Line Gap for Dec.  (in seconds of arc)
	decsr  = RADTOSA(decran)
	dexgap = decsr / DEC_NUM_TRY

	# Find 'Rounded' Figure for Dec Gap
	decint = cha_near (dexgap, decgap, NDECGAP)

	# Starting line of declination
	decsec = round_up (RADTOSA(decmin), decint)

	ramax = ramax + rainc / 2.0
	delra = 1.0 / RA_INCR

	repeat {
	    # For each line of constant declination
	    dec = SATORAD(decsec)
	    ra  = ramin
	    call sph_cart (ra, dec, x, y, pl_cnst)
	    call gamove (gp, x, y)

	    # RA increment per polyline segment along lines of declination
	    # is based on the width of the side with least RA range and the 
	    # declination
	    if (polar)
		rainc = decran * delra
	    else if (north)
		rainc = (sera - swra) * delra
	    else
		# South
		rainc = (nera - nwra) * delra

	    repeat {
		# For each segment in the polyline
		ra = ra + rainc
		call sph_cart (ra, dec, x, y, pl_cnst)
		call gadraw (gp, x, y)
	    } until (ra > ramax) 

	    decsec = decsec + decint

	} until (SATORAD(decsec) > decmax)


	# Get Scale of E,W Sides (ESCALE)
	# and N,S,Sides (NSCALE,SSCALE)
	# in radians/mm.

	escale = (nedec - sedec) / (top - bottom)
	nscale = (nera 	- nwra)  / (left - right)
	sscale = (sera  - swra)  / (left - right)

#	if (raint < 60.0)
#	    raint = 60.0

	#   1.  Mark Dec. Scales up Sides
	#       If near the Pole,due to Non-linear Side Dec Scale,
	#       Marks are not down both edges,but on Centre Meridian
	#       Only.

	yleast = top
	ymost  = bottom
	lindec = 0

	if (nearpole) {
	    decsec = round_up (RADTOSA(decmin), decint)
	    decchk = decmax
	} else {
	    decsec = round_up (RADTOSA(sedec), decint)
	    decchk = nedec
	}

	repeat {
	    dec = SATORAD(decsec)
	    if (nearpole) 
		call sph_cart (racen, dec, x, y, pl_cnst)
	    else
		y = ((dec - sedec) / escale) - top

	    decsec = decsec + decint
	    lindec = lindec + 1

	    if (y < yleast)
		yleast= y
	    if (y > ymost)
		ymost = y

	} until (SATORAD(decsec) > decchk)

	yrange = ymost - yleast
#	if ((lindec >= 5) && (sizemm < 100.0)) {
	if (lindec >= 5) {
	    alternate = true
	    donext = true
	    if ((lindec/2*2) != lindec)
		lindec = lindec + 1
	    lindec = lindec / 2
	} else {
	    alternate = false
	    donext = true
	}

	if (nearpole) 
	    decsec = round_up (RADTOSA(decmin), decint)
	else
	    decsec = round_up (RADTOSA(sedec), decint)

	edge = (left - right) / EDGE_FACTOR

	repeat {
	    dec = SATORAD(decsec)

	    if (!alternate)
		donext = true

	    if (nearpole) {
		call sph_cart (racen, dec, x, y, pl_cnst)
		if (abs (y) <= top) 
		    if (donext) {
			call rad_dms (dec, Memc[label], Memc[units], SZ_LINE)
			call gtext (gp, x, y, Memc[label], "h=c;v=c")
			call gtext (gp, x, y, Memc[units], "h=c;v=b")
			donext = false
		    } else
			donext = true
	    } else {
		if (MIRROR_FLIP(pl_cnst) == YES)
		    x = right
		else
		    x = left

		y = ((dec - sedec) / escale) - top

		if (donext) {
		    call rad_dms (dec, Memc[label], Memc[units], SZ_LINE)
		    call gtext (gp, x, y, Memc[label], "h=r;v=c")
		    call gtext (gp, x, y, Memc[units], "h=r;v=b")
		    donext = false
		} else
		    donext = true
	    }

	    decsec = decsec + decint

	} until (SATORAD(decsec) > decchk)


	if (nera - nwra < (7.0/6.0)*HALFPI) {
	    #   2.   Mark RA Points on the N Side
	    #        Only if the Range in RA is Reasonable (<7H. R.A.)

	    linra  = 0
	    xleast = left
	    xmost  = right
	    rasec  = mod (round_up (RADTOST(nwra), raint), double (STPERDAY))

	    repeat {
		ra = STTORAD(rasec)
		x = ((ra - nwra) / nscale) + right
		xleast = min (x, xleast)
		xmost  = max (x, xmost)
		linra = linra + 1
		rasec = rasec + raint

	    } until (STTORAD(rasec) > nera)

	    xrange = xmost - xleast
	    if (linra >= 5) {
		alternate = true
		donext = true
		if ((linra/2*2) != linra)
		    linra = linra + 1
		else
		    xrange = xrange * (real (linra) - 2.0) / 
			(real (linra) - 1.0)

		linra = linra / 2
	    } else {
		alternate = false
		donext = true
	    }

	    edge = (top - bottom) / EDGE_FACTOR
	    y = top + edge
	    rasec = mod (round_up (RADTOST(nwra), raint), double (STPERDAY))

	    repeat {
		ra = STTORAD(rasec)
		x = ((ra - nwra) / nscale) + right
		if (!alternate)
		    donext = true
		if (donext) {
		    call rad_hms (ra, Memc[label], Memc[units], SZ_LINE)
		    call gtext (gp, x, y, Memc[label], "h=c;v=c")
		    call gtext (gp, x, y, Memc[units], "h=c;v=b")
		    donext = false
		} else
		    donext = true

		rasec = rasec + raint

	    } until (STTORAD(rasec) > nera)
	}


	if (sera - swra <= (7.0/6.0)*HALFPI) {
	    #   3.  Mark RA on Southern Side
	    #       only if the RA Range is Reasonable (< 7H RA )

	    linra  = 0
	    xleast = left
	    xmost  = right
	    rasec  =  mod (round_up (RADTOST(swra), raint), double (STPERDAY))

	    repeat {
		ra = STTORAD(rasec)
		x  = ((ra - swra) / sscale) + right

		if (x < xleast)
		    xleast = x
		if (x.gt.xmost)
		    xmost = x

		linra = linra + 1
		rasec = rasec + raint

	    } until (STTORAD(rasec) >= sera)

	    xrange = xmost - xleast
	    if (linra >= 5) {
		alternate = true
		donext = true
		if ((linra / 2 * 2) != linra)
		    linra = linra+1
		else
		    xrange = xrange * (real (linra) - 2.0) / 
			(real (linra) - 1.0)

		linra = linra / 2
	    } else {
		alternate = false
		donext = true
	    }

	    y = bottom - edge
	    rasec = mod (round_up (RADTOST(swra), raint), double(STPERDAY))

	    repeat {

		ra = STTORAD(rasec)
		x  = ((ra - swra) / sscale) + right

		if (!alternate)
		    donext = true

		if (donext) {
		    call rad_hms (ra, Memc[label], Memc[units], SZ_LINE)
		    call gtext (gp, x, y, Memc[label], "h=c;v=t")
		    call gtext (gp, x, y, Memc[units], "h=c;v=c")
		    donext = false
		} else
		    donext = true

		rasec = rasec + raint

	    } until (STTORAD(rasec) > sera)
	}

	call sfree (sp)
end
