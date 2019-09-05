include <math.h>
include	"metric.h"

define PC_ROT 0.2878
define WF_ROT 0.05334

#  metric_do -- perform the geometric distortion correction
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  19-Oct-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure metric_do (x0, y0, ax, ay, trans, ipin, ipref, iccd, order, hms, 
			centroid, boxsize, instru,rastr,decstr)

real	x0, y0
real	ax[(MAX_ORDER+1)**2]
real	ay[(MAX_ORDER+1)**2]
real	trans[SZ_TRANS]
pointer	ipin
pointer	ipref
int	iccd, order
bool	hms
bool	centroid
int	boxsize
int	instru
	
real	xc0, yc0, xc, yc
real	dx, dy
real	xch, ych
real	xx, yy
real	rot, xxx, yyy
double	ra, dec
pointer	mw, ct
char	rastr[SZ_LINE], decstr[SZ_LINE]

pointer	mw_openim(), mw_sctran()
#==============================================================================
begin

	# if needs to be centroided?
	if (centroid)
	    call mpc_cntr (ipin, x0, y0, boxsize, xc0, yc0)
	else {
	    xc0 = x0
	    yc0 = y0
	}

	# Shawn Ewald changed his origin to (0.5, 0.5) (2/15/93)
	if (instru == WFPC) {
	    xc = xc0 - 0.5
	    yc = yc0 - 0.5
	} else {
	    xc = xc0
	    yc = yc0
	}
	
	# perform intra-chip corrections
	#if (instru == WFPC) {
	    call legendre (xc, yc, dx, dy, ax, ay, order)
	    xch = xc - dx
	    ych = yc - dy
	#} else {
	    #call rcube (xc, yc, dx, dy, ax, ay, order)
	    #xch = xc + dx
	    #ych = yc + dy
	#}

	# perform inter-chip corrections
	call inter_chip (xch, ych, xx, yy, trans)

	# Shawn Ewald added another global rotation to each camera (2/15/93)
	if (instru == WFPC) {
	    if (iccd > 4) 
	        rot = PC_ROT / RADIAN
	    else
	        rot = WF_ROT / RADIAN 
	    xxx = cos(rot) * xx - sin(rot) * yy
	    yyy = sin(rot) * xx + cos(rot) * yy
	} else {
	    xxx = xx 
	    yyy = yy
	}

	# calculate RA and Dec
	mw = mw_openim (ipref)
	ct = mw_sctran (mw, "logical", "world", 3)
	call mw_c2trand (ct, double(xxx), double(yyy), ra, dec)
	call mw_close (mw)
    
	# write the result to output
	call printf ("%7.2f %7.2f %7.2f %7.2f %7.2f %7.2f ")
	    call pargr (x0)
	    call pargr (y0)
	    call pargr (xc0)
	    call pargr (yc0)
	    call pargr (xch)
	    call pargr (ych)
	call printf ("%7.2f %7.2f ")
	    call pargr (xxx)
	    call pargr (yyy)

	if (hms) {
	    call printf ("%0.4h %0.3h")
		call pargd (ra/15.d0)
		call pargd (dec)
	    call sprintf (rastr, SZ_LINE, "%0.4h")
		call pargd (ra/15.d0)
	    call sprintf (decstr, SZ_LINE, "%0.3h")
		call pargd (dec)
	} else {
        call printf ("%11.7f %11.7f")
        call pargd (ra)
        call pargd (dec)
        call sprintf (rastr, SZ_LINE, "%11.7f")
        call pargd (ra)
        call sprintf (decstr, SZ_LINE, "%11.7f")
        call pargd (dec)
    }
    call printf ("\n")

	# write to output CL parameters
	#call clpstr ("ra", rastr)
	#call clpstr ("dec", decstr)
end
