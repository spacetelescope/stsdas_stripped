include <math.h>

include	"invmetric.h"

define PC_ROT 0.2878
define WF_ROT 0.05334

#  wxy2rd -- convert X/Y to RA/Dec for WFPC and WFPC2, including 
#  geometric corrections
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  13-Jun-1995  J.-C. Hsu		Adapted from metric_do.x
#------------------------------------------------------------------------------
procedure wxy2rd (xc0, yc0, ra, dec, ax, ay, trans, ipref, iccd, order, instru)

real	xc0, yc0
double	ra, dec
real	ax[(MAX_ORDER+1)**2]
real	ay[(MAX_ORDER+1)**2]
real	trans[SZ_TRANS]
pointer	ipref
int	iccd, order
int	instru
	
real	xc, yc
real	dx, dy
real	xch, ych
real	xx, yy
real	rot, xxx, yyy
pointer	mw, ct

pointer	mw_openim(), mw_sctran()
#==============================================================================
begin

	# Shawn Ewald changed his origin to (0.5, 0.5) (2/15/93)
	if (instru == WFPC) {
	    xc = xc0 - 0.5
	    yc = yc0 - 0.5
	} else {
	    xc = xc0
	    yc = yc0
	}
	
	# perform intra-chip corrections
	call legendre (xc, yc, dx, dy, ax, ay, order)
	xch = xc - dx
	ych = yc - dy

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
end
