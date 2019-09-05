include <math.h>
include	"wmosaic.h"

define PC_ROT 0.2878
define WF_ROT 0.05334

#  mosaic_do -- perform the geometric distortion correction
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Feb-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mosaic_do (ipin, outbuf, norm)

pointer	ipin
real	outbuf[SZ_OUT, SZ_OUT]
real	norm[SZ_OUT, SZ_OUT]

int	instru
char	strval[SZ_LINE]
int	iccd, geo_order, edge_order
real	geo_x[(MAX_ORDER+1)**2]
real	geo_y[(MAX_ORDER+1)**2]
real	edge_x[EDGE_ORDER+1]
real	edge_y[EDGE_ORDER+1]
real	trans[SZ_TRANS]
real	cosrot, sinrot
real	xc, yc
real	dx, dy
real	xch, ych
real	x, y, xx, yy
real	factor
real	scale, scalesq, half
int	xint, yint
int	i, j
real	b, d
pointer	pic, ipx

int	imgeti()
pointer	imgs2r()
bool	streq()
#==============================================================================
begin

        # which instrument and CCD was used?
        call imgstr (ipin, "INSTRUME", strval, SZ_LINE)
        if (streq (strval, "WFPC2")) instru = WFPC2
        else if (streq (strval, "WFPC")) instru = WFPC
        else call error (1, "illegal instrument")

	# read the detector number, and the corresponding coefficients
	iccd = imgeti (ipin, "DETECTOR")
	if (instru == WFPC) {
	    call get_coeff (geo_x, geo_y, iccd, geo_order)
	    call get_edge (edge_x, edge_y, iccd, edge_order)
	    call get_trans (iccd, trans)
	} else if (instru == WFPC2) {
	    call get_coeff2 (geo_x, geo_y, iccd, geo_order)
	    call get_edge2 (edge_x, edge_y, iccd, edge_order)
	    call get_trans2 (iccd, trans)
	}
	scale = trans[3]
	scalesq = scale**2
	half = 0.5 * scale

	# Shawn Ewald added another global rotation to each camera
	if (iccd > 4) {
	    sinrot = sin(PC_ROT/RADIAN)
	    cosrot = cos(PC_ROT/RADIAN)
	} else {
	    sinrot = sin(WF_ROT/RADIAN)
	    cosrot = cos(WF_ROT/RADIAN)
	}

	# read the input data
	pic = imgs2r(ipin, 1, DIM_X, 1, DIM_Y)

	# go through each point
	do j = 1, DIM_Y {

	    # Shawn Ewald changed his origin to (0.5, 0.5) (2/15/93)
	    if (instru == WFPC)
	        yc = real(j) - 0.5
	    else
		yc = real(j)

	    do i = 1, DIM_X {

		call inshadow (real(i), real(j), edge_x, edge_y, 
				edge_order, factor)
		if (factor <= 0.) next

		# Shawn Ewald changed his origin to (0.5, 0.5) (2/15/93)
	    	if (instru == WFPC)
		    xc = real(i) - 0.5
		else
		    xc = real(i)
	
		# perform the geometric correction
                #if (instru == WFPC) {
                    call legendre (xc, yc, dx, dy, geo_x, geo_y, geo_order)
                    xch = xc - dx
                    ych = yc - dy
                #} else {
                #    call rcube (xc, yc, dx, dy, geo_x, geo_y, geo_order)
                #    xch = xc + dx
                #    ych = yc + dy
                #}

		# perform inter-chip corrections
		call inter_chip (xch, ych, x, y, trans)

		# global rotation
		if (instru == WFPC) {
		    xx = cosrot * x - sinrot * y
		    yy = sinrot * x + cosrot * y
		} else {
		    xx = x
		    yy = y
		}

		# reftting into the new coordiante system by keeping group 4's
		# orientation unchanged.  Also move the origin of the meta
		# coordiante system to (790, 790) in the new system.
		x = real(SZ_OUT)-(xx+790.)
		y = real(SZ_OUT)-(yy+790.)
		
		xint = int(x)
		yint = int(y)
	
		ipx = pic + (j-1)*DIM_X + i-1
	    	b = x - xint
	    	d = y - yint
		
		# rescale for the WFPC2
		if (instru == WFPC2 && iccd == 1) {
		    if (b-half > 0.5) 
			b = 1.
		    else if (b+half > 0.5) 
			b = (b+half-0.5)/scale
		    else
			b = 0.
		}
	    	norm[xint, yint] = factor * (1.-b) * (1.-d) + norm[xint, yint]
	    	norm[xint+1, yint] = factor * b * (1.-d) + norm[xint+1, yint]
	    	norm[xint, yint+1] = factor * (1.-b) * d + norm[xint, yint+1]
	    	norm[xint+1, yint+1] = factor * b * d + norm[xint+1, yint+1]

		factor = factor * Memr[ipx]
		if (instru == WFPC2 && iccd == 1) 
		    factor = factor/scalesq
	    	outbuf[xint, yint] = factor * (1.-b) * (1.-d) + 
					outbuf[xint, yint]
	    	outbuf[xint+1, yint] = factor * b * (1.-d) +
					outbuf[xint+1, yint]
	    	outbuf[xint, yint+1] = factor * (1.-b) * d +
					outbuf[xint, yint+1]
	    	outbuf[xint+1, yint+1] = factor * b * d +
					outbuf[xint+1, yint+1]
	    }
	}
end
