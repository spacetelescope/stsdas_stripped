include	"invmetric.h"

define	TOL	0.005
define	EPS	5.
define	BL	-50.
define	BU	850.
define	L8	6435.
define	RANGE	400.

#  invmetric_do -- convert RA/Dec to X/Y for WFPC and WFPC2, including 
#  geometric corrections
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  13-Jun-1995  J.-C. Hsu		Adapted from metric_do.x
#------------------------------------------------------------------------------
procedure invmetric_do (ra_str, dec_str, ax, ay, xo, yo, trans, 
			ipref, iccd, order, ra_unit, instru, fixchip)

char	ra_str[ARB], dec_str[ARB]
real	ax[(MAX_ORDER+1)**2, *]
real	ay[(MAX_ORDER+1)**2, *]
real	xo[*]		# boundary coefficients
real	yo[*]
real	trans[SZ_TRANS, *]
pointer	ipref
int	iccd, order
char	ra_unit[ARB]
int	instru
int	fixchip
	
double	ra, dec
real	xc0, yc0
real	x0, y0, newx, newy, dx, dy
double	x0d, y0d
double	a11, a12, a21, a22, det
double	ra_x_1, ra_x_2, ra_y_1, ra_y_2
double	dec_x_1, dec_x_2, dec_y_1, dec_y_2
double	ra0, dec0, dra, ddec
double	interval
pointer	mw, ct
#int	newchip
int	chip
int	k, kp1, kp2, iter
char	xstr[SZ_LINE], ystr[SZ_LINE], chipstr[SZ_LINE]
char	ch

pointer	mw_openim(), mw_sctran()
int	ctod()
int	stridx()
#==============================================================================
begin
	iter = 10

	# determine if the input RA is in hours or degrees
	# ra and dec are in degrees during calculations in this routine
	kp1 = 1
	kp2 = 1
	ch = ':'
	k = ctod (ra_str, kp1, ra)
	k = ctod (dec_str, kp2, dec)
	if (ra_unit[1] == 'a') {
	    k = stridx(ch, ra_str)
	    if (k > 0) ra = ra * 15.d0
	} else if (ra_unit[1] == 'h') ra = ra * 15.d0

	# calculate initial X and Y
	mw = mw_openim (ipref)
	ct = mw_sctran (mw, "world", "logical", 3)
	call mw_c2trand (ct, ra, dec, x0d, y0d)
	call mw_close (mw)
	x0 = real(x0d)
	y0 = real(y0d)

	# (roughly) find out the point is at which chip
	if (fixchip != 0)
	    chip = fixchip
	else {
	    if (x0 >= xo[2] && y0 >= yo[2]) {
	        chip = 2
	    } else if (x0 >= xo[2] && y0 < yo[2]) {
	        chip = 1
	    } else if (x0 < xo[2] && y0 >= yo[2]) {
	        chip = 3
	    } else if (x0 < xo[2] && y0 < yo[2]) {
	        chip = 4
	    }
	}
	if (chip == 1) {
	    if (instru == WFPC2) {
	        xc0 = -(y0-yo[2]) * PC1SCALE + xo[1]
	        yc0 =  (x0-xo[2]) * PC1SCALE + yo[1]
	    } else {
	        xc0 = -(y0-yo[2]) + xo[1]
	        yc0 =  (x0-xo[2]) + yo[1]
	    }
	} else if (chip == 2) {
	    xc0 = x0
	    yc0 = y0
	} else if (chip == 3) {
	    xc0 =  (y0-yo[2]) + xo[3]
	    yc0 = -(x0-xo[2]) + yo[3]
	} else if (chip == 4) {
	    xc0 = -(x0-xo[2]) + yo[4]
	    yc0 = -(y0-yo[2]) + xo[4]
	}

	# send out warning if out of bound
	if (xc0 > BU || yc0 > BU || xc0 < BL || yc0 < BL) {
	    call printf("# The point is out of bound.   This may cause INCORRECT results.\n")
	    call printf("# Input RA=%0.7f(deg) Dec=%0.7f(deg)\n") 
		call pargd (ra)
		call pargd (dec)
	    call printf("# If input sexagesimal RA/Dec from the command line, make sure you have\n") 
	    call printf("# double quotes around the input numbers\n")
	    call flush(STDOUT)
	}

	do k = 1, iter {

#used for debugging
#call printf ("x=%7.2f  y=%7.2f    chip=%d \n")
#call pargr(xc0)
#call pargr(yc0)
#call pargi(chip)
#call flush(STDOUT)

	    if (abs(xc0/RANGE) > (INDEF/L8)**(1./MAX_ORDER) || 
	    	abs(yc0/RANGE) > (INDEF/L8)**(1./MAX_ORDER) || 
	    	abs(xc0*yc0/RANGE**2) > (INDEF/L8**2)**(1./order)) {
		chip = INDEFI
		break
	    }

	    # calculate d(ra)/dx, d(ra)/dy, d(dec)/dx, d(dec)/dy
	    call wxy2rd (xc0, yc0, ra0, dec0, ax[1,chip], ay[1,chip], 
			 trans[1,chip], ipref, iccd, order,instru)
	    call wxy2rd (xc0-EPS, yc0, ra_x_1, dec_x_1, ax[1,chip], 
			 ay[1,chip], trans[1,chip], ipref, iccd, order,instru)
	    call wxy2rd (xc0+EPS, yc0, ra_x_2, dec_x_2, ax[1,chip], 
			 ay[1,chip], trans[1,chip], ipref, iccd, order,instru)
	    call wxy2rd (xc0, yc0-EPS, ra_y_1, dec_y_1, ax[1,chip], 
			 ay[1,chip], trans[1,chip], ipref, iccd, order,instru)
	    call wxy2rd (xc0, yc0+EPS, ra_y_2, dec_y_2, ax[1,chip], 
			 ay[1,chip], trans[1,chip], ipref, iccd, order,instru)
    
	    # calculate the inverse matrix
	    interval = double(2.*EPS)
	    a11 = (ra_x_2 - ra_x_1) / interval
	    a21 = (ra_y_2 - ra_y_1) / interval
	    a12 = (dec_x_2 - dec_x_1) / interval
	    a22 = (dec_y_2 - dec_y_1) / interval
	    det = a11*a22 - a12*a21

	    dra = ra - ra0
	    ddec = dec - dec0

	    # calculate the new X and Y
	    dx = real((a22*dra - a21*ddec) / det)
	    dy = real((-a12*dra + a11*ddec) / det)

	    newx = xc0 + dx
	    newy = yc0 + dy

	    # figure out which chip is the new point at
	    #call whichchip (newx, newy, chip, fixchip, bx, by, newchip)
	    #if (newchip != chip) {
	    #chip = newchip
	    #} else {
	        if (abs(newx-xc0) < TOL && abs(newy-yc0) < TOL) break
	        else {
		    xc0 = newx
		    yc0 = newy
		}
	    #}
	    if (k == iter) 
		call printf("### Iterations did not converge, results in the next line may be incorrect.\n")
	}
	
	# write the result to output
	call printf ("%s %s ")
	    call pargstr (ra_str)
	    call pargstr (dec_str)
	if (chip == INDEFI)
	    call printf ("  INDEF   INDEF INDEF")
	else {
	    call printf ("%7.2f %7.2f     %d")
	        call pargr (xc0)
	        call pargr (yc0)
	        call pargi (chip)
	}
	call printf ("\n")

	# write to output CL parameters
	if (chip == INDEFI) {
	    call clpstr ("x", "INDEF")
	    call clpstr ("y", "INDEF")
	    call clpstr ("chip", "INDEF")
	} else {
	    call sprintf (xstr, SZ_LINE, "%8.2f")
	        call pargr (xc0)
	    call sprintf (ystr, SZ_LINE, "%8.2f")
	        call pargr (yc0)
	    call sprintf (chipstr, SZ_LINE, "%d")
	        call pargi (chip)
	    call clpstr ("x", xstr)
	    call clpstr ("y", ystr)
	    call clpstr ("chip", chipstr)
	}
end
