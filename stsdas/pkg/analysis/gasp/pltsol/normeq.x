include "pls.h"

procedure normeq (pn, nstars, xa, ya)

#	Routine to form equation of condition to solve the llsq
#	problem based on a plate model.
#	The statement equations give up to a full 20 terms for the
#	plate model.

pointer  pn			# pointer to data structure
int	 nstars			# number of positions in the solution
double	 xa[nstars, NTERMS_MODEL]  # complete plate model in X
double	 ya[nstars, NTERMS_MODEL]  # complete plate model in Y
int	j
double	xref, yref
real	mag, color

begin

	do j = 1, nstars {
#
#	   form the x_terms of the plate model
#
	   xref = Memd[X_PREF(pn)+j-1]
	   yref = Memd[Y_PREF(pn)+j-1]
	   mag  = Memr[PMAG(pn)+j-1]
	   color= Memr[PCOL(pn)+j-1]

	   xa(j,1)  = xref
	   xa(j,2)  = yref
	   xa(j,3)  = 1.0d0
	   xa(j,4)  = xref**2
	   xa(j,5)  = xref * yref
	   xa(j,6)  = yref**2
	   xa(j,7)  = xref**2 + yref**2
	   xa(j,8)  = xref**3
	   xa(j,9)  = xref**2 * yref
	   xa(j,10) = xref * yref**2
	   xa(j,11) = yref**3
	   xa(j,12) = xref * (xref**2 + yref**2)
	   xa(j,13) = xref * (xref**2 + yref**2)**2
	   xa(j,14) = mag
	   xa(j,15) = mag**2
	   xa(j,16) = mag**3
	   xa(j,17) = mag * xref
	   xa(j,18) = mag * (xref**2 + yref**2)
	   xa(j,19) = mag * xref * (xref**2 + yref**2)
	   xa(j,20) = color
#
#	   Form the y_terms of the plate model
#
	   ya(j,1) = yref
	   ya(j,2) = xref
	   ya(j,3) = 1.0d0
	   ya(j,4) = yref**2
	   ya(j,5) = yref * xref
	   ya(j,6) = xref**2
	   ya(j,7) = yref**2 + xref**2
	   ya(j,8) = yref**3
	   ya(j,9) = yref**2 * xref
	   ya(j,10) = yref * xref**2
	   ya(j,11) = xref**3
	   ya(j,12) = yref * (yref**2 + xref**2)
	   ya(j,13) = yref * (yref**2 + xref**2)**2
	   ya(j,14) = mag
	   ya(j,15) = mag**2
	   ya(j,16) = mag**3
	   ya(j,17) = mag * yref
	   ya(j,18) = mag * (yref**2 + xref**2)
	   ya(j,19) = mag * yref* (yref**2 + xref**2)
	   ya(j,20) = color

	}

	return
end
