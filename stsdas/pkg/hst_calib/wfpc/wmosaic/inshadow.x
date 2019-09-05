#  inshadow -- Decide if the pixel is inside the pyramid shadow
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Mar-1993  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure inshadow (x, y, ax, ay, order, factor)

real	x, y
real	ax[*]
real	ay[*]
int	order
real	factor

int	i
real	xside, yside
real	xfactor, yfactor
#==============================================================================
begin

	# for x axis, the curve equation is y = ax[1] + ax[2]*x + ax[3]*x^2
	# for y axis, the curve equation is x = ay[1] + ay[2]*y + ay[3]*y^2
	xside = ax[1]
	yside = ay[1]
	do i = 1, order {
	    xside = xside + ax[i+1]*x**i
	    yside = yside + ay[i+1]*y**i
	}

	# Add 0.5 to the factor because if the pixel is right at the boundary, 
	# half of the flux is inside the shadow and half is outside
	xfactor = x - yside + 0.5
	yfactor = y - xside + 0.5
	if (xfactor >= 1.) 
	    xfactor = 1.
	if (xfactor <= 0.) 
	    xfactor = 0.
	if (yfactor >= 1.) 
	    yfactor = 1.
	if (yfactor <= 0.) 
	    yfactor = 0.

	factor = xfactor * yfactor
end
