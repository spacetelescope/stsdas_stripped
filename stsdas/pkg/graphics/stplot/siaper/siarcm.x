include <math.h>

# si_arc_form - Draw sections of an arc
#
# History
#   12Mar91 - Created by Jonathan D. Eisenhamer.
#---------------------------------------------------------------------------

procedure si_arc_form (radius, start_angle, angle_extent, x, y, npts)

real radius            # I:  The radius of the circle that the arc lies on.
real start_angle       # I:  The start angle in degrees.
real angle_extent      # I:  The size in degrees of the arc.
real x[npts], y[npts]  # O: The x, y vectors represent the segments of the arc.
int  npts              # I: The number of points in the x,y vectors.

# Declarations.
real angle            # The current angle being plotted.
real angle_incr       # The amount the angle will change

int i                 # Segment counter.

begin
	angle_incr = angle_extent / (npts - 1.)
	angle = start_angle
	for (i = 1; i <= npts; i = i + 1) {
	    x[i] = radius * sin (DEGTORAD (angle))
	    y[i] = radius * cos (DEGTORAD (angle))
	    angle = angle + angle_incr
	}
end
#---------------------------------------------------------------------------
# End of si_arc_form
#---------------------------------------------------------------------------
