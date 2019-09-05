include	<pkg/gtools.h>
include "iraf$pkg/xtools/icfit/icfit.h"

# SET_AXES -- Set axes data.
# The applications program may set additional axes types.

procedure set_axes (ic, gt, axis, x, y, npts)

pointer	ic				# ICFIT pointer
pointer	gt				# GTOOLS pointer
int	axis				# Output axis
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
int	npts				# Number of points

int	axistype, gtlabel[2], gtunits[2]

data	gtlabel/GTXLABEL, GTYLABEL/
data	gtunits/GTXUNITS, GTYUNITS/

begin
	axistype = IC_AXES(ic, IC_GKEY(ic), axis)
	switch (axistype) {
	case 'x':	# Ref_x
	    call gt_sets (gt, gtlabel[axis], "Ref_x")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,1)])
	case 'y':	# Ref_y
	    call gt_sets (gt, gtlabel[axis], "Ref_y")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	case 'i':	# xi_res
	    call gt_sets (gt, gtlabel[axis], "xi_res")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	case 'e':	# eta_res
	    call gt_sets (gt, gtlabel[axis], "eta_res")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	default:	# User axes types.
	}
end
