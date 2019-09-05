include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include "iraf$pkg/xtools/icfit/icfit.h"
include "pls.h"

define	MSIZE		2.		# Mark size

# UNDELETEP -- Undelete data point nearest the cursor.
# The nearest point to the cursor in NDC coordinates is determined.

procedure undeletep (ic, gp, gt, x, y, wts, npts, wx, wy)

pointer	ic					# ICFIT pointer
pointer	gp					# GIO pointer
pointer	gt					# GTOOLS pointer
double	x[npts], y[npts]			# Data points
real	wts[npts]				# Weight arrays
int	npts					# Number of points
real	wx, wy					# Position to be nearest

int	gt_geti()

begin

	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call u1d (ic, gp, x, y, wts, npts, wx, wy)
	else
	    call u1d (ic, gp, y, x, wts, npts, wy, wx)

end


# ICG_U1 -- Do the actual undelete.

procedure u1d (ic, gp, x, y, wts, npts, wx, wy)

pointer	ic					# ICFIT pointer
pointer	gp					# GIO pointer
double	x[npts], y[npts]			# Data points
real	wts[npts]				# Weight arrays
int	npts					# Number of points
real	wx, wy					# Position to be nearest

int	i, j
real	x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.

	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point to a point with zero weight.

	r2min = MAX_REAL
	do i = 1, npts {
	    if (wts[i] != 0.)
		next

	    call gctran (gp, real (x[i]), real (y[i]), x0, y0, 1, 0)

	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		j = i
	    }
	}

	# Unmark the deleted point and reset the weight.

	if (j != 0) {
	    call gscur (gp, real (x[j]), real (y[j]))
	    call gseti (gp, G_PMLTYPE, GL_CLEAR)
	    call gmark (gp, real (x[j]), real (y[j]), GM_CROSS, MSIZE, MSIZE)
	    call gseti (gp, G_PMLTYPE, GL_SOLID)
	    call gmark (gp, real (x[j]), real (y[j]), GM_DIAMOND, MSIZE, MSIZE)
	    wts[j] = 1.0
	    IC_NEWWTS(ic) = YES
	}
end
