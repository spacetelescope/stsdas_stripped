# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<gset.h>
include	<pkg/gtools.h>
include "iraf$pkg/xtools/icfit/icfit.h"
define	NGRAPH		100		# Number of fit points to graph
define	MSIZE		2.		# Mark size

# PLOT4W -- 4 residuals plots in one screen.

procedure plot4w (ic, gp, gt, x, y, wts, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
real	wts[npts]			# Weights
int	npts				# Number of points


begin

	call g1d4 (gp, gt, x, y, wts, npts)

	if (IC_NREJECT(ic) > 0)
	    call g2d (ic, gp, gt, x, y, wts, npts)


end

procedure g1d4 (gp, gt, x, y, wts, npts)

pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	x[npts]				# Ordinates
double	y[npts]				# Abscissas
real	wts[npts]			# Weights
int	npts				# Number of points

int	i
pointer	sp, xr, yr, xr1, yr1, gt1

begin
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (xr1, 2, TY_REAL)
	call salloc (yr1, 2, TY_REAL)
	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")
	call gt_sets (gt1, GTMARK, "cross")


	    # Set the graph scale and axes.

	    call gascale (gp, Memr[xr], npts, 1)
	    call gascale (gp, Memr[yr], npts, 2)
	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)

	    Memr[xr1] = Memr[xr]
	    Memr[yr1] = Memr[yr]
	    do i = 1, npts {
		if (wts[i] == 0.) {
		    call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1) 
		} else {
		    Memr[xr1+1] = Memr[xr+i-1]
		    Memr[yr1+1] = Memr[yr+i-1]
		    call gt_plot (gp, gt, Memr[xr1], Memr[yr1], 2)
		    Memr[xr1] = Memr[xr1+1]
		    Memr[yr1] = Memr[yr1+1]
		}
	    }

	call sfree (sp)
	call gt_free (gt1)
end
