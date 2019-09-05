# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<error.h>
include	<pkg/gtools.h>
include <gset.h>
include "iraf$pkg/xtools/icfit/icfit.h"
include "pls.h"

define	KEY	"pltsol$pls.key"
define  PROMPT  "pls options"


# ICG_FIT -- Interactive curve fitting with graphics.

procedure icg_fitp (ic, gp, cursor, gt, im, pn, plate_model,
		    npts, x_pixel_size, y_pixel_size,
		    crpix1, crpix2, crval1, crval2, plate_scale)	

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
char	cursor[ARB]		# GIO cursor input
pointer	gt			# GTOOLS pointer
pointer	im			# Image descriptor pointer
pointer pn			# Pointer to objects structure
int	plate_model[NTERMS_MODEL] # Flag the terms to use in model
int	npts			# Number of points
real	x_pixel_size		# Pixel size (microns)
real	y_pixel_size		# Pixel size (microns)
double	crpix1, crpix2, crval1, crval2, plate_scale

double	x_coeff[NTERMS_MODEL]
double	y_coeff[NTERMS_MODEL]
real	x_sigma[NTERMS_MODEL]
real	y_sigma[NTERMS_MODEL]
int	nterms			# number of terms in the model to use.
double  xi_sigma, eta_sigma
double	xi_mean, eta_mean
double	xchisqr, ychisqr
int	i

pointer	sp, pmark, xir, etar, x, y, u, v, w, cvm, mdx, mdy
real	wx, wy
int	wcs, key
char	cmd[SZ_LINE]
int	newgraph, axes[2]

int	gt_gcur1()
int	nearestp()


begin

	# The weights are copied because they are changed when points are
	# deleted.

	call smark (sp)
	call salloc (xir, npts, TY_DOUBLE)
	call salloc (etar, npts, TY_DOUBLE)
	call salloc (x, npts, TY_DOUBLE)
	call salloc (y, npts, TY_DOUBLE)

	# Initialize
	IC_NEWWTS(ic) = NO
	IC_NREJECT(ic) = 0
	call ic_pkey(ic, 1, 'x', 'i')
	call ic_pkey(ic, 2, 'x', 'e')
	call ic_pkey(ic, 3, 'y', 'i')
	call ic_pkey(ic, 4, 'y', 'e')
	call ic_pkey(ic, 5, 'i', 'e')
	# Read cursor commands.

	# Start with fit routine and display xi_res/eta_res.
	key = 'f'
	IC_GKEY(ic) = 5
	axes[1] = IC_AXES(ic, IC_GKEY(ic), 1)
	axes[2] = IC_AXES(ic, IC_GKEY(ic), 2)

	repeat {
	    switch (key) {
	    case '?' : # help screen
		call gpagefile (gp, KEY, PROMPT)

	    case ':': # List or set parameters
		if (cmd[1] == '/')
	            call gt_colon (cmd, gp, gt, newgraph)
		else
		  call colon (ic, im, cmd, gp, gt, pn, Memd[xir],
			     Memd[etar], x_coeff, y_coeff, x_sigma, y_sigma,
			     xchisqr, ychisqr, crpix1, crpix2, crval1, crval2, 
			     plate_scale, plate_model, nterms, npts,
			     x_pixel_size, y_pixel_size)

	    case 'c': # Print the positions of data points.
		i = nearestp (gp, gt, Memd[x], Memd[y], npts, wx, wy)

	    	if (i != 0) {
		  call printf ("Xi = %g  Xi_fit = %g  Eta = %g  Eta_fit = %g\n")
			call pargd (Memd[PXI(pn)+i-1])
			call pargd (Memd[PXIC(pn)+i-1])
			call pargd (Memd[PETA(pn)+i-1])
			call pargd (Memd[PETAC(pn)+i-1])
		}

	    case 'd': # Delete data points.
		call deletep (ic, gp, gt, Memd[x], Memd[y], WEIGHT(pn), 
			npts, wx, wy)

	    case 'f': # Fit the function and reset the flags.
	        nterms = 0
	        do i = 1, NTERMS_MODEL
		   if (plate_model[i] != 0)
		      nterms = nterms + 1
	 	call smark (pmark)
		call salloc (u, npts*nterms, TY_DOUBLE)
		call salloc (v, nterms*nterms, TY_DOUBLE)
		call salloc (w, nterms, TY_DOUBLE)
		call salloc (cvm, nterms*nterms, TY_DOUBLE)
	        call salloc (mdx, nterms*npts, TY_DOUBLE)
		call salloc (mdy, nterms*npts, TY_DOUBLE)

		call dosetup (ic, pn, plate_model, npts, nterms, Memd[mdx],
			       Memd[mdy])
			      
		call fitsvd (Memd[mdx], XI(pn), WEIGHT(pn), npts, x_coeff, 
			     nterms, Memd[u], Memd[v], Memd[w], xchisqr)
	        # Reduced chisqr
		if (npts - nterms - 1 > 0)
		    xchisqr = xchisqr/(npts-nterms-1)
		call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
		do i =1, nterms {
#		   x_sigma[i] = sqrt(cvm[i,i])
		   x_sigma[i] = sqrt(Memd[cvm+(i-1) + (i-1)*nterms])
		}
		call fitsvd (Memd[mdy], ETA(pn), WEIGHT(pn), npts, y_coeff, 
			    nterms, Memd[u], Memd[v], Memd[w], ychisqr)
	        # Reduced chisqr
		if (npts - nterms - 1 > 0)
		    ychisqr = ychisqr/(npts-nterms-1)
		call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
		do i =1, nterms {
#		   y_sigma[i] = sqrt(cvm[i,i])
		   y_sigma[i] = sqrt(Memd[cvm+(i-1)+(i-1)*nterms])
		}
		call stats (pn, x_coeff, y_coeff, Memd[mdx], Memd[mdy],
		   npts, nterms, Memd[xir], Memd[etar], xi_sigma, eta_sigma, 
		   xi_mean, eta_mean)
		call amovd(Memd[xir], Memd[x], npts)
		call amovd(Memd[etar], Memd[y], npts)
		newgraph = YES
		call sfree(pmark)
	    case 'h':
		if (IC_GKEY(ic) != 1) {
		    IC_GKEY(ic) = 1
		    newgraph = YES
		}
		call amovd(X_REF(pn), Memd[x], npts)
		call amovd(Memd[xir], Memd[y], npts)

	    case 'i':
		if (IC_GKEY(ic) != 2) {
		    IC_GKEY(ic) = 2
		    newgraph = YES
		}
		call amovd(X_REF(pn), Memd[x], npts)
		call amovd(Memd[etar], Memd[y], npts)

	    case 'j':
		if (IC_GKEY(ic) != 3) {
		    IC_GKEY(ic) = 3
		    newgraph = YES
		}
		call amovd(Y_REF(pn), Memd[x], npts)
		call amovd(Memd[xir], Memd[y], npts)

	    case 'k':
		if (IC_GKEY(ic) != 4) {
		    IC_GKEY(ic) = 4
		    newgraph = YES
		}
		call amovd(Y_REF(pn), Memd[x], npts)
		call amovd(Memd[etar], Memd[y], npts)

	    case 'l':
		if (IC_GKEY(ic) != 5) {
		    IC_GKEY(ic) = 5
		    newgraph = YES
		}
		call amovd(Memd[xir], Memd[x], npts)
		call amovd(Memd[etar], Memd[y], npts)

	    case 'r': # Redraw the graph
		newgraph = YES

	    case 'u': # Undelete data points.
		call undeletep (ic, gp, gt, Memd[x], Memd[y], WEIGHT(pn),
			npts, wx, wy)

	    case 'v': # View the 4 error plots
		call gclear (gp)
		call gsview (gp, 0.12, 0.5, 0.6, 1.0)
		call gt_sets (gt, GTXLABEL, "Ref_x")
		call gt_sets (gt, GTYLABEL, "xi_res")
		call plot4w (ic, gp, gt, X_REF(pn), Memd[xir], WEIGHT(pn),npts)
		call gsview (gp, 0.55, 1.0, 0.6, 1.0)
		call gt_sets (gt, GTXLABEL, "Ref_x")
		call gt_sets (gt, GTYLABEL, "eta_res")
		call plot4w (ic, gp, gt, X_REF(pn),Memd[etar], WEIGHT(pn),npts)
		call gseti (gp, G_DRAWTITLE, NO)
		call gsview (gp, 0.12, 0.5, 0.1, 0.45)
		call gt_sets (gt, GTXLABEL, "Ref_y")
		call gt_sets (gt, GTYLABEL, "xi_res")
		call plot4w (ic, gp, gt, Y_REF(pn), Memd[xir], WEIGHT(pn),npts)
		call gsview (gp, 0.55, 1.0, 0.1, 0.45)
		call gt_sets (gt, GTXLABEL, "Ref_y")
		call gt_sets (gt, GTYLABEL, "eta_res")
		call plot4w (ic, gp,gt, Y_REF(pn),Memd[etar], WEIGHT(pn),npts)
		newgraph = NO
		call gactivate (gp, 0)
		call gpl_flush()
		call greset (gp, GR_RESETALL)

	    case '/': # keystroke help
	        call plhelp

	    default: # Bell
		call printf ("\07")
	    }

	    # Redraw the graph if necessary.
10	    if (newgraph == YES) {
		if (IC_AXES(ic, IC_GKEY(ic), 1) != axes[1]) {
		    axes[1] = IC_AXES(ic, IC_GKEY(ic), 1)
		    call gt_setr (gt, GTXMIN, INDEFR)
		    call gt_setr (gt, GTXMAX, INDEFR)
		}
		if (IC_AXES(ic, IC_GKEY(ic), 2) != axes[2]) {
		    axes[2] = IC_AXES(ic, IC_GKEY(ic), 2)
		    call gt_setr (gt, GTYMIN, INDEFR)
		    call gt_setr (gt, GTYMAX, INDEFR)
		}
	    	call graph_err (ic, gp, gt, Memd[x], Memd[y], WEIGHT(pn), npts)
		newgraph = NO
	    }
	} until (gt_gcur1 (gt, cursor, wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	call sfree (sp)	
end
