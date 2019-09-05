include <error.h>
include <gset.h>
include <imhdr.h>
include <imset.h>
include <mach.h>
include <syserr.h>

#################################################################################
#										#
# NOISEMODEL -- This routine is used to determine the noise characteristics  	#
#		of a detector by examining the way short-scale fluctuations 	#
#		in a flat field (or other) frame vary with data number.  The 	#
#		empirical results may be compared with various noise models.  	#
#		This routine is based upon FORTRAN code written by K. Horne.  	#
#										#
#	7/91	Initial SPP code:				 RAShaw		#
#	8/91	Calculation of Chi-square:			 RAShaw		#
#										#

procedure t_noisemodel ()

include "nmcom.h"

#  Parameters:	
real	ksig			# Kappa-sigma rejection threshold
int	xsize, ysize		# Size of bin along first, second axis

#  Local variables:
double	chisqr			# Chi-square for noisemodel
char	command[SZ_LINE]	# Input cursor command
char	device[SZ_FNAME]	# Graphics device
pointer	gp			# Graphics descriptor
int	group			# Image group
pointer	in			# Pointer to input images
pointer	input			# Pointer to input image filenames
int	key			# 
pointer	list			# List of input image rootnames
pointer	mean			# Mean values from input images
real	MIN_REAL		# Minimum possible REAL value
pointer	moddn			# DN for noise model
pointer	modsig			# Sigma from noise model
pointer	msighi			# Sigma from noise model
pointer	msiglo			# Sigma from noise model
int	jpts, nbins		# Assumed, actual number of bins for all images
int	nfit			# Non-rejected bins for chisqr, fit
int	nfnpts			# Number of points in noise function
char	newplot[SZ_LINE]	# Flag for updating plot
int	nimages			# No. of input images
pointer	sigma			# Measured deviations from input images
pointer	sp			# Pointer to begining of stack memory
char	title[2*SZ_LINE]	# Title for plot
int	wcs			# Index for WCS coordinate system
real	xmin, xmax		# min, max values of "mean" array
real	ymin, ymax		# min, max value of "sigma" array
char	xlabel[SZ_FNAME]	# Label for X-axis
char	ylabel[SZ_FNAME]	# Label for Y-axis
int	xleng, yleng		# Length of x,y axes in pixels
real	xpos, ypos		# World coordinates of cursor

# Functions used:
int	clgcur()		# Graphics/cl cursor
int	clgeti()		# Get INT keyword from cl
real	clgetr()		# Get REAL keyword from cl
pointer gopen()			# Returns a pointer to the graphics structure
pointer	gf_map()		# Map an image into memory
int	imtgetim()		# Get next image filename from list
int	imtlen()		# Determine no. of entries in an input list
pointer	imtopenp()		# Open CL parameter list
bool	streq()			# Is string A equal to string B?

errchk	calcnoise, chis, evalmodel, gf_map, nm_colon, plotnm

begin
	MIN_REAL = 1. / MAX_REAL

#  Get the list of input images
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	list = imtopenp ("input")
	nimages = imtlen (list)
	if (nimages < 1) call error (1, "No input file given")
	group = clgeti ("group")

#  Get noise model parameters
	readn  = clgetr ("readnoise")
	gain   = clgetr ("gain")
	if (gain <= 0.) call error (1, "Detector gain must exceed 0.")
	scalen = clgetr ("scalenoise")

#  Calculate noise from images
	ksig  = clgetr ("ksigreject")
	if (ksig <= 0.) call error (1, "ksigreject must exceed 0.")
	xsize = clgeti ("xbinsize")
	ysize = clgeti ("ybinsize")
	xypts = xsize * ysize
	if (xypts < BINMIN) call error (1, "Area of bin too small")

#  Assume that the number of bins is <= the number that would fit on a single
#  image times the number of input images
	key  = imtgetim (list, Memc[input], SZ_FNAME)
	in   = gf_map (Memc[input], READ_ONLY, 0)
	xleng = IM_LEN (in,1)
	yleng = IM_LEN (in,2)
	jpts = (xleng / xsize) * (yleng / ysize) * nimages
	call gf_unmap (in)
	call imtrew (list)
	call calloc (mean,  jpts, TY_REAL)
	call calloc (sigma, jpts, TY_REAL)
	call calcnoise (Memr[mean], Memr[sigma], jpts, nbins, group, 
			xsize, ysize, ksig, list)

#  Resize the buffers to the actual number of bins 
	call realloc (mean,  nbins, TY_REAL)
	call realloc (sigma, nbins, TY_REAL)

#  Compute chi-squared for model parameters, subject to hi/low rejection
	cliphi = clgetr ("cliphi")
	cliplo = clgetr ("cliplo")
	call chis (Memr[mean], Memr[sigma], nbins, chisqr, nfit)

#  Set the initial plot limits 
	ymin = 1.
	ymax = 3000.
	xmin = -5.
	xmax = 4095.

#  Evaluate noise model for plotting
	nfnpts = 3001
	call calloc (moddn,  nfnpts, TY_REAL)
	call calloc (modsig, nfnpts, TY_REAL)
	call calloc (msighi, nfnpts, TY_REAL)
	call calloc (msiglo, nfnpts, TY_REAL)
	call evalmodel (Memr[moddn], Memr[modsig], Memr[msighi], Memr[msiglo], 
			xmin, xmax, nfnpts)
	call amaxkr (Memr[modsig], MIN_REAL, Memr[modsig], nfnpts)

#  Set device, labels for plot
	call clgstr ("device", device, SZ_FNAME)
	call strcpy ("DN", xlabel, SZ_FNAME)
	call strcpy ("Sigma (DN)", ylabel, SZ_FNAME)

#  Update title with parameter info.
	call udtitle (title, group, xsize, ysize, ksig, chisqr, nfit)

#  Open graphics stream and select Elog,log axes
	gp = gopen (device, NEW_FILE, STDGRAPH)	
	call gseti (gp, G_XTRAN, GW_ELOG)
	call gseti (gp, G_YTRAN, GW_LOG)
	call gseti (gp, G_NTITLELINES, 4)

#  Plot calculated noise, model, and +/-Clip * sigma uncertainties
	call plotnm (gp, Memr[mean], Memr[sigma], Memr[moddn], Memr[modsig], 
		Memr[msighi], Memr[msiglo], xmin, xmax, ymin, ymax, nbins, 
		nfnpts, title, xlabel, ylabel) 
	call strcpy (EOS, newplot, SZ_LINE)		# Initialize newplot flag

#  Enter interactive loop
	while (clgcur ("cur", xpos, ypos, wcs, key, command, SZ_LINE) 
		!= EOF) {
	    switch (key) {

#	Quit task
	    case 'q', 'I':
		break

#  Re-plot calculated noise, latest model, and +/-Clip * sigma uncertainties
	    case 'r':

#	Update the labels
		call gframe (gp)
		call udtitle (title, group, xsize, ysize, ksig, chisqr, nfit)
		call plotnm (gp, Memr[mean], Memr[sigma], Memr[moddn], 
			Memr[modsig], Memr[msighi], Memr[msiglo], xmin, xmax, 
			ymin, ymax, nbins, nfnpts, title, xlabel, ylabel) 

#	Colon command
	    case ':':
		call nm_colon (gp, command, group, ksig, xsize, ysize, 
		    xmin, xmax, ymin, ymax, newplot)

#	Cursor help
	    case '?':
		call gpagefile (gp, HELP, PROMPT)

#	Illegal cursor command -- ring bell
	    default:
		call printf ("\7")
		call printf ("Invalid cursor command")
	    }

#  RESPOND TO COLON COMMAND: Recalculate noise from images
	    if (streq (newplot, "data")) {
		xypts = xsize * ysize
		jpts  = (xleng / xsize) * (yleng / ysize) * nimages
		call realloc (mean,  jpts, TY_REAL)
		call realloc (sigma, jpts, TY_REAL)
		call calcnoise (Memr[mean], Memr[sigma], jpts, nbins, 
				group, xsize, ysize, ksig, list)
		call realloc (mean,  nbins, TY_REAL)
		call realloc (sigma, nbins, TY_REAL)

#	Compute chi-squared for model parameters
		call chis (Memr[mean], Memr[sigma], nbins, chisqr, nfit)

#	Update label
		call udtitle (title, group, xsize, ysize, ksig, chisqr, nfit)

#	Advance frame & replot axes
		call gframe (gp)
		call plotnm (gp, Memr[mean], Memr[sigma], Memr[moddn], 
			Memr[modsig], Memr[msighi], Memr[msiglo], xmin, xmax, 
			ymin, ymax, nbins, nfnpts, title, xlabel, ylabel) 
		call strcpy (EOS, newplot, SZ_LINE)

#  RESPOND TO COLON COMMAND: Reevaluate noisemodel
	    } else if (streq (newplot, "model")) {
		call evalmodel (Memr[moddn], Memr[modsig], Memr[msighi], 
			Memr[msiglo], xmin, xmax, nfnpts)
		call amaxkr (Memr[modsig], MIN_REAL, Memr[modsig], nfnpts)

#	Compute chi-squared for model parameters
		call chis (Memr[mean], Memr[sigma], nbins, chisqr, nfit)

#	Plot predicted (mean vs. sigma) from noise model
		call gpline (gp, Memr[moddn], Memr[modsig], nfnpts, xmin, xmax)
		call gseti (gp, G_PLTYPE, GL_DASHED)
		call gpline (gp, Memr[moddn], Memr[msighi], nfnpts, xmin, xmax)
		call gpline (gp, Memr[moddn], Memr[msiglo], nfnpts, xmin, xmax)
		call gseti (gp, G_PLTYPE, GL_SOLID)
		call gflush (gp)
		call strcpy (EOS, newplot, SZ_LINE)

#  RESPOND TO COLON COMMAND: Replot with new x,yrange
#	Advance frame & plot axes
	    } else if (streq (newplot, "plot")) {
		call evalmodel (Memr[moddn], Memr[modsig], Memr[msighi], 
			Memr[msiglo], xmin, xmax, nfnpts)
		call amaxkr (Memr[modsig], MIN_REAL, Memr[modsig], nfnpts)

#	Compute chi-squared for model parameters
		call chis (Memr[mean], Memr[sigma], nbins, chisqr, nfit)
		call gframe (gp)

#	Update labels
		call udtitle (title, group, xsize, ysize, ksig, chisqr, nfit)
		call plotnm (gp, Memr[mean], Memr[sigma], Memr[moddn], 
			Memr[modsig], Memr[msighi], Memr[msiglo], xmin, xmax, 
			ymin, ymax, nbins, nfnpts, title, xlabel, ylabel) 
		call strcpy (EOS, newplot, SZ_LINE)
	    }
	}

#  Close plot, free memory
	call gclose (gp)
	call mfree (mean, TY_REAL)
	call mfree (sigma, TY_REAL)
	call mfree (moddn, TY_REAL)
	call mfree (modsig, TY_REAL)
	call mfree (msighi, TY_REAL)
	call mfree (msiglo, TY_REAL)
	call sfree (sp)
	call imtclose (list)
end


#################################################################################
#										#
# UDTITLE --	Update plot title with the latest noisemodel parameters.  	#
#										#
#	8/91	RAShaw:	Initial code						#

procedure udtitle (title, group, xsize, ysize, ksig, chisqr, nfit)

include	"nmcom.h"

# Calling arguments:
char	title[2*SZ_LINE]	# Title for plot
int	group			# Image group
int	xsize, ysize		# Size of bin along first, second axis
real	ksig			# Kappa-sigma rejection threshold
double	chisqr			# Value of Chi-squared
int	nfit			# Non-rejected bins for chisqr, model fit

# Local variables:
char	title1[SZ_LINE]		# Title for plot, first line
char	title2[SZ_LINE]		# Title for plot, second line
char	title3[SZ_LINE]		# Title for plot, third line
char	title4[SZ_LINE]		# Title for plot, fourth line

begin
	if IS_INDEFI (group) 
	     call strcpy ("STSDAS Noisemodel Analysis\n", title1, SZ_LINE)
	else {
	    call sprintf (title1, SZ_LINE, 
		"STSDAS Noisemodel Analysis for Image Group [%4d]\n")
		call pargi (group)
	}
	call sprintf (title2, SZ_LINE, 
		"Gain=%5.2f e-/DN, Readnoise=%5.2f DN, Scalenoise=%5.2f\\045\n")
	    call pargr (gain)
	    call pargr (readn)
	    call pargr (scalen)
	call sprintf (title3, SZ_LINE, 
		"Binsize: x=%3d, y=%3d   PixelRej:%4.2f\n")
	    call pargi (xsize)
	    call pargi (ysize)
	    call pargr (ksig)
	call sprintf (title4, SZ_LINE, 
		"ChiSqr=%7.5e  Nbins:%5d  ClipLo:%5.2f  ClipHi:%5.2f")
	    call pargd (chisqr)
	    call pargi (nfit)
	    call pargr (cliplo)
	    call pargr (cliphi)
	call strcpy (title1, title, 2*SZ_LINE)
	call strcat (title2, title, 2*SZ_LINE)
	call strcat (title3, title, 2*SZ_LINE)
	call strcat (title4, title, 2*SZ_LINE)
end


#################################################################################
#										#
#  PLOTNM --	Plot the latest noisemodel, calculated noise, and error bars.  	#
#	8/91	RAShaw:	Initial code						#

procedure plotnm (gp, mean, sigma, moddn, modsig, msighi, msiglo, xmin, xmax, 
		ymin, ymax, nbins, mpts, title, xlabel, ylabel) 

#  Calling arguments:
pointer	gp			# Graphics descriptor
real	mean[nbins]		# Mean values from input images
real	sigma[nbins]		# Measured deviations from input images
real	moddn[mpts]		# DN for noise model
real	modsig[mpts]		# Sigma from noise model
real	msighi[mpts]		# Sigma from noise model
real	msiglo[mpts]		# Sigma from noise model
real	xmin, xmax		# min, max values of "mean" array
real	ymin, ymax		# min, max value of "sigma" array
int	nbins			# Actual number of bins for all images
int	mpts			# No. points for calculated noise function
char	title[2*SZ_LINE]	# Updated title for plot
char	xlabel[SZ_FNAME]	# Label for X-axis
char	ylabel[SZ_FNAME]	# Label for Y-axis

#  Local variables:
real	sz_sym			# Size of plotted symbol

begin
	call gswind (gp, xmin, xmax, ymin, ymax)	# Set WCS window
	call glabax (gp, title, xlabel, ylabel)		# Plot axes & labels

#  Plot predicted (mean vs. sigma) from noise model, with +/- CLIP*sigma limits
	call gpline (gp, moddn, modsig, mpts, xmin, xmax)
	call gseti (gp, G_PLTYPE, GL_DASHED)
#	if (!IS_INDEFR (msighi[1]))
	    call gpline (gp, moddn, msighi, mpts, xmin, xmax)
#	if (!IS_INDEFR (msiglo[1]))
	    call gpline (gp, moddn, msiglo, mpts, xmin, xmax)
	call gseti (gp, G_PLTYPE, GL_SOLID)

#  Plot measured means and sigmas
	call gpmark (gp, mean, sigma, nbins, GM_POINT, sz_sym, sz_sym)
	call gflush (gp)
end
