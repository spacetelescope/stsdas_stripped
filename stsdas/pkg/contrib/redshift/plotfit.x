include	<gset.h>
include	<pkg/gtools.h>
include	"fquot.h"

procedure plotfit(x, quot, gtitle, lo, lup, z)

real	x[ARB]
complex	quot[ARB]
char	gtitle[ARB]
int	lo, lup
real	z[3]

char	command[SZ_FNAME]
int	i
int	wc, key
real	wx, wy
real	sig2
real	amp[MAXPTS2], fit[MAXPTS2]

pointer	gt, gfd

int	gt_gcur()
pointer	gopen(), gt_init()
real	exp()
complex	cexp1()

begin

# Form array of amplitudes of the data and the fit.

	sig2 = exp ( 2. * z[2] )
	for ( i = 1; i <= lup; i = i + 1)
	{
	    amp[i] = real ( quot[i] * cexp1(0., x[i] * z[1]) )
	    if ( i >= lo )
		fit[i] = z[3] * exp( - sig2 * x[i]**2 / 2. )
	    else
		fit[i] = 0.
	}

# Plot the Fourier amplitude spectrum of the quotient
	# Open plotter and eliminate y-axis minor ticks.
	gfd = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	call gseti (gfd, G_YNMINOR, 0)

	# Initialize graph format
	gt = gt_init()
	call gt_sets (gt, GTTITLE, gtitle)

	call replot (gfd, gt, amp, lup, x[1], x[lup])

# Overplot the fit

	call gvline(gfd, fit, lup, x[1], x[lup])

# Interact with the cursor so the user can view the plot

	while (gt_gcur ("cursor", wx, wy, wc, key, command, SZ_FNAME) != EOF) {
	}

# Close up graph window

	call gclear(gfd)
	call gclose(gfd)
	call gt_free(gt)
end
