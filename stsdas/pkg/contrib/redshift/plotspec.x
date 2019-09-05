###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure plotspec(npoints, pix, gtitle, x1, x2)
#		int	npoints
#		real	pix[ARB]
#		char	gtitle[ARB]
#		real	x1, x2

#		procedure fixit( npts, pix, d1, d2)
#		int	npts
#		real	pix[ARB]
#		int	d1, d2

#  Description:	PLOTSPEC uses IRAF plot routines to plot a spectrum on STDGRAPH.
#		
#		FIXIT edits a spectrum by interpolating between marked points.		
#  Arguments:	int	npoints		Number of points in spectrum
#		real	pix[ARB]	Array of points to plot
#		char	gtitle[ARB]	Title for the plot
#		real	x1, x2		Beginning and ending x coordinates
#
#		int	npts		Number of points in spectrum
#		real	pix[ARB]	Array of points to edit
#		int	d1, d2		Pixels to interpolate between

#  Returns:	None

#  Notes:	Adapted from splot in the IRAF onedspec package

#  History:	June	1987	Gerard Kriss

###########################################################################

include	<gset.h>
include	<pkg/gtools.h>
include	"fquot.h"

procedure plotspec(npoints, pix, gtitle, x1, x2)

int	npoints
real	pix[ARB]
char	gtitle[ARB]
real	x1, x2

bool	first
char	command[SZ_FNAME]
int	wc, key, newgraph
int	d1, d2
real	wx, wy
pointer	gt, gfd

int	gt_gcur()
real	clgetr()
pointer	gopen(), gt_init()

include	"fquot.com"

begin
	# Open plotter and eliminate y-axis minor ticks.
	gfd = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	call gseti (gfd, G_YNMINOR, 0)

	# Initialize graph format
	gt = gt_init()
	call gt_sets (gt, GTTITLE, gtitle)
#	if (options[ZERO] == YES)
#	    call gt_setr (gt, GTYMIN, 0.)

	# Initial plot of data array

	call replot (gfd, gt, pix, npoints, x1, x2)
	newgraph = NO
	first = true

	while (gt_gcur ("cursor", wx, wy, wc, key, command, SZ_FNAME) != EOF) {
	    switch (key) {
	    case 'r':
		newgraph = YES

	    case ':':
		if (command[1] == '/')
		    call gt_colon (command, gt, newgraph)
		else
		    call printf ("07")

	    case 'd':
		if ( first ) {
			d1 = wx
			call printf("d again:")
			first = false
		}
		else {
			first = true
			d2 = wx
			call fixit(npoints, pix, d1, d2)
			newgraph = YES
		}

	    case 'p':		# Applicable within xcorfit.
		z[1] = wx	# Set estimate of cross-correlation peak
				# to the cursor value
	    case 'z':
		z0 = clgetr("z0")

	    default:
		# Default = 'c'
		call printf ("x,y: %10.3f %10.4g")
		    call pargr (wx)
		    call pargr (wy)
		call flush (STDOUT)

	    }

	    if (newgraph == YES) {
		call replot (gfd, gt, pix, npoints, x1, x2)
		newgraph = NO
	    }
	}

# Close up graph window

	call gclear(gfd)
	call gclose(gfd)
	call gt_free(gt)
end

procedure fixit( npts, pix, d1, d2)

int	npts
real	pix[ARB]
int	d1, d2

int	i, temp
real	p0, slope

begin

# If d2 < d1, switch the order.
	if ( d2 < d1 )
	{
		temp = d1
		d1 = d2
		d2 = temp
	}

# If d2 = d1, increase it by one.
	if ( d2 == d1 )
		d2 = d2 + 1

	if ( d1 > 1 && d2 < npts )
	{
		p0 = pix[d1 - 1]
		slope = (pix[d2 + 1] - p0) / (d2 - d1)
	}
	else if ( d1 == 1 && d2 < npts )
	{
		p0 = pix[d2 + 1]
		slope = 0.
	}
	else if ( d1 > 1 && d2 == npts )
	{
		p0 = pix[d1 - 1]
		slope = 0.
	}

	for ( i = d1; i <= d2; i = i + 1)
	{
		pix[i] = p0 + (i - d1) * slope
	}
end
