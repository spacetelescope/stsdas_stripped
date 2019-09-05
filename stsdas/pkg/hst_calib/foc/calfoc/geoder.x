include <imhdr.h>
include <math/gsurfit.h>
define	NPTS		16	# fit to grid of NPTS+1 by NPTS+1 points

# geo_der -- compute matrix of derivatives
# This routine fits a plane to the middle half (quarter of the area) of
# a geo file in order to compute the derivatives of old pixel coordinates
# with respect to new pixel coordinates.
#
# Phil Hodge, 26-Oct-1990  Subroutine created.
# Phil Hodge, 14-May-1991  Replace call to logerr with logmsg.
# Phil Hodge,  9-Aug-1991  Add naxis1 & naxis2 to calling sequence;

procedure geo_der (geopt, naxis1, naxis2, o_n)

pointer geopt		# i: imhdr pointer for geo correction file
int	naxis1, naxis2	# i: size of geometrically corrected image
real	o_n[2,2]	# o: partial derivatives of old wrt new pixel coords
#--
pointer sf_xi, sf_eta	# surface fit pointers
pointer g		# pointer to a line of data in geopt
real	x, y		# pixel numbers in "new" image
real	wt		# weight (1) for each point
real	xmin, xmax	# limits to fitting region (integer values)
real	ymin, ymax	# limits to fitting region (integer values)
int	ixmin, ixmax, iymin, iymax
int	incr		# spacing of samples for fitting plane
int	y_offset	# offset from x to y
int	ier		# error return from gssolve
int	i, j
pointer imgl2r()

begin
	# A geo file contains xi & eta values along one row, and the values
	# are at pixel corners, so there's an extra element.

	# Take the middle half.
	ixmin = naxis1 / 4 + 1
	ixmax = ixmin + (naxis1 + 1) / 2
	iymin = naxis2 / 4 + 1
	iymax = iymin + (naxis2 + 1) / 2

	incr = (ixmax - ixmin) / NPTS
	incr = max (incr, 1)

	xmin = ixmin
	xmax = ixmax
	ymin = iymin
	ymax = iymax

	y_offset = IM_LEN(geopt,1) / 2

	# Initialize for fitting a plane.
	call gsinit (sf_xi,  GS_POLYNOMIAL, 2, 2, NO, xmin, xmax, ymin, ymax)
	call gsinit (sf_eta, GS_POLYNOMIAL, 2, 2, NO, xmin, xmax, ymin, ymax)

	# Get a grid of points in the middle half of the image and include
	# them in the fit.
	do j = iymin, iymax, incr {
	    y = j
	    # Get current line containing xi & eta values.  Note that we
	    # compare x=i with xi=Memr[g+i] and similarly for eta.
	    g = imgl2r (geopt, j)
	    do i = ixmin, ixmax, incr {
		x = i
		call gsaccum (sf_xi, x, y, Memr[g+i], wt, WTS_UNIFORM)
		call gsaccum (sf_eta, x, y, Memr[g+y_offset+i], wt, WTS_UNIFORM)
	    }
	}

	# Find the coefficients of fit.
	call gssolve (sf_xi, ier)
	if (ier != OK)
	    call logmsg ("geo_der:  can't fit xi portion")
	call gssolve (sf_eta, ier)
	if (ier != OK)
	    call logmsg ("geo_der:  can't fit eta portion")

	# Compute the first derivatives of the fits.
	x = (naxis1 + 1.) / 2.
	y = (naxis2 + 1.) / 2.
	call gsder (sf_xi, x, y, o_n[1,1], 1, 1, 0)
	call gsder (sf_xi, x, y, o_n[1,2], 1, 0, 1)
	call gsder (sf_eta, x, y, o_n[2,1], 1, 1, 0)
	call gsder (sf_eta, x, y, o_n[2,2], 1, 0, 1)

	call gsfree (sf_xi)
	call gsfree (sf_eta)
end
