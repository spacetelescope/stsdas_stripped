include <imhdr.h>
include <math/gsurfit.h>
define	MAX_LOOPS	50	# maximum number of loops when searching
define	SMALL_BOX	5	# box is small enough for final search
define	W_FACTOR	4	# reduce box size by this factor with each loop

# geo_find_crpix -- find crpix in geo file
# This routine searches the geo correction file for ocrpix.  If it finds it,
# the index of ocrpix is the new crpix, ncrpix.  The returned value of ncrpix
# is interpolated by a plane fit to the nearest neighbors, and it is corrected
# for the 1.0 pixel offset.  That offset is due to the fact that the values
# in the geo file are at pixel corners, using the convention (not standard in
# iraf) that pixel N runs from N-1 to N.  Thus, if there's no geometric
# distortion the values at pixels 1, 2, 3, etc will be 0, 1, 2, etc.
#
# Phil Hodge, 26-Oct-1990  Subroutine created.
# Phil Hodge, 14-May-1991  Replace call to logerr with logmsg.
# Phil Hodge,  9-Aug-1991  Add naxis1 & naxis2 to calling sequence;
#			move geo_get_val from geocoord.x to this file.

procedure geo_find_crpix (geopt, naxis1, naxis2, ocrpix, ncrpix, found)

pointer geopt		# i: for geo correction file
int	naxis1, naxis2	# i: size of geometrically corrected image
real	ocrpix[2]	# i: old values of crpix
real	ncrpix[2]	# o: new values of crpix
bool	found		# o: was ocrpix found in geo correction file?
#--
real	crpix[2]	# current value of new crpix
int	box[2,2]	# current lower & upper limits of search region
int	nloops		# number of loops
bool	done		# loop-termination flag
bool	outside		# is crpix outside the image?
bool	last_loop	# only need to do one more iteration for crpix?

begin
	found = true		# reset later if outside image

	# Begin with a box size that's half the image size in each direction.
	box[1,1] = naxis1 / 4 + 1
	box[1,2] = box[1,1] + (naxis1 + 1) / 2
	box[2,1] = naxis2 / 4 + 1
	box[2,2] = box[2,1] + (naxis2 + 1) / 2

	done = false
	nloops = 1
	while ( ! done ) {

	    # Fit a plane and evaluate to find crpix.
	    call geo_solve_crpix (geopt, box, ocrpix, crpix)

	    # Update size of box; check if outside image or if we're done.
	    call geo_update_box (naxis1, naxis2, box, crpix,
			outside, last_loop)

	    # Allow crpix to be outside the image on the first loop.
	    if (outside && nloops > 1) {
		found = false
		ncrpix[1] = ocrpix[1]
		ncrpix[2] = ocrpix[2]
		return
	    }

	    if (last_loop || nloops >= MAX_LOOPS) {
		# solve for crpix once more
		call geo_solve_crpix (geopt, box, ocrpix, crpix)
		done = true
	    }
	    nloops = nloops + 1
	}
	if (nloops >= MAX_LOOPS)
	    call logmsg ("geo_find_crpix:  max number of loops reached")

	ncrpix[1] = crpix[1]
	ncrpix[2] = crpix[2]
end

# geo_solve_crpix -- solve for crpix
# This routine fits a plane to four points at the corners of a box and
# solves for the new crpix value corresponding to the old crpix value.
#
# The subscripts for box, xi, eta may be confusing, so here's a picture:
#
#                            |                  |
# y = box[2,2] -->  ------------------------------------------
#                            |                  |
#                            |                  |
#                            |                  |
# y = box[2,1] -->  ------------------------------------------
#                            |                  |
#                            |                  |
#                      x = box[1,1]       x = box[1,2]
#
# The xi,eta values at the corners of the box are xi[i,j] and eta[i,j]
# with the indexes:
#
#                          [2,1]              [2,2]
#
#
#                          [1,1]              [1,2]

procedure geo_solve_crpix (geopt, box, ocrpix, crpix)

pointer geopt		# i: for geo correction file
int	box[2,2]	# i: current lower & upper limits of search region
real	ocrpix[2]	# i: location of crpix in old image
real	crpix[2]	# o: new crpix value
#--
pointer sf_x, sf_y	# surface fit pointers
real	x, y		# pixel numbers in "new" image
real	wt		# weight (1) for each point (set by gsaccum)
real	xi[2,2]		# values of xi at corners of box
real	eta[2,2]	# values of eta at corners of box
real	xmin, xmax	# limits to fitting region (integer values)
real	ymin, ymax	# limits to fitting region (integer values)
int	i, j
int	ier		# error return from gssolve
real	gseval()

begin
	# Get the xi & eta values at the corners of the box.
	# lower row:
	call geo_get_val (geopt, box[1,1], box[2,1], xi[1,1], eta[1,1])	# left
	call geo_get_val (geopt, box[1,2], box[2,1], xi[1,2], eta[1,2])	# right
	# upper row:
	call geo_get_val (geopt, box[1,1], box[2,2], xi[2,1], eta[2,1])	# left
	call geo_get_val (geopt, box[1,2], box[2,2], xi[2,2], eta[2,2])	# right

	xmin = xi[1,1]
	xmax = xmin
	ymin = eta[1,1]
	ymax = ymin
	do j = 1, 2 {
	    do i = 1, 2 {
		if (xi[i,j] < xmin)
		    xmin = xi[i,j]
		if (xi[i,j] > xmin)
		    xmax = xi[i,j]
		if (eta[i,j] < ymin)
		    ymin = eta[i,j]
		if (eta[i,j] > ymin)
		    ymax = eta[i,j]
	    }
	}

	# Initialize surface fitting routines for fitting a plane.
	call gsinit (sf_x, GS_POLYNOMIAL, 2, 2, NO, xmin, xmax, ymin, ymax)
	call gsinit (sf_y, GS_POLYNOMIAL, 2, 2, NO, xmin, xmax, ymin, ymax)

	# Include each of the corners in the fit.  Note that we are fitting
	# x & y (pixel coordinates in corrected image) as a function of
	# xi & eta (pixel coords in distorted image).
	do j = 1, 2 {			# lower & upper rows
	    do i = 1, 2 {		# left & right columns
		x = box[1,j]
		y = box[2,i]
		call gsaccum (sf_x, xi[i,j], eta[i,j], x, wt, WTS_UNIFORM)
		call gsaccum (sf_y, xi[i,j], eta[i,j], y, wt, WTS_UNIFORM)
	    }
	}

	# Find the coefficients of fit.
	call gssolve (sf_x, ier)
	if (ier != OK)
	    call logmsg ("geo_solve_crpix:  can't fit x portion")
	call gssolve (sf_y, ier)
	if (ier != OK)
	    call logmsg ("geo_solve_crpix:  can't fit y portion")

	# Evaluate the fit at the old crpix location.
	crpix[1] = gseval (sf_x, ocrpix[1], ocrpix[2])
	crpix[2] = gseval (sf_y, ocrpix[1], ocrpix[2])

	call gsfree (sf_x)
	call gsfree (sf_y)
end

# geo_update_box -- update size of box
# If the location of crpix is reasonable, this routine divides the box
# size by four, and the box is shifted to be centered on crpix, except that
# it stops at the borders of the image.  If crpix is outside the box or
# outside the image, the box is moved only half of the way from its previous
# position to the crpix location.  This is to limit the distance the box
# can jump in one step.  The boolean variable outside is set to true if
# crpix is outside the image.

procedure geo_update_box (naxis1, naxis2, box, crpix, outside, last_loop)

int	naxis1, naxis2	# i: size of geometrically corrected image
int	box[2,2]	# io: current lower & upper limits of search region
real	crpix[2]	# i: current value of new crpix
bool	outside		# o: is crpix outside the image?
bool	last_loop	# o: only need to do one more iteration for crpix?
#--
int	cent1, cent2	# location of center of box
int	wid1, wid2	# width of box

begin
	wid1 = box[1,2] - box[1,1]
	wid2 = box[2,2] - box[2,1]
	last_loop = false		# may be reset below

	if (crpix[1] < 1 || crpix[1] > naxis1 ||
	    crpix[2] < 1 || crpix[2] > naxis2)
	    outside = true

	if (crpix[1] < box[1,1] || crpix[1] > box[1,2] ||
	    crpix[2] < box[2,1] || crpix[2] > box[2,2]) {

	    # We're outside the box (and possibly outside the image).  Move
	    # the box center to a point halfway from current center to crpix.
	    cent1 = (box[1,1] + box[1,2] + 2.*crpix[1]) / 4.
	    cent2 = (box[2,1] + box[2,2] + 2.*crpix[2]) / 4.

	} else {

	    # Move the box center to the new crpix, and make box smaller.
	    cent1 = nint (crpix[1])
	    cent2 = nint (crpix[2])
	    wid1 = wid1 / W_FACTOR	# smaller with each iteration
	    wid2 = wid2 / W_FACTOR
	    wid1 = max (1, wid1)	# must not be smaller than one
	    wid2 = max (1, wid2)
	    if (wid1 <= SMALL_BOX && wid2 <= SMALL_BOX) {
		# Box is small enough that we can get the final value,
		# so set flag to stop looping.
		last_loop = true
	    }
	}

	# Reassign box limits based on new center, but stop at image edges.
	box[1,1] = cent1 - wid1 / 2
	box[2,1] = cent2 - wid2 / 2
	box[1,1] = max (1, box[1,1])
	box[2,1] = max (1, box[2,1])

	box[1,2] = box[1,1] + wid1
	box[2,2] = box[2,1] + wid2
	if (box[1,2] > naxis1) {
	    box[1,2] = naxis1
	    box[1,1] = box[1,2] - wid1
	}
	if (box[2,2] > naxis2) {
	    box[2,2] = naxis2
	    box[2,1] = box[2,2] - wid2
	}
end

# geo_get_val -- get a value
# This routine gets an x,y pair from geopt.  One is added to each of
# i & j before using them as indexes into the geo file.  This is because
# the values in the geo file are zero indexed.  If there is no distortion
# the returned values of x & y should be equal to i & j.

procedure geo_get_val (geopt, i, j, x, y)

pointer geopt		# i: pointer to geo coord struct
int	i, j		# i: pixel numbers
real	x, y		# o: (x,y) corresponding to (i,j) from data in geopt
#--
pointer z		# pointer to x or y for current pixel
int	offset		# offset from x to y
int	i1, j1		# i+1, j+1
pointer imgs2r()

begin
	offset = IM_LEN(geopt,1) / 2
	i1 = i + 1
	j1 = j + 1

	z = imgs2r (geopt, i1, i1, j1, j1)
	x = Memr[z]

	z = imgs2r (geopt, i1+offset, i1+offset, j1, j1)
	y = Memr[z]
end
