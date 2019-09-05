include <math.h>
include "streakflat.h"

define	LEN_STREAK  2*DIM_X
define	THRESHOLD   0.3

#  flat_smooth -- Do the 1-D smoothing of the streak flats
#  
#  Date		Author			Description
#  ----		------			-----------
#  14-May-1992  J.-C. Hsu		adapted from Jeff Hester's C code
#------------------------------------------------------------------------------

procedure flat_smooth (pic, mask, theta, hwidth)

real	pic[DIM_X, DIM_Y]
short	mask[DIM_X, DIM_Y]
real	theta		# input: streak angle in degrees
int	hwidth 		# input: one-D smoothing half width

pointer	work1, work2
int	indx
real	dx, dy		# intervals in X and Y between successive points within
			# one streak 
real	bdx, bdy	# intervals in X and Y borders between successive 
			# streaks
real	x0, y0, bx0, by0, x, y
real	rtheta
int	minpix
real	strbuf[LEN_STREAK]
int	bysteps
int	angle
int	i, j

real	streak_val()
#==============================================================================
begin
	rtheta = theta / RADIAN
	minpix = hwidth / 3

	call calloc (work1, DIM_X*DIM_Y, TY_REAL)
	call calloc (work2, DIM_X*DIM_Y, TY_REAL)

	if (abs(rtheta) < 0.5*atan2(1.,real(DIM_X))) 
	    angle = 0
	else if (abs(abs(rtheta)-HALFPI) < 0.5*atan2(1.,real(DIM_Y)))
	    angle = 90
	else if (theta > 0.)
	    angle = 1
	else
	    angle = -1

	# set border starting points
	bx0 = 1.
	by0 = 1.
	if (angle == 1)
	    by0 = DIM_Y

	# calculate the step sizes 
	# case of zero streak angle
	if (angle == 0) {
	    dx = 1.
	    dy = 0.
	    bdx = INDEF
	    bdy = 1.
	
	# case of 90 degree streak angle
	} else if (angle == 90) {
	    dx = 0.
	    dy = 1.
	    bdx = 1.
	    bdy = INDEF

	} else {
	    dx = cos(rtheta)
	    dy = sin(rtheta)
	    bdx = abs(1./dy)
	    if (angle == 1)
		bdy = -1./dx
	    else
	    	bdy = 1./dx
	}

	# do each streak intersecting with the left side
	# The separation between successive streaks is (artificially) set 
	# to ONE pixel
	if (angle != 90) {

	    # add 1 to bysteps (4/22/94 JCH)
	    bysteps = DIM_Y / abs(bdy) + 1
	    do i = 1, bysteps {
	        x0 = bx0
	        y0 = by0 + (i-1)*bdy
	        
	        # populate streak values
	        do j = 1, 2*DIM_X {
		    x = x0 + (j-1)*dx
		    y = y0 + (j-1)*dy
		    if (x < 1 || x > DIM_X || y < 1 || y > DIM_Y)
		        break
	   	    strbuf[j] = streak_val (pic, mask, x, y) 
	        }
	        call boxcar (strbuf, j-1, hwidth, minpix, x0, y0, dx, dy, 
				work1, work2)
	    }
	} else
	    y0 = by0

	# Now do each streak intersecting with the top or bottom side
	# use the last y0 from the last loop
	if (angle != 0) {
	    bysteps = DIM_X / bdx

	    # y0 should start at top or bottom (4/22/94 JCH)
	    if (angle == 1) y0 = 1
	    if (angle == -1) y0 = DIM_Y
	    do i = 1, bysteps {
	        x0 = bx0 + (i-1)*bdx
	    
	        # populate streak values
	        do j = 1, 2*DIM_Y {
		    x = x0 + (j-1)*dx
		    y = y0 + (j-1)*dy
		    if (x < 1 || x > DIM_X || y < 1 || y > DIM_Y)
		        break
	   	    strbuf[j] = streak_val (pic, mask, x, y) 
	        }
	        call boxcar (strbuf, j-1, hwidth, minpix, x0, y0, dx, dy, 
				work1, work2)
	    }
	}

	# calculate the final smoothed streak patterns
	do j = 1, DIM_Y {
	    do i = 1, DIM_X {
		indx = (j-1)*DIM_X + i - 1
		if (Memr[work2+indx] < THRESHOLD)
		    pic[i,j] = BADVAL
		else
		    pic[i,j] = Memr[work1+indx] / Memr[work2+indx]
	    }
	}

	call mfree (work1, TY_REAL)
	call mfree (work2, TY_REAL)
end

# streak_val -- interpolate pixel value by using values of the surrounding 
#		4 pixels
#-----------------------------------------------------------------------------

real procedure streak_val (pic, mask, x, y) 

real	pic[DIM_X, DIM_Y]
short	mask[DIM_X, DIM_Y]
real	x, y

real	val, b, d
int	i, j

begin
	i = int(x)
	j = int(y)

	if (i < 1 || i >= DIM_X || j < 1 || j >= DIM_Y) 
	    return (BADVAL)

	if (mask[i,j] != OKVAL || mask[i+1,j] != OKVAL ||
	    	mask[i,j+1] != OKVAL || mask[i+1,j+1] != OKVAL ||
		pic[i,j] == BADVAL || pic[i+1,j] == BADVAL ||
	    	pic[i,j+1] == BADVAL || pic[i+1,j+1] == BADVAL)
	    return (BADVAL)
	else {
	    b = x - i
	    d = y - j
	    val = (1.-b)*(1.-d)*pic[i,j] +
		  (1.-b)*d*pic[i,j+1] +
		  b*(1.-d)*pic[i+1,j] +
	 	  b*d*pic[i+1,j+1]
	    return (val)
	} 
end	

# boxcar -- do the "boxcar" averaging
#-----------------------------------------------------------------------------

procedure boxcar (strbuf, len, hwidth, minpix, x0, y0, dx, dy, work1, work2)

real	strbuf[1]
int	len		# length of the streak
int	hwidth		# HALF width of the boxcar filter
int	minpix
real	x0, y0, dx, dy
pointer	work1, work2

real	x, y, sum
int	j, hw, npts
int	ifront, iback
#------------------------------------------------------------------------------

begin
	hw = hwidth
	if (hwidth >= len)
	    hw = len - 1

	# do the summation for the first point, less the rightmost point
	sum = 0.
	npts = 0
	do j = 1, hw {
	    if (strbuf[j] != BADVAL) {
		sum = sum + strbuf[j]
		npts = npts + 1
	    }
	}

	# do the averaging
	do j = 1, len {
	    iback = j - hw
	    ifront = j + hw
	    if (iback > 0) {
		if (strbuf[iback] != BADVAL) {
		    sum = sum - strbuf[iback]
		    npts = npts - 1
		}
	    }
	    if (ifront <= len) {
		if (strbuf[ifront] != BADVAL) {
		    sum = sum + strbuf[ifront]
		    npts = npts + 1
		}
	    }
	    #if (npts > minpix) {
	    if (npts > 0) {

		# put the smoothed streak values back
		x = x0 + (j-1)*dx
		y = y0 + (j-1)*dy
		call smooth_val (Memr[work1], Memr[work2], x, y, 
					sum/real(npts))
	    }
	} 
end

# smooth_val -- put back smoothed pixel values by distributing to the 
#		surrounding 4 pixels
#------------------------------------------------------------------------------

procedure smooth_val (work1, work2, x, y, pixval) 

real	work1[DIM_X, DIM_Y]
real	work2[DIM_X, DIM_Y]
real	x, y
real	pixval

real	wt, b, d
int	i, j
#------------------------------------------------------------------------------

begin
	i = int(x)
	j = int(y)

	if (i < 1 || i >= DIM_X || j < 1 || j >= DIM_Y) 
	    return

	b = x - i
	d = y - j

	wt = (1.-b)*(1.-d)
	work1[i,j] = work1[i,j] + pixval*wt
	work2[i,j] = work2[i,j] + wt

	wt = (1.-b)*d
	work1[i,j+1] = work1[i,j+1] + pixval*wt
	work2[i,j+1] = work2[i,j+1] + wt

	wt = b*(1.-d)
	work1[i+1,j] = work1[i+1,j] + pixval*wt
	work2[i+1,j] = work2[i+1,j] + wt

	wt = b*d
	work1[i+1,j+1] = work1[i+1,j+1] + pixval*wt
	work2[i+1,j+1] = work2[i+1,j+1] + wt
end	
