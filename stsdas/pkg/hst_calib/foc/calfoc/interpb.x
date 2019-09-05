# interpb -- compute value for a pixel
# This routine computes the value to be assigned to a given output pixel
# based on the values of pixels in the input image.
# If the 'box_check' flag is set, the values of 'area' and 'bad' will also
# be computed and returned; otherwise, those values will be set to zero.

procedure interpb (data, nx, ny, nbx, nby, x, y,
		box_check, value, area, bad)

real	data[nx, ny]	# i: input image (pixel array)
int	nx, ny		# i: size of pixel array
int	nbx, nby	# i: size of box to include segment
real	x[5], y[5]	# i: coordinates of segment corners
bool	box_check	# i: check for errors in computing pixel overlap?
real	area		# o: area of segment calculated from summing
			# pixel contributions
real	value		# o: value of interpolated intensity
real	bad		# o: area that is 'bad', i.e. outside frame or = 0
#--
real	xm		# minimum x value
int	ixm		# negint (xm)
real	darea
int	i, j, k, ix, iy
bool	lbad
int	negint()

begin
	area = 0.0
	value = 0.0
	bad = 0.0

	#  Find min. x value
	xm = x[1]
	do k = 2, 4 {
	    if (x[k] < xm) xm = x[k]
	}	
	ixm = negint (xm)

	#  now do the hard work
	#  for each pixel [ix,iy]:

	do i = nby, 1, -1 {
	    iy = negint (y[1]) +  i

	    do j = nbx, 1, -1 {
		ix = ixm + j

		# initialise variables

		lbad = false
		darea = 0.0

		if (box_check) {

		    if (ix < 1 || ix > nx || iy < 1 || iy > ny)
			lbad = true

		    #  Here's where Bill's subroutine is called.
		    call boxer (ix-1, iy-1, x, y, darea)

		    #  That's it; now work out mean intensity, etc.

		    if (lbad) {
			bad = bad + darea
		    } else {
			area = area + darea
			value = value + darea * data [ix, iy]
		    }

		} else if (ix >= 1 && ix <= nx && iy >= 1 && iy <= ny) {

		    # only compute value if within image
		    call boxer (ix-1, iy-1, x, y, darea)
		    value = value + darea * data [ix, iy]
		}
	    }
	}
end
