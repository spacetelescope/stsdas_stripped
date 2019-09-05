# cz_interp1 -- interpolate within one array
# This routine (and cz_interp2) perform linear interpolation to fill in
# bad points in one (or two) arrays.  The bad points are listed in array bp.

procedure cz_interp1 (oxr, n, bp, nbad)

real	oxr[n]		# io: array within which bad points will be replaced
int	n		# i: size of array to be fixed
int	bp[nbad]	# i: list of bad pixel locations
int	nbad		# i: size of array bp
#--
int	n1, n2		# replace all values in interval [n1,n2]
int	start		# starting index for finding contiguous bad pixels
int	j		# loop index
bool	done		# loop-termination flag

begin
	if (nbad <= 0) {

	    return

	} else if (nbad >= n) {

	    do j = 1, n
		oxr[j] = 0.

	} else {

	    # For each set of contiguous bad pixels, interpolate and
	    # fill in values in oxr.
	    start = 1
	    done = false
	    while ( ! done ) {
		if (start > nbad) {
		    done = true
		} else {
		    call cz_contig (bp, nbad, start, n1, n2)
		    call cz_interp (oxr, n, n1, n2)
		}
	    }
	}
end

# cz_interp2 -- interpolate within two arrays
# This routine performs linear interpolation to fill in bad points in two
# arrays.  The bad points are listed in array bp.

procedure cz_interp2 (oxr, oxi, n, bp, nbad)

real	oxr[n]		# io: array within which bad points will be replaced
real	oxi[n]		# io: second array with bad points
int	n		# i: size of array(s) to be fixed
int	bp[nbad]	# i: list of bad pixel locations
int	nbad		# i: size of array bp
#--
int	n1, n2		# replace all values in interval [n1,n2]
int	start		# starting index for finding contiguous bad pixels
int	j		# loop index
bool	done		# loop-termination flag

begin
	if (nbad <= 0) {

	    return

	} else if (nbad >= n) {

	    do j = 1, n {
		oxr[j] = 0.
		oxi[j] = 0.
	    }

	} else {

	    # For each set of contiguous bad pixels, interpolate and
	    # fill in values in oxr & oxi.
	    start = 1
	    done = false
	    while ( ! done ) {
		if (start > nbad) {
		    done = true
		} else {
		    call cz_contig (bp, nbad, start, n1, n2)
		    call cz_interp (oxr, n, n1, n2)
		    call cz_interp (oxi, n, n1, n2)
		}
	    }
	}
end

# cz_contig -- find contiguous set of bad pixel locations

procedure cz_contig (bp, nbad, start, n1, n2)

int	bp[nbad]	# i: list of bad pixel locations
int	nbad		# i: size of array bp
int	start		# io: starting index for finding contiguous bad pixels
int	n1, n2		# o: replace all values in interval [n1,n2]
#--
int	previous	# for comparing adjacent values in bp array
int	j		# loop index
bool	done

begin
	n1 = bp[start]
	n2 = n1			# initial value
	previous = n1
	j = start

	done = false
	while ( ! done ) {
	    j = j + 1
	    if (j > nbad) {
		done = true
	    } else {
		if (bp[j] == (previous+1)) {
		    n2 = bp[j]
		    previous = bp[j]
		} else {
		    done = true
		}
	    }
	}
	start = j		# next point (if any) in list
end


# cz_interp -- interpolate within an array
# This routine performs linear interpolation to fill in bad points in
# an interval specified by endpoints n1 and n2.

procedure cz_interp (ox, n, n1, n2)

real	ox[n]		# io: array within which bad points will be replaced
int	n		# i: size of array to be fixed
int	n1, n2		# i: replace all values in interval [n1,n2]
#--
real	v1, v2		# values just beyond endpoints of a range of bad pixels
real	width		# width of an interval to be filled in
real	p, q		# fractions of interval (p + q = 1)
int	j		# loop index

begin
	if (n1 <= 1) {

	    v2 = ox[n2+1]
	    do j = 1, n2
		ox[j] = v2

	} else if (n2 >= n) {

	    v1 = ox[n1-1]
	    do j = n1, n
		ox[j] = v1

	} else if (n1 == n2) {

	    ox[n1] = (ox[n1-1] + ox[n1+1]) / 2.

	} else {

	    v1 = ox[n1-1]
	    v2 = ox[n2+1]
	    width = n2 - n1 + 2.
	    do j = n1, n2 {
		p = (j - n1 + 1.) / width
		q = 1. - p
		ox[j] = q * v1 + p * v2
	    }
	}
end
