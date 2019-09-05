include	<mach.h>

# DISTRIBUTION -- Compute distribution function

procedure ks_distribution (dist, ddict, x, y, n)

# This routine generates a requested distribution function (the integral of the
# probability density function, rather than the probability density; the names
# are sometimes used loosely). Note that any monotonic non-decreasing function,
# bounded between 0 and 1 (inclusive) can be legitimely considered to be a
# distribution function.
#
# The input array, X, is in order, from lowest to highest, and may contain
# duplicate values (which should be checked for, since a considerable savings
# in computer time may be obtained). The output array is likewise a list, but
# of distribution function falues; in a formula: Y(I) = F(X(I)), I=1,N. Note
# that the function F should be robust: if out-of-range values are given, the
# function should return either 0 or 1, depending on whether the value was too
# low or too high. If the distribution is discrete, then the input should be
# tested to see if it is fractional (i.e. expect either 1, 2, or 3, but get 2.5)
# the fractional part should then be chopped off, as per the standard
# statistical extention of discrete distributions to be (almost everywhere)
# continuous.

char	dist[ARB]	# Distribution name
char	ddict[ARB]	# Distribution dictionary
real	x[*]		# Sorted array of sample values
real	y[*]		# Array of distribution function values:
			# y(i) = F( x(i) ),  i=1,2,3...n
int	n		# Dimension of both x and y

real	clgetr(), fmin, fmax, ple	# Distribution parameters

real	a, b, hold, z
int	i, strdic(), strlen()
bool	old

begin
	# Get distribution function extrema
	fmin = clgetr ("fmin")
	fmax = clgetr ("fmax")

	switch (strdic (dist, dist, strlen(dist), ddict)) {

	case 3:		# Uniform distribution: f(x) = 1/(b-a)    [x>a]
			#                       F(x) = (x-a)/(b-a)

	    # Check and set parameters.
	    if (fmin > fmax) {
		hold = fmin
		fmin = fmax
		fmax = hold
	    }

	    if (fmin == fmax) {
		a = x[1]
		if (x[n] == x[1])
		    b = 0
		else
		    b = 1.0 / (x[n] - x[1])
	    } else {
		a = fmin
		b = 1.0 / (fmax - fmin)
	    }

	    # Calculate distribution value.

	    if (b == 0)
		do i = 1, n
		    y[i] = 1.0
	    else {
		do i = 1, n {
		    old = i > 1			# Force calculation for i=1
		    if (old)
			old = x[i-1] == x[i]	# True if argument repeats
		    if (old)
			y[i] = y[i-1]		# Repeat result
		    else {
			z = (x[i] - a) * b	# Uniformly distributed
			if (z <= EPSILON)
			    y[i] = 0.0
			else if (1.0-z < EPSILON)
			    y[i] = 1.0
			else
			    y[i] = z
		    }
		}
	    }

	case 4:		#                                      -b(x-a)
			# Exponential distribution:  f(x) = b e           [x>a]
			#                                       -b(x-a)
			#                            F(x) = 1 - e

	    # Check parameters and set internal versions.
	    if (fmax < EPSILON) {
		a = x[1]
		if (x[n] == x[1])
		    b = 0.0
		else
		    b = 1.0 / (x[n] - x[1])
	    } else {
		a = fmin		# Lower cutoff point
		b = 1.0 / fmax	# Scale parameter
	    }

	    # Calculate distribution function value.
	    if (b == 0)
		do i = 1, n
		    y[i] = 1.0
	    else {
		do i = 1, n {
		    old = i > 1			# Force calculation for i=1
		    if (old)
			old = x[i-1] == x[i]	# True if argument repeats
		    if (old)
			y[i] = y[i-1]		# Repeat result
		    else {
			z = (x[i] - a) * b
			if (z .le. EPSILON)
			    y[i] = 0.0
			else
			    y[i] = 1.0 - exp (-z) # Exponential probability
		    }
		}
	    }

	case 5,6:	#                                               c-1
			# Power law distribution:  f(x) = b c [b(x-a+1)]
			#                                               c
			#                          F(x) = 1 - [b(x-a)+1]

	    # Get and set parameters.
	    ple = - clgetr ("power")		# power law exponent
	    if (fmax < EPSILON) {
		a = x[1]
		if (x[n] == x[1])
		    b = 0.0
		else
		    b = 1.0 / (x[n] - x[1])
	    } else {
		a = fmin		# Lower cutoff point
		b = 1.0 / fmax	# Scale parameter
	    }

	    # Calculate distribution function value.
	    if (b == 0)
		do i = 1, n
		    y[i] = 1.0
	    else {
		do i = 1, n {
		    old = i > 1			# Force calculation for i=1
		    if (old)
			old = x[i-1] == x[i]	# True if argument repeats
		    if (old)
			y[i] = y[i-1]		# Repeat result
		    else {
			z = (x[i] - a) * b
			if (z .le. EPSILON)
			    y[i] = 0.0
			else
			    y[i] = 1.0 - (z+1)**ple # Power-law probability
		    }
		}
	    }

	default:
	    call error (0, "unknown comptype")
	}
end
