include	<mach.h>

# CONTINUOUS -- Determine Kolmogorov statistics on continuous distribution.

procedure ks_continuous (x, y, n, dplus, dminus)

# Search through the sorted input lists, X and Y, and determine the Kolmogorov
# statistics: D+, D-, and D=max(abs(D+),abs(D-)). The input array Y contains
# the values from a continuous distribution sampled for every value of X:
# y[i] = F(x[i]), for i = 1...n.
# Note: X and Y must have been sorted in ascending order before.

real	x[*]	# Sample values (in ascending order)
real	y[*]	# Cumulative distribution (order y=F(x))
int	n	# Dimensions of the two input arrays
real	dplus	# Most positive discrepancy : S(X) - Y
real	dminus	# Most negative discrepancy : Y - S(X)

real	d	# Largest absolute discrepancy
int	i
#bool	notdone

begin
	dplus = 0.0
	dminus = 0.0

	# Check out each discrepancy, and retain the smallest (most negative)
	# and the largest (most positive).

	do i = 1, n {

# the following block is deleted (commented out) by JC Hsu on 12/4/91 due
# its abuse of i
	    # Shuffle past duplicate values : F(x) = Pr{ X =< x }
	    # Be suspicious and make sure that the comparison distribution
	    # repeats the values if the sample does so.

	    #notdone = (i < n)			# No shuffle if at the end
	    #if (notdone)
		#notdone = x[i] == x[i+1]	# No shuffle if different

	    #while (notdone) {

		#if (y[i] != y[i+1])		# Remember : I+1 =< N
		    #return

		# Determine if we go for another round.  Does the next sample
		# match the current ?  If they aren't the same, we are done.

		#i = i + 1
		#notdone = (i < n)		# Done if at the end
		#if (notdone)
		    #notdone = x[i] == x[i+1]	# Done if different
	     #}
# end of deletion

	    # Ensure that Y is between 0 and 1.
	    if (y[i] < 0. || y[i] > 1.0)
		return

	    # The Kolmogorov statistic is the discrepancy between the sample
	    # and the distribution.

	    d = real (i) / n - y[i]

	    if ((dplus - d) < EPSILON)
		dplus = d
	    else if ((dminus + d) < EPSILON)
		dminus = -d

	}
end
