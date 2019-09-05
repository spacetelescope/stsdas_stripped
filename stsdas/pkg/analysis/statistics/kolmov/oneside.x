include	<mach.h>

# ONESIDE -- Calculate the probability that the observed discrepancies would
# be seen, if the distribution was exact.

procedure ks_oneside (d, nr, p)

real	d		# The maximum discrepancy (one sided)
real	nr		# Number of samples (or effective number)
real	p		# Probability of d or worse

double	binomial	# Binomial coefficient of M above, I below
double	cz		# Complement of Z, which is (1 - Z)
double	czm		# (1 - Z)**I
double	dn		# D times N
double	sum		# Accumulator for summation
double	z		# Probability of F(X(i)) > min[ i/n + D, 1]
double	zi		# Z**(I-1)

int	i, j, m, n

# Calculate probability using formula 2.1.12 from monograph by J. Durbin,
# "Distribution Theory for Tests Based on the Sample Distribution Function";
# published by the Society for Industrial and Applied Mathematics (SIAM),
# Philadelphia, 1973.
# The formula, which is given below, is a simple sum of binomial probabilities
# (with an intervening term of differential probability "d".
#                                               i-1                 n-i
#    Pr{ D+ > d }  =   Sumof {  B(i,n) (i/n + d)    d  (1 - [i/n+d])    }
#                        i
# The sum is taken for i = 0, 1, 2, ... j; where "j" is the greatest integer
# which is strictly less than the quantity n - n d. (Note that for any
# i >= n - n d, the last factor above would be negative or zero; negative
# probabilities are illegal, and a zero probability simply means a zero term
# in the sum.)
# An intuitive, but only partially correct justification of the formula above:
# the first factor in the sum, the quantity "(i/n + d)**(i-1)", is the
# probability that i-1 samples fall below the (i/n+d)_th quantile, the middle
# factor, "d", is the probability that one observation falls roughly between
# the (i/n)_th and the (i/n+d)_th quantile, and the last factor is the
# probability that the remaining n-i values all lie above the (i/n+d)_th
# quantile. The previously neglected binomial coefficient, "B(i,n)", is the
# number of unordered combinations of "n" things taken "i" at a time, and
# compensates for the fact that the occurance of values in the sample does not
# have to be in order, hence proportionally more samples will be observed which
# fall within the above criterion.
#
# One sided formula for  Pr{ D+ > D } = Pr{ D- > D }.
# Note:  the use of absolute values below may be invalid in most cases where
# they are used.  I'm just not sure and need to check.
#
# T.W.Lougheed, 28 August 1986

begin
	# Catch and quickly treat a few easy cases.

	if (d <= 0.0) {		# Special (pathological) case
	    p = 1.0		# Discrepancy of zero can't be beat
	    return
	}

	if (nr < 1) {
	    p = 0.0		# Somewhat arbitrary answer
	    return
	}

	if (d >= 1.0) {
	    p = 0.0		# Couldn't be worse than 1
	    return
	}

	# Assorted initializations.

	n = int (nr)
	dn = double (d) * double (nr)
	j = n - int (dn)	# Upper limit for sum index
	binomial = double(1)	# B(0,N)
	i = 1			# Exponent of Z (one term)
	m = n			# Exponent of 1-Z (zero term)
	sum = double (0)

	# Calculate probability using Durbin's formula

	while (i < j) {		# Final term is the I = J term

	    if (abs (binomial) - double (1) < EPSILOND)
		binomial = (binomial * m) / i	# Update binomial, now = B(i,n)
	    else
		binomial = (binomial / i) * m

	    m = m - 1			# Delayed reduction.  Exponent for cz
	    z = (i + dn) / n		# Prob of X(i) > (i/n + D)-th quantile
	    zi = z**(i-1)		# Prob of i-1 prev values < quantile
	    cz = 1 - z			# Prob of X(i) < (i/n + D)-th quantile

	    # As long as the probability is greater than zero, calculate the
	    # probability that n-i successive values > quantile (czm) and then
	    # add in net probability.  NB: a common factor of d is left out.

	    if (cz > EPSILOND) {
		czm = cz**m
		sum = sum + binomial * zi * czm
	    }
	    i = i + 1
	}

	# Treat first term (i=0) as a separate case.

	z = double (d)
	cz = double (1) - z

	# Final result (1 - Durbin's formula). p is the probability of a worse
	# fit, i.e., the probability of a larger discrepancy than the given d.
	# Note that the factor of d is included in this computation.

	p = cz**n + z*sum		# Zero order term + other terms
end
