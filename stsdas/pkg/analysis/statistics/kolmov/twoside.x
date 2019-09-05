include	<mach.h>
include	<math.h>

# TWOSIDE -- Calculate probability that the observed discrepancies would be
# seen if the twosided distribution was exact.

define	PISQR		9.8696044010893586188	# Famous constant squared
define	QTRPI		0.7853981633974483096	# Famous constant quartered
define	TOLER		1.0e-20			# Stopping tolerance
define	MAXITER		10			# Max number of convergence
						#  iterations

procedure ks_twoside (d, nr, p)

real	d		# The maximum discrepancy (two sided)
real	nr		# Number of samples (or effective number)
real	p		# Probability of D or worse

double	discr		# = n d**2, discriminant for choosing formula
double	q		# Constant factor in exponential expression
double	sum		# Accumulator for quantity
double	z1		# Invariant -- a constant
double	z2		# Invariant -- linear power of J
double	z3		# Invariant -- quadradic power of J
double	z, tol

int	j

# Two sided formula.
#
# The following formula is formula 3.4.8, page 22, of "Distribution Theory for
# Tests Based on the Sample Distribution Function", by J. Durbin, published
# 1973 by the Society for Industrial and Applied Mathematics (SIAM), in
# Philadelphia.
#
# There is an alternate form, formula 3.4.9, which will converge better when
# D*SQRT(N) is small, and which is of much the same form.
#
# Formula 3.4.8 is
#                                            j+1   -2 n d**2 j**2
#       P{ D* > d }  =   2 x  Sumof   [  (-1)   x e                ]   .
#                            j=1,2,...
#
# Formula 3.4.9 is
#                                                           - (2j-1)**2 q'
#       P{ D* > d }  = 1 - sqrt[2 pi/(n D*D)] x Sumof  [  e                ]  .
#                                              j=1,2,...
#
# where              q' =  pi**2 / ( 8 n d**2 )  .
#
# Supposedly, there is some nifty theorem in the study of theta-functions that
# proves the two formulas are the same.  Can't you tell, just by looking ?
# And the result of the calculation is the probability that a sample that
# really did come from the proposed distribution would produce a discrepancy as
# bad as D, or even worse. The bigger the number, the better the fit.

begin
	# Catch and quickly treat a few easy cases.

	if (d == 0.0) {		# Special case
	    p = 1.0		# Discrepancy of zero can't be beat
	    return
	}

	if (nr < 0) {
	    p = 0.0		# Somewhat arbitrary answer
	    return
	}

	# Discriminant value for choosing best formula.
	discr = double (nr) * double (d) * double (d)

	# Decide which formula is most efficient.  The dividing line is roughly
	# where discr = PI/4 :
	#     for discr > PI/4, Durbin's formula 3.4.8 is most efficient;
	#     for discr < PI/4, formula 3.4.9 is better.
	# There is an area of overlap qaround PI/4, where both work well, so
	# one needn't be too worried about differences of 1/100-th or so.

	# Note that as a check, each of the clauses of this main if statement
	# should yield identical results, or nearly identical, when truncation
	# of each series is taken into account.

	if (discr - QTRPI > EPSILOND) {	# Use formula 3.4.8

	    q = -double (2) * discr	# The value of q referred to below

	    # The following is formula 3.4.8, using the method of invariants.
	    # The invariants are (with "constant" q = -2*nr*d*d) :
	    #     z1, which is always exp (q);
	    #     z2, which is always exp (2*j * q); and
	    #     z3, which is always (-1)**j * exp (j*j * q).
	    # except that we have to cheat, briefly, while incrementing j.
	    #
	    # Derivation:
	    #
	    # Because (J + 1)**2 = J*J + 2*J + 1, we note that
	    #
	    #     exp( (j+1)**2*q ) = exp(j*j*q +     2*j*q  +      q)
	    #                       = exp(j*j*q) * exp(2*j*q) * exp(q)
	    #
	    # and because (-1)**(j+1+1) =  - (-1)**(j+1)
	    #
	    #     z3(j+1)     =  - z3(j) * z2(j) * z1(j).
	    #
	    # And of course, since 2*(J+1) = 2*J + 1 + 1, we have
	    #
	    #     z2(j+1)     =  z2(j) * z1(j) * z1(j).
	    #
	    # Hence the two above formulae allow you to tranform the invariant
	    # values to the values they take for their successor values as j
	    # changes to its successor, j+1.  The rest is simple :
	    #     p = 2 * sum of the z3's.

	    # Initialize invariants.
	    j = 1
	    z1 = exp (q)	# z1 =           exp(   q )
	    z2 = z1 * z1	# z2 =           exp( 2jq ) = z1*z1  for j=1
	    z3 = z1		# z3 = (-1)**j * exp( jjq ) = Z1     for j=1

	    # Now that the invariants are true, initialize the sum.
	    sum = z3		# First term in the sum
	    tol = TOLER		# Acceptable relative error
	    tol = z3 * tol	# Estimate of acceptable absolute error

	    while (abs (z3) >= tol && j < MAXITER) {

		# Change the invariants and sum.
		q = z2 * z1		# Intermediate value
		z3 = - z3 * q		# z3(j+1) = -z3*(z2*z1)
		z2 = z1 * q		# z2(j+1) =  (z2*z1)*z1
		sum = sum + z3
		j = j + 1
	    }

	    # Final result (1 - Durbin's formula).  p is now the probability of
	    # a worse fit, i.e., the probability of a larger discrepancy than
	    # the given d.

	    p = 2 * sum

	} else {			# Use formula 3.4.9

	    q = -PISQR / (double (8) * discr)	# Constant value used below

	    # The following is formula 3.4.9, using the method of invariants,
	    # in an almost perfectly identical manner to the above derivation.
	    # The invariants are (with "constant" q = - PI**2/(8 n d**2) :
	    #     z1 = exp(         8 q )
	    #     z2 = exp(       8 j q )
	    #     z3 = exp( [2j-1]**2 q )
	    # except that we have to cheat again while incrementing j.
	    #
	    # Derivation:
	    #
	    # Because (2[j+1] - 1)**2 = (2j-1)**2 + 8j, it follows that
	    #
	    #       exp[ (2j+1)**2 x q ] = exp[ (2j-1)q + 8jq ]
	    #                            = exp[(2j-1)q] x exp(8q)  .
	    #
	    # Hence, z3(j+1) = z3(j) * z2(j).
	    # And of course, since 8(j+1)q = 8jq + 8q, we see easily that
	    #
	    #                  z2(j+1)  =  z2(j) * z1  .
	    #
	    # Hence the two above formulae allow you to tranform the invariant
	    # values to the values they take for their successor values as j
	    # changes to its successor, j+1.  The rest is simple :
	    #     p = 1 - sqrt( 2pi/[n d**2] ) * Sum of the z3's

	    # Initialize invariants.
	    j = 1
	    z3 = exp (q)	# z3 = exp( (2j-1)**2 q ) = exp(q)
	    z = z3 * z3		# z  = exp(2q)
	    z = z * z		# z  = exp(4q)
	    z2 = z * z		# z2 = exp(8jq) = exp(8q),   for j=1
	    z1 = z2		# z1 = exp(8q) as well

	    # Now that the invariants are true, initialize the sum.
	    sum = z3		# First term in the sum
	    tol = TOLER		# Acceptable relative error
	    tol = z3 * tol	# Estimate of acceptable absolute error

	    while (abs (z3) >= tol && j < MAXITER) {

		# Change the invariants and sum.
		z3 = z3 * z2	# z3(j+1) = z3(j)*z2(j)
		z2 = z2 * z1	# z2(j+1) = z2(j)*z1
		sum = sum + z3
		j = j + 1
	    }

	    # Final result (1 - Durbin's formula).  p is now the probability of
	    # a worse fit, i.e., the probability of a larger discrepancy than
	    # the given d.

	    p = real (1) - sqrt (TWOPI / discr) * sum
	}
end
