# DISCRETE -- Compute Kolmogorov statistic for discrete distributions.

procedure ks_discrete (x, nx, y, ny, dplus, dminus)

# Search through the sorted input lists, X and Y, and determine the Kolmogorov
# statistics: D+, D-, and D=max(abs(D+),abs(D-)). The input array Y contains
# the values from a second sample.

real	x[*]	# Sample values (in ascending order)
int	nx	# Number of sample points
real	y[*]	# Comparison samples (also ordered)
int	ny	# Number of comparison points
real	dplus	# Largest "positive" discrepancy : S(X) - Y
real	dminus	# Largest "negative" discrepancy : Y - S(X)

real	d1, d2, p, q
int	i, j, k, l
bool	notdone

begin
	i = 1
	k = 1
	dplus = 0
	dminus = 0

	# Check out each discrepancy, and retain the smallest (most negative)
	# and the largest (most positive).

	while (i <= nx) {

	    # To take care of runs of identical samples, shuffle past duplicate
	    # values : F (x) = Pr{ X =< x }
	    #           X

	    j = i
	    notdone = j < nx			# No shuffle if at the end
	    if (notdone)
		notdone = x[i] == x[j+1]	# No shuffle if different

	    while (notdone) {

		# Next x is the same as the current, so increment j if you
		# want j to index the last sample x which is equal to x[i].

		j = j + 1

		notdone = j < nx		# Done if at the end
		if (notdone)
		    notdone = x[i] == x[j+1]	# Done if different
	    }

	    # At this point either x[i] < x[j+1] (note: strictly less than) or
	    # j = nx.  Following is the inner loop -- two of them in fact --
	    # wherein we loop over the value of j (and then k) until we get
	    #
	    #     y[1], ... , y[k-1] < y[k] = ... = y[l] =< x[i]
	    #
	    # Each value,  F (y[l]) - F (x[j])   and  F (y[k]) - F (x[i])
	    #               Y          X               Y          X
	    # has a shot at being the maximum discrepancy between the two
	    # discrete distributions F   and  F .
	    #                         Y        X

	    l = k
	    notdone = l < ny			# No shuffle if at the end
	    if (notdone)
		notdone = y[l+1] <= x[j]	# Want y[l+1] > x[j]

	    while (notdone) {

		k = l + 1			# Build upon work in inner loop

		# Inner loop: Find the upper end of a chain of equal data.  We
		# want l to index the last value, y[l], which is equal to y[k].
		# In the trivial case, we're just stuck with l=k.

		l = k
		notdone = l < ny		# No shuffle if at the end
		if (notdone)
		    notdone = y[l+1] == y[k]	# Want y[l+1] > y[k]

		while (notdone) {

		    l = l + 1

		    # No check for order is needed since in this loop we expect
		    # only to see y[l] = y[k].

		    notdone = l < ny		# No shuffle at end
		    if (notdone)
			notdone = y[l+1] == y[k]	# Continue to go up
		}

		# At this point, y[k] .. y[l], is a complete chain of equal
		# values (note that k may equal l), all of which are either
		# less than or equal to the chain x[i] .. x[j].  The loop over
		# y is finished when the point beyond (l+1) the end (l) of a
		# chain of equal values exceeds the value held by the chain
		# x[i] ... x[j].

		notdone = l < ny		# No shuffle if at the end
		if (notdone)
		    notdone = y[l+1] <= x[j]	# Want y[k+1] >= x[j]
	    }

	    # Calculate potential discrepancies d1 below the jump in value of
	    # x[i], and d2 above the jump in value of x.  Either d1 or d2 may
	    # provide the maximum value for either of dplus and dminus.

	    if (x[j] == y[l]) {
		p = real (i-1) / nx		# Proportion of x's < x(i)=x(j)
		q = real (k-1) / ny		# Proportion of y's < x(i)=y(k)
		d1 = max (p, 0.0) - max (q, 0.0)	# Discrepancy in sample distribution
		d2 = real (j) / nx - real (l) / ny	# Discrepancy in x and y all =< X(I)
	    } else {
		p = real (i-1) / nx		# Proportion of x's < x(i)=x(j)
		q = real (l) / ny		# Proportion of y's < & =< x(i)
		d1 = max (p, 0.0) - q		# Discrepancy just below x(i) jump
		d2 = real (j) / nx - q		# Discrepancy just above x(i) jump
	    }

	    # Check for those discrepancies which have been exceeded.

	    if (dplus < d1)
		dplus = d1
	    if (dplus < d2)
		dplus = d2
	    if (dminus < -d1)
		dminus = -d1
	    if (dminus < -d2)
		dminus = -d2

	    i = j + 1
	}
end
