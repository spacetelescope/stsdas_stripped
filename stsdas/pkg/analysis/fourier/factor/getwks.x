define	MAX_PX	25

# GETWKS -- Get Winograd kernel sizes.
#
# Collapse powers of small prime factors into non-prime factor(s) for which
# explicit Winograd kernels are available.

procedure getwks (ns, t, p)

int	ns	# i: Number to factor
int	t	# o: Number of factors returned
int	p[ARB]	# o: Factors
#--
int	px[MAX_PX]
int	pf, tp, i

begin
	call amovki (int(0), px, MAX_PX)

	call prime_factor (ns, t, px)

	if (t == 0)
	    return

	pf=0
	tp=1

	while (px[tp+3] == 2) {			# factors of 16
	    pf=pf+1
	    p[pf]=16
	    tp=tp+4
	}

	while (px[tp+2] == 2) {			# factors of 8
	    pf=pf+1
	    p[pf]=8
	    tp=tp+3
	}

	while (px[tp+1] == 2) {			# factors of 4
	    pf=pf+1
	    p[pf]=4
	    tp=tp+2
	}

	if (px[tp] == 2) {			# residual factor of 2
	    pf=pf+1
	    p[pf]=2
	    tp=tp+1
	}

	while (px[tp+1] == 3) {			# factors of 9
	    pf=pf+1
	    p[pf]=9
	    tp=tp+2
	}

	if (px[tp] == 3) {			# residual factor of 3
	    pf=pf+1
	    p[pf]=3
	    tp=tp+1
	}

#	while (px[tp+1] == 5) {			# factors of 25
#	    pf=pf+1
#	    p[pf]=25
#	    tp=tp+2
#	}

	do i=tp, t {				# move the rest
	    pf=pf+1
	    p[pf]=px[i]
	}

	t=pf
end
