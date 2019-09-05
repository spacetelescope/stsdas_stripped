define	NPRIMES	170

# PRIME_FACTOR -- Factor an integer ns into its t prime factors p.
#
# Based on Knuth, Seminumerical Algorithms, sec. 4.5.4, algorithm A, p. 364.
# The algorithm depends on a list of primes and can only reliably factor
# numbers up to the square of the largest number in the list of primes.  If
# an input greater than this occurs, the number of factors t is returned as 0.
#
# C. D. Biemesderfer, STScI, Nov 87
# Phil Hodge, 19-Nov-1991  Include an explicit comparison between ns and lfn.
# Phil Hodge,  6-Aug-1993  Include test on ns < 4.

procedure prime_factor (ns, t, p)

int	ns	# i: Number to be factored
int	t	# o: Number of prime factors
int	p[*]	# o: Prime factors of ns
#--
pointer	sp, d
int	n, k, q, r, lfn

begin
	if (ns < 4) {
	    t = 1
	    p[1] = ns
	    return
	}

	call smark (sp)
	call salloc (d, NPRIMES, TY_INT)

	call compute_primes (NPRIMES, Memi[d])
	lfn = Memi[d+NPRIMES-1] * Memi[d+NPRIMES-1]

	t=0
    	k=0

	if (ns > lfn)			# can't factor it
	    return

	for (n=ns; n > 1 && n <= lfn; ) {

	    q=n/Memi[d+k]
	    r=mod(n,Memi[d+k])

	    if (r == 0) {
		t=t+1
		p[t]=Memi[d+k]
		n=q
	    } else {
		if (q <= Memi[d+k])
		    break
		k=k+1
	    }
	}

	if (n > 1) {
	    t=t+1
	    p[t]=n
	}

	call sfree (sp)
end
