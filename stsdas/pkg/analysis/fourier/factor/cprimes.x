# COMPUTE_PRIMES -- Calculate the first N prime numbers.  Routine is
# based on Knuth, Fundamental Algorithms, sec. 1.3.2, algorithm P, p. 143

procedure compute_primes (nprimes, primes)

int	nprimes		# Number of primes to compute
int	primes[*]	# Array of first nprimes prime numbers
#---------------------------------------------------------------------------
int	n, j, k, q, r
bool	notdone

begin
	notdone = true

	primes[1]=2
	primes[2]=3

	do j = 3, nprimes {

	    for (n=primes[j-1]+2; notdone; n=n+2) {
		for (k=2; notdone; k=k+1) {
		    q=n/primes[k]
		    r=mod(n,primes[k])
		    if (r == 0 || q <= primes[k])
			break
		}
		if (r != 0 && q <= primes[k])
		    break
	    }
	    primes[j]=n
	}
end
