#* HISTORY *
#* D.Ball	18-Apr-88	from the ASURV routine of the same name
#* B.Simon	02-Sep-92	revised to use asurv 1.2

# STAT -- Compute the final test statistic from the scores

procedure stat (verb, id1, id2, n1, n2, ncomp, ntot, score, test)

bool	verb		# i: verbose output flag
int	id1[ntot]	# i: censor flag
int	id2[ntot]	# i: group indicator
int	n1		# i: number of uncensored observations
int	n2		# i: number of censored observations
int	ncomp		# i: total number of observations
int	ntot		# i: declared length of arrays
double	score[ntot]	# o: test scores
double	test		# o: test statistic
#--
int	i
double	ww, sum, variance

double	round()

begin
	ww = 0.0
	sum = 0.0

	do i = 1, ncomp {
	    sum = sum + score[i] * score[i]
	    if (id2[i] != 2)
		ww = ww + score[i]
	}

	variance = sum * real(n1) * real(n2) / (real(ncomp) * 
		   (real(ncomp) - 1.0))

	if (verb) {
	    call printf ("%9w Score    = %15.3f\n")
	    call pargd (round (ww, 3))
	    call printf ("%9w Variance = %15.3f\n")
	    call pargd (round (variance, 3))
	    call printf ("\n")
	}

	# Compute test statistic
	test = ww / sqrt (variance)

end
