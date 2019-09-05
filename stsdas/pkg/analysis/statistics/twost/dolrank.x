#* HISTORY *
#* B.Simon	02-Sep-92	converted from twost in ASURV 1.2

# DO_LRANK -- Do Logrank Test

procedure do_lrank (verbose, isign, id1, id2, xy, n1, n2, ncomp, ntot)

bool	verbose		# i: Print long form of output?
int	isign		# i: Sign reversal flag (-1 if sign has been changed)
int	id1[ARB]	# i: Censorship flag (0 = uncensored, 1 = censored)
int	id2[ARB]	# i: Group flag (1 = first group, 2 = second group)
double	xy[ARB]		# i: Observation values
int	n1		# i: Number of observations in first group
int	n2		# i: Number of observations in second group
int	ncomp		# i: Number of observations in both groups (n1 + n2)
int	ntot		# i: Number of observations in all groups (>= ncomp)
#--
int	i, j
double	test, prob, score, var, temp1, temp2
pointer	sp, d, e, r, d1, e1, r1, d2, e2, r2

string	title  "\n%7w Logrank Test \n\n"

double	round()

begin

	# Allocate work arrays

	call smark (sp)
	call salloc (d, ntot, TY_DOUBLE)
	call salloc (e, ntot, TY_DOUBLE)
	call salloc (r, ntot, TY_DOUBLE)
	call salloc (d1, ntot, TY_DOUBLE)
	call salloc (e1, ntot, TY_DOUBLE)
	call salloc (r1, ntot, TY_DOUBLE)
	call salloc (d2, ntot, TY_DOUBLE)
	call salloc (e2, ntot, TY_DOUBLE)
	call salloc (r2, ntot, TY_DOUBLE)

	call printf (title)

	# Calculate test results

	call lrank (id1, id2, xy, n1, n2, ncomp, ntot, test, prob,
		    Memd[d], Memd[e], Memd[r], Memd[d1], Memd[e1],
		    Memd[r1], Memd[d2], Memd[e2], Memd[r2], score, var)

	# Print test results

	if (verbose) {
	    call printf ("%9w Score    = %15.3f\n")
	    call pargd (round (score, 3))
	    call printf ("%9w Variance = %15.3f\n")
	    call pargd (round (var, 3))

	    j = 0
	    call printf ("\n%14w T(i)%8w Score(i) \n")

	    do i = 1, ncomp {
		if (i > 1) 
		    if (xy[i] == xy[i-1])
			next
		if (id1[i] == 1)
		    next

		temp1 = isign * xy[i]

		if (Memd[r+j] == 0.0) {
		    temp2 = 0.0
		} else {
		    temp2 = Memd[d2+j] - (Memd[r2+j] * Memd[d+j] / Memd[r+j])
		}

		call printf ("%5w %14.3f %14.3f\n")
		call pargd (round (temp1, 3))
		call pargd (round (temp2, 3))

		j = j + 1
	    }
	    call printf ("\n")
	}

	call printf ("%9w Test Statistic        = %11.3f \n")
	call pargd (round (test, 3))

	call printf ("%9w Probability           = %12.4f \n")
	call pargd (round (prob, 4))

	call sfree (sp)
end
