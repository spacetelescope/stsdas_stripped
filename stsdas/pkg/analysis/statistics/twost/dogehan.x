#* HISTORY *
#* B.Simon	02-Sep-92	converted from twost in ASURV 1.2

# DO_GEHAN -- Do Gehan's generalized Wilcoxon test with permutation variance

procedure do_gehan (verbose, isign, id1, id2, xy, ic, n1, n2, ncomp, ntot)

bool	verbose		# i: Print long form of output?
int	isign		# i: Sign reversal flag (-1 if sign has been changed)
int	id1[ARB]	# i: Censorship flag (0 = uncensored, 1 = censored)
int	id2[ARB]	# i: Group flag (1 = first group, 2 = second group)
double	xy[ARB]		# i: Observation values
int	ic		# i: Number of censored observations
int	n1		# i: Number of observations in first group
int	n2		# i: Number of observations in second group
int	ncomp		# i: Number of observations in both groups (n1 + n2)
int	ntot		# i: Number of observations in all groups (>= ncomp)
#--
int	i
double	test, prob, temp1
pointer	sp, r1, r2

string	title  "\n%7w Gehan's Generalized Wilcoxon Test -- \
Permutation Variance \n\n"

double	round()

begin

	# Allocate work arrays

	call smark (sp)
	call salloc (r1, ntot, TY_DOUBLE)
	call salloc (r2, ntot, TY_DOUBLE)

	call printf (title)

	# Calculate test results

	call gehan (verbose, xy, id1, id2, ic, n1, n2, ncomp, ntot,
		    Memd[r1], Memd[r2], test, prob)

	# Print test results

	if (verbose) {
	    call printf ("\n%16w T(i)%6w ID1(i)%3w ID2(i)%2w R1(i)%3w R2(i)\n")

	    do i = 1, ncomp {
		temp1 = isign * xy[i]

		call printf ("%5w %14.3f %9d %9d %9.1f %9.1f\n")
		call pargd (round (temp1, 3))
		call pargi (id1[i])
		call pargi (id2[i])
		call pargd (round (Memd[r1+i-1], 1))
		call pargd (round (Memd[r2+i-1], 1))
	    }
	    call printf ("\n")
	}

	call printf ("%9w Test Statistic        = %11.3f \n")
	call pargd (round (test, 3))

	call printf ("%9w Probability           = %12.4f \n")
	call pargd (round (prob, 4))

	call sfree (sp)
end
