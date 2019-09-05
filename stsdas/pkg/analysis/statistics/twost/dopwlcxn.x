#* HISTORY *
#* B.Simon	02-Sep-92	converted from twost in ASURV 1.2

# DO_PWLCXN -- Do Peto & Peto generalized Wilcoxon test 

procedure do_pwlcxn (verbose, isign, id1, id2, xy, xm, h, 
		     ic, n1, n2, ncomp, ntot)

bool	verbose		# i: Print long form of output?
int	isign		# i: Sign reversal flag (-1 if sign has been changed)
int	id1[ARB]	# i: Censorship flag (0 = uncensored, 1 = censored)
int	id2[ARB]	# i: Group flag (1 = first group, 2 = second group)
double	xy[ARB]		# i: Observation values
double	xm[ARB]		# i: Multiplicity of the i-th distinct failure time
double	h[ARB]		# i: Estimate of the survivor function
int	ic		# i: Number of censored observations
int	n1		# i: Number of observations in first group
int	n2		# i: Number of observations in second group
int	ncomp		# i: Number of observations in both groups (n1 + n2)
int	ntot		# i: Number of observations in all groups (>= ncomp)
#--
int	i, iwlcx
double	test, prob, temp1
pointer	sp, score

string	title  "\n%7w Peto & Peto Generalized Wilcoxon Test \n\n"

double	round()

begin

	# Allocate work arrays

	call smark (sp)
	call salloc (score, ntot, TY_DOUBLE)

	call printf (title)

	# Calculate test results

	call pwlcxn (verbose, xy, id1, id2, ic, n1, n2, ncomp, ntot,
		     h, xm, Memd[score], test, prob, iwlcx)

	# Print test results

	if (verbose) {
	    call printf ("%14w T(i)%8w Score(i)\n")

	    do i = 1, ncomp {
		temp1 = isign * xy[i]

		call printf ("%5w %14.3f %14.3f\n")
		call pargd (round (temp1, 3))
		call pargd (round (Memd[score+i-1], 3))
	    }
	    call printf ("\n")
	}

	call printf ("%9w Test Statistic        = %11.3f \n")
	call pargd (round (test, 3))

	call printf ("%9w Probability           = %12.4f \n")
	call pargd (round (prob, 4))

	call sfree (sp)
end
