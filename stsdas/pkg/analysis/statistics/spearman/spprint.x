#* HISTORY *
#* B.Simon	23-Nov-92	adapted from Asurv 1.2

# SP_PRINT -- Print results from calculation of Spearman's rho

procedure sp_print (ntot, rho, prob)

int	ntot		# i: Number of data points
double	prob		# i: Probability that variables are not correlated
double	rho		# i: Spearman's rho
#--
double	round()

begin
	call printf ("%6w Spearman's rho =%10.3f\n")
	call pargd (round(rho, 3))

	call printf ("%6w Probability    =%11.4f\n")
	call pargd (round (prob, 4))
	call printf ("%6w (Probability that a correlation is not present)\n")

	if (ntot <= 30) {
	    call printf ("\n%6w WARNING: Test is accurate only if N > 30")
	    call printf ("   (N = %d)\n")
	    call pargi (ntot)
	}

	call printf ("\n")
end
