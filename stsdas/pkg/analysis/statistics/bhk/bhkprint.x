#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	18-Nov-92	extracted from t_bhk

# BHKPRINT -- Print results from BHK method

procedure bhkprint (zvalue, prob, tau)

double	zvalue		# i: Approximately normally distributed statistic
double	prob		# i: Probability that variables are not correlated
double	tau		# i: Kendall's tau
#--

begin
	call printf ("%6w Kendall's tau =%13.4f\n")
	call pargd (tau)

	call printf ("%6w Z-value       =%12.3f\n")
	call pargd (zvalue)

	call printf ("%6w Probability   =%13.4f\n")
	call pargd (prob)
	call printf ("%6w (Probability that a correlation is not present)\n")

	call printf ("\n")
end
