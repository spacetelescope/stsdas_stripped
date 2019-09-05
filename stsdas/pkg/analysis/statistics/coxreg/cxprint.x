#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	19-Nov-92	extracted from t_coxreg

# CXPRINT -- Print results from cox proportional hazard model

procedure cxprint (nvar, prob, chi)

int	nvar		# i: Number of degrees of freedom
double	prob		# i: Probability that variables are not correlated
double	chi		# i: Chi squared statistic
#--
double	round()

begin
	call printf ("%5w Global chi-square =%9.3f \n")
	call pargd (round (chi, 3))

	call printf ("%5w with %d degrees of freedom \n\n")
	call pargi (nvar)

	call printf ("%5w Probability       =%10.4f \n")
	call pargd (round(prob, 4))

	call printf ("%5w (Probability that a correlation is not present)\n")
	call printf ("\n")

end
