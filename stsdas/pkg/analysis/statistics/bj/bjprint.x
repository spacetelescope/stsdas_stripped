#* HISTORY *
#* D.Ball	05-Apr-88	adapted from bj routine in asurv
#* B.Simon	12-Nov-92	extracted from bj.x

procedure bjprint (alpha, sigmaa, ite, nvar)

double	alpha[ARB]	# i: regression coefficients and std. dev. of fit
double	sigmaa[ARB]	# i: standard deviation of regression coefficients
int	ite		# i: number of iterations performed
int	nvar		# i: number of independent variables
#--
int	i 

string	intcptfmt "%7w Intercept coefficient: %10.4f\n"
string	slopefmt  "%7w Slope coefficient %3d: %10.4f, with stand.dev. %10.4f\n"
string	stddevfmt "%7w Standard deviation of the regression:  %10.4f\n"
string	iterfmt   "%7w Iterations used:  %d \n"

double	round()

begin
	# Print final regression coefficients
	
	call printf (intcptfmt)
	call pargd (round (alpha[1], 4))
	
	do i = 2, nvar+1 {
	    call printf (slopefmt)
	    call pargi (i-1)
	    call pargd (round (alpha[i], 4))
	    call pargd (round (sigmaa[i], 4))
	}
	call printf ("\n")
	
	call printf (stddevfmt)
	call pargd (round (alpha[nvar+2], 4))
	
	call printf (iterfmt)
	call pargi (ite)
	
	call printf ("\n")
end
