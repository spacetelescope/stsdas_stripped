#* HISTORY *
#* D.Ball	05-Apr-88	adapted from em routine in asurv
#* B.Simon	23-Sep-92	extracted from em.x

procedure emprint (alpha, sigmaa, tol, mplone, icheck, ifault)

double	alpha[ARB]	# i: regression coefficients and std. dev. of fit
double	sigmaa[ARB]	# i: standard deviation of regression coefficients
double	tol		# i: Convergence tolerance
int	mplone		# i: size of alpha and sigmaa arrays
int	icheck		# i: Nonzero if em method ended early, 0 otherwise
int	ifault		# i: failure indicator
#--
int	i, ite

string	intcptfmt "%7w Intercept coefficient: %10.4f, with stand.dev. %10.4f\n"
string	slopefmt  "%7w Slope coefficient %3d: %10.4f, with stand.dev. %10.4f\n"
string	stddevfmt "%7w Standard deviation of the regression:  %10.4f\n"
string	iterfmt   "%7w Iterations used:  %d (with tolerance %12.6f)\n"

string	warn1a    "%4w Maximum number of iterations reached and\n"
string	warn1b    "%4w convergence has not been obtained.\n"
string	warn2a    "%4w Confined observation not properly given\n"
string	warn2b    "%4w (\"upper\" limit <= \"lower\" limit).\n"
string	warn3a    "%4w Problems with the tolerance?\n"
string	warn4a    "%4w Number of completely specified data is\n"
string	warn4b    "%4w less than number of variables + 2.\n"
string	warn5a    "%4w The covariance matrix is not positive definite\n"
string	warn5b    "%4w and could not be inverted.\n"
string	warn6a    "%4w The matrix to estimate errors in the coefficients\n"
string	warn6b    "%4w is not positive definite and could not be inverted.\n"
string	warn7a    "%4w Length of work arrays is not sufficient \n"
string	warn7b    "%4w to preform regression\n"
string	warndef   "%4w An unknown error has occured in subroutine EMALGO\n"

double	round()

begin
	# Print final regression coefficients

	if (ifault >= 0) {
	    if (icheck != 0)
		ite = icheck
	    else
		ite = ifault

	    call printf (intcptfmt)
	    call pargd (round (alpha[1], 4))
	    call pargd (round (sigmaa[1], 4))

	    do i = 2, mplone-1 {
		call printf (slopefmt)
		call pargi (i-1)
		call pargd (round (alpha[i], 4))
		call pargd (round (sigmaa[i], 4))
	    }
	    call printf ("\n")

	    call printf (stddevfmt)
	    call pargd (round (alpha[mplone], 4))

	    call printf (iterfmt)
	    call pargi (ite)
	    call pargd (tol)

	# Print error message instead if a problem has ocurred

	} else {
	    call printf("%4w WARNING:\n")
	    switch (ifault) {

	    case -1:
		call printf (warn1a)
		call printf (warn1b)

	    case -2:
		call printf (warn2a)
		call printf (warn2b)

	    case -3:
		call printf (warn3a)

	    case -4:
		call printf (warn4a)
		call printf (warn4b)

	    case -5:
		call printf (warn5a)
		call printf (warn5b)

	    case -6:
		call printf (warn6a)
		call printf (warn6b)

	    case -7:
		call printf (warn7a)
		call printf (warn7b)

	    default:
		call printf (warndef)
	    }
	}

	call printf ("\n")
end
