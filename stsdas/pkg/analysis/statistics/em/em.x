#* HISTORY *
#* D.Ball	05-Apr-88	adapted from em routine in asurv
#* B.Simon	23-Sep-92	revised to use asurv 1.2

# EM -- EM method linear regression

# This computes a maxixmum likelihood estimation in a linear model 
# from confined and censored data. References: M.S. Wolynetz AS 139 
# APL.STATIST.VOL 28 195 (1979) plus corrections in later issues 
# of Applied Statistics

procedure em (ind, x, y, y2, nvar, ntot, nc, tol, niter, alpha)

int	ind[ARB]	# i: indicator of censoring
double	x[ntot,nvar]	# i: data for each of the independent variables
double	y[ARB]		# i: dependent variable
double	y2[ARB]		# i: upper limits in dependent variable
int	nvar		# i: Number of variables in the data
int	ntot		# i: number of data values
int	nc		# i: number of censored points in dependent variable
double	tol		# i: tolerance for the regression fit
int	niter		# i: maximum number of iterations to try
double	alpha[ARB] 	# u: regression coefficients and standard dev. of fit
#--
int	i, j, icheck, ifault, mplone, lenw, lenwrk
pointer	sp, tola, sigmaa, w, wcen, vcov, work

begin
	# Compute work array dimensions

	mplone = nvar + 1
	lenw = ntot + mplone
	lenwrk = ntot * mplone

	# Allocate working arrays for the calculations

	call smark (sp)
	call salloc (tola, mplone, TY_DOUBLE)
	call salloc (sigmaa, mplone, TY_DOUBLE)
	call salloc (w, lenw, TY_DOUBLE)
	call salloc (wcen, lenw, TY_DOUBLE)
	call salloc (vcov, lenwrk, TY_DOUBLE)
	call salloc (work, lenwrk, TY_DOUBLE)

	# Set the tolerances in each coefficient

	call amovkd (tol, Memd[tola], mplone)

	# Compute the regression coefficients

	icheck = 0
	call emalgo (ntot, y, y2, ind, mplone, x, Memd[w], Memd[wcen], lenw, 
		     Memd[vcov], Memd[work], lenwrk, alpha, Memd[tola],
		     niter, ifault, icheck, nc)

	# Compute standard deviations from covariance matrix

	if (ifault < 0 || icheck != 0) {
	    call aclrd (Memd[sigmaa], mplone)

	} else {
	    j = 0
	    do i = 1, mplone {
		Memd[sigmaa+i-1] = sqrt (abs (Memd[vcov+j]))
		j = j + mplone + 1
	    }
	}

	# Report results and /or errors

	call emprint (alpha, Memd[sigmaa], tol, mplone, icheck, ifault)

	call sfree(sp)
end
