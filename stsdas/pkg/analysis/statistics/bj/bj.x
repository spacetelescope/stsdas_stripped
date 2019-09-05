#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	12-Nov-92	revised to use asurv 1.2

# BJ -- Linear regression with censored data : buckley-james method

procedure bj (ind, x, y, nvar, ntot, nu, nc, icens, tol, niter, alpha)

int	ind[ARB]	# i: indicator of type of censoring
double	x[nvar,ntot]	# i: independent variable values
double	y[ARB]		# i: dependent variable values
int	nvar		# i: number of regression coefficients
int	ntot		# i: number of data values
int	nu		# i: number of obervations that were uncensored
int	nc		# i: number of observations that were censored
int	icens		# i: type of censoring
double	tol		# i: tolerance for regression fit
int	niter		# i: maximum number of iterations for fit
double	alpha[ARB]	# o: regression coeddicients
#--
int	i, j, nvar1, nvar2, ite
pointer	sp, sigmaa, array, v, bu, test, test2
pointer	ind2, xx, z, w, wx, t, ty, zy, ipt, nd, no

begin
	# Allocate dynamic memory for temporary arrays

	nvar1 = nvar + 1
	nvar2 = nvar + 2

	call smark (sp)
	call salloc (sigmaa, nvar1, TY_DOUBLE)
	call salloc (array, nvar*nvar, TY_DOUBLE)
	call salloc (v, nvar, TY_DOUBLE)
	call salloc (bu, nvar, TY_DOUBLE)
	call salloc (test, nvar1, TY_DOUBLE)
	call salloc (test2, nvar1, TY_DOUBLE)
	call salloc (ind2, ntot, TY_INT)
	call salloc (xx, nvar*ntot, TY_DOUBLE)
	call salloc (z, ntot, TY_DOUBLE)
	call salloc (w, ntot, TY_DOUBLE)
	call salloc (wx, ntot, TY_DOUBLE)
	call salloc (t, ntot, TY_DOUBLE)
	call salloc (ty, ntot, TY_DOUBLE)
	call salloc (zy, ntot, TY_DOUBLE)
	call salloc (ipt, ntot, TY_INT)
	call salloc (nd, ntot, TY_INT)
	call salloc (no, ntot, TY_INT)

	# If censoring is due to upper limits, change the signs of data
	# x(i) and y(i) because b-j method assumes lower limits.

	if (icens < 0) {
	    do i = 1, ntot {
		do j = 1, nvar {
		    x(j,i) = - x(j,i)
		}
		y(i) = - y(i)
	    }
	}

	# Buckly : the subroutine which performs the buckley and
	# james method.

	call buckly (x, y, ind, tol, ntot, nvar, nvar1, nvar2, nu, nc, 
		     niter, alpha, Memd[sigmaa], ite, Memd[array], Memd[v], 
		     Memd[bu], Memd[test], Memd[test2], Memi[ind2], Memd[xx],
		     Memd[z], Memd[w], Memd[wx], Memd[t], Memd[ty], Memd[zy], 
		     Memi[ipt], Memi[nd], Memi[no])

	# Correct the signs of the data to the original ones, if the
	# censoring is upper limit.

	if (icens < 0) {
	    do i = 1, ntot {
		do j = 1, nvar {
		    x(j,i) = - x(j,i)
		}
		y(i) = - y(i)
	    }

	    alpha(1) = - alpha(1)
	}

	# Print the results of the Buckly-James regression

	call bjprint (alpha, Memd[sigmaa], ite, nvar)

	call sfree (sp)
end
