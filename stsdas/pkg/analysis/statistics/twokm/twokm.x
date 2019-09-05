#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	16-Sep-92	revised to use asurv 1.2

# TWOKM -- Compute regression slope and intercept by Schmitt's binned method

# This is the routine twokm in the Astronomical Survival
# analysis package.  Here we compute linear regression coefficients
# intercept and slope by Schmitt's binned method.  Because of the
# binning, if you take a finer binsize the results will be better but
# the time required may be very much greater. Bootstrap error analysis is
# implemented.  If the data contain only one type of censoring in the
# dependent (Y) variable, we recommend the EM algorithm.  
#
# Warning: The user should be warned that this program actually changes 
# the data!! First, it redefines some limits to detections.  If the bins 
# are chosen to be too narrow, then virtually all limits could be changed.  
# Second, it pushes each limit into the adjacent bin.  If the bins are 
# chosen to be too wide, this substantially alters the measured values. 
# Thus, the user must tread a fine line in chosing bin sizes.

procedure twokm (ind, x, y, ntot, nxbin, nybin, xsize, ysize, 
		 xorg, yorg, tol, niter, nboot, verbose, nc)

int	ind[ARB]	# i: censor indicator
double	x[ARB]		# i: independent variable values
double	y[ARB]		# i: dependent variable values
int	ntot		# i: number of data values
int	nxbin		# i: number of bins in independent variable
int	nybin		# i: number of bins in dependent variable
double	xsize		# i: size of bins in independent variable
double	ysize		# i: size of bins in dependent variable
double	xorg		# i: origin of bins in independent variable
double	yorg		# i: origin of bins in dependent variable
double	tol		# i: tolerance for regression fit
int	niter		# i: maximum number of iterations for fit
int	nboot		# i: number of bootstrap iterations
bool	verbose		# i: print everything?
int	nc[8]		# i: number of censored points of each type
#--
double	alpha, beta, amean, bmean, aoldmn, boldmn, asigma, bsigma
int	iboot, itot, jtot, totbin, nm[8]
long	seed
pointer	sp, xt, yt, indt, nu, nl, fc, f, a

data	seed / 27453 /

real urand()

begin
	# Allocate dynamic memory for temprorary arrays

	call smark (sp)
	totbin = nxbin * nybin
	call salloc (xt, ntot, TY_DOUBLE)
	call salloc (yt, ntot, TY_DOUBLE)
	call salloc (indt, ntot, TY_INT)
	call salloc (nu, totbin, TY_INT)
	call salloc (nl, 8 * totbin, TY_INT)
	call salloc (fc, totbin, TY_DOUBLE)
	call salloc (f, totbin, TY_DOUBLE)
	call salloc (a, 5 * totbin, TY_DOUBLE)

	# Estimate uncertainty in regression by bootstrap method

	do iboot = 1, nboot {

	    # Choose random sample of data points with replacement

	    do itot = 0, ntot-1 {
		jtot = int(ntot * urand (seed)) + 1
		Memd[xt+itot] = x[jtot]
		Memd[yt+itot] = y[jtot]
		Memi[indt+itot] = ind[jtot]
	    }

	    # Compute regression slope and intercept using sample

	    call schmit (Memd[xt], Memd[yt], Memi[indt], nc, xsize, ysize, 
			 xorg, yorg, tol, niter, ntot, nxbin, nybin, totbin, 
			 nm, Memi[nu], Memi[nl], Memd[fc], Memd[f], Memd[a], 
			 alpha, beta)

	    # Recursive formula for mean and standard deviation
	    # from Knuth's "Art of Computer Programming", Vol. 2 p. 216

	    if (iboot == 1) {
		amean = alpha
		bmean = beta
		asigma = 0.0
		bsigma = 0.0

	    } else {
		aoldmn = amean
		boldmn = bmean

		amean = aoldmn + (alpha - aoldmn) / double (iboot)
		bmean = boldmn + (beta - boldmn) / double (iboot)

		asigma = asigma + (alpha - aoldmn) * (alpha - amean)
		bsigma = bsigma + (beta - boldmn) * (beta - bmean)
	    }
	}

	if (nboot > 1) {
	    asigma = sqrt (asigma / double (nboot - 1))
	    bsigma = sqrt (bsigma / double (nboot - 1))
	} else {
	    asigma = -1.0
	    bsigma = -1.0
	}

	# Compute regression coefficients from actual data

	call schmit (x, y, ind, nc, xsize, ysize, xorg, yorg, tol, 
		     niter, ntot, nxbin, nybin, totbin, nm, Memi[nu], 
		     Memi[nl], Memd[fc], Memd[f], Memd[a], alpha, beta)

	# Print results

	call tkmprint (alpha, beta, asigma, bsigma, nxbin, nybin, xsize, ysize,
		       xorg, yorg, nm, ntot, Memd[f], nboot, verbose)

	call sfree (sp)
end
