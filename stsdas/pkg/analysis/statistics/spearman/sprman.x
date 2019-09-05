#* HISTORY *
#* B.Simon	20-Nov-92	Adapted from Asurv 1.2

# SPRMAN -- Compute Spearman's rho

procedure sprman (ind, x, y, ntot, indx, xf, xx, rho, prob)

int	ind[ARB]	# i: Censor indicator
double	x[ARB]		# i: Independent variable
double	y[ARB]		# i: Dependent variable
int	ntot		# i: Number of observations
int	indx[2,ntot]	# o: Separated censor indicator array
double	xf[2,ntot]	# o: Adjusted rank array
double	xx[2,ntot]	# o: Data array
double	rho		# o: Spearman's rho
double	prob		# o: Probability null hypothesis is true
#--
double	t1,tavg
int	i, j, it, iad

begin
	# Initialize observation and rank arrays

	do i = 1, ntot {
	    xf[1,i] = 0.0
	    xf[2,i] = 0.0

	    xx[1,i] = x[i]
	    xx[2,i] = y[i]
	}
	
	# Compute separate censor array

	do i = 1, ntot {
	    indx[1,i] = 0
	    indx[2,i] = 0

	    iad = abs (ind[i])
	    switch (iad) {
	    case 1:
		indx[2,i] = ind[i] / iad
	    case 2:
		indx[1,i] = ind[i] / iad
	    case 3:
		indx[1,i] = ind[i] / iad
		indx[2,i] = ind[i] / iad
	    case 4:
		indx[1,i] = - ind[i] / iad
		indx[2,i] = ind[i] / iad
	    }
	}

	# Compute the Akritas rank (xf)

	do j = 1, 2 {
	    call sp_sort (ind, indx, xf, xx, j, ntot)
	    call akrank (indx, xx, j, 2, ntot, xf)
	}

	# Make sure that ranks agree at tied points

	do i = 1, ntot-1 {
	    it = 1
	    t1 = xf[1,i]
	    do j = i+1, ntot {
		if (xx[1,i] == xx[1,j] && indx[1,i] == indx[1,j]) {
		    it = it + 1
		    t1 = t1 + xf[1,j]
		}
	    }

	    if (it > 1) {
		tavg = t1 / double(it)
		xf[1,i] = tavg
		do j = i+1, ntot {
		    if(xx[1,i] == xx[1,j] && indx[1,i] == indx[1,j]) {
			xf[1,j] = tavg
		    }
		}
	    }

	    it = 1
	    t1 = xf[2,i]
	    do j = i+1, ntot {
		if (xx[2,i] == xx[2,j] && indx[2,i] == indx[2,j]) {
		    it = it + 1
		    t1 = t1 + xf[2,j]
		}
	    }

	    if (it > 1) {
		tavg = t1 / double(it)
		xf[2,i] = tavg
		do j = i+1, ntot {
		    if (xx[2,i] == xx[2,j] && indx[2,i] == indx[2,j])
			xf[2,j] = tavg
 		}
	    }
       }

	# Compute Spearman's rho from Akritas rank 

	call sperho (xf, ntot, rho, prob)

end
