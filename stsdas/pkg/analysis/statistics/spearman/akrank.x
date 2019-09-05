#* HISTORY *
#* B.Simon	20-Nov-92	Adapted from Asurv 1.2

# AKRANK -- Compute the Akritas' rank 

procedure akrank (indx, xx, jcol, ncol, ntot, xf)

int	indx[ncol,ntot]	# i: Separated censor indicator array
double	xx[ncol,ntot]	# i: Data array
int	jcol		# i: Selected column
int	ncol		# i: Total number of columns
int	ntot		# i: Number of observations
double	xf[ncol,ntot]	# o: Adjusted rank array
#--
int	isign, iu, ic, ltot, ichange, nchange, i, j
double	smean, error
pointer	sp, zu, zc, risk, atie, pl, vr, lx, fmass, ptemp, f

begin
	# Allocate work arrays

	call smark (sp)
	call salloc (zu, ntot, TY_DOUBLE)
	call salloc (zc, ntot, TY_DOUBLE)
	call salloc (risk, ntot, TY_DOUBLE)
	call salloc (atie, ntot, TY_DOUBLE)
	call salloc (pl, ntot, TY_DOUBLE)
	call salloc (vr, ntot, TY_DOUBLE)
	call salloc (lx, ntot, TY_INT)
	call salloc (fmass, ntot, TY_DOUBLE)
	call salloc (ptemp, ntot, TY_DOUBLE)
	call salloc (f, ntot, TY_DOUBLE)

	# Separate censored data from uncensored data
	# The output arrays risk and atie are not use in this procedure

	call xvar (indx, xx, jcol, ncol, ntot, isign, Memd[zu], Memd[zc], 
		   iu, ic, Memd[risk], Memd[atie], ltot)

	# Reversing the output arrays sorts them if they were
	# multiplied by -1

	if (isign < 0) {
	    call revrsd (Memd[zu], iu)
	    call revrsd (Memd[zc], ic)
	}

	# Compute the Kaplan-Meier estimator

	call plestm (Memd[zu], Memd[zc], iu, ic, Memd[pl], Memd[vr], 
		ntot, smean, error, ichange, nchange, Memi[lx])

	# Adjust product limit estimator to include censored data points

	if (ic != 0) {
	    if (isign < 0) {
		Memd[fmass] = 1.0 - Memd[pl]
		do i = 1, iu-1
		    Memd[fmass+i] = Memd[pl+i-1] - Memd[pl+i]

		call revrsd (Memd[fmass], iu)

		do i = 0, iu-1 {
		    Memd[ptemp+i] = 1.0
		    do j = 0, i
			Memd[ptemp+i] = Memd[ptemp+i] - Memd[fmass+j]
		}

	    } else {
		do i = 0, iu-1
		    Memd[ptemp+i] = Memd[pl+i]
	    }

	    # Assign product limit estimator values to censored points
	    
	    if (indx[jcol,1] == 0) {
		Memd[pl] = Memd[ptemp]
		j = 1
	    } else {
		Memd[pl] = 1.0
		j = 0
	    }
	    
	    do i = 1, ntot-1 {
		if (indx[jcol,i+1] == 0) {
		    Memd[pl+i] = Memd[ptemp+j]
		    j = j + 1
		} else {
		    Memd[pl+i] = Memd[pl+i-1]
		}
	    }
	}

	# Estimate the distribution function at all points

	do i = 0, ntot-1
	    Memd[f+i] = 1.0 - Memd[pl+i]


	# Compute Akritas' rank from distribution function

	do i = 1, ntot {
	    if (indx[jcol,i] == 0) {
		xf[jcol,i] = ntot * Memd[f+i-1]
	    } else if (indx[jcol,i] > 0) {
		xf[jcol,i] = ntot * (0.5 + 0.5 * Memd[f+i-1])
	    } else {
		xf[jcol,i] = ntot * (0.5 * Memd[f+i-1])
	    }
	}

end
