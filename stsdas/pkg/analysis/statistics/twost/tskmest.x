#* HISTORY *
#* B.Simon	04-Sep-92	original

# TS_KMEST -- Compute Kaplan Meier estimator for two sided test

procedure ts_kmest (verbose, ind, ista, x, ifirst, isecond, jcol, ncol, ntot)

bool	verbose		# i: long form of printout		
int	ind[ARB]	# i: censor indicator
int	ista[ARB]	# i: group number
double	x[ncol,ARB]	# i: data values
int	ifirst		# i: first group number
int	isecond		# i: second group number
int	jcol		# i: data column to analyze
int	ncol		# i: number of data columns
int	ntot		# i: number of data rows
#--
int	igrp, itot, ncomp, group[2]
pointer	sp, xgrp, indgrp

begin
	# Allocate memory for work arrays

	call smark (sp)
	call salloc (xgrp, ntot, TY_DOUBLE)
	call salloc (indgrp, ntot, TY_INT)

	call printf ("\n *** Kaplan Meier Estimators *** \n")

	# Copy observations for current group into work arrays

	group[1] = ifirst
	group[2] = isecond

	do igrp = 1, 2 {
	    ncomp = 0

	    do itot = 1, ntot {

		if (ista[itot] == group[igrp]) {
		    Memd[xgrp+ncomp] = x[jcol,itot]
		    Memi[indgrp+ncomp] = ind[itot]
		    ncomp = ncomp + 1
		}
	    }

	    # Compute and print Kaplan Meier estimator for this group

	    call printf ("\n")

	    call kmestm (Memi[indgrp], Memd[xgrp], 1, 1, ncomp, 
			 0.0, 0.0, 0, false, verbose)
	}

	call sfree (sp)
end
