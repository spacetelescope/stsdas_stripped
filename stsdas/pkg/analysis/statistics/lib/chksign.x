# CHKSIGN -- Check to see if all censor indicators have the same sign

procedure chksign (ind, ntot, isign)

int	ind[ARB]	# i: array of censor indicators
int	ntot		# i: number of censor indicators
int	isign		# o: sign of indicators
#--
int	irow

begin
	isign = 0
	do irow = 1, ntot {
	    if (ind[irow] != 0) {
		if (!IS_INDEFI (ind[irow])) {
		    if (isign == 0) {
			isign = sign (1, ind[irow])

		    } else if (isign != sign (1, ind[irow])) {
			isign = 0
			return
		    }
		}
	    }
	}

	if (isign == 0)
	    isign = 1
end
