#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	15-Sep-92	revised to use asurv 1.2

# CENSTAT -- Compute censoring statistics for bivariate algorithms

procedure censor_statistics (verbose, ind, ntot, nd, nc, icens, nyc, nxc, nbc)

bool	verbose		# i: print diagnostic messages?
int	ind[ARB]	# i: indicator of censoring
int	ntot		# i: number of data values
int	nd		# o: number of detected points
int	nc[8]		# o: number of censored points
int	icens		# o: type of censoring
int	nyc		# nc[1] + nc[5]
int	nxc		# nc[2] + nc[6]
int	nbc		# nc[3] + nc[4] + nc[7] + nc[8]
#--
int	nlc, nuc, j

begin

	# Accumulate statistics about censoring
	nd = 0; nc[1] = 0; nc[2] = 0; nc[3] = 0; nc[4] = 0
	nc[5] = 0; nc[6] = 0; nc[7] = 0; nc[8] = 0

	do j = 1, ntot {
	    switch (ind[j]) {

	    case -4:
		# number of y upper, x lower limits
		nc[8] = nc[8] + 1
	    case -3:
		# number of double upper limits
		nc[7] = nc[7] + 1
	    case -2:
		# number of x upper limits
		nc[6] = nc[6] + 1
	    case -1:
		# number of y upper limits
		nc[5] = nc[5] + 1
	    case 0:
		# number of detected points
		nd = nd + 1
	    case 1:
		# number of y lower limits
		nc[1] = nc[1] + 1
	    case 2:
		# number of x lower limits
		nc[2] = nc[2] + 1
	    case 3:
		# number of double lower limits
		nc[3] = nc[3] + 1
	    case 4:
		# number of y lower, x upper limits
		nc[4] = nc[4] + 1
	    }
	}

	nyc = nc[1] + nc[5]
	nxc = nc[2] + nc[6]
	nbc = nc[3] + nc[4] + nc[7] + nc[8]
	nlc = nc[1] + nc[2] + nc[3] + nc[4]
	nuc = nc[5] + nc[6] + nc[7] + nc[8]
	icens = 0
	if (nlc == 0)
		icens = -1
	if (nuc == 0)
		icens = 1

	if (verbose) {
	    call printf ("%9w Number of data points : %5d\n")
	    call pargi (ntot)

	    switch (icens) {
	    case -1:
		call printf ("%9w Upper limits in  Y    X    Both  Mix\n")
		call printf ("%23w%5d%5d   %5d%5d\n")
		call pargi (nc[5])
		call pargi (nc[6])
		call pargi (nc[7])
		call pargi (nc[8])
		
	    case 0:
		call printf ("%9w Lower limits in  Y    X    Both  Mix\n")
		call printf ("%23w%5d%5d   %5d%5d\n")
		call pargi (nc[1])
		call pargi (nc[2])
		call pargi (nc[3])
		call pargi (nc[4])
		call printf ("\n")
		call printf ("%9w Upper limits in  Y    X    Both  Mix\n")
		call printf ("%23w%5d%5d   %5d%5d\n")
		call pargi (nc[5])
		call pargi (nc[6])
		call pargi (nc[7])
		call pargi (nc[8])
		
	    case 1:
		call printf ("%9w Lower limits in  Y    X    Both  Mix\n")
		call printf ("%23w%5d%5d   %5d%5d\n")
		call pargi (nc[1])
		call pargi (nc[2])
		call pargi (nc[3])
		call pargi (nc[4])
		
	    }
	    call printf ("\n")
	}

end
