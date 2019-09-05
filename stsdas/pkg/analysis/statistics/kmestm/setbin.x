# SETBIN -- Set the bin size and starting value if INDEF

procedure setbin (xlo, xhi, binstart, binsize, nbin, bstart, bsize)

double	xlo		# i: smallest data value to be binned
double	xhi		# i: largest data value to be binned
double	binstart	# i: starting bin value
double	binsize		# i: size of each bin
int	nbin		# i: number of bins
double	bstart		# o: starting bin value
double	bsize		# o: starting bin size
#--
double	xdif, place
int	nplace

begin
	if (!IS_INDEFD (binstart)) {
	    bstart = binstart
	} else {
	    xdif = (xhi - xlo) / double (nbin)
	    place = - log10 (2.0 * xdif)
	    if (place > 0.0) {
		nplace = place - 1.0
	    } else {
		nplace = place
	    }
	    
	    bstart = aint (xlo * 10.0 ** nplace)
	    bstart = bstart * 10.0 ** (- nplace)
	}
	    

	if (!IS_INDEFD (binsize)) {
	    bsize = binsize
	} else {
	    xdif = (xhi - bstart) / double (nbin)
	    place = - log10 (2.0 * xdif)
	    if (place > 0.0) {
		nplace = place - 1.0
	    } else {
		nplace = place
	    }
	    
	    bsize = aint (xdif * 10.0 ** nplace + 1.0)
	    bsize = bsize * 10.0 ** (- nplace)
	}

end
