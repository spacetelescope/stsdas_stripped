real procedure clip_mean(vec, npix, sig)

# inputs
pointer	vec
int	npix

# output
real	sig

# local
int	ngpix
real	initmean, initsig, mean, lcut, hcut
real	cut1

int	awvgr()

begin

	# Start off by determining the mean and sigma for the full dataset
	call aavgr (Memr[vec], npix, initmean, initsig)

	# Calculate the cutoff values at plus/minus 3-sigma 
	cut1 = initmean - 3. * initsig
	hcut = initmean + 3. * initsig

	# For the low cut, we don't want negative values, so set lower limit at
	# the arbitrary value of 1e-3 or mean-3sigma, whichever is greater.
	# However, if HCUT is less than zero, then we need to consider a 
	# different lcut in order to avoid an error in 'awvgr' where 
	# lcut > hcut...
	if(hcut > 0.001)
		lcut = max (0.001, cut1)
	else 
		lcut = cut1

	# Now, determine the new MEAN and SIGMA using the cutoff limits
	# we just calculated...
	ngpix = awvgr(Memr[vec], npix, mean, sig, lcut, hcut)
	if (ngpix == 0 || ngpix == 1) return (0.)
	
	# OK, re-determine the data limits using the new mean and sigma
	hcut = mean + 3. * sig
	cut1 = mean - 3. * sig

	if (hcut > 0.001)
		lcut = max (0.001, cut1)
	else 
		lcut = cut1

	# Do a final calculation of the mean and sigma using the final limits...
	ngpix = awvgr(Memr[vec], npix, mean, sig, lcut, hcut)
	if (ngpix == 0 || ngpix == 1) return (0.)

	# Return the final value of mean
	return(mean)
end
