# CR_MODE -- Procedure to compute the mode from the histogram.

# Adapted from images$iminfo/t_imstat.x
# Dec-05-1995	J.-C. Hsu

procedure cr_mode (histgrm, nbins, hwidth, hmin, hmax,   mode)

# inputs:
int	histgrm[ARB]	# histogram of the pixels
int	nbins		# number of bins in the histogram
real	hwidth		# resolution of the histogram
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value

# output:
real	mode		# the mode

# local:
int	i, bpeak
real	hpeak, dh1, dh2, denom
bool	fp_equalr()
#==============================================================================
begin
	# If there is a single bin return the midpoint of that bin.
	if (nbins == 1) {
	    mode = hmin + 0.5 * hwidth
	    return
	}

	# If there are two bins return the midpoint of the greater bin.
	if (nbins == 2) {
	    if (histgrm[1] > histgrm[2])
	        mode = hmin + 0.5 * hwidth
	    else if (histgrm[2] > histgrm[1])
	        mode = hmin + 1.5 * hwidth
	    else
	        mode = hmin + hwidth
	    return
	}

	# Find the bin containing the histogram maximum.
	hpeak = histgrm[1]
	bpeak = 1
	do i = 2, nbins {
	    if (histgrm[i] > hpeak) {
		hpeak = histgrm[i]
		bpeak = i
	    }
	}

	# If the maximum is in the first bin return the midpoint of the bin.
	if (bpeak == 1) {
	    mode = hmin + 0.5 * hwidth
	    return
	}

	# If the maximum is in the last bin return the midpoint of the bin.
	if (bpeak == nbins) {
	    mode = hmin + (nbins - 0.5) * hwidth
	    return
	}

	# Compute the lower limit of bpeak.
	bpeak = bpeak - 1

	# Do a parabolic interpolation to find the peak.
	dh1 = histgrm[bpeak+1] - histgrm[bpeak]
	dh2 = histgrm[bpeak+1] - histgrm[bpeak+2]
	denom = dh1 + dh2
	if (fp_equalr (denom, 0.0)) {
	    mode = hmin + (bpeak + 0.5) * hwidth
	} else {
	    mode = bpeak + 1 + 0.5 * (dh1 - dh2) / denom
	    mode = hmin + (mode - 0.5) * hwidth
	}

	dh1 =   histgrm[bpeak] * (hmin + (bpeak - 0.5) * hwidth) +
	      histgrm[bpeak+1] * (hmin + (bpeak + 0.5) * hwidth) +
	      histgrm[bpeak+2] * (hmin + (bpeak + 1.5) * hwidth)
	dh2 = histgrm[bpeak] + histgrm[bpeak+1] + histgrm[bpeak+2]
end
