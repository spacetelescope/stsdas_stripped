# INTEGRATE_PIX -- Integrate all the flux contained within the boundaries
# of a pixel for each pixel using a simple trapezoidal integration rule.
# This algorithm is supposed to be flux conserving!
#
# Perry Greenfield, original 2 November 1995

procedure integrate_pix (wave, det_flux, nall, index, pixind,
		 npix, npix2, istart, texp, counts)

real	wave[ARB]	# i: array of wavelengths
real	det_flux[ARB]	# i: array of "detected" flux corresp. to wave
int	nall		# i: size of wave and det_flux
int	index[ARB]	# i: index of pixel boundaries in wave
int	pixind[ARB]	# i: mapping of pix # to value in index
int	npix		# i: number of pixel boundaries being integrated
int	npix2		# i: size of counts
int	istart		# i: starting pixel in counts corresp. to index[0]
real 	texp		# i: exposure time in seconds
real	counts[ARB]	# o: array containing integrated detected flux
#--

int 	i		# loop index for all pixels covered by index
int	j		# loop index for all wavelength segments in pixel
int	nsegments	# number of segments in pixel	
int	ind		# temporary variable to contain index[i]
real	dwave		# delta lambda used for integration

begin
	do i = 1, npix-1 {
	    nsegments = index[pixind[i]] - index[pixind[i+1]]
	    ind = index[pixind[i+1]]
	    do j = 1, nsegments {
		dwave = wave[ind+j] - wave[ind+j-1]
		counts[i+istart] = counts[i+istart] + 
		   texp * dwave * ( det_flux[ind+j-1] + det_flux[ind+j] ) / 2.0
	    } 
	}
end


