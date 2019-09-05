procedure stoke_norm (flux1, sigma1, flux2, sigma2, npix)

# Normalize the Stokes parameters and errors by the intensity.
#
# Modified 25-Jun-93 HAB: propagate errors correctly; take care
# of (potential) floating overflows in calculating errors by
# doing the divisions in dble precision and reduce huge error values.
#
# Modified Oct-93 HAB: drop use of data quality values; detect bad points
# by looking for simultaneous values of zero for flux and flux error instead.
#
# Modified Nov-93 HAB: drop use of double-precision and clipping of sigma
# values to 1.e6.
#
# Modified Aug-94 HAB: Fixed bug in computing normalized error.

# Passed variables
real	flux1[ARB], sigma1[ARB], flux2[ARB], sigma2[ARB]
int	npix

# Local variables
int	i
real	dt1, dt2

begin
	do i=1, npix {
	
	   # Flux and error zero, set outputs to zero.
	   if ((flux2[i] == 0.0) && (sigma2[i] == 0.0)) {

		flux1[i] = 0.0
		sigma1[i] = 0.0

	   } else {

		# Calculate normalized sigma:
		if (flux1[i] == 0.0) {
		    sigma1[i] = abs (sigma1[i] / flux2[i])
		} else {
		    dt1 = sigma1[i] / flux2[i]
		    dt2 = (sigma2[i] / flux2[i]) * (flux1[i] / flux2[i])
		    sigma1[i] = sqrt( dt1*dt1 + dt2*dt2 )
		}

		# Calculate normalized flux:
		flux1[i] = flux1[i] / flux2[i]

	   }

	}
end
