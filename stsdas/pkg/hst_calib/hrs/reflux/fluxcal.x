# FLUXCAL -- Perform new flux calibration on HRS data
# 
# Task removes previous flux calibration and applies
# new flux calibration to HRS data
#
# S. Hulbert, Jul 91	Original

procedure fluxcal (flux0, wave, npix_flux, abs0, net0, npix_abs0,
                        abs1, net1, npix_abs1, flux1)

real    flux0[ARB]		#I: original flux-calibrated data
real    wave[ARB]		#I: corresponding wavelengths for flux0 
int     npix_flux		#I: number of pixels in original flux image 
real    abs0[ARB]		#I: original absolute sensitivity data
real    net0[ARB]		#I: original wavelength net for abs0 
int     npix_abs0		#I: number of pixels in original sens. image
real    abs1[ARB]		#I: new absolute sensitivity data 
real    net1[ARB]		#I: new wavelength net for abs0 
int     npix_abs1		#I: number of pixels in new sensitivity image
real    flux1[ARB]		#O: new flux-calibrated data 

pointer abs0_int, abs1_int

pointer sp

begin

        call smark (sp)

	# allocate buffer for interpolated sensitivities
        call salloc (abs0_int, npix_flux, TY_REAL)
        call salloc (abs1_int, npix_flux, TY_REAL)

        # interpolate the old sensitivities
        call quad_int (net0, abs0, npix_abs0, wave, npix_flux, Memr[abs0_int])

        # interpolate the new sensitivities
        call quad_int (net1, abs1, npix_abs1, wave, npix_flux, Memr[abs1_int])

        # multiply flux-calibrated data by old sensitivities
        call amulr (flux0, Memr[abs0_int], flux1, npix_flux)

        # now divide by new sensitivities
        call adivr (flux1, Memr[abs1_int], flux1, npix_flux)

        call sfree (sp)

end

