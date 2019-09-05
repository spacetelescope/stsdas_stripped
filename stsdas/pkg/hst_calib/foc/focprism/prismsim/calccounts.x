include	"prismsim.h"

# CALCCOUNTS -- Calculate the counts spectrum for a dispersed image given
# simulation parameters
#
# Dave Bazell,  Original.
# Phil Hodge, 28-Oct-1993  Change calling sequence, use allocated memory
#	instead of common blocks, replace spec and invsens with linterp.
# Phil Hodge,  1-Jul-1994  Include telescope area in calling sequence.

procedure calccounts (spectrum, obsmode, npix2,
		wave_disp, pix_disp, n_disp,
		wstart, wstop, dw, offset, texp, area,
		pix, counts)

char	spectrum[ARB]	# i: name of spectrum
char	obsmode[ARB]	# i: observation mode
int	npix2		# i: allocated size of pix & counts
real	wave_disp[ARB]	# i: wavelengths in dispersion file
real	pix_disp[ARB]	# i: pixel locations in dispersion file
int	n_disp		# i: size of wave_disp & pix_disp
real	wstart		# i: starting wavelength (Angstroms)
real	wstop		# i: ending wavelength (Angstroms)
real	dw		# i: wavelength increment for scratch array
real	offset		# i: Y location (pixels) of undispersed image
real	texp		# i: exposure time (seconds)
real	area		# i: area of telescope aperture in sq cm
real	pix[ARB]	# o: pixel array
real	counts[ARB]	# o: counts array of dispersed image
#--
pointer	sp
pointer wave
pointer iform		# unit string for input spectrum (e.g. "flam")
pointer y2disp, u_temp	# scratch for spline

pointer wav_pix		# array of output pixel wavelengths (at boundaries)
int	npix

pointer wav_sens	# array of sensitivity wavelengths
pointer sens		# array of values of sensitivity
int	nsens		# size of wlisens & isens arrays

pointer wav_spec	# array of source wavelengths
pointer spec		# array of values of spectrum
int	nspec		# size of wlspec & specf arrays

pointer wav_all		# array of wavelengths for combined arrays
pointer det_flux	# array of detected flux for wav_all
pointer index		# index of wav_pix into wav_all
int	nall		# size of wav_all

int	istart, istop, status, iw
real	lampix(), pixlam()
pointer	pixind		# array of indicies into wav_pix indicating
			#   the entry of wav_pix corresponding to pix #

begin
	# Allocate memory
	call smark (sp)
	call salloc (wave, MAXWAVE, TY_REAL)
	call salloc (iform, SZ_FNAME, TY_CHAR)

	# These are used by the spline interpolation routine; utemp
	# is scratch space for the spline routine and is not used later.

	call salloc (y2disp, n_disp, TY_REAL)
	call salloc (u_temp, n_disp, TY_REAL)

        # Initialize the spline interpolation.  The 2.e30 values are flags
        # to say use a "natural" spline.

	call spline (wave_disp, pix_disp, n_disp, 2.e30, 2.e30,
		Memr[u_temp], Memr[y2disp])

	# generate the wavelenths at the pixel boundaries 
	#   start first by calculating the starting and ending pixels
	#   for the given wavelength range

	istart = int (-pixlam (wstop, wave_disp, pix_disp,
			Memr[y2disp], ndisp, status) + offset)
	if (status != 0)
		call error (1, "error from pixlam (from calccounts)")
	istart = max (istart, 1)

	istop  = int (-pixlam (wstart, wave_disp, pix_disp,
			Memr[y2disp], ndisp, status) + offset + 0.99999)
	if (status != 0)
		call error (1, "error from pixlam (from calccounts)")
	istop = min (istop, npix2)

	npix = istop - istart + 2
	call salloc (wav_pix, npix, TY_REAL)
	call salloc (pixind,  npix, TY_INT)
	
	# tricky part here, wav_pix would normaly  run from greater to
	# to lower if pix went from 1 to npix. So that it can work
	# with merge3 and mergearr, it is being generated in increasing
	# value meaning that it corresponds to decreasing pixel coords.

	do iw = 1, npix {
		Memr[wav_pix+npix-iw] = lampix (-(iw-1 + istart) +
			offset, wave_disp, pix_disp, Memr[y2disp],
			ndisp, status)
		if (status != 0)
			call error (1, "error from lampix (from calcounts)")
	}

	# Read in reference spectrum that is to be made into a prism image.
	# Note that readspec allocates wav_spec & spec.

	call readspec (spectrum, wav_spec, spec, nspec, Memc[iform])
	call strlwr (Memc[iform])

	# Convert the units of the spectrum from Memc[iform] to "flam".

	call fspecf (nspec, Memr[wav_spec], Memr[spec], Memc[iform],
	               Memr[spec], "flam")

	do iw = 1, npix2 {
		pix[iw] = iw
		Memi[pixind+iw-1] = npix + 1 - iw
		counts[iw] = 0.
	}

	# Calculate the sensitivity isens of obsmode over array wav_sens
	# of wavelengths.  Note that calcsens allocates wav_sens and sens.

        call calcsens (obsmode, wstart, wstop, dw, area, wav_sens,
			sens, nsens)

	# allocate arrays for combined data

	nall = npix + nspec + nsens

	call salloc (wav_all,  nall, TY_REAL)
	call salloc (det_flux, nall, TY_REAL)
	call salloc (index, npix, TY_INT)

	# now call the procedure that combines all of the wavelength
	# arrays and resamples and combines the sensitivity and flux
	# arrays using linear interpolation.

	call merge3 (Memr[wav_spec], Memr[wav_sens], Memr[wav_pix],
		     Memr[spec], Memr[sens], nspec, nsens, npix,
		     Memr[wav_all], Memr[det_flux], nall, Memi[index])

	call integrate_pix (Memr[wav_all], Memr[det_flux], nall,
			Memi[index], Memi[pixind], npix, npix2,
			istart, texp, counts )

	call mfree (wav_sens, TY_REAL)
	call mfree (spec, TY_REAL)
	call mfree (wav_spec, TY_REAL)

	call sfree (sp)
end
