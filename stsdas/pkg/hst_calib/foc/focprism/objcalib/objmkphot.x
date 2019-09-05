# obj_mkphot -- create photometric calibration array
#
# Original based on a script by Warren Hack.
# Phil Hodge,  1-Jul-1994  Include area in calling sequence.

procedure obj_mkphot (obsmode, graphtab, comptab,
		wfirst, dw, nwx, encirc, area,
		wave_phot, fac_phot, n_phot)

char	obsmode[SZ_FNAME]	# i: observation mode string
char	graphtab[SZ_FNAME]	# i: graph table name
char	comptab[SZ_FNAME]	# i: component table name
real	wfirst			# i: starting wavelength in output
real	dw			# i: wavelength spacing in output
int	nwx			# i: number of elements in wavelength array
real	encirc			# i: encircled energy fraction
real	area			# i: telescope area in sq cm
real  	wave_phot[ARB]		# o: array of wavelengths
real	fac_phot[ARB]		# o: array of photometric correction factors
int	n_phot			# o: number of elements in output arrays
#--
pointer sp
pointer wave			# wavelengths at which throughput is computed
pointer filt			# grand throughput
pointer flterr			# grand throughput error (ignored)
int	i, j

begin
	call smark (sp)
	call salloc (wave, nwx, TY_REAL)
	call salloc (filt, nwx, TY_REAL)
	call salloc (flterr, nwx, TY_REAL)

	# Assign values for wavelengths.
	do i = 0, nwx-1					# zero indexed
	    Memr[wave+i] = wfirst + dw * i

	# Compute the throughput for each wavelength.
	call evalbandx (obsmode, nwx, Memr[wave], graphtab, comptab,
		Memr[filt], Memr[flterr])

	# Convert from flamda to counts.
	call fspecf (nwx, Memr[wave], Memr[filt], "flam", Memr[filt], "counts")

	# Include encirc factor and telescope area, and convert to
	# inverse sensitivity.
	do i = 0, nwx-1 {
	    if (!IS_INDEFR(Memr[filt+i]) && Memr[filt+i] > 0.)
		Memr[filt+i] = dw / (Memr[filt+i] * encirc * area)
	}

	# Copy the results to the output arrays.
	j = 0
	do i = 1, nwx {
	    if (!IS_INDEFR(Memr[filt+i-1]) && Memr[filt+i-1] > 0.) {
		j = j + 1
		wave_phot[j] = Memr[wave+i-1]
		fac_phot[j] = Memr[filt+i-1]
	    }
	}
	n_phot = j

	call sfree (sp)
end
