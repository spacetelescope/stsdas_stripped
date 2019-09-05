# CALCSENS -- Calculate the sensitivity of a spectrophotometric passband
# This routine allocates memory for wlisens and isens.  These must be
# deallocated by another routine.
#
# Dave Bazell, original.
# Phil Hodge,  1-Nov-1993  Common block removed; etc.
# Phil Hodge, 28-Feb-1994  Convert to new synphot expression syntax.
# Phil Hodge,  1-Jul-1994  Use evalbandx instead of compspec;
#			include telescope area in calling sequence;
#			rename grftbl & cmptbl to graphtab & comptab.

procedure calcsens (obsmode, wstart, wstop, dw, area, wlisens, isens, nisens)

char	obsmode[ARB]	# i: observation mode string
real	wstart		# i: starting wavelength for simulation
real	wstop		# i: ending wavelength for simulation
real	dw		# i: wavelength increment
real	area		# i: area of telescope aperture in sq cm
pointer wlisens		# o: wavelength array
pointer isens		# o: sensitivity array
int	nisens		# o: number of points in isens array
#--
pointer	sp, graphtab, comptab, flterr
int	i, nwave

begin
	call smark  (sp)
	call salloc (graphtab, SZ_FNAME, TY_CHAR)
	call salloc (comptab, SZ_FNAME, TY_CHAR)

	# Get graph and component table names
	call clgstr ("graphtab", Memc[graphtab], SZ_FNAME)
	call clgstr ("comptab", Memc[comptab], SZ_FNAME)

	# How many elements should we assign for wlisens and isens?
	nwave = (wstop - wstart) / dw
	if (wstart + (nwave-1)*dw < wstop)
	    nwave = nwave + 1

	# Allocate memory.
	call malloc (wlisens, nwave, TY_REAL)
	call malloc (isens, nwave, TY_REAL)

	# Assign wavelengths.
	do i = 0, nwave-1
	    Memr[wlisens+i] = wstart + i * dw

	# Compute the throughput for each wavelength.
	call salloc (flterr, nwave, TY_REAL)		# ignored
	call evalbandx (obsmode, nwave, Memr[wlisens],
		Memc[graphtab], Memc[comptab], Memr[isens], Memr[flterr])

	# Convert from flamda to counts.
	call fspecf (nwave, Memr[wlisens], Memr[isens], "flam",
		Memr[isens], "counts")

        # Multiply by telescope area to get get total count rate;
	# also divide by spacing of wavelength array.
        call amulkr (Memr[isens], area/dw, Memr[isens], nwave)

        nisens = nwave

        call sfree (sp)
end
