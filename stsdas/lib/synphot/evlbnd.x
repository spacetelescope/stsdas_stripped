# EVLBND -- Fortran interface to evalbandx

procedure evlbnd (modstr, nwave, wave, grftbl, cmptbl, thruput, thruerr)

%       character*(*)   modstr
%       character*(*)   grftbl
%       character*(*)   cmptbl
int	nwave			# length of wavelength and output arrays
real	wave[ARB]		# wavelengths at which output is computed
real	thruput[ARB]		# grand throughput (output)
real	thruerr[ARB]		# grand throughput error (output)
#--
int	mxmode
pointer	sp, mode, graphtab, comptab

begin
	# Get length of fortran strings

	mxmode = len(modstr)

	# Allocate memory for temporary spp strings

	call smark (sp)
	call salloc (mode, mxmode, TY_CHAR)
	call salloc (graphtab, SZ_FNAME, TY_CHAR)
	call salloc (comptab, SZ_FNAME, TY_CHAR)

	# Convert strings from fortran to spp

	call f77upk (modstr, Memc[mode], mxmode)
	call f77upk (grftbl, Memc[graphtab], SZ_FNAME)
	call f77upk (cmptbl, Memc[comptab], SZ_FNAME)

	# Call spp interface procedure

	call evalbandx (Memc[mode], nwave, wave, Memc[graphtab], 
			Memc[comptab], thruput, thruerr)

	call sfree (sp)
end
