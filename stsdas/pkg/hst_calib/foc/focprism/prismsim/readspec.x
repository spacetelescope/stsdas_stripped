include <tbset.h>

# READSPEC -- Read in a reference spectrum
# This routine allocates memory for wave and spec.  These must be
# deallocated by another routine.
#
# Dave Bazell, original.
# Phil Hodge,  1-Nov-1993  Allocate memory.

procedure readspec (specname, wave, spec, nwave, form)

char	specname[ARB]		# i: Name of spectrum file
pointer wave			# o: pointer to wavelength array
pointer spec			# o: pointer to spectrum array
int	nwave			# o: number of values in wave and spec
char	form[ARB]		# o: units of spectrum
#--
pointer sp
pointer	tp, wpt, fpt, nulflg
pointer	tbtopn()
int	tbpsta()

begin
	# Get table pointer and pointers to wavelength and flux cols
	tp = tbtopn (specname, READ_ONLY, NULL)
	call tbcfnd (tp, "WAVELENGTH", wpt, 1)
	call tbcfnd (tp, "FLUX", fpt, 1)
	if (wpt == NULL)
	    call error ("WAVELENGTH column not found in input spectrum")
	if (fpt == NULL)
	    call error ("FLUX column not found in input spectrum")

	nwave = tbpsta (tp, TBL_NROWS)

	# Allocate memory for wavelength and flux.
	call malloc (wave, nwave, TY_REAL)
	call malloc (spec, nwave, TY_REAL)

	# Read in wavelength and flux.
	call smark (sp)
	call salloc (nulflg, nwave, TY_BOOL)
	call tbcgtr (tp, wpt, Memr[wave], Memb[nulflg], 1, nwave)
	call tbcgtr (tp, fpt, Memr[spec], Memb[nulflg], 1, nwave)
	call sfree (sp)

	# Get flux units
	call tbcigt (fpt, TBL_COL_UNITS, form, SZ_FNAME)

	call tbtclo (tp)
end
