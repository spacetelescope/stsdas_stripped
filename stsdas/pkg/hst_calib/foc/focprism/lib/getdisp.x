include <tbset.h>

# GETDISP -- Read in a dispersion file
# This routine reads wavelengths and pixel locations from a table.
# The arguments wave_disp and pix_disp are allocated here and must be
# deallocated by another routine.  The number of elements pointed to
# is n_disp, but the allocated size of the arrays may be larger.
#
# Dave Bazell, original
# Phil Hodge,  1-Nov-1993  Rewritten using table I/O and eliminating common.

procedure getdisp (dispfile, wcol, pcol, wave_disp, pix_disp, n_disp)

char	dispfile[ARB]	# i: dispersion file name
char	wcol[ARB]	# i: column name for wavelengths
char	pcol[ARB]	# i: column name for pixel locations
pointer wave_disp	# o: pointer to array of wavelengths
pointer pix_disp	# o: pointer to array of pixel locations
int	n_disp		# o: size of wave_disp, pix_disp
#--
pointer tp		# pointer to table descriptor
pointer	cpw, cpp	# pointers to column descriptors
real	wl, pix		# temporary variables for wavelength and pixel
int	adisp		# allocated size of wave_disp, pix_disp
int	row		# row number
int	i		# counter for non-indef values
pointer tbtopn()
int	tbpsta()

begin
	# Open table.
	tp = tbtopn (dispfile, READ_ONLY, NULL)

	# Find columns.
	call tbcfnd (tp, wcol, cpw, 1)
	call tbcfnd (tp, pcol, cpp, 1)
	if (cpw == NULL)
	    call error (1, "wavelength column not found in dispersion file")
	if (cpp == NULL)
	    call error (1, "pixel column not found in dispersion file")

	# n_disp may be smaller if there are indef values.
	adisp = tbpsta (tp, TBL_NROWS)
	call malloc (wave_disp, adisp, TY_REAL)
	call malloc (pix_disp, adisp, TY_REAL)

	# Read values.
	i = 0
	do row = 1, adisp {
	    call tbegtr (tp, cpw, row, wl)
	    call tbegtr (tp, cpp, row, pix)
	    # Ignore if either value is indef.
	    if (!IS_INDEFR(wl) && !IS_INDEFR(pix)) {
		i = i + 1
		Memr[wave_disp+i-1] = wl
		Memr[pix_disp+i-1] = pix
	    }
	}
	n_disp = i

	call tbtclo (tp)
end
