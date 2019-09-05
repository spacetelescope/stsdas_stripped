# Shared by t_immakeb.x, t_irfftesb.x

# This is to write npix real values from xi to xo

procedure outr_b (xi, xo, npix, line)

real	xi[npix,ARB], xo[ARB]
int	npix, line

begin
	# Write data to output image
	call amovr (xi[1,line], xo, npix)
end

# This is to write the magnitudes of npix complex values from xi (shifted
# by npix/2) to xo.

procedure outc_b (xi, xo, npix, line)

complex	xi[npix,ARB]
real	xo[ARB]
int	npix, line

int	sh1		# Amount of shift

pointer	zrpt, zipt	# Array pointers for working space

begin
	# Dynamic memory allocation for working space
	call malloc (zrpt, npix, TY_REAL) 
	call malloc (zipt, npix, TY_REAL) 

	# Write shifted data to output image
	sh1 = npix / 2
	call aupxr (xi[1,line], Memr[zrpt], Memr[zipt], npix)
	call amagr (Memr[zrpt], Memr[zipt], xo, npix)
	call lnshift (xo, Memr[zrpt], npix, sh1)
	call amovr (Memr[zrpt], xo, npix)

	# Free dynamic memory
	call mfree (zrpt, TY_REAL)
	call mfree (zipt, TY_REAL)
end
