procedure rdata (iname, set, npix, det, i, q, u, v, ierr, qerr, uerr, verr)

# Modified May-94 by HAB: Added flipping of data vectors for AMBER detector

# Passed variables
char	iname[ARB], det[ARB]
int	set, npix
real	i[ARB], q[ARB], u[ARB], v[ARB]
real	ierr[ARB], qerr[ARB], uerr[ARB], verr[ARB]

# Local variables
int	ig, imgl1r(), immap()
real	datamin, datamax
pointer	iptr
bool	streq()

begin

	iptr = immap (iname, READ_ONLY, 0)

	# Read the Stokes spectra from the input data file
	ig = 14*(set-1)
	call gf_opengr (iptr, ig+1, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], i, npix)
	call gf_opengr (iptr, ig+2, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], q, npix)
	call gf_opengr (iptr, ig+3, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], u, npix)
	call gf_opengr (iptr, ig+4, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], v, npix)
	call gf_opengr (iptr, ig+5, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], ierr, npix)
	call gf_opengr (iptr, ig+6, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], qerr, npix)
	call gf_opengr (iptr, ig+7, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], uerr, npix)
	call gf_opengr (iptr, ig+8, datamin, datamax, 0)
	call amovr (Memr[imgl1r(iptr)], verr, npix)

	call imunmap (iptr)

	# If AMBER detector, flip vectors
	if (streq (det, "AMBER")) {
	    call vflip (i, npix)
	    call vflip (q, npix)
	    call vflip (u, npix)
	    call vflip (v, npix)
	    call vflip (ierr, npix)
	    call vflip (qerr, npix)
	    call vflip (uerr, npix)
	    call vflip (verr, npix)
	}

end
