# NCAR_FFT -- Test Swarztrauber's FFT routines

procedure t_ncar_fft ()

char	inreal[SZ_FNAME]
char	outreal[SZ_FNAME]
char	outimag[SZ_FNAME]
pointer	x, y, yc, tp, a, b
int	npts, rdfunc(), wsiz, rfd, ifd, open(), i

begin
	# Get input file(s)
	call clgstr ("inreal", inreal, SZ_FNAME)

	# Get output files
	call clgstr ("outreal", outreal, SZ_FNAME)
	call clgstr ("outimag", outimag, SZ_FNAME)

	# Get input data
	npts = rdfunc (inreal, x, y)

	# Pack input real data into complex
	call calloc (yc, npts, TY_COMPLEX)
	call achtrx (Memr[y], Memx[yc], npts)

	# Allocate scratch space and initialize trig table
	wsiz = 4*npts + 15
	call calloc (tp, wsiz, TY_REAL)
	call cffti (npts, Memr[tp])

	call cfftf (npts, Memx[yc], Memr[tp])

	# Unpack complex results
	call calloc (a, npts, TY_REAL)
	call calloc (b, npts, TY_REAL)
	call aupxr (Memx[yc], Memr[a], Memr[b], npts)

	# Report answers
	rfd = open (outreal, NEW_FILE, TEXT_FILE)
	ifd = open (outimag, NEW_FILE, TEXT_FILE)

	do i = 0, npts-1 {
		call fprintf (rfd, "%g\n")
		    call pargr (Memr[a+i])
		call fprintf (ifd, "%g\n")
		    call pargr (Memr[b+i])
	}

	call close (rfd)
	call close (ifd)

	call mfree (b, TY_REAL)
	call mfree (a, TY_REAL)
	call mfree (tp, TY_REAL)
	call mfree (yc, TY_COMPLEX)
	call mfree (y, TY_REAL)
	call mfree (x, TY_REAL)
end
