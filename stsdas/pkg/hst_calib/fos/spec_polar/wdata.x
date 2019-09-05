include <imhdr.h>

# Modified May-94 by HAB: Added flipping of data vectors for AMBER detector.

procedure wdata (iroot, iextn, oroot, wroot, set, nbin, det, wave, I, Q, U, V,
		 Is, Qs, Us, Vs, PL, PC, TH, PLs, PCs, THs, first)

# Passed variables
char	iroot[ARB], iextn[ARB], oroot[ARB], wroot[ARB], det[ARB]
int	set, nbin
real	wave[ARB], I[ARB], Q[ARB], U[ARB], V[ARB], Is[ARB], Qs[ARB], Us[ARB], 
	Vs[ARB], PL[ARB], PC[ARB], TH[ARB], PLs[ARB], PCs[ARB], THs[ARB]
bool	first

# Local variables
pointer	iptr, optr
int	ig, impl1r(), immap()
char	iname[SZ_FNAME], oname[SZ_FNAME]
bool	streq()

begin

	# If AMBER detector, flip the vectors
	if (streq( det, "AMBER")) {
	    call vflip (wave, nbin)
	    call vflip (I, nbin)
	    call vflip (Q, nbin)
	    call vflip (U, nbin)
	    call vflip (V, nbin)
	    call vflip (Is, nbin)
	    call vflip (Qs, nbin)
	    call vflip (Us, nbin)
	    call vflip (Vs, nbin)
	    call vflip (PL, nbin)
	    call vflip (PC, nbin)
	    call vflip (TH, nbin)
	    call vflip (PLs, nbin)
	    call vflip (PCs, nbin)
	    call vflip (THs, nbin)
	}

	# Write the binned wavelength data to the output file
	if (first) {
	    call mkfname (wroot, "c0h", 1, 0, iname, SZ_FNAME)
	    call mkfname (oroot, "c0h", 1, 4, oname, SZ_FNAME)
	    iptr = immap (iname, READ_ONLY, 0)
	    optr = immap (oname, NEW_COPY, iptr)
	    IM_LEN(optr,1) = nbin
	    call amovr (wave, Memr[impl1r(optr)], nbin)
	    call imunmap (iptr)
	    call imunmap (optr)
	}
	ig = 1
	if (set==2) ig = 2
	call mkfname (wroot, "c0h", ig,  0, iname, SZ_FNAME)
	call mkfname (oroot, "c0h", set, 0, oname, SZ_FNAME)
	iptr = immap (iname, READ_ONLY, 0)
	optr = immap (oname, NEW_COPY, iptr)
	IM_LEN(optr,1) = nbin

	call amovr (wave, Memr[impl1r(optr)], nbin)

	call imunmap (iptr)
	call imunmap (optr)

	# Write the binned spectra to the output file
	if (first) {
	    call mkfname (iroot, iextn, 1,  0, iname, SZ_FNAME)
	    call mkfname (oroot, iextn, 1, 56, oname, SZ_FNAME)
	    iptr = immap (iname, READ_ONLY, 0)
	    optr = immap (oname, NEW_COPY, iptr)
	    IM_LEN(optr,1) = nbin
	    call amovr (I, Memr[impl1r(optr)], nbin)
	    call imunmap (iptr)
	    call imunmap (optr)
	    first = false
	}

	ig = 14*(set-1)
	call wspec (iroot, iextn, oroot, ig+1 , I  , nbin)
	call wspec (iroot, iextn, oroot, ig+2 , Q  , nbin)
	call wspec (iroot, iextn, oroot, ig+3 , U  , nbin)
	call wspec (iroot, iextn, oroot, ig+4 , V  , nbin)
	call wspec (iroot, iextn, oroot, ig+5 , Is , nbin)
	call wspec (iroot, iextn, oroot, ig+6 , Qs , nbin)
	call wspec (iroot, iextn, oroot, ig+7 , Us , nbin)
	call wspec (iroot, iextn, oroot, ig+8 , Vs , nbin)
	call wspec (iroot, iextn, oroot, ig+9 , PL , nbin)
	call wspec (iroot, iextn, oroot, ig+10, PC , nbin)
	call wspec (iroot, iextn, oroot, ig+11, TH , nbin)
	call wspec (iroot, iextn, oroot, ig+12, PLs, nbin)
	call wspec (iroot, iextn, oroot, ig+13, PCs, nbin)
	call wspec (iroot, iextn, oroot, ig+14, THs, nbin)

end

procedure wspec (iroot, extn, oroot, ig, data, npix)

char	iroot[ARB], extn[ARB], oroot[ARB]
int	ig, npix
real	data[ARB]

char	iname[SZ_FNAME], oname[SZ_FNAME]
pointer	iptr, optr
int	impl1r(), immap()

begin
	call mkfname (iroot, extn, ig, 0, iname, SZ_FNAME)
	call mkfname (oroot, extn, ig, 0, oname, SZ_FNAME)
	iptr = immap (iname, READ_ONLY, 0)
	optr = immap (oname, NEW_COPY, iptr)
	IM_LEN(optr,1) = npix

	call amovr (data, Memr[impl1r(optr)], npix)

	call imunmap (iptr)
	call imunmap (optr)
end
