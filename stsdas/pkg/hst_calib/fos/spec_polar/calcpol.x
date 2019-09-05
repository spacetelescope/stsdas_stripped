include	<imhdr.h>

procedure calcpol (ofbuf)

# Passed arguments
pointer	ofbuf

# Local variables
int	npix, set
pointer	I, Q, U, V, Is, Qs, Us, Vs
pointer	PL, PC, TH, PLs, PCs, THs

begin

	# Determine length of spectra
	npix    = IM_LEN (ofbuf, 1)

	# Allocate dynamic memory
	call malloc (I, npix, TY_REAL)
	call malloc (Q, npix, TY_REAL)
	call malloc (U, npix, TY_REAL)
	call malloc (V, npix, TY_REAL)
	call malloc (Is, npix, TY_REAL)
	call malloc (Qs, npix, TY_REAL)
	call malloc (Us, npix, TY_REAL)
	call malloc (Vs, npix, TY_REAL)
	call malloc (PL, npix, TY_REAL)
	call malloc (PC, npix, TY_REAL)
	call malloc (TH, npix, TY_REAL)
	call malloc (PLs, npix, TY_REAL)
	call malloc (PCs, npix, TY_REAL)
	call malloc (THs, npix, TY_REAL)

	# Loop over the 4 datasets (1,2,combined,combined+corrected)
	do set = 1, 4, 1 {

	     # Read the I,Q,U,V and error spectra for this set
	     call rstoke (ofbuf, set, Memr[I], Memr[Q], Memr[U], Memr[V],
			  Memr[Is], Memr[Qs], Memr[Us], Memr[Vs], npix)

	     # Calculate polarization spectra
	     call cpolar (Memr[I], Memr[Q], Memr[U], Memr[V], Memr[Is],
		  Memr[Qs], Memr[Us], Memr[Vs], Memr[PL], Memr[PC], Memr[TH],
		  Memr[PLs], Memr[PCs], Memr[THs], npix)

	     # Write the polarization spectra to the output file
	     call wpolar (ofbuf, set, Memr[PL], Memr[PC], Memr[TH], Memr[PLs],
			  Memr[PCs], Memr[THs], npix)

	} # next set

	# Free memory
	call mfree  (I, TY_REAL)
	call mfree  (Q, TY_REAL)
	call mfree  (U, TY_REAL)
	call mfree  (V, TY_REAL)
	call mfree  (Is, TY_REAL)
	call mfree  (Qs, TY_REAL)
	call mfree  (Us, TY_REAL)
	call mfree  (Vs, TY_REAL)
	call mfree  (PL, TY_REAL)
	call mfree  (PC, TY_REAL)
	call mfree  (TH, TY_REAL)
	call mfree  (PLs, TY_REAL)
	call mfree  (PCs, TY_REAL)
	call mfree  (THs, TY_REAL)

end

procedure cpolar (I, Q, U, V, Is, Qs, Us, Vs, PL, PC, TH, PLs, PCs, THs, npix)

# Passed arguments
real	I[ARB], Q[ARB], U[ARB], V[ARB], Is[ARB], Qs[ARB], Us[ARB], Vs[ARB]
real	PL[ARB], PC[ARB], TH[ARB], PLs[ARB], PCs[ARB], THs[ARB]
int	npix

# Local variables
int	j
pointer	qu
real	errzero

extern	errzero

begin

	call malloc (qu, npix, TY_REAL)

	# Initialize all output spectra to zero 
	call aclrr (PL, npix)
	call aclrr (PC, npix)
	call aclrr (TH, npix)
	call aclrr (PLs, npix)
	call aclrr (PCs, npix)
	call aclrr (THs, npix)

	# Compute polarization position angle (Theta)
	do j = 1, npix {
	   if (Q[j]==0.0 && U[j]==0.0) next
	   TH[j] = 0.5 * atan2(U[j],Q[j])
	}

	# Compute circular polarization (PC)
	call advzr (V, I, PC, npix, errzero)
	     
	# Square the necessary variables
	call amulr (I, I, I, npix)
	call amulr (Q, Q, Q, npix)
	call amulr (U, U, U, npix)
	call amulr (V, V, V, npix)
	call amulr (Is, Is, Is, npix)
	call amulr (Qs, Qs, Qs, npix)
	call amulr (Us, Us, Us, npix)
	call amulr (Vs, Vs, Vs, npix)
	call aaddr (Q, U, Memr[qu], npix)

	# Compute linear polarization (PL)
	call advzr (Memr[qu], I, PL, npix, errzero)
	call asqrr (PL, PL, npix, errzero)

	# Compute polarization errors
	do j = 1, npix {
	   if (Memr[qu+j-1]==0.0) next

	   THs[j] = 0.5 * sqrt ( (U[j]*Qs[j] + Q[j]*Us[j]) /
				 (Memr[qu+j-1] * Memr[qu+j-1]) )

	   if (I[j]==0.0) next

	   PLs[j] = sqrt ( (Qs[j]*Q[j] + Us[j]*U[j]) / I[j] / Memr[qu+j-1] +
			    Is[j] * Memr[qu+j-1] / I[j] / I[j] )

	   PCs[j] = sqrt ( Vs[j]/I[j] + Is[j]*V[j]/I[j]/I[j] )
	}

	call mfree (qu, TY_REAL)
end


procedure wpolar (ofbuf, set, PL, PC, TH, PLs, PCs, THs, npix)

# Passed arguments
pointer	ofbuf
int	set
real	PL[ARB], PC[ARB], TH[ARB], PLs[ARB], PCs[ARB], THs[ARB]
int	npix

# Local variables
int	impl1r()
real	datamin, datamax

begin

	call gf_opengr (ofbuf, 14*(set-1)+9, datamin, datamax, 0)
	call amovr (PL, Memr[impl1r(ofbuf)], npix)

	call gf_opengr (ofbuf, 14*(set-1)+10, datamin, datamax, 0)
	call amovr (PC, Memr[impl1r(ofbuf)], npix)

	call gf_opengr (ofbuf, 14*(set-1)+11, datamin, datamax, 0)
	call amovr (TH, Memr[impl1r(ofbuf)], npix)

	call gf_opengr (ofbuf, 14*(set-1)+12, datamin, datamax, 0)
	call amovr (PLs, Memr[impl1r(ofbuf)], npix)

	call gf_opengr (ofbuf, 14*(set-1)+13, datamin, datamax, 0)
	call amovr (PCs, Memr[impl1r(ofbuf)], npix)

	call gf_opengr (ofbuf, 14*(set-1)+14, datamin, datamax, 0)
	call amovr (THs, Memr[impl1r(ofbuf)], npix)

	call imflush (ofbuf)

end


procedure rstoke (ofbuf, set, I, Q, U, V, Is, Qs, Us, Vs, npix)

# Passed arguments
pointer	ofbuf
int	set, npix
real	I[ARB], Q[ARB], U[ARB], V[ARB], Is[ARB], Qs[ARB], Us[ARB], Vs[ARB]

# Local variables
int	imgl1r()
real	mean, sigma, scale
real	datamin, datamax

begin

	call gf_opengr (ofbuf, 14*(set-1)+1, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], I, npix)

	call gf_opengr (ofbuf, 14*(set-1)+2, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], Q, npix)

	call gf_opengr (ofbuf, 14*(set-1)+3, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], U, npix)

	call gf_opengr (ofbuf, 14*(set-1)+4, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], V, npix)

	call gf_opengr (ofbuf, 14*(set-1)+5, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], Is, npix)

	call gf_opengr (ofbuf, 14*(set-1)+6, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], Qs, npix)

	call gf_opengr (ofbuf, 14*(set-1)+7, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], Us, npix)

	call gf_opengr (ofbuf, 14*(set-1)+8, datamin, datamax, 0)
	call amovr (Memr[imgl1r(ofbuf)], Vs, npix)

	# Determine an appropriate scale factor from I(err)
	call aavgr (Is, npix, mean, sigma)

	scale = 1.0
	if (mean < 1.0e-8 ) scale = 1.0e+12
	if (mean < 1.0e-16) scale = 1.0e+20
	if (mean < 1.0e-24) scale = 1.0e+28

	# Apply the scale factor to all spectra
	if (scale != 1.0) {
	    call amulkr (I, scale, I, npix)
	    call amulkr (Q, scale, Q, npix)
	    call amulkr (U, scale, U, npix)
	    call amulkr (V, scale, V, npix)
	    call amulkr (Is, scale, Is, npix)
	    call amulkr (Qs, scale, Qs, npix)
	    call amulkr (Us, scale, Us, npix)
	    call amulkr (Vs, scale, Vs, npix)
	}

end
