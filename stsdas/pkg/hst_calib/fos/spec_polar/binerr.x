procedure binerr (I, Is, Q, Qs, U, Us, V, Vs, wave, npix, paim, bincen,
		  binstrt, binend, nbins, variwgt, PL, PLs, TH, THs, PC, PCs, 
		  Ib, Qb, Ub, Vb, Isb, Qsb, Usb, Vsb, wavb)
#
#     Calculates linear polarization p, and its error, polarization
#     position angle and its error from the Q and U Stokes parameters. 
#     Bins the linear polarization, p, data and the position angle, theta,
#     data so that the p error per bin is less than or equal to PAIM. 
#     The circular polarization V and its error are also calulated
#     for this bin.
#     If BINCEN is > 1 then a bin starts from this channel in the 
#     increasing and decreasing channel directions.
#     Bad points with flux and flux-error equal to zero are 
#     ignored in computing polarization and errors.
#
#	Modified Aug-94 HAB: Added binstrt array
#
# Passed arguments
bool	variwgt
int	npix, bincen, nbins, binstrt[ARB], binend[ARB]
real	I[ARB], Is[ARB], Q[ARB], Qs[ARB], U[ARB], Us[ARB], V[ARB], Vs[ARB], 
	wave[ARB], PL[ARB], PLs[ARB], TH[ARB], THs[ARB], PC[ARB], PCs[ARB],
	Ib[ARB], Qb[ARB], Ub[ARB], Vb[ARB], Isb[ARB], Qsb[ARB], Usb[ARB], 
	Vsb[ARB], wavb[ARB]
real	paim

# Local variables
int	ii, i1, i2, im, j
real	mi, mq, mu, mv, merri, merrq, merru, merrv, mp, merp, mth, merth
real	sumw, scale

begin

	# Initialize output arrays to zero
	call aclrr (PL, npix)
	call aclrr (TH, npix)
	call aclrr (PC, npix)
	call aclrr (PLs, npix)
	call aclrr (THs, npix)
	call aclrr (PCs, npix)
	call aclrr (Ib, npix)
	call aclrr (Qb, npix)
	call aclrr (Ub, npix)
	call aclrr (Vb, npix)
	call aclrr (Isb, npix)
	call aclrr (Qsb, npix)
	call aclrr (Usb, npix)
	call aclrr (Vsb, npix)
	call aclrr (wavb, npix)
	call aclri (binstrt, npix)
	call aclri (binend,  npix)

        # Compute a scale factor for the Stokes spectra from the mean I(err);
        # this helps prevent floating overflows in subsequent math.
        call aavgr (Is, npix, merri, merrq)

        scale = 1.0
        if (merri < 1.0e-8)  scale = 1.0e+12
        if (merri < 1.0e-16) scale = 1.0e+20
        if (merri < 1.0e-24) scale = 1.0e+28

        # Apply the scale factor to the Stokes spectra
        if (scale != 1.0) {
            call amulkr (I,  scale, I,  npix)
            call amulkr (Q,  scale, Q,  npix)
            call amulkr (U,  scale, U,  npix)
            call amulkr (V,  scale, V,  npix)
            call amulkr (Is, scale, Is, npix)
            call amulkr (Qs, scale, Qs, npix)
            call amulkr (Us, scale, Us, npix)
            call amulkr (Vs, scale, Vs, npix)
        }

	# Compute binned data starting at pixel #1
	if (bincen==1) {
	    im = 0
	    i1 = 1

	    do ii = 1, npix, 1 {
	       # Compute the binned data
	       call cmpbin (I, Is, Q, Qs, U, Us, V, Vs, wave,
			    npix, i1, npix, 1, paim, variwgt,
			    mi, mq, mu, mv, merri, merrq, merru, merrv,
			    mp, mth, merp, merth, sumw, i2)

	       # Store the binned data in the output arrays
	       im = im + 1
	       PL[im]  = mp
	       PLs[im] = merp
	       TH[im]  = mth
	       THs[im] = merth
	       if (mi != 0.0) {
		   PC[im]  = mv / mi
		   PCs[im] = sqrt( merrv*merrv/(mi*mi) + 
				   merri*merri*mv*mv/(mi**4) )
	       }
	       Ib[im] = mi / scale
	       Qb[im] = mq / scale
	       Ub[im] = mu / scale
	       Vb[im] = mv / scale
	       Isb[im] = merri / scale
	       Qsb[im] = merrq / scale
	       Usb[im] = merru / scale
	       Vsb[im] = merrv / scale
	       wavb[im] = sumw / (i2-i1+1)
	       binstrt[im] = i1
	       binend[im] = i2

	       if (i2 == npix) {
		   nbins = im
		   return
	       } else {
		   i1 = i2 + 1
	       }
	    }

	# Loop for binning from BINCEN to end
	} else {
	    i1 = bincen
	    im = bincen - 1
	    
	    do ii = bincen, npix, 1 {

	       call cmpbin (I, Is, Q, Qs, U, Us, V, Vs, wave,
			    npix, i1, npix, 1, paim, variwgt,
			    mi, mq, mu, mv, merri, merrq, merru, merrv,
			    mp, mth, merp, merth, sumw, i2)

	       # Store the binned data in the output arrays
	       im = im + 1
	       PL[im]  = mp
	       PLs[im] = merp
	       TH[im]  = mth
	       THs[im] = merth
	       if (mi != 0.0) {
		   PC[im]  = mv / mi
		   PCs[im] = sqrt( merrv*merrv/(mi*mi) + 
				   merri*merri*mv*mv/(mi**4) )
	       }
	       Ib[im] = mi / scale
	       Qb[im] = mq / scale
	       Ub[im] = mu / scale
	       Vb[im] = mv / scale
	       Isb[im] = merri / scale
	       Qsb[im] = merrq / scale
	       Usb[im] = merru / scale
	       Vsb[im] = merrv / scale
	       wavb[im] = sumw / (i2-i1+1)
	       binstrt[im] = i1
	       binend[im] = i2

	       if (i2 != npix) {
		   i1 = i2 + 1
	       } else {
		   nbins = im
		   break
	       }
	    }

	    # Loop for binning from BINCEN-1 to beginning

	    i1 = bincen - 1
	    im = bincen

	    do ii = bincen-1, 1, -1 {

	       call cmpbin (I, Is, Q, Qs, U, Us, V, Vs, wave,
			    npix, i1, 1, -1, paim, variwgt,
			    mi, mq, mu, mv, merri, merrq, merru, merrv,
			    mp, mth, merp, merth, sumw, i2)

	       # Store the binned data in the output arrays
	       im = im - 1
	       PL[im]  = mp
	       PLs[im] = merp
	       TH[im]  = mth
	       THs[im] = merth
	       if (mi != 0.0) {
		   PC[im]  = mv / mi
		   PCs[im] = sqrt( merrv*merrv/(mi*mi) + 
				   merri*merri*mv*mv/(mi**4) )
	       }
	       Ib[im] = mi / scale
	       Qb[im] = mq / scale
	       Ub[im] = mu / scale
	       Vb[im] = mv / scale
	       Isb[im] = merri / scale
	       Qsb[im] = merrq / scale
	       Usb[im] = merru / scale
	       Vsb[im] = merrv / scale
	       wavb[im] = sumw / (i1-i2+1)
	       binstrt[im] = i2
	       binend[im] = i1

	       if (i2 != 1) {
		   i1 = i2 - 1
	       } else {
		 break
	       }
	    }

	    # Remove unused bins from output arrays
	    j = 0
	    do ii = im, nbins {
	       j = j + 1
	       PL[j]  = PL[ii]
	       TH[j]  = TH[ii]
	       PC[j]  = PC[ii]
	       PLs[j] = PLs[ii]
	       THs[j] = THs[ii]
	       PCs[j] = PCs[ii]
	       Ib[j]  = Ib[ii]
	       Qb[j]  = Qb[ii]
	       Ub[j]  = Ub[ii]
	       Vb[j]  = Vb[ii]
	       Isb[j] = Isb[ii]
	       Qsb[j] = Qsb[ii]
	       Usb[j] = Usb[ii]
	       Vsb[j] = Vsb[ii]
	       wavb[j] = wavb[ii]
	       binstrt[j] = binstrt[ii]
	       binend[j] = binend[ii]
	    }
	    nbins = j
	    do ii = nbins+1, npix {
	       PL[ii]  = 0.0
	       TH[ii]  = 0.0
	       PC[ii]  = 0.0
	       PLs[ii] = 0.0
	       THs[ii] = 0.0
	       PCs[ii] = 0.0 
	       Ib[ii]  = 0.0
	       Qb[ii]  = 0.0
	       Ub[ii]  = 0.0
	       Vb[ii]  = 0.0
	       Isb[ii] = 0.0
	       Qsb[ii] = 0.0
	       Usb[ii] = 0.0
	       Vsb[ii] = 0.0
	       wavb[ii] = 0.0
	       binstrt[ii] = 0
	       binend[ii] = 0
	    }
	}

	end
#
#
procedure cmpbin (I, Is, Q, Qs, U, Us, V, Vs, wave, npix, i1, nend, incr, paim,
		  variwgt, mi, mq, mu, mv, merri, merrq, merru, merrv, mp, mth,
		  merp, merth, sumw, i2)
#
#     Calculates linear polarization p, and its error, polarization
#     position angle and its error from the Q and U Stokes parameters. 
#     Bins the linear polarization, p, data and the position angle, theta,
#     data so that the p error per bin is less than or equal to PAIM. 
#     The circular polarization V and its error are also calulated
#     for this bin.
#     Bad points with flux and flux-error equal to zero are 
#     ignored in computing polarization and errors.
#
# Passed arguments
bool	variwgt
int	npix, i1, i2, nend, incr
real	paim, mi, mq, mu, mv, merri, merrq, merru, merrv, mp, mth, merp,
	merth, sumw
real	I[ARB], Is[ARB], Q[ARB], Qs[ARB], U[ARB], Us[ARB], V[ARB], Vs[ARB], 
	wave[ARB]

# Local variables
int	ii, k, badch
real	sumi, sumq, sumu, sumv, serri, serrq, serru, serrv, mi2, mqmu
real	erri, errq, erru, errv

begin

	# Initialize sums
	sumi  = 0.0
	sumq  = 0.0
	sumu  = 0.0
	sumv  = 0.0
	serri = 0.0
	serrq = 0.0
	serru = 0.0
	serrv = 0.0
	sumw  = 0.0
	badch = 0
	mi    = 0.0
	mq    = 0.0
	mu    = 0.0
	mv    = 0.0
	mp    = 0.0
	mth   = 0.0
	merp  = 0.0
	merth = 0.0

	do ii = i1, nend, incr {
	   i2 = ii
 
	   # Accumulate the wavelength values for all pixels in the bin
	   sumw = sumw + wave[ii]
 
	   # If this pixel is bad (sigma=0), then skip over it.
	   if (I[ii]==0.0 && Is[ii]==0.0) {
	       badch = badch + 1
	       if (i2 != nend) next
	   } else {
 
	   # Add the IQUV values to the current total for this bin and
	   # add the errors in quadrature to the error for the bin so far.
	   erri = Is[ii]*Is[ii]
	   errq = Qs[ii]*Qs[ii]
	   erru = Us[ii]*Us[ii]
	   errv = Vs[ii]*Vs[ii]

	   if (variwgt) {
	       if (erri != 0.0) sumi  = sumi + I[ii]/erri
	       if (errq != 0.0) sumq  = sumq + Q[ii]/errq
	       if (erru != 0.0) sumu  = sumu + U[ii]/erru
	       if (errv != 0.0) sumv  = sumv + V[ii]/errv
	       if (erri != 0.0) serri = serri + 1.0/erri
	       if (errq != 0.0) serrq = serrq + 1.0/errq
	       if (erru != 0.0) serru = serru + 1.0/erru
	       if (errv != 0.0) serrv = serrv + 1.0/errv
	   } else {
	       sumi  = sumi + I[ii]
	       sumq  = sumq + Q[ii]
	       sumu  = sumu + U[ii]
	       sumv  = sumv + V[ii]
	       serri = serri + erri
	       serrq = serrq + errq
	       serru = serru + erru
	       serrv = serrv + errv
	   }

	   # Compute the mean Stokes values for this bin so far.
           k = incr*(i2-i1) - badch + 1
	   if (variwgt) {
	       if (serri != 0.0) mi = sumi / serri
	       if (serrq != 0.0) mq = sumq / serrq
	       if (serru != 0.0) mu = sumu / serru
	       if (serrv != 0.0) mv = sumv / serrv
	       if (serri != 0.0) merri = sqrt(1.0/serri)
	       if (serrq != 0.0) merrq = sqrt(1.0/serrq)
	       if (serru != 0.0) merru = sqrt(1.0/serru)
	       if (serrv != 0.0) merrv = sqrt(1.0/serrv)
	   } else {
	       mi = sumi / k
	       mq = sumq / k
	       mu = sumu / k
	       mv = sumv / k
	       merri = sqrt(serri) / k
	       merrq = sqrt(serrq) / k
	       merru = sqrt(serru) / k
	       merrv = sqrt(serrv) / k
	   }
 
	   # Compute the linear polarization error for this bin so far.
	   if (mq==0.0 && mu==0.0) {
	       mp = 0.0
	       merp = sqrt (merrq*merrq + merru*merru)
	       if (i2 != nend) next
	   } else {
	       mi2 = mi*mi
	       mqmu = mq*mq + mu*mu
	       merp = sqrt( ((mq*merrq)**2 + (mu*merru)**2)/mi2/mqmu +
				merri*merri*mqmu/(mi2*mi2) )
	   }
	   }
 
	   # Check if the PL error of the bin is < or = PAIM or the end of the
	   # arrays have been reached.
           if (merp<=paim || i2==nend) {
 
	       # Compute polarization from mean Stokes values for this bin.
               mi2 = mi*mi
               mqmu = mq*mq + mu*mu
 
	       if (mi != 0.0)
		   mp = sqrt(mqmu) / mi
	       else
		   mp = 0.0
 
	       if (mq!=0.0 && mu!=0.0) {
		   mth   = 0.5 * atan2(mu,mq)
		   merth = 0.5 * (sqrt( ((mq*merru)**2 + (mu*merrq)**2) /
					     (mqmu*mqmu) ))
	       } else {
		   mth = 0.0
		   merth = 0.0
	       }
 
               return
           }
 
	   # MERRP still greater than PAIM; Continue looping.
	}
 
      end
