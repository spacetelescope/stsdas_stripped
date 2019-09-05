procedure binwav (I, Is, Q, Qs, U, Us, V, Vs, wave, npix, nbin, binstrt, 
		  binend, variwgt, PL, PLs, TH, THs, PC, PCs, Ib, Qb, Ub, Vb,
		  Isb, Qsb, Usb, Vsb, wavb)
#
#     Calculates linear polarization p, and its error, polarization
#     position angle and its error from the Q and U Stokes parameters. 
#     Bins the linear polarization, p, data and the position angle, 
#     theta, data and the circular polarization data and all errors
#     into NBIN bins.
#     Bad points with flux and flux-error equal to zero are 
#     ignored in computing polarization and errors.
#
#	Modified Aug-94 HAB: added binstrt array
#
# Passed arguments
bool	variwgt
int	npix, nbin, binstrt[ARB], binend[ARB]
real	I[ARB], Is[ARB], Q[ARB], Qs[ARB], U[ARB], Us[ARB], V[ARB], Vs[ARB], 
	PL[ARB], PLs[ARB], TH[ARB], THs[ARB], PC[ARB], PCs[ARB], Ib[ARB], 
	Qb[ARB], Ub[ARB], Vb[ARB], Isb[ARB], Qsb[ARB], Usb[ARB], Vsb[ARB], 
	wave[ARB], wavb[ARB]

# Local variables
int	ii, j, k, badch
real	mi, mq, mu, mv, merri, merrq, merru, merrv, mp, merp, mth, merth
real	erri, errq, erru, errv
real	sumw, scale, mi2, mqmu

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

	# Loop over output bins
	do j = 1, nbin, 1 {

	   # Initialize sums and results for this bin
	   mi=0.0
	   mq=0.0
	   mu=0.0
	   mv=0.0
	   merri=0.0
	   merrq=0.0
	   merru=0.0
	   merrv=0.0
	   sumw=0.0
	   mp=0.0
	   mth=0.0
	   merp=0.0
	   merth=0.0
	   badch=0

	   # Loop over the pixels for this bin
	   do ii = binstrt[j], binend[j], 1 {
 
	      # Accumulate the wavelength values for all pixels in the bin
	      sumw = sumw + wave[ii]
 
	      # If this pixel is bad (sigma=0), then skip over it.
	      if (I[ii]==0.0 && Is[ii]==0.0) {
		  badch = badch + 1
		  next
	      }
 
	      # Add the IQUV values to the current total for this bin and
	      # add the errors in quadrature to the error for the bin so far.
	      erri = Is[ii]*Is[ii]
	      errq = Qs[ii]*Qs[ii]
	      erru = Us[ii]*Us[ii]
	      errv = Vs[ii]*Vs[ii]

	      if (variwgt) {
		  if (erri != 0.0) mi = mi + I[ii]/erri
		  if (errq != 0.0) mq = mq + Q[ii]/errq
		  if (erru != 0.0) mu = mu + U[ii]/erru
		  if (errv != 0.0) mv = mv + V[ii]/errv
		  if (erri != 0.0) merri = merri + 1.0/erri
		  if (errq != 0.0) merrq = merrq + 1.0/errq
		  if (erru != 0.0) merru = merru + 1.0/erru
		  if (errv != 0.0) merrv = merrv + 1.0/errv
	      } else {
		  mi = mi + I[ii]
		  mq = mq + Q[ii]
		  mu = mu + U[ii]
		  mv = mv + V[ii]
		  merri = merri + erri
		  merrq = merrq + errq
		  merru = merru + erru
		  merrv = merrv + errv
	      }
	   } # Next pixel in this bin
 
	   # Compute the mean Stokes values for this bin
	   k = binend[j] - binstrt[j] - badch + 1
	   if (k > 0) {
	       if (variwgt) {
		   if (merri != 0.0) mi = mi / merri
		   if (merrq != 0.0) mq = mq / merrq
		   if (merru != 0.0) mu = mu / merru
		   if (merrv != 0.0) mv = mv / merrv
		   if (merri != 0.0) merri = sqrt (1.0/merri)
		   if (merrq != 0.0) merrq = sqrt (1.0/merrq)
		   if (merru != 0.0) merru = sqrt (1.0/merru)
		   if (merrv != 0.0) merrv = sqrt (1.0/merrv)
	       } else {
		   mi = mi / k
		   mq = mq / k
		   mu = mu / k
		   mv = mv / k
		   merri = sqrt(merri) / k
		   merrq = sqrt(merrq) / k
		   merru = sqrt(merru) / k
		   merrv = sqrt(merrv) / k
	       }
	   }
 
	   # Compute polarization from mean Stokes values for this bin
	   mi2  = mi*mi
	   mqmu = mq*mq + mu*mu

	   if (mi != 0.0) {
	       mp = sqrt(mqmu) / mi
	       if (mqmu != 0.0)
		   merp = sqrt ( ((mq*merrq)**2 + (mu*merru)**2)/mi2/mqmu +
				   merri*merri*mqmu/(mi2*mi2) )
	   }

	   if (mq!=0.0 && mu!=0.0) {
	       mth = 0.5 * atan2(mu,mq)
	       merth = 0.50 * (sqrt( ((mq*merru)**2 + (mu*merrq)**2) /
					(mqmu*mqmu) ))
	   }

	   # Store the mean Stokes and polarization values for this bin
	   PL[j]  = mp
	   TH[j]  = mth
	   PLs[j] = merp
	   THs[j] = merth
	   if (mi != 0.0) {
	       PC[j]  = mv / mi
	       PCs[j] = sqrt( merrv*merrv/mi2 + merri*merri*mv*mv/(mi2*mi2) )
	   }
	   Ib[j] = mi / scale
	   Qb[j] = mq / scale
	   Ub[j] = mu / scale
	   Vb[j] = mv / scale
	   Isb[j] = merri / scale
	   Qsb[j] = merrq / scale
	   Usb[j] = merru / scale
	   Vsb[j] = merrv / scale
	   wavb[j] = sumw / (binend[j]-binstrt[j]+1)
	}

	end
