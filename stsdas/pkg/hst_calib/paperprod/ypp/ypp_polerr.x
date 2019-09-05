include	<imhdr.h>

# Calculate the FOS polarimetry mean values and errors
# Remove the NREAD dependency of grp's 5/4/99 JC Hsu
# Modified to properly calculate bin ranges. 27 July 2001 WJH

procedure ypp_polerr ()

char	input_c0[SZ_FNAME]
char	input_c3[SZ_FNAME]
char	ftype[SZ_FNAME]
char	output[SZ_FNAME]
int	pstart, pend, npix
int	nread

int	fd
int	nbins, npts
pointer	c0h, c3h
pointer	lambda, flux, q, u, ferr, qerr, uerr
real    dmin, dmax
int	i, j, k, np
int	grp, fgrp, qgrp, ugrp
int	ferrgrp, qerrgrp, uerrgrp
real	qsum, usum, dqsq, dusq, qr, ur, qq, uu, dq, du
real	wmin, wmax, ww, dw
int     istart,iend

pointer	immap()
int	open()
int	imgeti()
int	clgeti()
pointer	imgl1r()
pointer	imgl2r()
bool	streq()

begin

	# read parameters
	call clgstr ("input_c0", input_c0, SZ_FNAME)
	call clgstr ("input_c3", input_c3, SZ_FNAME)
	call clgstr ("ftype", ftype, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	pstart = clgeti ("pstart")
	pend = clgeti ("pend")
	npix = clgeti ("npix")
	nread = clgeti ("nread")
 
	nbins = (abs(pend - pstart) + 1) / npix
        grp = 1
        fgrp = 43
        qgrp = 44
        ugrp = 45
        ferrgrp = 47
        qerrgrp = 48
        uerrgrp = 49
    
	# open input files
	c0h = immap (input_c0, READ_ONLY, 0)
	c3h = immap (input_c3, READ_ONLY, 0)

	npts = imgeti (c0h, "i_naxis1")

	# allocate data space
	call calloc (lambda, npts, TY_REAL)
	call calloc (flux, npts, TY_REAL)
	call calloc (q, npts, TY_REAL)
	call calloc (u, npts, TY_REAL)
	call calloc (ferr, npts, TY_REAL)
	call calloc (qerr, npts, TY_REAL)
	call calloc (uerr, npts, TY_REAL)

	# read input data
	if (streq(ftype,"fits")) {
	    call amovr (Memr[imgl2r(c0h,grp)], Memr[lambda], npts)
	    call amovr (Memr[imgl2r(c3h,fgrp)], Memr[flux], npts)
	    call amovr (Memr[imgl2r(c3h,qgrp)], Memr[q], npts)
	    call amovr (Memr[imgl2r(c3h,ugrp)], Memr[u], npts)
	    call amovr (Memr[imgl2r(c3h,ferrgrp)], Memr[ferr], npts)
	    call amovr (Memr[imgl2r(c3h,qerrgrp)], Memr[qerr], npts)
	    call amovr (Memr[imgl2r(c3h,uerrgrp)], Memr[uerr], npts)
	} else {
	    call gf_opengr (c0h, grp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c0h)], Memr[lambda], npts)
	    call gf_opengr (c3h, fgrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[flux], npts)
	    call gf_opengr (c3h, qgrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[q], npts)
	    call gf_opengr (c3h, ugrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[u], npts)
	    call gf_opengr (c3h, ferrgrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[ferr], npts)
	    call gf_opengr (c3h, qerrgrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[qerr], npts)
	    call gf_opengr (c3h, uerrgrp, dmin, dmax, 0)
	    call amovr (Memr[imgl1r(c3h)], Memr[uerr], npts)
	}

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# determine which way the index should go
	k = 1
	if (pstart > pend) k = -1

	# loop through each bin
	do j = 1, nbins {
	    wmin = 1.e20
	    wmax = 0.
	    qsum = 0.
	    usum = 0.
	    dqsq = 0.
	    dusq = 0.
	    np = 0

        istart = pstart+(k*(j-1)*npix)
        iend = pstart+(k*j*npix)
        # Modified so that the ranges get properly calculated
        # whether k is 1 or -1... WJH 27 July 2001    
	    do i = istart, iend, k {

#call printf ("I = %6.3f Q = %6.3f U = %6.3f Ierr = %6.3f Qerr = %6.3f Uerr = %6.3f\n")
#call pargr(Memr[flux+i-1])
#call pargr(Memr[q+i-1])
#call pargr(Memr[u+i-1])
#call pargr(Memr[ferr+i-1])
#call pargr(Memr[qerr+i-1])
#call pargr(Memr[uerr+i-1])
		
		wmin = min (Memr[lambda+i-1], wmin)
		wmax = max (Memr[lambda+i-1], wmax)

	        if (Memr[flux+i-1] <= 0.) next
		np = np + 1
		qr = Memr[q+i-1]/Memr[flux+i-1] 
		ur = Memr[u+i-1]/Memr[flux+i-1] 
		qsum = qr + qsum
		usum = ur + usum
		dqsq = (Memr[qerr+i-1]**2 + (qr*Memr[ferr+i-1])**2) / 
			Memr[flux+i-1]**2 + dqsq
		dusq = (Memr[uerr+i-1]**2 + (ur*Memr[ferr+i-1])**2) / 
			Memr[flux+i-1]**2 + dusq
	    }

	    ww = (wmin + wmax) / 2.
	    dw = (wmax - wmin) / 2.

	    if (np == 0) {
		qq = INDEF
		uu = INDEF
		dq = INDEF
		du = INDEF
	    } else {
	    	qq = qsum / float(np) *100.
	    	uu = usum / float(np) *100.
		dq = sqrt(dqsq/float(np)) *100.
		du = sqrt(dusq/float(np)) *100.
	    }
        
	    # write to output file:
	    # column 1: wavelength
	    # column 2: wavelength error bar
	    # column 3: Q/I (%) 
	    # column 4: Q/I error bar
	    # column 5: U/I (%) 
	    # column 6: U/I error bar
	    call fprintf (fd, "%0.2f %0.3f %0.2f %0.2f %0.2f %0.2f\n")
		call pargr(ww)
		call pargr(dw)
		call pargr(qq)
		call pargr(dq)
		call pargr(uu)
		call pargr(du)
	}
	
	# free memory
	call mfree (lambda, TY_REAL)
	call mfree (flux, TY_REAL)
	call mfree (q, TY_REAL)
	call mfree (u, TY_REAL)
	call mfree (ferr, TY_REAL)
	call mfree (qerr, TY_REAL)
	call mfree (uerr, TY_REAL)

	# close files
	call imunmap (c0h)
	call imunmap (c3h)
	call close (fd)
end
