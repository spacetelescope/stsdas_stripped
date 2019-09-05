# NICBOT -- Print header specific to the Near-Infrared Camera and Multi-Object 
# Spectrometer (NICMOS)
#
# 2003-06-03 H.Bushouse: Modified to accomodate new pattern keywords and
# zsigcorr and barscorr calibration switches.

procedure nicbot (im)

int     imgeti()
int     nextend, camera, numiter, nread, nsamp, numpos, patpos, numpos2

real    imgetr()
real    exptime, chopsz, dithsz

bool    streq(), strne()

char    filter[SZ_FNAME], imagetyp[SZ_FNAME], mode[SZ_FNAME]
char	aperture[SZ_FNAME], sampseq[SZ_FNAME], readout[SZ_FNAME]
char	pattern[SZ_FNAME], asnid[SZ_FNAME]
char    zoffdone[SZ_FNAME], maskdone[SZ_FNAME], biasdone[SZ_FNAME]
char	noisdone[SZ_FNAME], darkdone[SZ_FNAME], nlindone[SZ_FNAME]
char	flatdone[SZ_FNAME], unitdone[SZ_FNAME], photdone[SZ_FNAME]
char	criddone[SZ_FNAME], backdone[SZ_FNAME], warndone[SZ_FNAME]
char	illmdone[SZ_FNAME], zsigdone[SZ_FNAME], barsdone[SZ_FNAME]
char    outcal[75]

string  performed "PERFORMED"
string	zsig	  " ZSIG"
string	zoff	  " ZOFF"
string  mask      " MASK"
string  bias      " BIAS"
string  nois      " NOIS"
string  dark      " DARK"
string  nlin      " NLIN"
string  bars      " BARS"
string  flat      " FLAT"
string	unit      " UNIT"
string	phot      " PHOT"
string	crid      " CRID"
string	back      " BACK"
string	warn      " WARN"
string	illm      " ILLM"
string  none      " none" 

pointer im

begin

        iferr (nextend = imgeti (im, "NEXTEND"))
            nextend = 0
        
        iferr (call imgstr (im, "IMAGETYP", imagetyp, SZ_FNAME))
            imagetyp[1] = EOS

	iferr (camera = imgeti (im, "CAMERA"))
	    camera = 0
        iferr (call imgstr (im, "FILTER", filter, SZ_FNAME))
            filter[1] = EOS
        iferr (call imgstr (im, "OBSMODE",     mode,     SZ_FNAME))
            mode[1] = EOS
	iferr (call imgstr (im, "APERTURE", aperture, SZ_FNAME))
	    aperture[1] = EOS
        iferr (exptime = imgetr (im, "EXPTIME"))
            exptime = 0.0
	iferr (numiter = imgeti (im, "NUMITER"))
	    numiter = 0
	iferr (nread = imgeti (im, "NREAD"))
	    nread = 0
	iferr (nsamp = imgeti (im, "NSAMP"))
	    nsamp = 0
	iferr (call imgstr (im, "SAMP_SEQ", sampseq, SZ_FNAME))
	    sampseq[1] = EOS
	iferr (call imgstr (im, "READOUT", readout, SZ_FNAME))
	    readout[1] = EOS
	iferr (call imgstr (im, "PATTERN1", pattern, SZ_FNAME)) {
	    iferr (call imgstr (im, "PATTERN", pattern, SZ_FNAME))
	        pattern[1] = EOS
	}
	iferr (numpos2 = imgeti (im, "P2_NPTS"))
	    numpos2 = 0
	iferr (numpos = imgeti (im, "P1_NPTS")) {
	    iferr (numpos = imgeti (im, "NUMPOS"))
	        numpos = 0
	}
	if (numpos2 > 0) numpos = numpos * numpos2
	iferr (patpos = imgeti (im, "PATTSTEP")) {
	    iferr (patpos = imgeti (im, "PATT_POS"))
	        patpos = 0
	}
	iferr (call imgstr (im, "ASN_ID", asnid, SZ_FNAME))
	    asnid[1] = EOS
	iferr (chopsz = imgetr (im, "P1_LSPAC")) {
	    iferr (chopsz = imgetr (im, "CHOPSIZE"))
	        chopsz = 0.0
	}
	iferr (dithsz = imgetr (im, "P1_PSPAC")) {
	    iferr (dithsz = imgetr (im, "DITHSIZE"))
	        dithsz = 0.0
	}

        iferr (call imgstr (im, "ZSIGDONE", zsigdone, SZ_FNAME))
           zsigdone[1]=EOS
        iferr (call imgstr (im, "ZOFFDONE", zoffdone, SZ_FNAME))
           zoffdone[1]=EOS
        iferr (call imgstr (im, "MASKDONE", maskdone, SZ_FNAME))
           maskdone[1]=EOS
        iferr (call imgstr (im, "BIASDONE", biasdone, SZ_FNAME))
           biasdone[1]=EOS
        iferr (call imgstr (im, "NOISDONE", noisdone, SZ_FNAME))
           noisdone[1]=EOS
        iferr (call imgstr (im, "DARKDONE", darkdone, SZ_FNAME))
           darkdone[1]=EOS
        iferr (call imgstr (im, "NLINDONE", nlindone, SZ_FNAME))
           nlindone[1]=EOS
        iferr (call imgstr (im, "BARSDONE", barsdone, SZ_FNAME))
           barsdone[1]=EOS
        iferr (call imgstr (im, "FLATDONE", flatdone, SZ_FNAME))
           flatdone[1]=EOF
        iferr (call imgstr (im, "UNITDONE", unitdone, SZ_FNAME))
           unitdone[1]=EOF
        iferr (call imgstr (im, "PHOTDONE", photdone, SZ_FNAME))
           photdone[1]=EOF
        iferr (call imgstr (im, "CRIDDONE", criddone, SZ_FNAME))
           criddone[1]=EOF
        iferr (call imgstr (im, "BACKDONE", backdone, SZ_FNAME))
           backdone[1]=EOF
        iferr (call imgstr (im, "WARNDONE", warndone, SZ_FNAME))
           warndone[1]=EOF
        iferr (call imgstr (im, "ILLMDONE", illmdone, SZ_FNAME))
           illmdone[1]=EOF

        call printf ("Image type = %s%31tNumber of extensions = %d\n\n")
           call pargstr (imagetyp)
	   call pargi (nextend)

	call printf ("Camera%11t= %d%31tAperture%42t= %s%56tFilter%66t= %s\n")
	   call pargi (camera)
	   call pargstr (aperture)
	   call pargstr (filter)

	if (streq (mode, "ACCUM")) {
	    call printf ("Obs Mode%11t= %s%31tNum Reads%42t= %d\n")
		 call pargstr (mode)
		 call pargi (nread)
	} else if (streq (mode, "MULTIACCUM")) {
	    call printf (
		 "Obs Mode%11t= %s%31tNum Samps%42t= %d%56tSamp Seq%66t= %s\n")
		 call pargstr (mode)
		 call pargi (nsamp)
		 call pargstr (sampseq)
	} else if (streq (mode, "RAMP")) {
	    call printf ("Obs Mode%11t= %s%31tNum Samps%42t= %d\n")
		 call pargstr (mode)
		 call pargi (nsamp)
	} else {
	    call printf ("Obs Mode%11t= %s\n")
		 call pargstr (mode)
	}

	call printf ("Exp Time%11t= %g%31tRead Speed%42t= %s\n")
	     call pargr (exptime)
	     call pargstr (readout)

	if (strne (asnid, "NONE")) {
	    call printf ("\nAssoc ID%11t= %s%31tNum Iters%42t= %d\n")
		 call pargstr (asnid)
		 call pargi (numiter)
	}

	if (strne (pattern, "NONE")) {
	    call printf (
		 "Pattern%11t= %s%31tNum Posns%42t= %d%56tPatt Posn%66t= %d\n")
		 call pargstr (pattern)
		 call pargi (numpos)
		 call pargi (patpos)

	    call printf ("Dith Size%11t= %g%31tChop Size%42t= %g\n")
		 call pargr (dithsz)
		 call pargr (chopsz)
	}

        # Build string of calibration switches done
        outcal[1]=EOS
        if (streq (zsigdone, performed)) call strcat (zsig, outcal, 75)
        if (streq (zoffdone, performed)) call strcat (zoff, outcal, 75)
        if (streq (maskdone, performed)) call strcat (mask, outcal, 75)
        if (streq (biasdone, performed)) call strcat (bias, outcal, 75)
        if (streq (noisdone, performed)) call strcat (nois, outcal, 75)
        if (streq (darkdone, performed)) call strcat (dark, outcal, 75)
        if (streq (nlindone, performed)) call strcat (nlin, outcal, 75)
        if (streq (barsdone, performed)) call strcat (bars, outcal, 75)
        if (streq (flatdone, performed)) call strcat (flat, outcal, 75)
        if (streq (unitdone, performed)) call strcat (unit, outcal, 75)
        if (streq (photdone, performed)) call strcat (phot, outcal, 75)
        if (streq (criddone, performed)) call strcat (crid, outcal, 75)
        if (streq (backdone, performed)) call strcat (back, outcal, 75)
        if (streq (warndone, performed)) call strcat (warn, outcal, 75)
        if (outcal[1] == EOS) call strcat (none, outcal, 75)
 
        call printf ("\nCalibration steps done:\n")
        call printf (" %s\n")
           call pargstr (outcal)

end

