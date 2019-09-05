# WF3BOT -- Print header specific to the Wide Field Camera 3 (WFC3)
#

procedure wf3bot (im)

int     imgeti()
int     nrptexp, crsplit, totpts, numpts1, patstep, numpts2
int	binning, nsamp

real    imgetr()
real    exptime, pspac1, pspac2, gain

bool    imgetb(), streq(), strne()
bool	subarray

char    detector[8], filter[24], sampseq[SZ_FNAME]
char	aperture[SZ_FNAME], ccdamp[8], sclamp[32]
char	pattern1[SZ_FNAME], pattern2[SZ_FNAME], asnid[SZ_FNAME]
char    dqicorr[10], atodcorr[10], blevcorr[10], biascorr[10]
char	crcorr[10], shadcorr[10], darkcorr[10], flatcorr[10]
char	photcorr[10], rptcorr[10], drizcorr[10], flshcorr[10]
char	zsigcorr[10], zoffcorr[10], nlincorr[10], unitcorr[10]
char    outcal[75]

string  complete "COMPLETE"
string	dqi 	  " DQI"
string	atod	  " ATOD"
string  blev      " BLEV"
string  bias      " BIAS"
string	flsh	  " FLSH"
string  cr        " CRRJ"
string  shad      " SHAD"
string  dark      " DARK"
string  flat      " FLAT"
string	phot      " PHOT"
string	rpt       " RPT "
string	driz      " DRIZ"
string	zsig	  " ZSIG"
string	zoff	  " ZOFF"
string	nlin	  " NLIN"
string	unit	  " UNIT"
string  none      " none" 

pointer im

begin

	iferr (call imgstr (im, "DETECTOR", detector, 8))
	    detector[1] = EOS
        iferr (call imgstr (im, "FILTER", filter, 24))
            filter[1] = EOS
	iferr (call imgstr (im, "APERTURE", aperture, SZ_FNAME))
	    aperture[1] = EOS
	iferr (subarray = imgetb (im, "SUBARRAY"))
	    subarray = false
	iferr (binning = imgeti (im, "BINAXIS1"))
	    binning = 0
        iferr (exptime = imgetr (im, "EXPTIME"))
            exptime = 0.0
	iferr (nrptexp = imgeti (im, "NRPTEXP"))
	    nrptexp = 0
	iferr (crsplit = imgeti (im, "CRSPLIT"))
	    crsplit = 0
	iferr (call imgstr (im, "CCDAMP", ccdamp, 4))
	    ccdamp[1] = EOS
	iferr (gain = imgetr (im, "CCDGAIN"))
	    gain = 0.0
	iferr (call imgstr (im, "SCLAMP", sclamp, 30))
	    sclamp[1] = EOS

	iferr (call imgstr (im, "SAMP_SEQ", sampseq, SZ_FNAME))
	    sampseq[1] = EOS
	iferr (nsamp = imgeti (im, "NSAMP"))
	    nsamp = 0

	iferr (call imgstr (im, "PATTERN1", pattern1, SZ_FNAME))
	    pattern1[1] = EOS
	iferr (call imgstr (im, "PATTERN2", pattern2, SZ_FNAME))
	    pattern2[1] = EOS
	iferr (numpts1 = imgeti (im, "P1_NPTS"))
	    numpts1 = 0
	iferr (numpts2 = imgeti (im, "P2_NPTS"))
	    numpts2 = 0
	if (numpts2 > 0) 
	    totpts = numpts1 * numpts2
	else
	    totpts = numpts1
	iferr (patstep = imgeti (im, "PATTSTEP"))
	    patstep = 0
	iferr (pspac1 = imgetr (im, "P1_PSPAC"))
	    pspac1 = 0.0
	iferr (pspac2 = imgetr (im, "P2_PSPAC"))
	    pspac2 = 0.0
	iferr (call imgstr (im, "ASN_ID", asnid, SZ_FNAME))
	    asnid[1] = EOS

        iferr (call imgstr (im, "DQICORR", dqicorr, 10))
           dqicorr[1]=EOS
        iferr (call imgstr (im, "ATODCORR", atodcorr, 10))
           atodcorr[1]=EOS
        iferr (call imgstr (im, "BLEVCORR", blevcorr, 10))
           blevcorr[1]=EOS
        iferr (call imgstr (im, "BIASCORR", biascorr, 10))
           biascorr[1]=EOS
        iferr (call imgstr (im, "FLSHCORR", flshcorr, 10))
           flshcorr[1]=EOS
        iferr (call imgstr (im, "CRCORR", crcorr, 10))
           crcorr[1]=EOS
        iferr (call imgstr (im, "SHADCORR", shadcorr, 10))
           shadcorr[1]=EOS
        iferr (call imgstr (im, "DARKCORR", darkcorr, 10))
           darkcorr[1]=EOS
        iferr (call imgstr (im, "FLATCORR", flatcorr, 10))
           flatcorr[1]=EOF
        iferr (call imgstr (im, "PHOTCORR", photcorr, 10))
           photcorr[1]=EOF
        iferr (call imgstr (im, "RPTCORR", rptcorr, 10))
           rptcorr[1]=EOF
        iferr (call imgstr (im, "DRIZCORR", drizcorr, 10))
           drizcorr[1]=EOF

        iferr (call imgstr (im, "ZSIGCORR", zsigcorr, 10))
           zsigcorr[1]=EOF
        iferr (call imgstr (im, "ZOFFCORR", zoffcorr, 10))
           zoffcorr[1]=EOF
        iferr (call imgstr (im, "NLINCORR", nlincorr, 10))
           nlincorr[1]=EOF
        iferr (call imgstr (im, "UNITCORR", unitcorr, 10))
           unitcorr[1]=EOF

	call printf ("Detector%10t= %s%25tAperture%35t= %s%50tSubarray%60t= %b\n")
	   call pargstr (detector)
	   call pargstr (aperture)
	   call pargb   (subarray)

	call printf ("Filter%10t= %s%25tBinning%35t= %d%50tCal Lamp%60t= %s\n")
	   call pargstr (filter)
	   call pargi   (binning)
	   call pargstr (sclamp)

	call printf ("CCD Amps%10t= %s%25tCCD Gain%35t= %g\n")
	   call pargstr (ccdamp)
	   call pargr   (gain)

	if (nsamp > 0) {
	    call printf ("Samp Seq%10t= %s%25tNum Samps%35t= %d\n")
		call pargstr (sampseq)
		call pargi   (nsamp)
	}

	call printf ("Exp Time%10t= %g%25tCR Splits%35t= %d%50tNum Repeat%60t= %d\n")
	   call pargr (exptime)
	   call pargi (crsplit)
	   call pargi (nrptexp)

	if (strne (asnid, "NONE")) {
	    if (totpts > 0) {
		call printf ("\nAssoc ID%10t= %s%35tTotal Pts%45t= %d%55tPatt Step%66t= %d\n")
		   call pargstr (asnid)
		   call pargi   (totpts)
		   call pargi   (patstep)
	    } else {
	        call printf ("\nAssoc ID%10t= %s\n")
		   call pargstr (asnid)
	    }
	}

	if (numpts1 > 0) {
	    call printf (
		 "Pattern1%10t= %s%35tNum Pts%45t= %d%55tPt Spacing%66t= %g\n")
		 call pargstr (pattern1)
		 call pargi (numpts1)
		 call pargr (pspac1)
	}

	if (numpts2 > 0) {
	    call printf (
		 "Pattern2%10t= %s%35tNum Pts%45t= %d%55tPt Spacing%66t= %g\n")
		 call pargstr (pattern2)
		 call pargi (numpts2)
		 call pargr (pspac2)
	}

        # Build string of calibration switches done
        outcal[1]=EOS
        if (streq (dqicorr,  complete)) call strcat (dqi,  outcal, 75)
        if (streq (atodcorr, complete)) call strcat (atod, outcal, 75)
        if (streq (blevcorr, complete)) call strcat (blev, outcal, 75)
        if (streq (biascorr, complete)) call strcat (bias, outcal, 75)
	if (streq (zsigcorr, complete)) call strcat (zsig, outcal, 75)
	if (streq (zoffcorr, complete)) call strcat (zoff, outcal, 75)
        if (streq (flshcorr, complete)) call strcat (flsh, outcal, 75)
        if (streq (crcorr,   complete)) call strcat (cr,   outcal, 75)
        if (streq (shadcorr, complete)) call strcat (shad, outcal, 75)
        if (streq (darkcorr, complete)) call strcat (dark, outcal, 75)
        if (streq (nlincorr, complete)) call strcat (nlin, outcal, 75)
        if (streq (flatcorr, complete)) call strcat (flat, outcal, 75)
        if (streq (photcorr, complete)) call strcat (phot, outcal, 75)
        if (streq (unitcorr, complete)) call strcat (unit, outcal, 75)
        if (streq (rptcorr,  complete)) call strcat (rpt,  outcal, 75)
        if (streq (drizcorr, complete)) call strcat (driz, outcal, 75)
        if (outcal[1] == EOS) call strcat (none, outcal, 75)
 
        call printf ("\nCalibration steps done:\n")
        call printf (" %s\n")
           call pargstr (outcal)

end

