# write the exposure summary for STIS paper product

procedure opp_expsum (rootname, fstat, igi_output)

string	rootname	{prompt="Root name of input files"}
string	fstat		{prompt="File used for computing statistics"}
string	igi_output 	{prompt="Output igi script file name"}

begin
	# Declarations
	string	root		# root name
	string	script		# igi script.
	string	fname		# file name
	string	obstype, detector, obsmode
	string	lrc_xsts, lrc_fail
	int	binaxis1, binaxis2, npix
	int	flen
	real	x1, x2, yoff
	
	# Get interactive parameters
	root = rootname
	script = igi_output

        # decide which file to use for the summary 
        if (access(root//"_sfl.fits")) {
            fname = root//"_sfl.fits"
        } else if (access(root//"_crj.fits")) {
            fname = root//"_crj.fits"
        } else if (access(root//"_flt.fits")) {
            fname = root//"_flt.fits"
        } else if (access(root//"_raw.fits")) {
            fname = root//"_raw.fits"
        }
	fname = fname//"[sci,1]"

	# redefines the window
        printf ("reset; fontset hard\n", >> script)
        printf ("vpage 0.65 1. 0.00 0.92\n", >> script)
        printf ("limits 0 30 40 0\n", >> script)
        printf ("move 0 0; draw 0 40\n", >> script)
	printf ("expand 0.65\n", >> script)

	# tab positions
	x1 = 19.
	x2 = 20.
	yoff = -1.
    
	# print OBS keywords
	yoff = yoff + 2.
        keypar (fname, "ROOTNAME", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Rootname:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "TDATEOBS", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Obs. Date:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "TTIMEOBS", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Obs. Time:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	# print TARGET keywords
	yoff = yoff + 2.
        keypar (fname, "TARGNAME", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Target Name:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "RA_TARG", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'R.A. (J2000):'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%0.2H'\n", 
		x2, yoff, real(keypar.value), >> script)

	yoff = yoff + 1.
        keypar (fname, "DEC_TARG", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Dec. (J2000):'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%0.1h'\n", 
		x2, yoff, real(keypar.value), >> script)

	# read other keywords
        keypar (fname, "DETECTOR", silent=yes)
	detector = keypar.value
        keypar (fname, "OBSMODE", silent=yes)
	obsmode = keypar.value
        keypar (fname, "OBSTYPE", silent=yes)
	obstype = keypar.value
        keypar (fname, "BINAXIS1", silent=yes)
	if (keypar.found)
	    binaxis1 = int(keypar.value)
	else {
	    printf ("Keyword BINAXIS1 not found in %s\n", fname)
	    binaxis1 = 0
	}
        keypar (fname, "BINAXIS2", silent=yes)
	if (keypar.found)
	    binaxis2 = int(keypar.value)
	else {
	    printf ("Keyword BINAXIS2 not found in %s\n", fname)
	    binaxis2 = 0
	}

	# print IMAGING/SPECTROSCOPIC keywords
	yoff = yoff + 2.
	printf ("justify 7; move %0.4f %0.4f; label 'Detector:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, detector, >> script)

	yoff = yoff + 1.
	printf ("justify 7; move %0.4f %0.4f; label 'Observation Mode:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, obsmode, >> script)

	yoff = yoff + 1.
	printf ("justify 7; move %0.4f %0.4f; label 'Observation Type:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, obstype, >> script)

	yoff = yoff + 1.
        keypar (fname, "SCLAMP", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Lamp:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "APERTURE", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Aperture:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	# print IMAGING keywords
	if (obstype == "IMAGING") {
	    yoff = yoff + 1.
            keypar (fname, "FILTER", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Filter:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)

	    yoff = yoff + 1.
            keypar (fname, "OPT_ELEM", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Mirror:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)

	}

	# print SPECTROSCOPIC keywords
	if (obstype == "SPECTROSCOPIC") {
	    yoff = yoff + 1.
            keypar (fname, "OPT_ELEM", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Grating:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)

	    yoff = yoff + 1.
            keypar (fname, "CENWAVE", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Central Wavelength:'\n", 
		    x1, yoff, >> script)
	    if (keypar.value == "")
	        printf ("justify 9; move %0.4f %0.4f; label '(N/A)'\n", 
		    	x2, yoff, >> script)
	    else
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    	x2, yoff, nint(real(keypar.value)), >> script)
	}

	# print IMAGING/SPECTROSCOPIC keywords
	yoff = yoff + 1.
        keypar (fname, "TEXPTIME", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Total Exposure Time:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%0.2f'\n", 
			x2, yoff, real(keypar.value), >> script)
	else 
	    printf ("Keyword TEXPTIME not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "NRPTEXP", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label '# of Exposures:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    x2, yoff, int(keypar.value), >> script)
	else 
	    printf ("Keyword NRPTEXP not found in %s\n", fname)

	# print CCD keywords
	if (detector == "CCD") {
	    yoff = yoff + 2.
            keypar (fname, "CCDAMP", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'CCDAMP:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)

	    yoff = yoff + 1.
            keypar (fname, "CCDGAIN", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'CCDGAIN:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword CCDGAIN not found in %s\n", fname)
	}

	# print MAMA keywords
	if (detector == "FUV-MAMA" || detector == "NUV-MAMA") {
	    yoff = yoff + 2.
            keypar (fname, "GLOBRATE", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Global Count Rate:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	    	printf ("justify 9; move %0.4f %0.4f; label '%0.1f'\n", 
		    	x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword GLOBRATE not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "LRC_XSTS", silent=yes)
	    lrc_xsts = keypar.value
	    if (lrc_xsts == "yes") 
	 	lrc_xsts = "T"
	    else
	 	lrc_xsts = "F"
	    printf ("justify 7; move %0.4f %0.4f; label 'Local Rate Check Image Exists:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, lrc_xsts, >> script)

	    yoff = yoff + 1.
            keypar (fname, "MOFFSET1", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'MAMA Axis 1 Offset:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword MOFFSET1 not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "MOFFSET2", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'MAMA Axis 2 Offset:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword MOFFSET2 not found in %s\n", fname)
	}

	# print BINNING keywords
	if (binaxis1 > 1 || binaxis2 >1) {
	    yoff = yoff + 2.
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 1 Binning:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    x2, yoff, binaxis1, >> script)

	    yoff = yoff + 1.
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 2 Binning:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    x2, yoff, binaxis2, >> script)
	}

	# print SUBARRAY keywords
        keypar (fname, "SUBARRAY", silent=yes)
	if (keypar.value == "yes") {
	    yoff = yoff + 2.
            keypar (fname, "CENTERA1", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 1 Subarray Center:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword CENTERA1 not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "CENTERA2", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 2 Subarray Center:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword CENTERA2 not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "SIZAXIS1", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 1 Size:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword SIZAXIS1 not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "SIZAXIS2", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Axis 2 Size:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword SIZAXIS2 not found in %s\n", fname)
	}

	# print ACQ keywords
	if (detector == "CCD" && obsmode == "ACQ") {
	    yoff = yoff + 2.
            keypar (fname, "ACQTYPE", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Acquisition Type:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)
	}

	# print ACQ/PEAK keywords
	if (detector == "CCD" && obsmode == "ACQ/PEAK") {
	    yoff = yoff + 2.
            keypar (fname, "PKSEARCH", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Search Method:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)

	    yoff = yoff + 1.
            keypar (fname, "NUMSTEPS", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label '# of Steps:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword NUMSTEPS not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "PEAKSTEP", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Step Size:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword PEAKSTEP not found in %s\n", fname)
	}

	# print MAMA LRC keywords
	if (detector == "FUV-MAMA" || detector == "NUV-MAMA") {
	    if (lrc_xsts == "T") {
	    	yoff = yoff + 2.
            	keypar (fname, "LRC_FAIL", silent=yes)
	        lrc_fail = keypar.value
	        if (lrc_fail == "yes") 
	 	    lrc_fail = "T"
	        else
	 	    lrc_fail = "F"
	    	printf ("justify 7; move %0.4f %0.4f; label 'Local Rate Check Failed:'\n", x1, yoff, >> script)
	    	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    	x2, yoff, lrc_fail, >> script)
	    }
	}

	# print STATS keywords, if the obs mode is not ACQ or ACQ/PEAKUP
	if (obsmode != "ACQ" && obsmode != "ACQ/PEAK") {
            if (!access (fstat)) {
                printf ("Can not access file '%s', skip.\n", fstat)
                bye
            }
	    fname = fstat // "[sci,1]"
	    flen = strlen (fstat)

	    # skip the 1-D spec case for now 3/10/98
	    if (substr(fstat,flen-7,flen-5) == "x1d" ||
	    	substr(fstat,flen-7,flen-5) == "sx1") bye

	    yoff = yoff + 2.
	    npix = 1
            keypar (fname, "i_naxis1", silent=yes)
	    if (keypar.found) npix = int(keypar.value) * npix
            keypar (fname, "i_naxis2", silent=yes)
	    if (keypar.found) npix = int(keypar.value) * npix
	    printf ("justify 7; move %0.4f %0.4f; label '# Pixels:'\n", 
		    x1, yoff, >> script)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, npix, >> script)
    
	    # the LRC case
	    if (substr(fstat,flen-7,flen-5) == "lrc") {
	        yoff = yoff + 1.
                keypar (fname, "DATAMIN", silent=yes)
	        printf ("justify 7; move %0.4f %0.4f; label 'Minimum:'\n", 
		        x1, yoff, >> script)
	        if (keypar.found)
	            printf ("justify 9; move %0.4f %0.4f; label '%0.2f'\n", 
		            x2, yoff, real(keypar.value), >> script)
	        else 
	            printf ("Keyword DATAMIN not found in %s\n", fname)
    
	        yoff = yoff + 1.
                keypar (fname, "DATAMAX", silent=yes)
	        printf ("justify 7; move %0.4f %0.4f; label 'Maximum:'\n", 
		        x1, yoff, >> script)
	        if (keypar.found)
	            printf ("justify 9; move %0.4f %0.4f; label '%0.2f'\n", 
		            x2, yoff, real(keypar.value), >> script)
	        else 
	            printf ("Keyword DATAMAX not found in %s\n", fname)

		bye
	    }

	    yoff = yoff + 1.
            keypar (fname, "NGOODPIX", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label '# Good Pixels:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		        x2, yoff, int(keypar.value), >> script)
	    else 
	        printf ("Keyword NGOODPIX not found in %s\n", fname)
    
	    yoff = yoff + 1.
            keypar (fname, "GOODMIN", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Good Minimum:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.5g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword GOODMIN not found in %s\n", fname)
    
	    yoff = yoff + 1.
            keypar (fname, "GOODMAX", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Good Maximum:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.5g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword GOODMAX not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "GOODMEAN", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Good Mean:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.5g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword GOODMEAN not found in %s\n", fname)

	    yoff = yoff + 1.
            keypar (fname, "SNRMIN", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Min S/N:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.4g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword SNRMIN not found in %s\n", fname)
    
	    yoff = yoff + 1.
            keypar (fname, "SNRMAX", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Max S/N:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.4g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword SNRMAX not found in %s\n", fname)
    
	    yoff = yoff + 1.
            keypar (fname, "SNRMEAN", silent=yes)
	    printf ("justify 7; move %0.4f %0.4f; label 'Mean S/N:'\n", 
		    x1, yoff, >> script)
	    if (keypar.found)
	        printf ("justify 9; move %0.4f %0.4f; label '%0.4g'\n", 
		        x2, yoff, real(keypar.value), >> script)
	    else 
	        printf ("Keyword SNRMEAN not found in %s\n", fname)
	}
end
