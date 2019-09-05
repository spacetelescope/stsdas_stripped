procedure wavecal (input, wavecal)

string input	 {"", prompt="Input calibrated science file"}
string wavecal   {"", prompt="Input raw wavecal file"}
string debugfile {"", prompt = "File for calstis4 debug output"}
bool   save_w2d  {no, prompt="Save calibrated and 2-D rectified wavecal file?"}
bool   verbose   {yes, prompt="Print additional info?"}
string option	 {"linear", prompt="Interpolation option",
			min = "nearest|linear"}
real   angle     {INDEF, prompt = "angle of long slit used with echelle",
			min=-90., max=90.}
real   sh_closed {51126., prompt="internal use only--don't change"}
string Version   {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin
	string	inp, wave
	string	wavestr, rootname, fwv, fwv1, w2d, detector, lamp
	string	opt_elem		# for checking for echelle
	string	wavecal_file		# name of calibrated wavecal file
	bool	echelle			# data were taken with an echelle?
	real	texpstart
	int	pos
	int	nfiles, n		# for looping over input files
	int	ndebug			# number of debug files
	string	inp_n, wave_n, debug_n	# Nth file in list

	if (!deftask ("countfiles"))
	    error (1, "The imgtools package must be loaded.")

	# Get input parameters from par file.

	inp = input
	wave = wavecal

	countfiles (inp)
	nfiles = countfiles.output
	countfiles (wave)
	n = countfiles.output
	if (nfiles != n) {
	    print ("'input' is ", nfiles, " files, but 'wavecal' is ",
			n, " files.\n")
	    error (1, "The lists must be the same length.")
	}
	countfiles (debugfile)
	ndebug = countfiles.output
	if (ndebug > 0 && ndebug != n) {
	    print ("'wavecal' is ", n, " files, but 'debugfile' is ",
			ndebug, " files.\n")
	    error (1, "The lists must be the same length.")
	}

	# Loop over the list of input files.

	for (n = 1;  n <= nfiles;  n = n + 1) {

	    # Pick the nth file from the 'input' and 'wavecal' lists,
	    # and optionally from the 'debugfile' list.
	    pickfile (inp, n)
	    inp_n = pickfile.output
	    pickfile (wave, n)
	    wave_n = pickfile.output
	    if (ndebug > 0) {
		pickfile (debugfile, n)
		debug_n = pickfile.output
	    }

	    # Determine whether an echelle grating was used.
	    keypar (wave_n//"[0]", "OPT_ELEM", silent=yes)
	    opt_elem = keypar.value
	    echelle = (substr (opt_elem, 1, 1) == "E")

	    # Define temporary files.

	    pos = stridx ("_", wave_n)
	    if (pos > 0) {
		rootname = substr (wave_n, 1, pos-1)
	    } else {
		pos = stridx (".", wave_n)
		if (pos > 0) {
		    rootname = substr (wave_n, 1, pos-1)
		} else {
		    error (1, "Can't find '_' or '.' in file name.")
		}
	    }

	    fwv = rootname // "_fwv_tmp.fits"
	    w2d = rootname // "_w2d_tmp.fits"


	    # Run BASIC2D on raw wavecal image.

	    basic2d (wave_n, fwv, outblev="",
		dqicorr="perform", atodcorr="omit", blevcorr="perform",
		doppcorr="omit", lorscorr="omit",
		glincorr="omit", lflgcorr="omit",
		biascorr="perform", darkcorr="perform", flatcorr="perform",
		shadcorr="omit", photcorr="omit",
		statflag=yes, verbose=verbose)

	
	    # Run cs11 if science image is CCD and HITM system only!

	    keypar (fwv//"[0]", "DETECTOR", silent=yes)
	    detector = keypar.value
	    keypar (fwv//"[0]", "SCLAMP", silent=yes)
	    lamp = keypar.value
	    keypar (fwv//"[0]", "TEXPSTRT", silent=yes)
	    texpstart = real (keypar.value)

	    # Subtract science image from wavecal, if appropriate.
	    # The task parameter sh_closed is the date (MJD) after which
	    # the shutter was closed for all wavecals.
	    if (detector == "CCD" &&
		(lamp == "HITM1" || lamp == "HITM2") &&
		texpstart < sh_closed) {

		fwv1 = rootname // "_fwv1_tmp.fits"
		_cs11 (inp_n, fwv, fwv1, verbose=verbose)
		if (access (fwv1)) {
		    delete (fwv, yes, verify=no)
		    rename (fwv1, fwv, field="all")
		} else {
		    print ("Warning:  CalStis11 output file not found.")
		}
	    }

	    if (echelle) {
		wavecal_file = fwv
	    } else {
		wavecal_file = w2d
	    }
	
	    # Run X2D on wavecal image.

	    if (!echelle) {
		x2d (fwv, w2d, helcorr="omit", fluxcorr="omit", statflag=no,
			verbose=verbose)
	    }


	    # Determine MSM offset from wavecal (cs4).

	    if (ndebug > 0) {
		_cs4 (wavecal_file, debugfile=debug_n,
			angle=angle, verbose=verbose)
	    } else {
		_cs4 (wavecal_file, debugfile="",
			angle=angle, verbose=verbose)
	    }

	
	    # Write MSM offset from wavecal to sci header (cs12).

	    _cs12 (inp_n, wavecal_file, verbose=verbose, option=option)


	    # Delete temporary files.

	    if (echelle) {
		# w2d was not created if we have echelle data
		if ( !save_w2d ) {
		    delete (fwv, yes, verify=no)
		}
	    } else {
		delete (fwv, yes, verify=no)
		if ( !save_w2d ) {
		    delete (w2d, yes, verify=no)
		}
	    }
	}

	# exit
	print ("")

end
