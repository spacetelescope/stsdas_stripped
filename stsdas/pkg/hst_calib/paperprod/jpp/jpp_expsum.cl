# write the exposure summary for ACS paper product

procedure jpp_expsum (rootname, rootext, igi_output)

string	rootname	{prompt="Root name of input files"}
string	rootext	    {prompt="Filename extension of observation"}
string	igi_output 	{prompt="Output igi script file name"}

begin
	# Declarations
	string	root		# root name
    string  prodext     # product extension
	string	script		# igi script.
	string	fname		# file name
	string	obstype, detector, obsmode, frame
	string	lrc_xsts, lrc_fail
	int	binaxis1, binaxis2, npix
	int	flen
	real	x1, x2, yoff
	
	# Get interactive parameters
	root = rootname
    prodext = rootext
	script = igi_output

    fname = root//prodext
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
        keypar (fname, "DATE-OBS", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Obs. Date:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "TIME-OBS", silent=yes)
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

	# print IMAGING/PATTERN keywords
	yoff = yoff + 2.
	printf ("justify 7; move %0.4f %0.4f; label 'Detector:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, detector, >> script)

	yoff = yoff + 1.
        keypar (fname, "APERTURE", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Aperture:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
	printf ("justify 7; move %0.4f %0.4f; label 'Observation Mode:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, obsmode, >> script)


	# print IMAGING keywords
	yoff = yoff + 1.
        keypar (fname, "FILTER1", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Filter1:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "FILTER2", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Filter2:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		x2, yoff, keypar.value, >> script)


	# print IMAGING keywords
	yoff = yoff + 1.
        keypar (fname, "EXPTIME", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Exp. Time:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%0.2f'\n", 
			x2, yoff, real(keypar.value), >> script)
	else 
	    printf ("Keyword EXPTIME not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "SUBARRAY", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Frame:'\n", 
		x1, yoff, >> script)
	if (keypar.found){
        if (keypar.value == "no")
            frame = "FULL"
        else
            frame = "SUBARRAY"
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, frame, >> script)
	} else 
	    printf ("Keyword SUBARRAY not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "ORIENTAT", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Image Orientation:'\n", 
		x1, yoff, >> script)
	printf ("justify 9; move %0.4f %0.4f; label '%s degrees'\n", 
		x2, yoff, keypar.value, >> script)

	yoff = yoff + 1.
        keypar (fname, "PATTERN1", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Pattern Name:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%s'\n", 
		    x2, yoff, keypar.value, >> script)
	else 
	    printf ("Keyword PATTERN1 not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "P1_ORINT", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Pattern Orient:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%0.2f'\n", 
		    x2, yoff, real(keypar.value), >> script)
	else 
	    printf ("Keyword P1_ORINT not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "P1_NPTS", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Number of Positions:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    x2, yoff, int(keypar.value), >> script)
	else 
	    printf ("Keyword P1_NPTS not found in %s\n", fname)

	yoff = yoff + 1.
        keypar (fname, "PATTSTEP", silent=yes)
	printf ("justify 7; move %0.4f %0.4f; label 'Position Number:'\n", 
		x1, yoff, >> script)
	if (keypar.found)
	    printf ("justify 9; move %0.4f %0.4f; label '%d'\n", 
		    x2, yoff, int(keypar.value), >> script)
	else 
	    printf ("Keyword PATTSTEP not found in %s\n", fname)
	
end
