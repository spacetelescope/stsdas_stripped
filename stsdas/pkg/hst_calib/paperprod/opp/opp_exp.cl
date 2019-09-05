# generate the STIS exposure page according to the mode

procedure opp_exp (rootname, igi_output)

string	rootname	{prompt="Rootname of observation"}
string	igi_output 	{prompt="Name of the output igi script file"}
string	fname0		{prompt="File name used"}
string	oned		{prompt="1-D spectrum?"}

begin
	# Declarations
	string	raw, sx1, x1d			# Input data images.
	string	root				# root name
	string	script				# igi script.
	string	obstype, detector, obsmode	# keyword values
	
	# Get input parameters
	root = rootname
	script = igi_output
	oned = "no"

	# construct file names
	raw = root//"_raw.fits[0]"
	sx1 = root//"_sx1.fits"
	x1d = root//"_x1d.fits"

	# read keywords to determine what plot to generate
        keypar (raw, "DETECTOR", silent=yes)
	detector = keypar.value
        keypar (raw, "OBSMODE", silent=yes)
	obsmode = keypar.value
        keypar (raw, "OBSTYPE", silent=yes)
	obstype = keypar.value

	# Draw the plot according to the mode (see STIS ISR 95-014, p.3)
	# ACQ Image Plot
	if (obsmode == "ACQ") {
	    opp_acq (root, script)
	    fname0 = opp_acq.fname0

	# ACQ/PEAK Image Plot
	} else if (obsmode == "ACQ/PEAK") {
	    opp_peakup (root, script)
	    fname0 = opp_peakup.fname0

	# Image Plot
	} else if ((substr(obsmode, 1, 5) == "ACCUM" || obsmode == "TIME-TAG") && obstype == "IMAGING") {
	    opp_accum (root, script, "CCD")
	    fname0 = opp_accum.fname0

	# Spectral Image Plot
	} else if ((substr(obsmode, 1, 5) == "ACCUM" || obsmode == "TIME-TAG") && obstype == "SPECTROSCOPIC") {
	    opp_2dsp (root, script)
	    fname0 = opp_2dsp.fname0

	    # 1-D Extracted Spectrum Plot
	    if (access (sx1) || access (x1d)) {
	        oned = "yes"
	    }
	} else
	    printf ("Unsupported mode for observation %s\n", root)
end
