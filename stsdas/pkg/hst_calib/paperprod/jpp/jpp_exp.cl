# generate the ACS exposure page according to the mode

procedure jpp_exp (rootname, rootext, igi_output)

string	rootname	{prompt="Rootname of observation"}
string	rootext	    {prompt="Filename extension of observation"}
string	igi_output 	{prompt="Name of the output igi script file"}

begin
	# Declarations
	string	raw     			# Input data images.
	string	root				# root name
    string  prodext             # product extension
	string	script				# igi script.
	string	obstype, detector, obsmode	# keyword values
	
	# Get input parameters
	root = rootname
    prodext = rootext
	script = igi_output

	# construct file name
	raw = root//prodext//"[0]"

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
	    jpp_acq (root, prodext, script)
        
	# Image Plot
	} else if ((substr(obsmode, 1, 5) == "ACCUM") && obstype == "IMAGING") {
	    jpp_accum (root, prodext, script, "CCD")
	} else
	    printf ("Unsupported mode for observation %s\n", root)
end
