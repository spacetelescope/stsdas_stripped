procedure yfluxcal (input)

#
# Modified 12 January 1998 by M. De La Pena: 'Getreffile' changed to a foreign
# task called 'getref' in stlocal.cdbsutil.  'Upreffile' has an additional 
# parameter for adding new header keywords (addkey) and has moved to 
# toolbox.headers.  Removed hidden "cdbspar" pset parameter to 'yfluxcal'.
# Modified 10 November 1999 by M. De La Pena to accommodate changes to the
# 'upreffile' interface.
#

file	input {prompt="List of observations to flux calibrate"}
bool	ais {yes,prompt="Use AIS method to flux calibrate?"}
bool	upref {yes,prompt="Update reference files?"}
bool	calibrate {no,prompt="Execute calfos on observations?"}
bool	specmode {no,prompt="Special processing mode?"}
file	outext {"_flx",prompt="Extension on rootname for recalibrated output"}
bool	verbose {no,prompt="Informational output"}

string	*in_list

begin
	# Declarations
	file	fname
	file	in_list_file
	file	obs_list_file
	file	pinput
	file	raw_list_file
	string 	sx
	string	tmproot
	
	# Get interactive parameters
	pinput = input

	# Make temporary file names.
	tmproot = mktemp ("tmp$Yfluxcal")
	in_list_file = tmproot//"_ilf.txt"
	obs_list_file = tmproot//"_olf.txt"
	raw_list_file = tmproot//"_rlf.txt"
	
	# Check for packages.
	sx = ""
	if (!deftask ("fparse"))
	    sx = sx//"stsdas "
	if (upref) {
	    if (!deftask ("getref"))
		sx = sx//"stlocal.cdbsutil "
	    if (!deftask ("upreffile"))
		sx = sx//"stsdas.toolbox.headers "
	}
	if (!deftask ("ckwfos"))
	    sx = sx//"stsdas.hst_calib.ctools "
	if (!deftask ("putcal"))
	    sx = sx//"stsdas.hst_calib.ctools "
	if (!deftask ("calfos") && calibrate)
	    sx = sx//"fos "
	if (sx != "")
	    error (1, "The following packages need to be loaded: "//sx)
	
	# Get file list
	files (pinput, sort=yes, > in_list_file)

	# Create two lists.  One of just observations names, and one
	# of raw data image names.
	if (verbose) {
	    if (upref)
		print ("yfluxcal: Updating D0H image headers for new reference files.")
	    else
		print ("yfluxcal: Creating list of observations.")
	}
	in_list = in_list_file
	while (fscan (in_list, sx) != EOF) {

	    # Create lists.
	    fparse (sx, verbose=no)
	    fname = fparse.directory//fparse.root
	    print (fname, >> obs_list_file)
	    print (fname//".d0h", >> raw_list_file)

	    # Update reference files.
	    if (upref) {
		if (verbose)
		    print ("yfluxcal:    Updating observation "//fparse.root)
		getref (fparse.root) | 
			    upreffile ("", yes, source="ref,obc,okr", verify=no)
			    #upreffile ("", yes, template="*.d0h", add=yes, 
                            #           verify=no)
	    }
	}

	# Setup the ckwfos pset.
	ckwfos.defddtbl = " "
	ckwfos.bachfile = " "
	ckwfos.fl1hfile = " "
	ckwfos.fl2hfile = " "
	ckwfos.iv1hfile = " "
	ckwfos.iv2hfile = " "
	ckwfos.aishfile = " "
	ckwfos.rethfile = " "
	ckwfos.ddthfile = " "
	ckwfos.dq1hfile = " "
	ckwfos.dq2hfile = " "
	ckwfos.ccg2 = " "
	ckwfos.ccs0 = " "
	ckwfos.ccs1 = " "
	ckwfos.ccs2 = " "
	ckwfos.ccs3 = " "
	ckwfos.ccs4 = " "
	ckwfos.ccs5 = " "
	ckwfos.ccs6 = " "
	ckwfos.ccs7 = " "
	ckwfos.ccs8 = " "
	ckwfos.ccs9 = " "
	ckwfos.ccsa = " "
	ckwfos.ccsb = " "
	ckwfos.ccsc = " "
	ckwfos.ccsd = " "
	ckwfos.cnt_corr = "perform"
	ckwfos.off_corr = "omit"
	ckwfos.ppc_corr = "perform"
	ckwfos.bac_corr = "perform"
	ckwfos.gmf_corr = "perform"
	ckwfos.sct_corr = "perform"
	ckwfos.flt_corr = "perform"
	ckwfos.sky_corr = "perform"
	ckwfos.wav_corr = "perform"
	if (ais) {
	    ckwfos.flx_corr = "omit"
	    ckwfos.apr_corr = "perform"
	    ckwfos.ais_corr = "perform"
	    ckwfos.tim_corr = "perform"
	} else {
	    ckwfos.flx_corr = "perform"
	    ckwfos.apr_corr = "omit"
	    ckwfos.ais_corr = "omit"
	    ckwfos.tim_corr = "omit"
	}
	ckwfos.err_corr = "perform"
	if (specmode)
	    ckwfos.mod_corr = "perform"
	else
            ckwfos.mod_corr = "omit"
	
	# Modify the headers
	if (verbose)
	    print ("yfluxcal: Updating calibration switches for flux calibration.")
	putcal ("@"//raw_list_file, "ckwfos", add=yes, verbose=verbose)

	# Recalibrate the data.
	if (calibrate) {
	    print ("yfluxcal: Starting recalibration with extension "//outext)
	    in_list = obs_list_file
	    while (fscan (in_list, fname) != EOF) {
		if (verbose)
		    printf ("\nyfluxcal: Recalibrating observation "//fname//"\n")
		calfos (fname, fname//outext)
	    }
	}

	# That's all folks.
	delete (tmproot//"*", verify=no, >& "dev$null")
end
#---------------------------------------------------------------------------
# End of yfluxcal
#---------------------------------------------------------------------------
