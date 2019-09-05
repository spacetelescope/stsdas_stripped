# Generate paper product for STIS

procedure pp_stis (input, device)

string	input	{prompt="Input file template of STIS obs rootname(s)"}
string	device	{"stdgraph", prompt="Graphics device to send output to"}

string	dirname {"", prompt="data directory name to be printed"}
bool	verbose	{no, prompt="print out debugging messages?"}

begin
	# Declarations
	string	linput		# Input list.
	string	pdevice		# Graphics output device.
	bool	debug

	string	fname		# File name.
	string	fname0		# 
	string	igi_list	# List of output scripts.
	string	visit_igi	# visit level igi output.
	string	root		# Rootname of the input files.
	string	raw		# the raw image.
	string	tmproot		# Temporary file rootname.
	string	timetag		# time tag
	string	obsmode		# observation mode
	string	detector	# detector used
	string	lrc_xsts	# lrc flag
	string	script
	string	jitter, jroot
	int	pg		# page number
	int	nf
    int pgobs  # number of pages / observation
	string	str1, str2, str3
	
	# Get interactive parameters.
	linput = input
	pdevice = device
	debug = verbose

	# Create some file names.
	tmproot = mktemp ("tmp$PPO")
	igi_list = tmproot//"_igi"
	visit_igi = tmproot//"_visit"

	# get the time tag
	time | scan (line)
	timetag = line
	pg = 0

	# Create the general summary.
	if (debug) print ("Creating summary...")

	# generate the visit level summary
	if (pr_parts.cover) {
	    ppdirbox (dirname, >> visit_igi)
	}
	pplist (linput, "", visit_igi, "stis", pr_parts.output, 
		timetag = timetag, page = pg)
	print (visit_igi, >> igi_list)
	pg = pplist.page
	
	# Create paper products for each observation.
	if (pr_parts.obs) {
	countfiles (linput)
	nf = countfiles.output
	for (i = 1; i <= nf; i = i+1) {
        pgobs = 0
	    pickfile (linput, i)
	    fname = pickfile.output

	    # Retrieve just the pathname without extension.
	    fparse (fname, verbose=no)
	    root = fparse.directory//fparse.root
	    raw = root//"_raw.fits"
	    if (!access (raw)) {
		printf ("No science file(s) for %s, no exposure page is generated.\n", root)
		next
	    }
	    raw = raw//"[0]"
	    
	    # construct the texts in the banner
	    keypar (raw, "LINENUM", silent=yes)
	    str1 = "Visit-Exp#: "//keypar.value
	    keypar (raw, "ROOTNAME", silent=yes)
	    str2 = "Observation: "//keypar.value
	    keypar (raw, "PROPOSID", silent=yes)
	    str3 = "Proposal: "//keypar.value
	    keypar (raw, "OBSMODE", silent=yes)
	    obsmode = keypar.value
	    
	    if (debug) print ("working on ", root)

	    # Confirm that this observation is an STIS observation.
	    keypar (raw, "INSTRUME", silent=yes)
	    if (!keypar.found || keypar.value != "STIS") {
		printf ("WARNING: %s is not a STIS observation, Skip.\n", raw)
		next
	    }

            # read keywords to determine what plot to generate
            keypar (raw, "DETECTOR", silent=yes)
            detector = keypar.value
            keypar (raw, "LRC_XSTS", silent=yes)
            lrc_xsts = keypar.value

	    script = mktemp (tmproot)

            # Local Rate Check Image Plot
	    # Skip the LRC plot JCH 3/9/98, OPR 36487
            #if ((detector == "FUV-MAMA" || detector == "NUV-MAMA") &&
                  #lrc_xsts == "yes") {
	        #pp_banner (script, str1, str2, str3, "STIS", timetag = timetag,
			    #page = pg)
	        #pg = pp_banner.page
                #opp_accum (root, script, "MAMA")
	        #opp_expsum (root, opp_accum.fname0, script)
	    #}

	    # generate the exposure plot
	    pp_banner (script, str1, str2, str3, "STIS", timetag = timetag, 
			page = pg)
	    pg = pp_banner.page
	    opp_exp (root, script)
	    fname0 = opp_exp.fname0
	    opp_expsum (root, opp_exp.fname0, script)
        # 
        # Compute how many pages this observation will require
        #   Set page number negative so rootname will show, but other
        #   pages are hidden in bookmarks section.
        #
        pgobs = 1
        if (opp_exp.oned == "yes") {
            pgobs = pgobs + 1
            pgobs = -pgobs
        }
	    if (substr(obsmode, 1, 5) == "ACCUM" || obsmode == "TIME-TAG") {
            pgobs = pgobs + 2
            pgobs = -pgobs
        }
                
        pp_pdfsection (script, pg, pgobs, id=root)
        pp_pdfbook(script, pg, title="Exposure Plot")
	    
        # generate 1-D spectrum plot if any
            if (opp_exp.oned == "yes") {
	        pp_banner (script, str1, str2, str3, "STIS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page
            pp_pdfbook(script, pg, title="1-D Spectrum Plot")

                opp_1dsp (root, script)
	        fname0 = opp_1dsp.fname0
	        opp_expsum (root, opp_1dsp.fname0, script)
	    }

	    print (script, >> igi_list)

	    if (substr(obsmode, 1, 5) == "ACCUM" || obsmode == "TIME-TAG") {

		# skip summary pages for internal exposures
	    	keypar (raw, "SCLAMP", silent=yes)
	    	if (!keypar.found || keypar.value != "NONE") next
	    	keypar (raw, "TARGNAME", silent=yes)
	    	if (!keypar.found || keypar.value == "BIAS" || 
			keypar.value == "DARK") next

	        # Create the observation summary page.
	        script = mktemp (tmproot)
	        pp_banner (script, str1, str2, str3, "STIS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page
            pp_pdfbook(script, pg, title="Observation Summary")
	        
            opp_expsum (root, opp_exp.fname0, script)            
	        opp_obsum (root, opp_exp.fname0, script)

		# plot the jitter grey scale image 
		# (take out of opp_obsum 6/25/98)
		jitter = opp_obsum.jitter
		fparse (jitter, verbose = no)
		jroot = fparse.directory//fparse.root//fparse.extension
	        if (access (jroot)) {
		    opp_jitter (jitter, script, 0.7, 0.95, 0.15, 0.5)
		}
		
	        print (script, >> igi_list)

	        # Create the calibration status summary page.
	        script = mktemp (tmproot)
	        pp_banner (script, str1, str2, str3, "STIS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page
            pp_pdfbook(script, pg, title="Calibration Status")
	        
            opp_calib (root, fname0, script)
	        print (script, >> igi_list)
	    }
	} # end of loop over i
	}

	# Print it out.
	if (debug) print ("Output the igi product...")
	pp_igi (igi_list, pdevice, metacode="", debug=debug)

	# Delete tmp files
	if (!debug) {
	    delete (tmproot//"*", verify=no, >& "dev$null")
	    delete ("tmp$PPOhist*", verify=no, >& "dev$null")
	}
end
