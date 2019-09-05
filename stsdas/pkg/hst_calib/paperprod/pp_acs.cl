# Generate paper product for ACS

procedure pp_acs (input, device)

string	input	{prompt="Input file template of ACS obs rootname(s)"}
string	device	{"stdgraph", prompt="Graphics device to send output to"}

string	dirname {"", prompt="data directory name to be printed"}
bool	verbose	{no, prompt="print out debugging messages?"}
struct  *list   {"", prompt="Internal variable, DO NOT USE"}

begin
	# Declarations
	string	linput		# Input list.
	string	pdevice		# Graphics output device.
	bool	debug
    bool    skip

	string	igi_list	# List of output scripts.
	string	visit_igi	# visit level igi output.
	string	prod_list	# list of unique input files.
	string	root_list	# list of unique input FULL filenames.
	string	root		# Rootname of the input files.
	string	raw		# the raw image.
	string	tmproot		# Temporary file rootname.
	string	timetag		# time tag
	string	obsmode		# observation mode
	string	detector	# detector used
	string	script
	string  jroot
    string  asnprod
    string  subarray
	int	pg		# page number
	int	nf, nc
    int pgobs  # number of pages / observation
	string	str1, str2, str3
	
    string  prodext
    int     i
    
	# Get interactive parameters.
	linput = input
	pdevice = device
	debug = verbose
    skip = no

	# Create some file names.
	tmproot = mktemp ("tmp$PPO")
	igi_list = tmproot//"_igi"
	visit_igi = tmproot//"_visit"
    prod_list = tmproot//"_prod"
    root_list = tmproot//"_root"

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
    
    # Generate the list of unique products and their extensions.
    jpp_prods(linput, prod_list, root_list)

	pplist ("@"//root_list, "", visit_igi, "acs", pr_parts.output, 
		timetag = timetag, page = pg)
    
	print (visit_igi, >> igi_list)
	pg = pplist.page
	
	# Create paper products for each observation.
	if (pr_parts.obs) {
	countfiles ("@"//root_list)
	nf = countfiles.output
        
    list = prod_list
	for (i = 1; i <= nf; i = i+1) {
        pgobs = 0
        
        asnprod = "no"
        
        nc = fscanf (list, "%s %s", root, prodext)
        
        # Retrieve just the pathname without extension.
	    raw = root//prodext
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

		# skip summary pages for internal exposures
	    keypar (raw, "SCLAMP", silent=yes)
	    if (!keypar.found || keypar.value != "NONE") skip=yes
	    keypar (raw, "TARGNAME", silent=yes)
	    if (!keypar.found || keypar.value == "BIAS" || 
		keypar.value == "DARK") skip=yes
        keypar (raw, "ASN_TAB", silent=yes)
        # Now, check to see if there is an ASN table listed
        # and if it exists... If it is listed but doesn't exist, 
        #   treat as a single image.
        if (keypar.value != "" && access(keypar.value)) 
            asnprod = "yes"
        else 
            asnprod = "no"
	    
	    if (debug) print ("working on ", root)

	    # Confirm that this observation is an ACS observation.
	    keypar (raw, "INSTRUME", silent=yes)
	    if (!keypar.found || keypar.value != "ACS") {
		    printf ("WARNING: %s is not a ACS observation, Skip.\n", raw)
		    next
	    }

        # read keywords to determine what plot to generate
        keypar (raw, "DETECTOR", silent=yes)
        detector = keypar.value
        keypar (raw, "SUBARRAY", silent=yes)
        subarray = keypar.value
        
	    script = mktemp (tmproot)

	    # generate the exposure plot
	    pp_banner (script, str1, str2, str3, "ACS", timetag = timetag,page = pg)
	    pg = pp_banner.page

	    jpp_exp (root, prodext, script)
	    jpp_expsum (root, prodext, script)
        # 
        # Compute how many pages this observation will require
        #   Set page number negative so rootname will show, but other
        #   pages are hidden in bookmarks section.
        #
        pgobs = 1
               
        # If this is an ASN product, account for thumbnail page
        if (asnprod == "yes") {
            pgobs = pgobs + 1
        }
	    
        # Account for exposure summary and calibration summary 
        # For WFC, also account for close-up image page
        if (substr(detector, 1, 3) == "WFC" && substr(obsmode,1,3) != "ACQ") {
            if (subarray == "no") {
                pgobs = pgobs + 3
            } else {
                pgobs = pgobs + 2
            }
        } else {
            pgobs = pgobs + 2
        }
        
        
        # For ACQ images, we don't produce a calibration summary
        # For internal images, we don't produce any summaries
        if (substr(obsmode, 1, 3) == "ACQ" || skip) {
            if (skip) pgobs = pgobs - 2
            else pgobs = pgobs - 1
        }
        
        # Safeguard against unusual/unexpected situations
        if (pgobs < 1) pgobs = 1
        pgobs = -pgobs
        
        pp_pdfsection (script, pg, pgobs, id=root)
        pp_pdfbook(script, pg, title="Exposure Plot")

        # generate zoom image around target position from full WFC images
        if (substr(detector, 1, 3) == "WFC" && subarray == "no"){
	        pp_banner (script, str1, str2, str3, "ACS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page            
            pp_pdfbook(script, pg, title="Close-up of Target Position")
            
            jpp_targ (script, root, prodext)        
            jpp_expsum (root, prodext, script)
        
        }
	    
        # generate thumbnail page for input exposures to this product
        if (asnprod == "yes") {
	        pp_banner (script, str1, str2, str3, "ACS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page            
            pp_pdfbook(script, pg, title="Input Exposures to Product")
            
            jpp_thumbs (script, root, prodext)
	    }

	    print (script, >> igi_list)


        if (skip) next 
	    # Create the observation summary page.
	    script = mktemp (tmproot)
	    pp_banner (script, str1, str2, str3, "ACS", timetag = timetag, 
			page = pg)
	    pg = pp_banner.page
        pp_pdfbook(script, pg, title="Observation Summary")

	    jpp_obsum (root, prodext, script)
        jpp_expsum (root, prodext, script)

	    print (script, >> igi_list)

	    if (substr(obsmode, 1, 3) != "ACQ") {
	        # Create the calibration status summary page for all products.
	        script = mktemp (tmproot)
	        pp_banner (script, str1, str2, str3, "ACS", timetag = timetag, 
			    page = pg)
	        pg = pp_banner.page
            pp_pdfbook(script, pg, title="Calibration Status")
            
            jpp_calib (root, prodext, script)
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
