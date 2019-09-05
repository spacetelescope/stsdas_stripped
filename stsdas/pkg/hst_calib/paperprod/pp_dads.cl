procedure pp_dads (input, device)

file	input		{"*", prompt="input files"}
string	device		{"stdplot",prompt="Output graphics device"}

string	output_select	{"all",prompt="Output which parts? (one or more of the following:all cover visit obs)"}

bool	print_dir	{no,prompt="Print Directory name? (for STScI use only)"}
bool	verbose		{no,prompt="Print out lengthy processing information?"}
string	version		{"Version 2.33 (May 13, 2003)",prompt="version number, do NOT change its value!"}

begin
	# Declarations
	string	infiles		# input files
	string	list_file	# Master list.
	string	obs_file	# Observations of single prop/instrument.
	string	ldev		# Device parameter.
	string	pp_file, pp1_file # List of observations for a pp command.
	string	imtype_file	# List of image types
	string	prop_file	# File of a single proposal.
	string	tmp		# Temporary file name root.
	string	x_file		# Generic file name.
	string	all_file	# file name of the list of all input files
	string	propid, piname, cover_page, end_page
	string	instrument
	string	dirname
	bool	debug
	string	s1

        # reset clobber to yes
        show clobber | scan(s1)
        if (s1 == "no") set clobber = yes

	# Get interactive parameters
	infiles = input
	ldev = device
	debug = verbose

	pr_parts (output_select)
 	if (!pr_parts.cover && !pr_parts.visit && !pr_parts.obs) {
	    printf (" Invalid output_select value '%s'.\n Use one or more of the following: 'all cover visit obs'.\n", output_select)
	    bye
	}	

	printf ("   === Space Telescope Paper Product, %s ===\n", version)

	# Make temporary file names.
	tmp = mktemp ("tmp$PP")
	list_file = tmp//"_list"
	obs_file = tmp//"_obs"
	pp_file = tmp//"_pp"
	imtype_file = tmp//"_imtype"
	pp1_file = tmp//"_pp1"
	prop_file = tmp//"_prop"
	x_file = tmp//"x"
	all_file = tmp//"_all"
	cover_page = tmp//"_cover"
	end_page = tmp//"_end"
	
	# if input is a file template, take it
	if (substr (infiles, 1, 1) == "@")
	    all_file = substr (infiles, 2, strlen(infiles))
	else
	    files (infiles, sort=yes, > all_file)

	# collect the root names and construct a table of unique root names
	# and corresponding attributes (proposal number, instrument, PI name, 
	# line number, and imtype) 
	pp_roots (all_file, list_file)
	if (substr (infiles, 1, 1) != "@")
	    delete (all_file, verify=no, >& "dev$null")

	# make sure there is at least one input file
	if (!access(list_file)) {
	    print ("No usable input files, exit.")
	    return
	} else {
	    tinfo (list_file, >& "dev$null")
	    if (tinfo.nrows <= 0) {
	    	print ("No usable input files, exit.")
	    	return
	    }
	}

	# sort according to the proposal number
	tsort (list_file, "c1", ascend=yes, casesens=no)

	# find the directory name
	if (print_dir) {
	    path | scan (s1)
	    autopi (s1)
	    dirname = autopi.subdirname
	} else 
	    dirname = ""

	# Loop through all the proposals.
	if (debug)
	    printf ("pp: Starting proposal loop.\n")
	tinfo (list_file, ttout=no)
	while (tinfo.nrows > 0) {
	    tabpar (list_file, "c1", 1)
	    propid = tabpar.value
            tabpar (list_file, "c4", 1)
            piname = tabpar.value
	    if (debug)
		printf ("pp:   Working on proposal '%s'.\n", propid)

	    delete (prop_file//","//x_file, verify=no, >& "dev$null")
	    tselect (list_file, prop_file, "c1=="//propid)
	    tselect (list_file, x_file, "c1!="//propid)
	    delete (list_file, verify=no, >& "dev$null")
	    rename (x_file, list_file, field="all", >& "dev$null")
	    
	    # sort according to the instrument
	    tsort (prop_file, "c2", ascend=yes, casesens=no)
	    tinfo (prop_file, ttout=no)

	    # Loop through all the instruments.
	    while (tinfo.nrows > 0) {
		tabpar (prop_file, "c2", 1)
		instrument = tabpar.value
		if (debug)
		    printf ("pp:     Working on instrument '%s'\n", instrument)
	    
		delete (obs_file//","//x_file, verify=no, >& "dev$null")
		tselect (prop_file, obs_file, "c2==\""//instrument//"\"")
		tselect (prop_file, x_file, "c2!=\""//instrument//"\"")
		delete (prop_file, verify=no, >& "dev$null")
		rename (x_file, prop_file, field="all", >& "dev$null")

		# Get just the list of file names.
		delete (pp_file, verify=no, >& "dev$null")
		delete (imtype_file, verify=no, >& "dev$null")

		# sort according to the line number
	        tsort (obs_file, "c5", ascend=yes, casesens=no)
		tproject (obs_file, pp_file, "c3", uniq=no)
		tproject (obs_file, imtype_file, "c6", uniq=no)

		# Generate paper products for this proposal/instrument.
		#
	    	# print cover page for first generation instruments
		if (instrument != "STIS" && instrument != "NICMOS" && instrument != "ACS") {
		    if (instrument != "FOC" && instrument != "WFPC2") {
			if (pr_parts.cover) {
            	            ppdirbox (dirname, >> cover_page)
	    	            ppcover (propid, piname, >> cover_page)
            	            igi (initcmd="", wlpars="", usewcs=no, wcspars="", 
			        device=ldev, append=no, debug=no, cursor="", 
			        < cover_page)
			}
		    }
		}

		# === FOS ===
		if (instrument == "FOS") {
		    if (pr_parts.obs || pr_parts.visit) {
		    	pp_fos ("@"//pp_file, "@"//imtype_file, ldev, 
				verbose = debug)
		    }
		    if (pr_parts.cover) {
	    	        ppend (propid, piname, > end_page)
            	        igi (initcmd="", wlpars="", usewcs=no, wcspars="", 
			    device=ldev, append=no, debug=no, cursor="", 
			    < end_page)
		    }

		# === GHRS ===
		} else if (instrument == "HRS" || instrument == "GHRS") {
		    if (pr_parts.obs || pr_parts.visit) {
		    	pp_ghrs ("@"//pp_file, "@"//imtype_file, ldev, 
				verbose = debug)
		    }

                
		# === WFPC2 ===
		} else if (instrument == "WFPC2") {
		    pp_wfpc2 ("@"//pp_file, "@"//imtype_file, ldev, 
				dirname = dirname, verbose = debug)

		# === FOC ===
		} else if (instrument == "FOC") {
		    pp_foc ("@"//pp_file, "@"//imtype_file, ldev, 
				dirname = dirname, verbose = debug)
		# === STIS ===
		} else if (instrument == "STIS") {
		    pp_stis ("@"//pp_file, ldev, dirname = dirname,
				verbose = debug)

		# === NICMOS ===
		} else if (instrument == "NICMOS") {
		    pp_nicmos ("@"//pp_file, ldev, dirname = dirname,
				verbose = debug)

		# === ACS ===
		} else if (instrument == "ACS") {
		    pp_acs ("@"//pp_file, ldev, dirname = dirname,
				verbose = debug)

		} else
		    printf ("pp:     *** Instrument '%s' not yet supported.\n",
			    instrument)

		gflush

		# Get the next instrument
		tinfo (prop_file, ttout=no)
	    }
	    delete (cover_page, verify=no, >& "dev$null")
	    delete (end_page, verify=no, >& "dev$null")

	    # Get the next proposal.
	    tinfo (list_file, ttout=no)
	}

	if (!debug) 
	    delete (tmp//"*", verify=no, >& "dev$null")
end
