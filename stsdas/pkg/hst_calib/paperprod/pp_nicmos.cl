# Generate paper product for NICMOS

procedure pp_nicmos (input, device)

char	input	{"", prompt="Input file template of NICMOS obs rootname(s)"}
char	device	{"stdgraph", prompt="Graphics device to send output to"}

string  dirname {"", prompt="data directory name to be printed"}
bool	verbose	{no, prompt="print out debugging messages?"}

begin
	# Declarations
	char	linput		# Input list.
	char	pdevice		# Graphics output device.
	bool	debug

	char	fname		# File name.
	char	igi_list	# List of output scripts.
	char	visit_igi	# visit level igi output.
	char	root		# Rootname of the input files.
	char	tmproot		# Temporary file rootname.
	string	timetag		# time tag
	string	obsmode		# observation mode
	char	script
	int	pg		# page number
	int	nf
	string	str1, str2, str3
	
	# Get interactive parameters.
	linput = input
	pdevice = device
	debug = verbose

	# Create some file names.
	tmproot = mktemp ("tmp$PPN")
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
	pplist (linput, "", visit_igi, "nicmos", pr_parts.output,
		timetag = timetag, page = pg) 
	print (visit_igi, >> igi_list)
	pg = pplist.page
	
	# Create paper products for each observation.
	if (pr_parts.obs) {
	countfiles (linput)
	nf = countfiles.output
	for (i = 1; i <= nf; i = i+1) {
	    pickfile (linput, i)
	    fname = pickfile.output

	    # Retrieve just the pathname without extension.
	    fparse (fname, verbose=no)
	    root = fparse.directory//fparse.root
	    
	    if (debug) print ("working on ", root)

	    # generate the exposure plot
	    script = mktemp (tmproot)
	    npp_exp (root, script, tmproot = script, timetag = timetag, 
			page = pg)
	    pg = npp_exp.page
	    if (access(script)) print (script, >> igi_list)

	    #if (substr(obsmode, 1, 5) == "ACCUM" || obsmode == "TIME-TAG") {

	        # Create the observation summary page.
	        ##script = mktemp (tmproot)
	        ##npp_obsum (root, script)
	        ##print (script, >> igi_list)

	        # Create the calibration status summary page.
	        ##script = mktemp (tmproot)
	        ##npp_calib (root, script)
	        ##print (script, >> igi_list)
	    #}
	}
	}

	# Print it out.
	if (debug) print ("Output the igi product...")
	pp_igi (igi_list, pdevice, metacode="", debug=debug)

	# Delete tmp files
	if (!debug) {
	    delete (tmproot//"*", verify=no, >& "dev$null")
	}
end
