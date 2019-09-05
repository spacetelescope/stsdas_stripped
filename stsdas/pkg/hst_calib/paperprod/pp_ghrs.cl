procedure pp_ghrs (input, fitstype, device)

char	input	{prompt="File containing list of GHRS observations"}
char    fitstype {prompt="File containing list of image types for observations"}
char	device	{prompt="Graphics device to send output to"}

bool	verbose

struct	*in_list, *type_list

begin
	# Declarations
	char	pinput		# Input list.
	char	pdevice		# Graphics output device.
	bool	debug   

	char	banner		# Name of the banner file.
	bool	first		# First observation.
	char	fname		# File name.
	char	igi_list	# Output script.
	char	visit_igi	# visit igi output.
	char	root		# Rootname of observation.
	char	shh		# SHH image.
	char	tmproot		# Temporary file rootname.
	char	tmp
	char	script
	char	ftype, pimtype	
	
	# Get interactive parameters.
	pinput = input
	pdevice = device
	debug = verbose
	pimtype = fitstype

	# Create some file names.
	tmproot = mktemp ("tmp$PPZ")
	banner = tmproot//"_banner.igi"
	igi_list = tmproot//"_igi.txt"
	visit_igi = tmproot//"_visit.igi"

	# Create the general summary.
	if (debug) print ("Creating summary...")
	if (pr_parts.visit) {
	    pplist (pinput, pimtype, visit_igi, "ghrs", 
			pr_parts.output, timetag="", page=0)
	    print (visit_igi, > igi_list)
	}	
	# Create paper products for each observation.
	if (pr_parts.obs) {
	  first = yes
	  in_list = substr (pinput, 2, strlen(pinput))
          type_list = substr(pimtype, 2, strlen(pimtype))
	  while (fscan (in_list, fname) != EOF && fscan (type_list,ftype) != EOF) {

	    # Retrieve just the pathname without extension.
	    fparse (fname, verbose=no)
	    root = fparse.directory//fparse.root
	    if (ftype == "geis") {
		shh = root//".shh"
	    } else {
		shh = root//"_shf.fits[0]"    
	    }

	    if (debug) print ("working on ", root, " of imtype ", ftype)

	    # Confirm that this observation is an GHRS observation.
	    keypar (shh, "instrume", silent=yes)
	    if (!keypar.found || keypar.value != "HRS") {
		printf ("WARNING: %s is not a GHRS observation, Skip.\n", shh)
		next
	    }

	    # First time initializations.
	    if (first) {
		first = no
	    }
	    tmp = mktemp ("z")
	    script = tmproot//tmp//".igi"

	    zpp (root, ftype, script, tmproot, verbose=no)

	    # skip those skipped in zpp (e.g. defcal observations)
	    if (access(script)) print (script, >> igi_list)
	  }
	}
	# Print it out.
	if (debug) print ("Output the igi product...")
	if (access (igi_list)) {
		pp_igi (igi_list, pdevice, metacode="", debug=debug)
	} else {
		if (debug) print ("No data in VISIT or OBS sections...\n")
	}

	# That's all folks.
	if (!debug) { 
		# print ("Deleting tmp files...")
        	delete (tmproot//"*", verify=no, >& "dev$null")
	}
end
