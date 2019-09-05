procedure pp_foc (input, fitstype, device)

char	input	{prompt="File containing list of FOC observations"}
char	fitstype {prompt="File containing list of image types for observations"}
char	device	{prompt="Graphics device to send output to"}

string  dirname {"", prompt="data directory name to be printed"}
bool    verbose {no, prompt="print out debugging messages?"}

struct	*in_list, *type_list

begin
	# Declarations
	char	banner		# Name of the banner file.
	bool	first		# First observation.
	char	fname		# File name.
	char	ftype		# Image Type for fname (FITS/GEIS/...)
	char	pinput		# Input list.
	char	pimtype		# Input image type list.
	char	igi_list	# Output script.
	char	pdevice		# Graphics output device.
	char	visit_igi	# visit igi output.
	char	root		# Rootname of observation.
	char	shh		# SHH image.
	char	tmproot		# Temporary file rootname.
	char	dummy
	char	ss_mode, spec_1
	string	timetag		# time tag
    int     pagenum
	
	bool	debug	

	# Get interactive parameters.
	pinput = input
	pimtype = fitstype
	pdevice = device
	debug = verbose

	# Create some file names.
	tmproot = mktemp ("tmp$PPX")
	banner = tmproot//"_banner"
	igi_list = tmproot//"_igi"
	visit_igi = tmproot//"_visit"

	# get the time tag
        time | scan (line)
        timetag = line
    pagenum = 1

	# Create the general summary.
	if (debug) print ("Creating summary...")
	if (pr_parts.cover) {
	    ppdirbox (dirname, >> visit_igi)
	}
	pplist (pinput, pimtype, visit_igi, "foc",pr_parts.output,
        	timetag=timetag, page=pagenum) 
    pagenum = pplist.page
	print (visit_igi, > igi_list)
	
	# Create paper products for each observation.
	if (pr_parts.obs) {
	  first = yes
	  #in_list = pinput
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

	    if (debug) print ("working on ", root, " with imtype of ", ftype)

	    # Confirm that this observation is a FOC observation.
	    keypar (shh, "instrume", silent=yes)
	    if (!keypar.found || keypar.value != "FOC") {
		printf ("WARNING: %s is not a FOC observation, Skip.\n", shh)
		next
	    }

	    # First time initializations.
	    if (first) {
		first = no
	    }

	    # Create the image display page
	    dummy =  mktemp (tmproot//"_IM")
	    xpp_image (root, dummy, ftype, calprt=no, page=pagenum)
		pagenum = xpp_image.page
	    print (dummy, >> igi_list)

	    # Next create the observation summary page.
	    dummy = mktemp (tmproot//"_OB")
	    xpp_obsum (root, dummy, ftype, page=pagenum)
		pagenum = xpp_obsum.page
	    print (dummy, >> igi_list)

	  }
	}
	# Print it out.
	if (debug) print ("Output the igi product...")
	pp_igi (igi_list, pdevice, metacode="", debug=debug)

	# That's all folks.
	if (!debug) {
		# print ("Deleting tmp files...")
		delete (tmproot//"*", verify=no, >& "dev$null")
		delete ("tmp$PPXtmp*", verify=no, >& "dev$null")
	}
end
