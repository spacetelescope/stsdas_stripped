procedure pp_fos (input, fitstype, device)

char	input	{prompt="File containing list of FOS observations"}
char    fitstype {prompt="File containing list of image types for observations"}
char	device	{prompt="Graphics device to send output to"}

bool    verbose {no, prompt="print out debugging messages?"}

struct	*in_list, *type_list

begin
	# Declarations
	char	pinput		# Input list.
	char	pdevice		# Graphics output device.
	bool	debug
        char    pimtype         # Input image type list.

	char	banner		# Name of the banner file.
	char	fname		# File name.
	char	igi_list	# Output script.
	char	visit_igi	# visit igi output.
	char	root		# Rootname of the input file
	char	shh		# SHH image.
	char	tmproot		# Temporary file rootname.
	char	dummy
	char	ss_mode, spec_1
	char	ftype
	
	# Get interactive parameters.
	pinput = input
	pdevice = device
	debug = verbose
        pimtype = fitstype

	# Create some file names.
	tmproot = mktemp ("tmp$PPY")
	banner = tmproot//"_banner"
	igi_list = tmproot//"_igi"
	visit_igi = tmproot//"_visit"

	# Create the general summary.
	if (debug) 
	    print ("Creating summary...")
	if (pr_parts.visit) {
	    pplist (pinput, pimtype, visit_igi, "fos", pr_parts.output, 
		timetag="", page=0) 
	    print (visit_igi, > igi_list)
	}

	# Create paper products for each observation.
	if (pr_parts.obs) {
	    in_list = substr (pinput, 2, strlen(pinput))
            type_list = substr(pimtype, 2, strlen(pimtype))
	    while (fscan (in_list, fname) != EOF && 
		   fscan (type_list,ftype) != EOF) {

	        # Retrieve just the pathname without extension.
	        fparse (fname, verbose=no)
	        root = fparse.directory//fparse.root
	        shh = root//".shh"

	        if (ftype == "geis")
                    shh = root//".shh"
                else
                    shh = root//"_shf.fits[0]"
 
                if (debug) 
		    print ("working on ", root, " with imtype of ", ftype)

	        # Confirm that this observation is an FOS observation.
	        keypar (shh, "instrume", silent=yes)
	        if (!keypar.found || keypar.value != "FOS") {
		    printf ("WARNING: %s is not a FOS observation, Skip.\n", 
				shh)
		    next
	        }

	        # The next set of pages are mode-dependent.
	        keypar (shh, "ss_mode", silent=yes)
	        ss_mode = keypar.value
	        keypar (shh, "spec_1", silent=yes)
	        spec_1 = keypar.value
	        keypar (shh, "opmode", silent=yes)
	        
	        if (ss_mode != "SPECTROPOLARIMETRY") {
	            if (keypar.value == "ACQ/BINARY") {
		        ypacqbin (root, tmproot, igi_list, ftype)
	            } else if (keypar.value == "ACCUM") {
		        ypaccrapid (root, tmproot, igi_list, "ACCUM", ftype)
	            } else if (keypar.value == "RAPID") {
		        ypaccrapid (root, tmproot, igi_list, "RAPID", ftype)
	            } else if (keypar.value == "ACQ/PEAK") {
		        ypacqpeak (root, tmproot, igi_list, ftype)
	            } else if (keypar.value == "ACQ") {
		        dummy = mktemp (tmproot//"IM")
		        ypp_image (root, dummy, ftype)
		        print (dummy, >> igi_list)
	            } else if (keypar.value == "IMAGE" && spec_1 == "MIRROR") {
		        dummy = mktemp (tmproot//"IM")
		        ypp_image (root, dummy, ftype)
		        print (dummy, >> igi_list)
	            } else if (keypar.value == "IMAGE" && spec_1 != "MIRROR") {
		        dummy = mktemp (tmproot//"IM")
		        ypp_imdsp (root, dummy, ftype)
		        print (dummy, >> igi_list)
	            } else {
		        printf ("pp_fos: OPMODE %s is unsupported.\n", 
				keypar.value)
	            }
	        } else 
		    yppolar (root, tmproot, igi_list, ftype)

	        # Next create the observation summary page.
	        dummy = mktemp (tmproot//"OB")
	        ypp_obsum (root, dummy, ftype)
	        print (dummy, >> igi_list)

	        # Create the calibration flags page.
	        dummy = mktemp (tmproot//"CAL")
	        ypp_calib (root, dummy, ftype)
	        print (dummy, >> igi_list)
	    }
	}

	# Print it out.
	if (debug) print ("Output the igi product...")
	if (access(igi_list))
	    pp_igi (igi_list, pdevice, metacode="", debug=debug)
	else
	    if (debug) print ("No data in VISIT or OBS sections...\n")
 
	# Delete tmp files
	if (!debug)
	    delete (tmproot//"*", verify=no, >& "dev$null")
end
