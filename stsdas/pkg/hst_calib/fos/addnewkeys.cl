# ADDNEWKEYS -- Add new header keywords to FOS raw data (d0h) files to
#		support the new scattered light correction step in
#		CALFOS v1.3.1.
#            -- Add new header keywords to FOS raw data (d0h) files to
#		support the new flux calibration steps in CALFOS v2.0.
#            -- 06 August 1997 MDD: Removed all default calibration filenames.
#            -- 24 November 1997 MDD: Added check for SPECTROPOLARIMETRY mode.
#               This is not supported for AIS correction, skip adding new flux 
#               calibration keywords and values.
#            -- 25 November 1997 MDD: Added checks where appropriate to verify
#               existence of input files.
#

procedure addnewkeys (images, ccs9, ccsa, ccsb, ccsc, ccsd, ais)

string 	images	{prompt="List of images to modify"}
char 	ccs9    {prompt="CCS9 reference file name"}
char 	ccsa    {prompt="CCSA reference file name"}
char 	ccsb    {prompt="CCSB reference file name"}
char 	ccsc    {prompt="CCSC reference file name"}
char 	ccsd    {prompt="CCSD reference file name"}
char	ais	{prompt="AISHFILE name"}
bool	scatter {yes,prompt="Add scattered light parameters?"}
bool	calib	{yes,prompt="Add flux calibration parameters?"}
bool	verbose {yes,prompt="verbose operation?"}
string  Version {"Nov97",prompt = "Date of installation"}

string	*list

begin
        
        file	temp, image, tmpfile
	string	det, fgwa, gndmode
        string  sccs9, sccsa, sccsb, sccsc, sccsd, sais
        string  ftemp
        
        # Check for packages loaded.
        temp = ""
	if (!defpac ("ctools"))
            temp = temp//"ctools "
	if (!defpac ("images"))
	    temp = temp//"images "
        if (!deftask("access")) 
            temp = temp//"language"
        if (strlen (temp) > 0)
            error (1, "addnewkeys: Please load packages: "//temp)
        
	# Construct input image list
	tmpfile = mktemp ("tmp$addlist")
	files (images, sort=no, >tmpfile)
	list = tmpfile

        # Load the input strings.
        sccs9 = ccs9
        sccsa = ccsa
        sccsb = ccsb
        sccsc = ccsc
        sccsd = ccsd
        sais  = ais

	# Main loop over image list
	while( fscan(list, image) != EOF) {

	# Add the scattered light related header keywords
	if (scatter) {

            # Check the input file CCS9 exists.
            fparse (input = sccs9, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input CCS9 file - " // ftemp // " - not found.")

	    hedit (image, "SCT_CORR", "PERFORM", add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "CCS9", sccs9, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)

	    # Add the group parameters
	    groupmod (image, "", "fos$addnewkeys.dat", "add",
	    	      names="c1", types="c2", initial="c3", comments="c4",
		      verbose=verbose)
	}

	# Add the flux calibration related keywords
	if (calib) {

	    # Get the detector and fgwa_id keywords from the image
	    imgets (image, "DETECTOR")
	    det = imgets.value

	    imgets (image, "FGWA_ID")
    	    fgwa = imgets.value

	    if ((det=="AMBER" && fgwa=="H13") ||
		(det=="BLUE"  && fgwa=="H57") ||
		(det=="BLUE"  && fgwa=="H78") ||
		(det=="BLUE"  && fgwa=="L65") ||
		(fgwa=="CAM")) {
		 print ("Unsupported mode "//det//"/"//fgwa//" in image "//image//", skipping.")
		 next
	    }

            # Get the ground mode from the image
            imgets (image, "GRNDMODE")
            gndmode = imgets.value
            if (gndmode=="SPECTROPOLARIMETRY") {
		 print ("SPECTROPOLARIMETRY is not supported for AIS flux correction, skipping.")
		 next
	    }

            # Check the necessary calibration file names have been supplied.
            fparse (input = sais, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input AISHFILE file - " // ftemp // " - not found.")

            fparse (input = sccsa, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input CCSA file - " // ftemp // " - not found.")

            fparse (input = sccsb, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input CCSB file - " // ftemp // " - not found.")

            fparse (input = sccsc, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input CCSC file - " // ftemp // " - not found.")

            fparse (input = sccsd, verbose = no)
            ftemp = fparse.directory // fparse.root // fparse.extension
            if (!access(ftemp))
                error (1, "Input CCSD file - " // ftemp // " - not found.")

	    hedit (image, "APR_CORR", "PERFORM", add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "AIS_CORR", "PERFORM", add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "TIM_CORR", "PERFORM", add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "AISHFILE", sais, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "CCSA", sccsa, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "CCSB", sccsb, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "CCSC", sccsc, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "CCSD", sccsd, add=yes, delete=no,
	           verify=no, show=verbose, update=yes)
	    hedit (image, "FLX_CORR", "OMIT", add=no, delete=no,
	           verify=no, show=verbose, update=yes)

	}
	}

	# Clean up
	list = ""
	delete (tmpfile, verify-, allversions+, >& "dev$null")

end
