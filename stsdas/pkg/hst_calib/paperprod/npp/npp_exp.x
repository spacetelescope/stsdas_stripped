# generate the NICMOS exposure page according to the mode

include	"npp.h"

procedure npp_exp ()

char	root[SZ_FNAME]		# root name
char	script[SZ_FNAME]	# output igi script file name
char	tmproot[SZ_LINE]	# temporary file root name
char	timetag[SZ_LINE]
int	page

char	fname[SZ_FNAME] 	# input image name
char	obsmode[SZ_LINE]	# observation mode
char	pep_expo[SZ_LINE]	
char	rootname[SZ_LINE]	
char	proposid[SZ_LINE]	
char	bstr[SZ_LINE,4]		# strings for the banner
char	dummy[SZ_LINE]	
pointer	im			# input image pointer
int     fd                      # output file pointer
int	file			# which kid of file to be used

char    pdftitle[SZ_LINE]
int     pgsect
	
pointer immap()
int     open()
int	access()
int	clgeti()
bool	streq()

begin

	# Get input parameters
        call clgstr ("input", root, SZ_LINE)
        call clgstr ("igi_output", script, SZ_LINE)
        call clgstr ("timetag", timetag, SZ_LINE)
        call clgstr ("tmproot", tmproot, SZ_LINE)
	page = clgeti ("page")

	# determine which file to use
	call sprintf (dummy, SZ_FNAME, "%s_raw.fits[sci,1]")
	    call pargstr (root)
        iferr (im = immap (dummy, READ_ONLY, 0)) {
	    call sprintf (dummy, SZ_FNAME, "%s_mos.fits[sci,1]")
	        call pargstr (root)
            iferr (im = immap (dummy, READ_ONLY, 0)) {
                file = SKIP
            } else {
		file = MOS
		call strcpy (dummy, fname, SZ_FNAME)

                # only do the target mos file (the one having an _asn file)
	        call sprintf (dummy, SZ_FNAME, "%s_asn.fits")
	            call pargstr (root)
                if (access (dummy, 0, 0) == NO) file = SKIP
            }
        } else {
	    file = RAW
	    call strcpy (dummy, fname, SZ_FNAME)

        # if the raw file belongs to an association, skip it
            call imgstr (im, "ASN_TAB", dummy, SZ_LINE)
            if (dummy[1] != EOS) {
                call imunmap (im)
                file = SKIP
            }
        }

	# use cal file if it exists
	if (file == RAW) {
            call sprintf (dummy, SZ_FNAME, "%s_cal.fits")
                call pargstr (root)
	    if (access (dummy, 0, 0) == YES) {
		call imunmap (im)
		call strcat ("[sci,1]", dummy, SZ_FNAME)
		im = immap (dummy, READ_ONLY, 0)
		file = CAL
	        call strcpy (dummy, fname, SZ_FNAME)
	    }
	}

	if (file != SKIP) {
        # Create section heading here for bookmarks
        # If this is a MOS image, we can't tell ahead of time
        #   how many pages it will require, so we do not encapsulate
        #   the headings for that image in a section (set pgsect = 0)
        if (file != MOS)  
            pgsect = -3
        else 
            pgsect = 0

            # open the output file
            fd = open (script, APPEND, TEXT_FILE)
    
	    # read keywords 
            call imgstr (im, "LINENUM", pep_expo, SZ_LINE)
            call imgstr (im, "ROOTNAME", rootname, SZ_LINE)
            call imgstr (im, "PROPOSID", proposid, SZ_LINE)
            call imgstr (im, "OBSMODE", obsmode, SZ_LINE)

	    # generate the banner
	    call sprintf (bstr[1,1], SZ_LINE, "Visit-Exp#: %s")
		call pargstr (pep_expo)
	    call sprintf (bstr[1,2], SZ_LINE, "Observation: %s")
		call pargstr (rootname)
	    call sprintf (bstr[1,3], SZ_LINE, "Proposal: %s")
		call pargstr (proposid)
	    call sprintf (bstr[1,4], SZ_LINE, "NICMOS")

	    # Draw the plot according to the mode 
	    # -----------------------------------
	    # ACQ Image Plot
	    if (streq (obsmode, "ACQ")) {
	    	call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], bstr[1,4],
				timetag, page)
        if (pgsect != 0)
            call pp_pdfsection (fd, page, pgsect, rootname)

            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            if (pgsect != 0) {
                call sprintf(pdftitle, SZ_LINE, "ACQ Image")
            } else {
                call sprintf(pdftitle, SZ_LINE, "%s ACQ Image")
                    call pargstr(root)
            }
            call pp_pdfbook(fd, page, pdftitle)

	        call npp_acq (im, fname, root, file, fd)
	    } else {
	    	call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], 
					bstr[1,4], timetag, page)

            if (pgsect != 0)
                call pp_pdfsection (fd, page, pgsect, rootname)
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            if (pgsect != 0) {
                call sprintf(pdftitle, SZ_LINE, "Image")
            } else {
                call sprintf(pdftitle, SZ_LINE, "%s Image")
                    call pargstr(root)
            }
            call pp_pdfbook(fd, page, pdftitle)

	        call npp_image (im, fname, file, fd)

		# MOS image
		if (file == MOS)
		    call npp_mos (im, fname, root, fd, tmproot, bstr, 
					timetag, page)
	    }

	    # generate the observation summary page
	    call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], 
					bstr[1,4], timetag, page)
        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        if (pgsect != 0) {
            call sprintf(pdftitle, SZ_LINE, "Observation Summary")
        } else {
            call sprintf(pdftitle, SZ_LINE, "%s Observation Summary")
                call pargstr(root)
        }
        call pp_pdfbook(fd, page, pdftitle)

	    call npp_expsum (im, fd)
	    call npp_obsum (root, fd)

	    # generate the calibration summary page
	    call pp_banner (fd, bstr[1,1], bstr[1,2], bstr[1,3], 
					bstr[1,4], timetag, page)

        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        if (pgsect != 0) {
            call sprintf(pdftitle, SZ_LINE, "Calibration Summary")
        } else {
            call sprintf(pdftitle, SZ_LINE, "%s Calibration Summary")
                call pargstr(root)
        }
        call pp_pdfbook(fd, page, pdftitle)

	    call npp_calib (root, file, fd)

	    # write the page number back to the parameter
	    call clputi ("page", page)

	    # close the input image and output file
            call imunmap (im)
            call close (fd)
	} else 

	    # need to write the page number back to keep track 4/28/97
	    call clputi ("page", page)
end
