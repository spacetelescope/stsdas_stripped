# Generate the Observation Summary Page for STIS 

include	"jpp.h"

procedure jpp_obsum ()

char	root[SZ_FNAME]		# Rootname of input files
char	prodext[SZ_FNAME]  	# Science file
char	script[SZ_FNAME]	# Name of the output igi script file

char    prodname[SZ_FNAME]  # file name of product exposure
char	jih[SZ_FNAME]	# jitter file.
char	jih0[SZ_FNAME]	# jitter file primary extension.
char	trl[SZ_FNAME]	# trailer file.
char    pipeline[SZ_LINE, MAX_SUMMARY]  # pipeline processing comments
char    caldq[SZ_LINE, MAX_SUMMARY]     # calibration processing comments
char	text[SZ_LINE]
char	str[SZ_LINE]
pointer im, ip, ip0
int     fd                      # the output igi script file pointer
real	x, y
int     i, len
int	ival
real	rval
bool	bval

pointer immap()
int	access()
int     open()
bool	imgetb()
int	imgeti()
real	imgetr()
int	strlen()

begin

	# get input parameters
        call clgstr ("root", root, SZ_FNAME)
        call clgstr ("prodext", prodext, SZ_FNAME)
        call clgstr ("igi_output", script, SZ_FNAME)

	# Create file names.
    call strcpy (root, prodname, SZ_FNAME)
    call strcat (prodext, prodname, SZ_FNAME)
    call strcat ("[0]", prodname, SZ_FNAME)
    
	call strcpy (root, jih, SZ_FNAME)
	call strcpy (root, trl, SZ_FNAME)
        len = strlen (root)
        if (root[len] < '0' || root[len] > '9')
	    jih[len] = 'j'
        call strcat ("_jif.fits", jih, SZ_FNAME)
        call strcat ("_trl.fits", trl, SZ_FNAME)
 
        # open the input image
        im = immap (prodname, READ_ONLY, 0)

        # open the output file
        fd = open (script, APPEND, TEXT_FILE)

	# HST Spacecraft Performance Summary.
	call fprintf (fd, "fontset hard\n")
        call fprintf (fd, "location 0 1 0 1\n")
	call fprintf (fd, "expand .8; vpage 0.05 .65 .4 .9\n")
	call fprintf (fd, "limits 0 100 20 0\n")

	call pp_move (fd, 0., 20.)
	call pp_draw (fd, 98., 20.)
	call fprintf (fd, "justify 3\n")
	call pp_label (fd, 0., 1., "\\fBHST Spacecraft Performance Summary")

	# generate the jitter box
	if (access(jih, 0, 0) == YES) {
	    call strcpy (jih, jih0, SZ_FNAME)
	    call strcat ("[0]", jih0, SZ_FNAME)
            ip0 = immap (jih0, READ_ONLY, 0)

	    # access the proper extension according to the file's pedigree
	    call pp_oms (jih)
            ip = immap (jih, READ_ONLY, 0)

	    # print summary text
	    # updated the list 5/1/97 JC Hsu, re OPR 33671
	    y = 2
	    ifnoerr (bval = imgetb (ip0, "T_ACQ2FL")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, "Target acquisition failure.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_GSFAIL")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, "Guide star acquisition failure.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_TAPDRP")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Possible loss of science data due to ETR problem.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_SLEWNG")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Slewing occurred during this observation.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_TDFDWN")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Take data flag NOT on throughout observation.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_SISAFE")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Science instrument safed during observation.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_FGSFAL")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"FGS astrometry target acquisition failed.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_NO_EPH")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, "No JPL ephemeris available.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_NO_SAM")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"SI requested small angle maneuver rejected.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_NOSLEW")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"NSSC-1 executive refused to honor slew request.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_SDLOST")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Science data may be lost due to SI error.")
		}
	    }
	    ifnoerr (bval = imgetb (ip0, "T_SISPND")) {
	        if (bval) {
		    y = y + 1
		    call pp_label (fd, 1., y, 
			"Science instrument suspended during observation.")
		}
	    }

	    # print jitter data
	    call fprintf (fd, "justify 3; expand 0.6\n")
	    x = 35
	    y = 12
	    iferr (ival = imgeti (ip, "NRECENT"))
	        call sprintf (text, SZ_LINE, "# Recenterings: N/A")
	    else {
	        call sprintf (text, SZ_LINE, "# Recenterings: %d")
		    call pargi (ival)
	    }
	    call pp_label (fd, x, y, text)

	    y = y + 1
	    iferr (ival = imgeti (ip, "NLOSSES"))
	        call sprintf (text, SZ_LINE, "# Losses of Lock: N/A")
	    else {
	        call sprintf (text, SZ_LINE, "# Losses of Lock: %d")
		    call pargi (ival)
	    }
	    call pp_label (fd, x, y, text)

	    y = y + 1
	    iferr (rval = imgetr (ip, "V2_RMS"))
	        call sprintf (text, SZ_LINE, "V2 Jitter (RMS): N/A")
	    else {
	        call sprintf (text, SZ_LINE, "V2 Jitter (RMS): %0.1f (mas)")
		    call pargr (rval)
	    }
	    call pp_label (fd, x, y, text)

	    y = y + 1
	    iferr (rval = imgetr (ip, "V3_RMS"))
	        call sprintf (text, SZ_LINE, "V3 Jitter (RMS): N/A")
	    else {
	        call sprintf (text, SZ_LINE, "V3 Jitter (RMS): %0.1f (mas)")
		    call pargr (rval)
	    }
	    call pp_label (fd, x, y, text)

	    y = y + 1
	    iferr (rval = imgetr (ip, "V2_P2P"))
	        call sprintf (text, SZ_LINE, "V2 Jitter (PP): N/A")
	    else {
	        call sprintf (text, SZ_LINE, "V2 Jitter (PP): %0.1f (mas)")
		    call pargr (rval)
	    }
	    call pp_label (fd, x, y, text)

	    y = y + 1
	    iferr (rval = imgetr (ip, "V3_P2P"))
	        call sprintf (text, SZ_LINE, "V3 Jitter (PP): N/A")
	    else {
	        call sprintf (text, SZ_LINE, "V3 Jitter (PP): %0.1f (mas)")
		    call pargr (rval)
	    }
	    call pp_label (fd, x, y, text)

	    # plot the gray-scale jitter image
	    #jpp_jitter (jih, script, 0.7, 0.95, 0.15, 0.5)
	} else {
	    call pp_label (fd, 50., 14., "(Jitter file not available)") 
	}

	# dredge up the summaries from the trailer file
	if (access (trl, 0, 0) == YES)
	    call opp_trlsum (trl, pipeline, caldq)

	# Pipeline Processing Summary.
	call fprintf (fd, "location 0 1 0 1\n")
	call fprintf (fd, "expand .8; vpage 0.05 .65 .2 .4\n")
	call fprintf (fd, "limits 0 100 12 0\n")
        call pp_move (fd, 0., 12.)
        call pp_draw (fd, 98., 12.)
	call fprintf (fd, "justify 3\n")
	call pp_label (fd, 0., 1., "\\fBPipeline Processing Summary")
	call fprintf (fd, "expand .65\n")

	y = 3.
        iferr (call imgstr (im, "CAL_VER", str, SZ_LINE)) str[1] = EOS
        call sprintf (text, SZ_LINE, "CALACS Version: %s")
            call pargstr (str)
        call pp_label (fd, 1., y, text)

	if (access (trl, 0, 0) == YES) {
	    do i = 1, 7
	        call pp_label (fd, 1., y+i, pipeline[1,i])
	} else {
		call pp_label (fd, 1., y+1, "(No Processing comments could be found.)")
	}

	# Calibration Data Quality Summary.
	call fprintf (fd, "expand .8; vpage 0.05 .65 .0 .2\n")
	call fprintf (fd, "limits 0 100 12 0\n")
        call fprintf (fd, "justify 3\n")
   	call pp_label (fd, 0., 1., "\\fBCalibration Data Quality Summary")

	if (access (trl, 0, 0) == YES) {
	    call fprintf (fd, "expand .65\n")
	    y = 2.

	    do i = 1, 7 
	        call pp_label (fd, 1., y+i, caldq[1,i])
	} else {
		call pp_label (fd, 1., y + 1, "(No data quality information could be found.)")
	}

	# prepare for the jitter plot
	call fprintf (fd, "vpage 0.05 .65 .4 .9\n")

	# close images
	call imunmap (im)
	if (ip0 != NULL) call imunmap (ip0)
	if (ip != NULL) call imunmap (ip)
 	call close (fd)

end
