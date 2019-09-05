# Generate the Observation Summary Page for NICMOS

include	"npp.h"

procedure npp_obsum (root, fd)

char	root[ARB]		# Rootname of input files
int	fd			# output file pointer

#char	jih[SZ_FNAME]		# jitter file.
char	pdq[SZ_FNAME]		# PDQ file.
char	trl[SZ_FNAME]		# trailer file.
char    hstqual[SZ_LINE]        # spacecraft performance quality
char    hstcom[SZ_LINE, MAX_SUMMARY]        # spacecraft performance comments
char    pipeline[SZ_LINE, MAX_SUMMARY]      # pipeline processing comments
char    caldq[SZ_LINE, MAX_SUMMARY]         # calibartion processing comments
int     nhst, npipe, ncal       # output: number of comments
real	x, y
int	i

#int	access()
int	strmatch()
bool	streq()

begin

	# Create the file names.
	# no longer use the jitter file 4/21/97 JC Hsu
	#k = strlen(root)
	#if (root[k] > '9' || root[k] < '0') {
	    #call strcpy (root, jih, k-1)
	    #call strcat ("j_jif.fits", jih, SZ_FNAME)
	#} else {
	    #call strcpy (root, jih, k)
	    #call strcat ("_jif.fits", jih, SZ_FNAME)
	#}
	call strcpy (root, trl, SZ_FNAME)
	call strcat ("_trl.fits", trl, SZ_FNAME)
	call strcpy (root, pdq, SZ_FNAME)
	call strcat ("_pdq.fits", pdq, SZ_FNAME)

	# HST Spacecraft Performance Summary.
	call fprintf (fd, "fontset hard\n")
	call fprintf (fd, "location 0 1 0 1\n")
	call fprintf (fd, "expand .8; vpage 0.05 .65 .4 .9\n")
	call fprintf (fd, "limits 0 100 20 0\n")
	call fprintf (fd, "move 0 20; draw 98 20\n")
	call fprintf (fd, "move 0 1; justify 3\n")
	call fprintf (fd, "label '%sfBHST Spacecraft Performance Summary'\n")
             call pargstr ("\\")

	call fprintf (fd, "justify 3; expand 0.65\n")
	x = 2
	y = 3
	#if (access(pdq, 0, 0) == YES) {

            # to accomodate different OMS version
            #call pp_oms (jih)

	    #ip = immap (jih, READ_ONLY, 0)

	    ## print jitter data
	    #iferr (tmpi = imgeti (ip, "NRECENT")) 
	        #call sprintf (str, SZ_LINE, "# Recenterings: N/A")
	    #else {
	    	#call sprintf (str, SZ_LINE, "# Recenterings: %d")
	    	    #call pargi (tmpi)
	    #}
	    #call pp_label (fd, x, y, str)

	    #y = y + 1
	    #iferr (tmpi = imgeti (ip, "NLOSSES")) 
	        #call sprintf (str, SZ_LINE, "# Losses of Lock: N/A")
	    #else {
	    	#call sprintf (str, SZ_LINE, "# Losses of Lock: %d")
	    	    #call pargi (tmpi)
	    #}
	    #call pp_label (fd, x, y, str)

	    #y = y + 1
	    #iferr (tmpr = imgetr (ip, "V2_RMS")) 
	        #call sprintf (str, SZ_LINE, "V2 Jitter (RMS): N/A")
	    #else {
	    	#call sprintf (str, SZ_LINE, "V2 Jitter (RMS): %0.1f mas")
	    	    #call pargr (tmpr)
	    #}
	    #call pp_label (fd, x, y, str)

	    ## print summary text
	    #ifnoerr (call imgstr (ip, "T_TAPDRP", str, SZ_LINE)) {
	    	#if (str[1] != EOS) {
		    #y = y + 1
		    #call strcat ("Guide star acquisition failure due to ", 
				#str, SZ_LINE)
		    #call pp_label (fd, 1., y, str)
		#}
	    #}
	    #ifnoerr (boolval = imgetb (ip, "T_SISAFE")) {
	    	#if (boolval) {
		    #y = y + 1
		    #call pp_label (fd, 1., y, 
				#"Obs. affected when science instrument safed.")
		#}
	    #}

	    # close the jitter image
	    #call imunmap (ip)

	#} else {
	    #call pp_label (fd, 1., 3., "(PDQ file not available)") 
	#}
	call pp_qualfits (pdq, hstqual, hstcom, nhst)

	if (strmatch(hstqual, "OK") != 0) {
	    nhst = 1
	    call strcpy ("No anomalies.", hstcom[1,1], SZ_LINE)
	} else {
	    if (nhst > 0 && strmatch(hstcom[1,1], "Cannot access") == 0)
	        call pp_label (fd, 50., 1.2, 
		 	"(See PDQ file for more information)")
	}

	x = 1.
	y = 2.
	do i = 1, nhst {
	    call pp_swapchar (hstcom[1,i], '\'', '`')
	    call pp_label (fd, x, y+i, hstcom[1,i]) 
	}

	# dredge up the summaries from the trailer file
	call npp_trlsum (trl, pipeline, caldq, npipe, ncal)

	# Pipeline Processing Summary.
	call fprintf (fd, "location 0 1 0 1\n")
	call fprintf (fd, "expand .8; vpage 0.05 .65 .2 .4\n")
	call fprintf (fd, "limits 0 100 12 0\n")
	call fprintf (fd, "move 0 12; draw 98 12\n")
	call fprintf (fd, "move 0 1; justify 3\n")
	call fprintf (fd, "label '%sfBPipeline Processing Summary'\n")
             call pargstr ("\\")
	call fprintf (fd, "expand .65\n")

	x = 1.
	y = 2.
	do i = 1, npipe {
	    call pp_swapchar (pipeline[1,i], '\'', '`')
	    call pp_label (fd, x, y+i, pipeline[1,i]) 
	}
	if (npipe > 1 || !(streq (pipeline[1,1], "No anomalies.") ||
		strmatch(pipeline[1,1], "Cannot access") != 0))
	    call pp_label (fd, 50., 1.2, "(See TRL file for more information)")

	# Calibration Data Quality Summary.
	call fprintf (fd, "expand .8; vpage 0.05 .65 .0 .2\n")
	call fprintf (fd, "limits 0 100 12 0\n")
	call fprintf (fd, "move 0 1; justify 3\n")
	call fprintf (fd, "label '%sfBCalibration Data Quality Summary'\n")
             call pargstr ("\\")
	call fprintf (fd, "expand .65\n")

	x = 1.
	y = 2.
	do i = 1, ncal {
	    call pp_swapchar (caldq[1,i], '\'', '`')
	    call pp_label (fd, x, y+i, caldq[1,i]) 
	}
	if (ncal > 1 || !(streq (caldq[1,1], "No anomalies.") ||
		strmatch(caldq[1,1], "Cannot access") != 0))
	    call pp_label (fd, 50., 1.2, "(See TRL file for more information)")
end
