include	<imhdr.h>
include	"pplist.h"

define	DY	0.6	# font size as well as the height offset for 
			# two-line column names            
define  JLEN 132. # number of positions in each line for text
define  PBORDER 17. # border amount for Pattern Strategy section
define  OBORDER 5.  # border amount for Optional Parameters section
define  TBORDER 10. # border amount for Targets List

# produce target list and observation list for ACS

procedure jpplist (tpin, output, selector, timetag, page)

pointer	tpin	            # template of input files
char	output[SZ_FNAME]	# output igi script file name
char	selector[SZ_LINE]	# output parts selector
char	timetag[SZ_LINE]	# time tag for the footnote
int	page			# page number counter for the footnote

int	fd			# output file pointer
char	fname[SZ_FNAME]		# input file name
char	root[SZ_FNAME]		# input file root name
pointer	raw, trl, jih   	# input file pointers
char	trl_ext[SZ_EXT]		# file extension name
char	jih_ext[SZ_EXT]		# file extension name
char	allvisit[SZ_LINE]	# overall visit numbers
char    covervisit[SZ_LINE]     # visit numbers on the cover page
char	propid[SZ_LINE]		# proposal ID

char	targ1[MAX_TARG], targ2[MAX_TARG]	# (wrap-around) target names
char	aper1[MAX_APER], aper2[MAX_APER]	# (wrap-around) aperture names
char	op_substr[MAX_OPTION,10]		# (wrap-around) options
char	str1[SZ_LINE], str2[SZ_LINE]		# scratch strings
char	dummy[SZ_LINE]
char	acs[SZ_LINE]
bool	tfound, vfound
int	ntarg					# number of different targets
int	nvisit					# number of different visits
int	nopsub					# number of option sub-strings
real	yoff
int	n, k, nobs,nchar, len

# observation attributes
char	tltarg[SZ_TARG, MAX_OBS]	# target names in the target list
char	unique_visit[SZ_VISIT+1, MAX_OBS]	# unique visits
double	ra[MAX_OBS], dec[MAX_OBS]	# RA and Dec of targets
char	desc[SZ_DESC, MAX_OBS]		# target descriptions
char	linenum[SZ_LINENUM, MAX_OBS]	# Line number
char	rootname[SZ_ROOT, MAX_OBS]	# observation root name
char	targ[SZ_TARG, MAX_OBS]		# target name
char	visit[SZ_VISIT+1, MAX_OBS]	# visit number
char	opmode[SZ_JMODE, MAX_OBS]	# observation mode
char	detector[SZ_JDET, MAX_OBS]	# detectors used
char	aper[SZ_JAPER, MAX_OBS]		# aperture used
char	filter1[SZ_JFILT, MAX_OBS]	# filter 1 used
char	filter2[SZ_JFILT, MAX_OBS]	# filter 2 used
char	option[MAX_OPTION, MAX_OBS]	# option parameters
char	subarray[SZ_LINE]			# SUBARRAY keyword value
char	frame[SZ_ROOT, MAX_OBS]  	# frame keyword value
char	imgtype[SZ_TARG, MAX_OBS]  	# IMAGETYP keyword value

char	pattern1[SZ_JAPER, MAX_OBS] # PATTERN1 keyword value
real	porient[MAX_OBS]		    # Pattern Orient keyword value 
int	    pnpts[MAX_OBS]			    # Number of Points in pattern 


real	exptime[MAX_OBS]		# exposure time
int	obsqual[MAX_OBS]		# observation quality flag
int	procqual[MAX_OBS]		# processing quality flag
int	calqual[MAX_OBS]		# calibration quality flag

char	first_name[SZ_LINE]		# PI's first name
char	last_name[SZ_LINE]		# PI's last name
char	title1[SZ_LINE], title2[SZ_LINE]	# proposal title
char    pdftitle[SZ_LINE]

pointer	immap()
pointer	tbtopn()
int	open()
real	imgetr()
int	imgeti()
int imtlen()
int imtgetim()
double	imgetd()
bool	streq()
int	strlen()
int	strmatch()
int	jobs_qual()
int	jacq_qual()
int	jproc_qual()
int	jcal_qual()

begin
	
    call strcpy ("ACS", acs, SZ_LINE)

	# construct necessary file name extensions
	call strcpy (".tra", trl_ext, SZ_EXT) 
	call strcpy ("_jif.fits", jih_ext, SZ_EXT) 

    nobs = imtlen(tpin)
	tfound = false
	vfound = false
	ntarg = 0
	nvisit = 0
    
	
	# loop over all root names
    do n = 1,nobs {
            
	    # construct file names of the raw data, the trailer, and the jitter,        
        nchar = imtgetim(tpin, fname, SZ_FNAME)
	    iferr (raw = immap (fname, READ_ONLY, 0)) {
		    call printf ("%s: cannot open product exposure image, skipping...\n")
		        call pargstr (fname)
		    next
		}

        # Extract the rootname from the full filename...
        # strip off the .??? or _???.fits suffix 
        # (the last character is carridge return)
        len = strlen (fname)+1
        fname[len] = EOS
        call strcpy (fname[len-8], str1, SZ_LINE)
            
        if (streq (str1, ".fits[0]")) {
            if (fname[len-12] == '_')
                call strcpy (fname, root, len-13)
        } else if (fname[len-8] == '.') {
            call strcpy (fname, root, len-9)
        }
		
	    # open the trailer file (an ASCII file)
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (trl_ext, fname, SZ_FNAME)
	    iferr (trl = tbtopn (fname, READ_ONLY, 0)) {
            trl = NULL
        }
			
        # open the jitter file
        call strcpy (root, fname, SZ_FNAME)
        k = strlen(fname)

        # change the last letter to "j" if not an association
        if (fname[k] > '9' || fname[k] < '0')
            fname[k] = 'j'
        call strcat (jih_ext, fname, SZ_FNAME)

 
        # to accomodate different OMS version
        call pp_oms (fname)
	    iferr (jih = immap (fname, READ_ONLY, 0))
		jih = NULL

	    # read target name
	    call imgstr (raw, "TARGNAME", targ[1,n], SZ_TARG)
	
	    # compare with previous target names
	    do k = 1, ntarg {
		tfound = streq (targ[1,n], tltarg[1,k])
		if (tfound) break
	    }

	    # if target not found in previous observations, add to the 
	    # target list and read other target related keywords
	    if (!tfound) {
	 	ntarg = ntarg + 1
		call strcpy (targ[1,n], tltarg[1,ntarg], SZ_TARG)

		ra[ntarg] = imgetd (raw, "RA_TARG")
		dec[ntarg] = imgetd (raw, "DEC_TARG")
	        iferr (call imgstr (raw, "TARDESCR", desc[1,ntarg], SZ_DESC))
		    call strcpy ("(N/A)", desc[1,ntarg], SZ_DESC)
	    }

	    # read visit number
        call imgstr (raw, "LINENUM", visit[1,n], SZ_VISIT)
        if (visit[2,n] == '.') {
            visit[2,n] = visit[1,n]
            visit[1,n] = '0'
        }

	    # compare with previous target names
	    do k = 1, nvisit {
		vfound = streq (visit[1,n], unique_visit[1,k])
		if (vfound) break
	    }

	    # if target not found in previous observations, add to the 
	    # unique visit list 
	    if (!vfound) {
	 	nvisit = nvisit + 1
		call strcpy (visit[1,n], unique_visit[1,nvisit], SZ_VISIT)
	    }
              	        
	    # read other observation list related keywords
	    call imgstr (raw, "LINENUM", linenum[1,n], SZ_LINENUM)
	    call imgstr (raw, "ROOTNAME", rootname[1,n], SZ_ROOT)
	    call imgstr (raw, "DETECTOR", detector[1,n], SZ_JDET)
	    call imgstr (raw, "OBSMODE", opmode[1,n], SZ_JMODE)
        call imgstr (raw, "APERTURE", aper[1,n], SZ_JAPER)
	    call imgstr (raw, "FILTER1", filter1[1,n], SZ_JFILT)
	    call imgstr (raw, "FILTER2", filter2[1,n], SZ_JFILT)
	    call imgstr (raw, "SUBARRAY", subarray, SZ_LINE)

        # Determine the type of observation being processed
        #   Default to UNKNOWN if this keyword is missing...
	    iferr (call imgstr (raw, "IMAGETYP", imgtype[1,n], SZ_TARG)) {
            call strcpy("UNKNOWN", imgtype[1,n], SZ_TARG)
        }
        
        # Determine whether it is a SUBARRAY or FULL image
        if (streq(subarray,"F")) {
    		call strcpy ("FULL", frame[1,n], SZ_LINE)
        } else {
    		call strcpy ("SUBARRAY", frame[1,n], SZ_LINE)
        }

        # Read in pattern keywords, and set appropriate defaults
	    iferr (call imgstr (raw, "PATTERN1", pattern1[1,n], SZ_JAPER)){
            call strcpy("NONE", pattern1[1,n], SZ_JAPER)
        }
            
	    iferr(porient[n] = imgetr (raw, "P1_ORINT"))
            porient[n] = 0.
	  
        iferr(pnpts[n] = imgeti (raw, "P1_NPTS"))
            pnpts[n] = 0
  

	    # get exposure time
	    exptime[n] = imgetr (raw, "EXPTIME")

			
	    # construct the quality flags
	    if ( streq (opmode[1,n], "ACQ"))
	        obsqual[n] = jacq_qual (jih)
	    else {
            obsqual[n] = jobs_qual (jih)
	    }        

        #procqual[n] = jproc_qual (NULL)
        procqual[n] = jproc_qual (trl)
        calqual[n] = jcal_qual (raw, trl)
        #calqual[n] = jcal_qual (raw, NULL)

	    # construct the option-parameter string
	    call joption_string (raw, detector[1,n], opmode[1,n], option[1,n])
        
	    # Get cover page info
	    if (n == 1) {
	        call imgstr (raw, "PROPOSID", propid, SZ_LINE)
        	call imgstr (raw, "pr_inv_l", last_name, SZ_LINE)
        	call imgstr (raw, "pr_inv_f", first_name, SZ_LINE)
		    call jpp_title (root, raw, title1, title2)

	    }
		
	    # close files
	    if (jih != NULL) call imunmap (jih)
	    if (trl != NULL) call tbtclo (trl)
        call imunmap (raw)
        	
    # End of loop over all root names (nobs)
	}
    	
	# construct the overall visit number(s)
	call strcpy (unique_visit[1, 1], allvisit, SZ_LINE)
        call strcpy (unique_visit[1, 1], covervisit, SZ_LINE)
        do k = 2, nvisit {
            call strcat (", ", covervisit, SZ_LINE)
            call strcat (unique_visit[1, k], covervisit, SZ_LINE)
        }
	if (nvisit <= 10) {
	    do k = 2, nvisit {
		call strcat (", ", allvisit, SZ_LINE)
		call strcat (unique_visit[1, k], allvisit, SZ_LINE)
	    }
	} else {
	    do k = 2, 8 {
		call strcat (", ", allvisit, SZ_LINE)
		call strcat (unique_visit[1, k], allvisit, SZ_LINE)
	    }
	    call strcat (", ..., ", allvisit, SZ_LINE)
	    call strcat (unique_visit[1, nvisit], allvisit, SZ_LINE)
	}

	# construct the banner strings
	call sprintf (str1, SZ_LINE, "Visit: %s")
	    call pargstr (allvisit) 
	call sprintf (str2, SZ_LINE, "Proposal: %s")
	    call pargstr (propid) 
        
	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# print the cover page and the explanatory page
	# ---------------------------------------------
	if (strmatch(selector, "cover") != 0 || strmatch(selector, "all") != 0)
	    call jpp_cover (fd, propid, covervisit, last_name, first_name, 
			title1, title2, timetag, page)

	# print the rest?
	if (strmatch(selector, "visit") == 0 && strmatch(selector, "all") == 0)
	    go to 10

	# print the target list
	# ---------------------
	call pp_banner (fd, str1, "", str2, acs, timetag, page)
    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Target List")
        call pargstr(propid)

    call pp_pdfbook(fd, page, pdftitle)

	yoff = 3
	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, acs,
			yoff)

	
	# print observation list
	# ----------------------
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		        call pp_erase (fd)
		        call pp_banner (fd, str1, "", str2, acs, timetag, page)
		        yoff = 3
		    }
		    call jobs_head (fd, yoff)
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Observation List")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)

	    } else if (yoff > BOTTOM-1.) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, acs, timetag, page)
		    yoff = 3
		    call jobs_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_str (aper[1,n], aper1, aper2, MAX_APER)

	    # print one line of data
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 10., yoff, rootname[1,n])
	    call pp_label (fd, 19., yoff, targ1)
	    call pp_label (fd, 30., yoff, imgtype[1,n])
	    call pp_label (fd, 44., yoff, detector[1,n])
	    call pp_label (fd, 52., yoff, frame[1,n])
	    call pp_label (fd, 64., yoff, opmode[1,n])

	    call pp_label (fd, 73., yoff, filter1[1,n])
	    call pp_label (fd, 81., yoff, filter2[1,n])
	    call pp_label (fd, 90., yoff, aper1)

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 106., yoff)
	    call fprintf (fd, "label '%8.1f'\n")
		call pargr (exptime[n])
	    call fprintf (fd, "justify 3\n")

	    # do the quality flags
	    if (obsqual[n] != UNKNOWN) {
	        call pp_move (fd, 108., yoff+0.25)
		if (obsqual[n] == GOOD)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    if (procqual[n] != UNKNOWN) {
	        call pp_move (fd, 114., yoff+0.25)
		if (procqual[n] == GOOD)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    if (calqual[n] != UNKNOWN) {
		if (calqual[n] == NA) {
		    call fprintf (fd, "justify 5\n")
	            call pp_label (fd, 120., yoff+0.25, "N/A")
	    	    call fprintf (fd, "justify 3\n")
		} else {
	            call pp_move (fd, 120., yoff+0.25)
		    if (calqual[n] == GOOD)
		        call fprintf (fd, "ptype 25 0\n")
		    else
		        call fprintf (fd, "ptype 25 3\n")
		    call fprintf (fd, "dot\n")
		}
	    }
	    
	    # if the target name is long, wrap it into two lines
	    if (targ2[1] != EOS || aper2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 19., yoff, targ2)
	        call pp_label (fd, 87., yoff, aper2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call pp_draw (fd, JLEN, yoff+DY)
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call pp_draw (fd, JLEN, yoff)

	# write flag captions
	call flag_caption (fd, yoff)

	yoff = yoff + 2.5
	
	# print the optional parameter list
	# ---------------------------------
	do n = 1, nobs {
	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_strn (option[1,n], op_substr, MAX_OPTION, nopsub)

	    if (n == 1) {
	        if (yoff+nopsub > (BOTTOM-4.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, acs, timetag, page)
		    yoff = 3
            #
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Optional Parameters")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
		}
		call joption_head (fd, yoff)
	    } else if (yoff+nopsub > BOTTOM) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, acs, timetag, page)
		yoff = 3
		call joption_head (fd, yoff)
	    }

	    # fill in one line
	    call pp_label (fd, 6., yoff, linenum[1,n])
	    call pp_label (fd, 17., yoff, rootname[1,n])
	    call pp_label (fd, 28., yoff, op_substr[1,1])

	    # if the target name and/or option is long, wrap it into two lines
	    if ( nopsub > 1) {
			yoff = yoff + DY
	        call pp_label (fd, 28., yoff, op_substr[1,2])
	    }
	    if (nopsub > 2) {
			yoff = yoff + DY
	        call pp_label (fd, 28., yoff, op_substr[1,3])
	    }

	    # draw a line
	    call pp_move (fd, 5., yoff+DY)
	    call pp_draw (fd, JLEN-5., yoff+DY)
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 5., yoff)
	call pp_draw (fd, JLEN-5., yoff)

	yoff = yoff + 2.5

	# print the Observing Patterns
	# ----------------------------
	call strcpy ("label '%8.1f'\n", dummy, SZ_LINE)
    
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		        call pp_erase (fd)
		        call pp_banner (fd, str1, "", str2, acs, timetag, page)
		        yoff = 3
	        } 
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Observing Patterns")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
    	    call jpatt_head (fd, yoff)
        }


	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 18., yoff, linenum[1,n])
	    call pp_label (fd, 28., yoff, rootname[1,n])
	    call pp_label (fd, 40., yoff, targ1)
	    call pp_label (fd, 52., yoff, detector[1,n])
	    call pp_label (fd, 61., yoff, opmode[1,n])

	    call pp_label (fd, 70., yoff, pattern1[1,n])

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 91., yoff)
	    call fprintf (fd, "label %6.2f\n")
		call pargr (porient[n])

	    call pp_move (fd, 101., yoff)
	    call fprintf (fd, "label '%7d'\n")
		call pargi (pnpts[n])
	    call fprintf (fd, "justify 3\n")

	    # if the target name is long, wrap it into two lines
	    if (targ2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 40., yoff, targ2)
	    }

	    # draw a line
	    call pp_move (fd, 17., yoff+DY)
	    call pp_draw (fd, JLEN-17., yoff+DY)
	    yoff = yoff + 1.
        
	}

	# draw the second of the double line
	call pp_move (fd, PBORDER, yoff)
    #
	call pp_draw (fd, JLEN-PBORDER, yoff)

	# close output file
10	call close (fd)
end

# produce observation list header for ACS

procedure jobs_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position

begin

	# initialize 
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. %6.2f %6.2f 1.\n")
	    call pargr (JLEN)
        call pargr (BOTTOM)
        

	# print the title
	call pp_move (fd, (JLEN/2), yoff)
    
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd, 10., yoff, "Rootname")
	call pp_label (fd, 19., yoff, "Target Name")
	call pp_label (fd, 30., yoff, "Observation Type")
	call pp_label (fd, 44., yoff, "Detector")
	call pp_label (fd, 52., yoff, "Frame")

	call pp_label (fd, 64., yoff-DY/2., "Operating")
	call pp_label (fd, 64., yoff+DY/2., "Mode")
	call pp_label (fd, 73., yoff-DY/2., "Spectral")
	call pp_label (fd, 73., yoff+DY/2., "Element 1")
	call pp_label (fd, 81., yoff-DY/2., "Spectral")
	call pp_label (fd, 81., yoff+DY/2., "Element 2")

	call pp_label (fd, 90., yoff, "Aperture")

	call pp_label (fd, 99., yoff-DY/2., "Exposure")
	call pp_label (fd, 99., yoff+DY/2., "Time (sec)")

	call pp_label (fd, 108., yoff-DY/2., "Quality Flags")
	call pp_label (fd, 108., yoff+DY/2., "Obs")
	call pp_label (fd, 114., yoff+DY/2., "Proc")
	call pp_label (fd, 120., yoff+DY/2., "Cal")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call pp_draw (fd, JLEN, yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call pp_draw (fd, JLEN, yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# produce optional parameter header for ACS

procedure joption_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position
real    lcen        # center position for line

begin

    lcen = JLEN / 2.0
    
	# initialize
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. %6.2f %6.2f 1.\n")
	    call pargr (JLEN)
	    call pargr (BOTTOM)

	# print the title
	call pp_move (fd, lcen, yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List-Optional Parameters'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd,  6., yoff, "Visit-Exp#")
	call pp_label (fd, 17., yoff, "Rootname")
	call pp_label (fd, 28., yoff, "Optional Parameters")

	# draw a double line
	call pp_move (fd, OBORDER, yoff+1.)
	call pp_draw (fd, JLEN-OBORDER, yoff+1.)
	call pp_move (fd, OBORDER, yoff+1.25)
	call pp_draw (fd, JLEN-OBORDER, yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# produce Observing Pattern Parameters header for ACS

procedure jpatt_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position
real    lcen        # center of page position for title

begin

	# initialize
    lcen = JLEN / 2.
    
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. %6.2f %6.2f 1.\n")
	    call pargr (JLEN)
	    call pargr (BOTTOM)

	# print the title
	call pp_move (fd, lcen, yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObserving Pattern Strategy'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd, 18., yoff, "Visit-Exp#")
	call pp_label (fd, 28., yoff, "Rootname")
	call pp_label (fd, 40., yoff, "Target Name")
	call pp_label (fd, 52., yoff, "Detector")
	call pp_label (fd, 61., yoff-DY/2., "Operating")
	call pp_label (fd, 61., yoff+DY/2., "Mode")
    
	call pp_label (fd, 70., yoff, "Pattern Name")
	call pp_label (fd, 86., yoff-DY/2., "Pattern")
	call pp_label (fd, 86., yoff+DY/2., "Orient")
	call pp_label (fd, 96., yoff-DY/2., "Number of")
	call pp_label (fd, 96., yoff+DY/2., "points")


	# draw a double line
	call pp_move (fd, PBORDER, yoff+1.)
	call pp_draw (fd, JLEN-PBORDER, yoff+1.)
	call pp_move (fd, PBORDER, yoff+1.25)
	call pp_draw (fd, JLEN-PBORDER, yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# generate the optional parameter string

procedure joption_string (ip, detector, mode, op_string)

pointer	ip		# input image pointer
char	detector[ARB]	# detector used
char	mode[ARB]	# observation mode
char	op_string[ARB]	# output option-parameter string

int	ival
int	len
char	str[SZ_LINE]
char	str1[SZ_LINE]

bool	streq()
int	strncmp()
int	strlen()
int	imgeti()

begin

	# initialize the string
	op_string[1] = EOS
	
	# construct the option parameter string according to the
	# detector and operation mode

	if (streq (detector, "HRC") || streq (detector, "WFC")) {
        if (strncmp(mode, "ACCUM", 5) == 0) {

		    iferr (ival = imgeti (ip, "GAIN"))
		        call sprintf (str, SZ_OOPTION, "GAIN=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "GAIN=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (ival = imgeti(ip, "SIZAXIS2"))
		        call sprintf (str, SZ_OOPTION, "SIZEAXIS2=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "SIZEAXIS2=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (call imgstr (ip, "PAREXP", str1, SZ_LINE))
		        call sprintf (str, SZ_OOPTION, "PAREXP=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "PAREXP=%s, ")
		            call pargstr (str1)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

	    }
    }
	# strip the ending comma
	len = strlen (op_string)
	if (op_string[len-1] == ',') op_string[len-1] = EOS
end


procedure jpp_title (root, raw, title1, title2)

char	root[ARB]			# input file root name
char	title1[ARB], title2[ARB]	# output title strings
pointer	raw				# file pointer for image data

pointer	spt				# pointer to SPT file
char	spt_ext[SZ_EXT]			# file extension name
char	fname[SZ_FNAME]			# input file name

pointer	immap()

begin
	call strcpy ("_spt.fits[0]", spt_ext, SZ_EXT) 

	# open the spt file 
	call strcpy (root, fname, SZ_FNAME)
	call strcat (spt_ext, fname, SZ_FNAME)
	iferr (spt = immap (fname, READ_ONLY, 0))
   		spt = NULL

    if (spt != NULL) {
	    iferr (call imgstr (spt, "propttl1", title1, SZ_LINE)) {
	        title1[1] = EOS
		} 
        
        iferr (call imgstr (raw, "propttl2", title2, SZ_LINE)) {
	        title2[1] = EOS
		} 
	} else {
        title1[1] = EOS
        title2[1] = EOS
    }

	# Close the SPT file
	call imunmap(spt)

end
