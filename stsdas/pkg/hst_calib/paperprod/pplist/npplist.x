include	"pplist.h"

define  DY      0.7     # font size as well as the height offset for
                        # two-line column names

# produce target list and observation list for NICMOS

procedure npplist (tpin, output, selector, timetag, page)

pointer	tpin			# template of input files
char	output[SZ_FNAME]	# output igi script file name
char  	selector[SZ_LINE]       # output parts selector
char	timetag[SZ_LINE]	# time tag for the footnote
int	page			# page number counter for the footnote

int	fd			# output file pointer
char	fname[SZ_FNAME]		# input file name
char	root[SZ_FNAME]		# input file root name
pointer	raw, spt, trl, pdq	# input file pointers
char	raw_ext[SZ_EXT]		# file extension name
char	spt_ext[SZ_EXT]		# file extension name
char	trl_ext[SZ_EXT]		# file extension name
#char	jih_ext[SZ_EXT]		# file extension name
char	pdq_ext[SZ_EXT]		# file extension name
char	mos_ext[SZ_EXT]		# file extension name
char	allvisit[SZ_LINE]	# overall visit numbers
char	covervisit[SZ_LINE]	# visit numbers on the cover page
char	propid[SZ_LINE]		# proposal ID

char	targ1[MAX_TARG], targ2[MAX_TARG]	# (wrap-around) target names
char	option1[MAX_OPTION], option2[MAX_OPTION]# (wrap-around) options
char	patt1[MAX_PATTERN], patt2[MAX_PATTERN]	# (wrap-around) pattern
char	str1[SZ_LINE], str2[SZ_LINE]		# scratch strings
char	dummy[SZ_LINE]
bool	tfound, vfound
int	ntarg					# number of different targets
int	nvisit					# number of different visits
real	yoff
int	nchar
int	j, n, k, nroots, nobs

# observation attributes
char	tltarg[SZ_TARG, MAX_OBS]	# target names in the target list
char	unique_visit[SZ_NVISIT+1, MAX_OBS]	# unique visits
double	ra[MAX_OBS], dec[MAX_OBS]	# RA and Dec of targets
char	desc[SZ_DESC, MAX_OBS]		# target descriptions
char	linenum[SZ_LINENUM, MAX_OBS]	# Line number
char	rootname[SZ_ROOT, MAX_OBS]	# observation root name
char	targ[SZ_TARG, MAX_OBS]		# target name
char	visit[SZ_NVISIT+1, MAX_OBS]	# visit number
char	opmode[SZ_NMODE, MAX_OBS]	# observation mode
char	filter[SZ_NFILT, MAX_OBS]	# filter used
char	pattern[SZ_NPATTERN, MAX_OBS]	# pattern
char	option[SZ_OOPTION, MAX_OBS]	# option parameters

real	exptime[MAX_OBS]		# exposure time
int	primecam[MAX_OBS]		# prime camera
int	camera[MAX_OBS]			# camera
int	numpos[MAX_OBS]			# number positions
int	numiter[MAX_OBS]		# number iterations
int	obsqual[MAX_OBS]		# observation quality flag
int	procqual[MAX_OBS]		# processing quality flag
int	calqual[MAX_OBS]		# calibration quality flag
real	porient[MAX_OBS]		# pattern orientation
real	dithsize[MAX_OBS], chopsize[MAX_OBS] # pattern keywords

char	first_name[SZ_LINE]		# PI's first name
char	last_name[SZ_LINE]		# PI's last name
char	title1[SZ_LINE], title2[SZ_LINE]	# proposal title
char    pdftitle[SZ_LINE]

pointer	immap()
pointer	tbtopn()
int	access()
int	open()
int	imtlen()
int	imtgetim()
real	imgetr()
int	imgeti()
double	imgetd()
bool	streq()
int   	strmatch()
int	nobs_qual()
int	nproc_qual()
int	ncal_qual()

begin
	# construct necessary file name extensions
	call strcpy ("_raw.fits[0]", raw_ext, SZ_EXT) 
	call strcpy ("_spt.fits[0]", spt_ext, SZ_EXT) 
	call strcpy ("_trl.fits", trl_ext, SZ_EXT) 
	#call strcpy ("_jif.fits", jih_ext, SZ_EXT) 
	call strcpy ("_pdq.fits", pdq_ext, SZ_EXT) 
	call strcpy ("_mos.fits[0]", mos_ext, SZ_EXT) 

        nroots = imtlen(tpin)
	tfound = false
	vfound = false
	ntarg = 0
	nvisit = 0
	n = 0

	# loop all root names
        do j = 1, nroots {

            # read the next input image name in the template list
            nchar = imtgetim (tpin, root, SZ_FNAME)

	    # construct file names of the raw data, the spt file, the trailer, 
	    # and the jitter, open them
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (raw_ext, fname, SZ_FNAME)
	    iferr (raw = immap (fname, READ_ONLY, 0)) {
	        call strcpy (root, fname, SZ_FNAME)
	        call strcat (mos_ext, fname, SZ_FNAME)
	        iferr (raw = immap (fname, READ_ONLY, 0)) {
		    call printf ("no data file exists for '%s', skip.\n")
		        call pargstr (root)
		    next
		} else {
	
		# only do the target mos file (the one having an _asn file)
	        call strcpy (root, fname, SZ_FNAME)
	        call strcat ("_asn.fits", fname, SZ_FNAME)
		if (access (fname, 0, 0) == NO) next
		}

	    # if the raw file belongs to an association, skip it
	    } else {
		call imgstr (raw, "ASN_TAB", dummy, SZ_LINE)
		if (dummy[1] != EOS) {
		    call imunmap (raw)
		    next
		}
	    }

	    n = n + 1

	    # open the spt file 
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (spt_ext, fname, SZ_FNAME)
	    iferr (spt = immap (fname, READ_ONLY, 0))
		spt = NULL

	    # open the trailer file (a FITS table)
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (trl_ext, fname, SZ_FNAME)
	    iferr (trl = tbtopn (fname, READ_ONLY, 0))
		trl = NULL

	    # open the PDQ file
	    call strcpy (root, fname, SZ_FNAME)
            #k = strlen(fname)

	    # change the last letter to "j" if not an association
	    #if (fname[k] > '9' || fname[k] < '0')
                #fname[k] = 'j'
	    #call strcat (jih_ext, fname, SZ_FNAME)
	    call strcat (pdq_ext, fname, SZ_FNAME)

            # to accomodate different OMS version
            #call pp_oms (fname)
	    iferr (pdq = tbtopn (fname, READ_ONLY, 0))
		pdq = NULL

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
	        iferr (call imgstr (spt, "TARDESCR", desc[1,ntarg], SZ_DESC))
		    call strcpy ("(N/A)", desc[1,ntarg], SZ_DESC)
	    }

	    # read visit number
	    call imgstr (raw, "LINENUM", visit[1,n], SZ_NVISIT)
	    if (visit[3,n] == '.') {
		visit[3,n] = visit[2,n]
		visit[2,n] = visit[1,n]
		visit[1,n] = '0'
	    } else if (visit[2,n] == '.') {
		visit[3,n] = visit[1,n]
		visit[2,n] = '0'
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
		call strcpy (visit[1,n], unique_visit[1,nvisit], SZ_NVISIT)
	    }

	    # read other observation list related keywords
	    call imgstr (raw, "LINENUM", linenum[1,n], SZ_LINENUM)
	    call imgstr (raw, "ROOTNAME", rootname[1,n], SZ_ROOT)
	    primecam[n] = imgeti (raw, "PRIMECAM")
	    camera[n]   = imgeti (raw, "CAMERA")
	    call imgstr (raw, "OBSMODE", opmode[1,n], SZ_NMODE)
	    call imgstr (raw, "FILTER", filter[1,n], SZ_NFILT)
	    call imgstr (raw, "PATTERN", pattern[1,n], SZ_NPATTERN)

	    # get exposure time, number position, and number iterations
	    exptime[n] = imgetr (raw, "EXPTIME")
	    iferr (numpos[n] = imgeti (raw, "NUMPOS")) numpos[n] = 1
	    iferr (numiter[n] = imgeti (raw, "NUMITER")) numiter[n] = 1

	    # construct the quality flags
	    obsqual[n] = nobs_qual (pdq)
	    procqual[n] = nproc_qual (trl)
	    calqual[n] = ncal_qual (raw, trl)

	    # construct the option-parameter string
	    call noption_string (raw, opmode[1,n], option[1,n])

	    # read the pattern keywords
	    porient[n] = imgetr (raw, "PORIENT")
	    dithsize[n] = imgetr (raw, "DITHSIZE")
	    chopsize[n] = imgetr (raw, "CHOPSIZE")
	    
	    # Get cover page info
	    if (n == 1) {
	        call imgstr (raw, "PROPOSID", propid, SZ_LINE)
        	call imgstr (raw, "pr_inv_l", last_name, SZ_LINE)
        	call imgstr (raw, "pr_inv_f", first_name, SZ_LINE)
        	iferr (call imgstr (spt, "propttl1", title1, SZ_LINE))
            	    title1[1] = EOS
        	iferr (call imgstr (spt, "propttl2", title2, SZ_LINE))
            	    title2[1] = EOS
	    }
		
	    # close files
	    if (spt != NULL) call imunmap (spt)
	    #if (jih != NULL) call imunmap (jih)
	    if (pdq != NULL) call tbtclo (pdq)
	    if (trl != NULL) call tbtclo (trl)
	    call imunmap (raw)
	}

	nobs = n


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
	    call npp_cover (fd, propid, covervisit, last_name, first_name, 
				title1, title2, timetag, page)

	# print the rest?
        if (strmatch(selector, "visit") == 0 && strmatch(selector, "all") == 0)
            go to 10

	# print the target list
	# ---------------------
	call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)

    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Target List")
        call pargstr(propid)

    call pp_pdfbook(fd, page, pdftitle)

	yoff = 3
	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, 
			"NICMOS", yoff)

	# Added to allow PostScript output to be readable... WJH 25 Jun 97
	if (nobs == 0) nobs = 1

	# print observation list
	# ----------------------
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		    yoff = 3
		}
		call nobs_head (fd, yoff)
        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "Prop %s Observation List")
            call pargstr(propid)

        call pp_pdfbook(fd, page, pdftitle)
	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		yoff = 3
		call nobs_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_str (pattern[1,n], patt1, patt2, MAX_PATTERN)

	    # print one line of data
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)

	    call pp_move (fd, 27., yoff)
	    call fprintf (fd, "label '%1d/%1d'\n")
		call pargi (camera[n])
		call pargi (primecam[n])

	    call pp_label (fd, 30., yoff, opmode[1,n])
	    call pp_label (fd, 42., yoff, filter[1,n])
	    call pp_label (fd, 49., yoff, patt1)

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 65., yoff)
	    call fprintf (fd, "label '%9.3f'\n")
		call pargr (exptime[n])
	    call pp_move (fd, 70., yoff)
	    call fprintf (fd, "label '%2d'\n")
		call pargi (numiter[n])
	    call fprintf (fd, "justify 3\n")

	    # do the quality flags
	    if (obsqual[n] != UNKNOWN) {
	        call pp_move (fd, 75., yoff+0.25)
		if (obsqual[n] == GOOD)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    if (procqual[n] != UNKNOWN) {
	        call pp_move (fd, 78., yoff+0.25)
		if (procqual[n] == GOOD)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    if (calqual[n] != UNKNOWN) {
	        call pp_move (fd, 81., yoff+0.25)
		if (calqual[n] == GOOD)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    # if the target name or the pattern is long, wrap it into two lines
	    if (targ2[1] != EOS || patt2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 15., yoff, targ2)
	        call pp_label (fd, 49., yoff, patt2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+DY)
	
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw 82. %6.2f\n")
	    call pargr (yoff)

	# write flag captions
	call flag_caption (fd, yoff)

	yoff = yoff + 2.5

	# print the Optional Parameter list
	# ---------------------------------
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		    yoff = 3
            #
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Optional Parameters")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
		}
		call noption_head (fd, yoff)
	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		yoff = 3
		call noption_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_str (option[1,n], option1, option2, MAX_OPTION)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_move (fd, 27., yoff)
	    call fprintf (fd, "label '%1d'\n")
		call pargi (camera[n])
	    call pp_label (fd, 29., yoff, opmode[1,n])
	    call pp_label (fd, 39., yoff, option1)

	    # if the target name is long, wrap it into two lines
	    if (targ2[1] != EOS || option2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 15., yoff, targ2)
	        call pp_label (fd, 39., yoff, option2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+DY)
	
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw 82. %6.2f\n")
	    call pargr (yoff)

	yoff = yoff + 2.5

	# print the Pattern Strategy
	# --------------------------
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		    yoff = 3
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Pattern Strategy")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
		}
		call npattern_head (fd, yoff)
	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, "NICMOS", timetag, page)
		yoff = 3
		call npattern_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_move (fd, 27., yoff)
	    call fprintf (fd, "label '%1d'\n")
		call pargi (camera[n])
	    call pp_label (fd, 29., yoff, opmode[1,n])
	    call pp_label (fd, 39., yoff, pattern[1,n])

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 58., yoff)
	    call fprintf (fd, "label '%7.2f'\n")
		call pargr (porient[n])
	    call pp_move (fd, 65., yoff)
	    call fprintf (fd, "label '%2d'\n")
		call pargi (numpos[n])
	    call pp_move (fd, 72., yoff)
	    call fprintf (fd, "label '%6.2f'\n")
		call pargr (dithsize[n])
	    call pp_move (fd, 78., yoff)
	    call fprintf (fd, "label '%6.2f'\n")
		call pargr (chopsize[n])
	    call fprintf (fd, "justify 3\n")

	    # if the target name is long, wrap it into two lines
	    if (targ2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 15., yoff, targ2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+DY)
	
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw 82. %6.2f\n")
	    call pargr (yoff)

	# close output file
10	call close (fd)
end

# produce observation list header for NICMOS

procedure nobs_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position

begin

	# initialize 
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. 82. %6.2f 1.\n")
	    call pargr (BOTTOM)

	# print the title
	call pp_move (fd, 41., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd,  7., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 25., yoff-DY/2., "Camera")
	call pp_label (fd, 23., yoff+DY/2., "Used/Prime")

	call pp_label (fd, 30., yoff-DY/2., "Operating")
	call pp_label (fd, 30., yoff+DY/2., "Mode")
	call pp_label (fd, 42., yoff-DY/2., "Spectral")
	call pp_label (fd, 42., yoff+DY/2., "Element")

	call pp_label (fd, 49., yoff-DY/2., "Observing")
	call pp_label (fd, 49., yoff+DY/2., "Pattern")
	call pp_label (fd, 59., yoff-DY, "Exposure")
	call pp_label (fd, 59., yoff,    "Time per")
	call pp_label (fd, 59., yoff+DY, "Iteration (sec)")

	call pp_label (fd, 67., yoff-DY/2., "Number")
	call pp_label (fd, 67., yoff+DY/2., "Iterations")

	call pp_label (fd, 75., yoff-DY/2., "Quality Flags")
	call pp_label (fd, 74., yoff+DY/2., "Obs")
	call pp_label (fd, 77., yoff+DY/2., "Proc")
	call pp_label (fd, 80., yoff+DY/2., "Cal")

	# draw a double line
	call pp_move (fd, 0., yoff+1.5)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.5)
	call pp_move (fd, 0., yoff+1.75)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.75)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 2.
end

# produce optional parameter header for NICMOS

procedure noption_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position

begin

	# initialize
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. 82. %6.2f 1.\n")
	    call pargr (BOTTOM)

	# print the title
	call pp_move (fd, 41., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List-Optional Parameters'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd,  7., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 24., yoff-DY/2., "Camera")
	call pp_label (fd, 24., yoff+DY/2., "Used")

	call pp_label (fd, 29., yoff-DY/2., "Operating")
	call pp_label (fd, 29., yoff+DY/2., "Mode")
	call pp_label (fd, 39., yoff, "Optional Parameters")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# produce Observing Pattern Strategy header for NICMOS

procedure npattern_head (fd, yoff)

int	fd		# output file pointer
real	yoff		# row position

begin

	# initialize
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. 82. %6.2f 1.\n")
	    call pargr (BOTTOM)

	# print the title
	call pp_move (fd, 41., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObserving Pattern Strategy'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd,  7., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 24., yoff-DY/2., "Camera")
	call pp_label (fd, 24., yoff+DY/2., "Used")

	call pp_label (fd, 29., yoff-DY/2., "Operating")
	call pp_label (fd, 29., yoff+DY/2., "Mode")
	call pp_label (fd, 39., yoff, "Pattern Name")
	call pp_label (fd, 53., yoff-DY/2., "Pattern Orient")
	call pp_label (fd, 53., yoff+DY/2., "(degrees)")

	call pp_label (fd, 62., yoff-DY/2., "Number")
	call pp_label (fd, 62., yoff+DY/2., "Positions")
	call pp_label (fd, 69., yoff-DY/2., "Dither")
	call pp_label (fd, 69., yoff+DY/2., "Size (\")")
	call pp_label (fd, 75., yoff-DY/2., "Chop")
	call pp_label (fd, 75., yoff+DY/2., "Size (\")")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# generate the optional parameter string

procedure noption_string (ip, mode, op_string)

pointer	ip		# input image pointer
char	mode[ARB]	# observation mode
char	op_string[ARB]	# output option-parameter string

int	ival
#real	rval
char	str[SZ_LINE]
char	str1[SZ_LINE]

bool	streq()
int	strncmp()
int	imgeti()
#real	imgetr()

begin

	# initialize the string
	op_string[1] = EOS
	
	# construct the option parameter string according to the operation mode

	# accum mode
	if (strncmp (mode, "ACCUM", 5) == 0) {
	    iferr (ival = imgeti (ip, "NREAD"))
		call sprintf (str, SZ_OOPTION, "NREAD=(N/A)  ")
	    else {
		call sprintf (str, SZ_OOPTION, "NREAD=%d  ")
		    call pargi (ival)
	    }
	    call strcat (str, op_string, SZ_OOPTION)

	# multiaccum mode
	} else if (streq (mode, "MULTIACCUM")) {
	    iferr (call imgstr (ip, "SAMP_SEQ", str1, SZ_LINE))
		call sprintf (str, SZ_OOPTION, "SAMP-SEQ=(N/A)  ")
	    else {
		if (streq (str1, "NONE"))
		    str[1] = EOS
		else {
		    call sprintf (str, SZ_OOPTION, "SAMP-SEQ=%s  ")
		        call pargstr (str1)
		}
	    }
	    call strcat (str, op_string, SZ_OOPTION)

	    iferr (ival = imgeti (ip, "NSAMP"))
		call sprintf (str, SZ_OOPTION, "NSAMP=(N/A)  ")
	    else {
		call sprintf (str, SZ_OOPTION, "NSAMP=%d  ")
		    call pargi (ival-1)
	    }
	    if (streq (str1, "NONE"))
		str[1] = EOS
	    call strcat (str, op_string, SZ_OOPTION)

	    #iferr (rval = imgetr (ip, "SAMPTIME"))
		#call sprintf (str, SZ_OOPTION, "SAMPTIME=(N/A)  ")
	    #else {
		#call sprintf (str, SZ_OOPTION, "SAMPTIME=%7.3f  ")
		    #call pargr (rval)
	    #}
	    #call strcat (str, op_string, SZ_OOPTION)
	}

	# add offset in both mode
	iferr (call imgstr (ip, "PATT_OFF", str1, SZ_LINE))
	    call sprintf (str, SZ_OOPTION, "OFFSET=(N/A)  ")
	else {
	    if (str1[1] == EOS)
		str[1] = EOS
	    else {
	        call sprintf (str, SZ_OOPTION, "OFFSET=%s")
		    call pargstr (str1)
	    }
	}
	call strcat (str, op_string, SZ_OOPTION)
end
