include	<imhdr.h>
include	"pplist.h"

define	DY	0.7	# font size as well as the height offset for 
			# two-line column names

# produce target list and observation list for STIS

procedure opplist (tpin, output, selector, timetag, page)

pointer	tpin			# template of input files
char	output[SZ_FNAME]	# output igi script file name
char	selector[SZ_LINE]	# output parts selector
char	timetag[SZ_LINE]	# time tag for the footnote
int	page			# page number counter for the footnote

int	fd			# output file pointer
char	fname[SZ_FNAME]		# input file name
char	root[SZ_FNAME]		# input file root name
pointer	raw, trl, jih, stat	# input file pointers
char	raw_ext[SZ_EXT]		# file extension name
char	wav_ext[SZ_EXT]		# file extension name
char	trl_ext[SZ_EXT]		# file extension name
char	jih_ext[SZ_EXT]		# file extension name
char	allvisit[SZ_LINE]	# overall visit numbers
char    covervisit[SZ_LINE]     # visit numbers on the cover page
char	propid[SZ_LINE]		# proposal ID

char	targ1[MAX_TARG], targ2[MAX_TARG]	# (wrap-around) target names
char	aper1[MAX_APER], aper2[MAX_APER]	# (wrap-around) aperture names
char	op_substr[MAX_OPTION,10]		# (wrap-around) options
char	str1[SZ_LINE], str2[SZ_LINE]		# scratch strings
char	dummy1[SZ_LINE]
char	stis[SZ_LINE]
bool	tfound, vfound
int	ntarg					# number of different targets
int	nvisit					# number of different visits
int	nopsub					# number of option sub-strings
real	yoff
int	nchar
int	n, k, nobs

# observation attributes
char	tltarg[SZ_TARG, MAX_OBS]	# target names in the target list
char	unique_visit[SZ_VISIT+1, MAX_OBS]	# unique visits
double	ra[MAX_OBS], dec[MAX_OBS]	# RA and Dec of targets
char	desc[SZ_DESC, MAX_OBS]		# target descriptions
char	linenum[SZ_LINENUM, MAX_OBS]	# Line number
char	rootname[SZ_ROOT, MAX_OBS]	# observation root name
char	targ[SZ_TARG, MAX_OBS]		# target name
char	visit[SZ_VISIT+1, MAX_OBS]	# visit number
char	opmode[SZ_OMODE, MAX_OBS]	# observation mode
char	detector[SZ_ODET, MAX_OBS]	# detectors used
char	aper[SZ_OAPER, MAX_OBS]		# aperture used
char	filter[SZ_OFILT, MAX_OBS]	# filter used
char	cwave[SZ_OCWAVE, MAX_OBS]	# central wavelength
char	option[SZ_OOPTION, MAX_OBS]	# option parameters
char	ext[SZ_FEXT, MAX_OBS]		# file extension
char	mtflag[SZ_MTFLAG]		# moving target flag
char	sclamp[SZ_LINE]			# lamp used

real	exptime[MAX_OBS]		# exposure time
int	nexp[MAX_OBS]			# exposure repeats
int	obsqual[MAX_OBS]		# observation quality flag
int	procqual[MAX_OBS]		# processing quality flag
int	calqual[MAX_OBS]		# calibration quality flag
int	npix[MAX_OBS]			# number of pixels in the image
int	ngoodpix[MAX_OBS]		# number of good pixels
real	gmin[MAX_OBS], gmax[MAX_OBS], gmean[MAX_OBS] # statistics of good pixels
real	snrmin[MAX_OBS], snrmax[MAX_OBS], snrmean[MAX_OBS]  # S/N statistics 
real	cw				# central wavelength
int	nstat

char	first_name[SZ_LINE]		# PI's first name
char	last_name[SZ_LINE]		# PI's last name
char	title1[SZ_LINE], title2[SZ_LINE]	# proposal title
char    pdftitle[SZ_LINE]

pointer	immap()
pointer	tbtopn()
int	open()
int	imtlen()
int	imtgetim()
real	imgetr()
int	imgeti()
double	imgetd()
bool	streq()
bool	strne()
int	strlen()
int	strmatch()
int	oobs_qual()
int	oacq_qual()
int	oproc_qual()
int	ocal_qual()

begin
	call strcpy ("STIS", stis, SZ_LINE)

	# construct necessary file name extensions
	call strcpy ("_raw.fits[0]", raw_ext, SZ_EXT) 
	call strcpy ("_wav.fits[0]", wav_ext, SZ_EXT) 
	call strcpy ("_trl.fits", trl_ext, SZ_EXT) 

	# use the primary header, OPR 33671, 5/1/97 JC Hsu
	call strcpy ("_jif.fits", jih_ext, SZ_EXT) 

        nobs = imtlen(tpin)
	tfound = false
	vfound = false
	ntarg = 0
	nvisit = 0

	# loop all root names
        do n = 1, nobs {

            # read the next input image name in the template list
            nchar = imtgetim (tpin, root, SZ_FNAME)

	    # construct file names of the raw data, the trailer, and the jitter,
	    # open them
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (raw_ext, fname, SZ_FNAME)
	    iferr (raw = immap (fname, READ_ONLY, 0)) {
	        call strcpy (root, fname, SZ_FNAME)
	        call strcat (wav_ext, fname, SZ_FNAME)
	        iferr (raw = immap (fname, READ_ONLY, 0)) {
		    call printf ("%s: cannot open input raw or wav file, skip.\n")
		    call pargstr (root)
		    next
		}
	    }

	    # open the trailer file (a FITS table)
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (trl_ext, fname, SZ_FNAME)
	    iferr (trl = tbtopn (fname, READ_ONLY, 0))
		trl = NULL

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

	    # decide which file to be used for the statistics
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat ("_sfl[1]", fname, SZ_FNAME)
	    call strcpy ("sfl", ext[1,n], SZ_FEXT)
	    iferr (stat = immap (fname, READ_ONLY, 0)) {
	        call strcpy (root, fname, SZ_FNAME)
	        call strcat ("_crj[1]", fname, SZ_FNAME)
	        call strcpy ("crj", ext[1,n], SZ_FEXT)
	        iferr (stat = immap (fname, READ_ONLY, 0)) {
	            call strcpy (root, fname, SZ_FNAME)
	            call strcat ("_flt[1]", fname, SZ_FNAME)
	            call strcpy ("flt", ext[1,n], SZ_FEXT)
	            iferr (stat = immap (fname, READ_ONLY, 0)) {
	                call strcpy (root, fname, SZ_FNAME)
	                call strcat ("_raw[1]", fname, SZ_FNAME)
	                call strcpy ("raw", ext[1,n], SZ_FEXT)
	                iferr (stat = immap (fname, READ_ONLY, 0)) {
	                    call strcpy (root, fname, SZ_FNAME)
	                    call strcat ("_wav[1]", fname, SZ_FNAME)
	                    call strcpy ("wav", ext[1,n], SZ_FEXT)
	            	    stat = immap (fname, READ_ONLY, 0)
			}
		    }
		}
	    }

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
	    call imgstr (raw, "DETECTOR", detector[1,n], SZ_ODET)
	    call imgstr (raw, "OBSMODE", opmode[1,n], SZ_OMODE)
	    call imgstr (raw, "APERTURE", aper[1,n], SZ_OAPER)
	    call imgstr (raw, "OPT_ELEM", filter[1,n], SZ_OFILT)
	    iferr (call imgstr (raw, "MTFLAG", mtflag, SZ_MTFLAG))
		mtflag[1] = EOS
	    iferr (call imgstr (raw, "SCLAMP", sclamp, SZ_LINE))
		call strcpy ("NONE", sclamp, SZ_LINE)

	    # get the central wavelength
	    iferr (cw = imgetr (raw, "CENWAVE")) 
		call strcpy ("(N/A)", cwave[1,n], SZ_OCWAVE)
	    else {
		call sprintf (cwave[1,n], SZ_OCWAVE, "%d")
		    call pargi (nint(cw))
	    }

	    # get exposure time and repeats
	    exptime[n] = imgetr (raw, "TEXPTIME")
	    iferr (nexp[n] = imgeti (raw, "NRPTEXP")) nexp[n] = 1
	    if (nexp[n] <= 1) {
	        iferr (nexp[n] = imgeti (raw, "CRSPLIT")) {
		    nexp[n] = 1
		}
	    }

	    # construct the quality flags
	    if (streq (mtflag, "T") || streq (opmode[1,n], "ACQ") || 
		streq (opmode[1,n], "ACQ/PEAK"))
	        obsqual[n] = oacq_qual (jih)
	    else {
		if (strne (sclamp, "NONE")) obsqual[n] = UNKNOWN
	        else obsqual[n] = oobs_qual (jih)
	    }
	    procqual[n] = oproc_qual (trl)
	    if (strne (sclamp, "NONE")) calqual[n] = UNKNOWN
	    else calqual[n] = ocal_qual (raw, trl)

	    # construct the option-parameter string
	    call ooption_string (raw, detector[1,n], opmode[1,n], option[1,n])

	    # read the statistics
	    npix[n] = IM_LEN (stat, 1)
	    if (IM_NDIM(stat) == 2) npix[n] = npix[n] * IM_LEN (stat, 2)
	    ngoodpix[n] = imgeti (stat, "NGOODPIX")
	    gmin[n] = imgetr (stat, "GOODMIN")
	    gmax[n] = imgetr (stat, "GOODMAX")
	    gmean[n] = imgetr (stat, "GOODMEAN")
	    snrmin[n] = imgetr (stat, "SNRMIN")
	    snrmax[n] = imgetr (stat, "SNRMAX")
	    snrmean[n] = imgetr (stat, "SNRMEAN")
	    
	    # Get cover page info
	    if (n == 1) {
	        call imgstr (raw, "PROPOSID", propid, SZ_LINE)
        	call imgstr (raw, "pr_inv_l", last_name, SZ_LINE)
        	call imgstr (raw, "pr_inv_f", first_name, SZ_LINE)
        	#iferr (call imgstr (raw, "propttl1", title1, SZ_LINE))
			#title1[1] = EOS
    		#iferr (call imgstr (raw, "propttl2", title2, SZ_LINE))
  		        #title2[1] = EOS
		call opp_title (root, raw, title1, title2)

	    }
		
	    # close files
	    if (jih != NULL) call imunmap (jih)
	    if (trl != NULL) call tbtclo (trl)
	    if (stat != raw) call imunmap (stat)
	    call imunmap (raw)
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
	    call opp_cover (fd, propid, covervisit, last_name, first_name, 
			title1, title2, timetag, page)

	# print the rest?
	if (strmatch(selector, "visit") == 0 && strmatch(selector, "all") == 0)
	    go to 10

	# print the target list
	# ---------------------
	call pp_banner (fd, str1, "", str2, stis, timetag, page)
    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Target List")
        call pargstr(propid)

    call pp_pdfbook(fd, page, pdftitle)

	yoff = 3
	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, stis,
			yoff)

	# print observation list
	# ----------------------
	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, stis, timetag, page)
		    yoff = 3
		}
		call oobs_head (fd, yoff)
        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "Prop %s Observation List")
            call pargstr(propid)

        call pp_pdfbook(fd, page, pdftitle)

	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, stis, timetag, page)
		yoff = 3
		call oobs_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_str (aper[1,n], aper1, aper2, MAX_APER)

	    # print one line of data
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_label (fd, 25., yoff, detector[1,n])
	    call pp_label (fd, 31., yoff, opmode[1,n])

	    call pp_label (fd, 39., yoff, aper1)
	    call pp_label (fd, 48., yoff, filter[1,n])
	    call pp_label (fd, 54., yoff, cwave[1,n])

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 63., yoff)
	    call fprintf (fd, "label '%8.1f'\n")
		call pargr (exptime[n])
	    call pp_move (fd, 67., yoff)
	    call fprintf (fd, "label '%d'\n")
		call pargi (nexp[n])
	    call fprintf (fd, "justify 3\n")
	    call pp_label (fd, 69., yoff, ext[1,n])

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
		if (calqual[n] == NA) {
		    call fprintf (fd, "justify 5\n")
	            call pp_label (fd, 81., yoff+0.25, "N/A")
	    	    call fprintf (fd, "justify 3\n")
		} else {
	            call pp_move (fd, 81., yoff+0.25)
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
	        call pp_label (fd, 15., yoff, targ2)
	        call pp_label (fd, 38., yoff, aper2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call pp_draw (fd, 82., yoff+DY)
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call pp_draw (fd, 82., yoff)

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
		    call pp_banner (fd, str1, "", str2, stis, timetag, page)
		    yoff = 3
            #
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Optional Parameters")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
		}
		call ooption_head (fd, yoff)
	    } else if (yoff+nopsub > BOTTOM) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, stis, timetag, page)
		yoff = 3
		call ooption_head (fd, yoff)
	    }

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_label (fd, 25., yoff, detector[1,n])
	    call pp_label (fd, 31., yoff, opmode[1,n])
	    call pp_label (fd, 39., yoff, op_substr[1,1])

	    # if the target name and/or option is long, wrap it into two lines
	    if (targ2[1] != EOS || nopsub > 1) {
		yoff = yoff + DY
	        call pp_label (fd, 15., yoff, targ2)
	        call pp_label (fd, 39., yoff, op_substr[1,2])
	    }
	    if (nopsub > 2) {
		yoff = yoff + DY
	        call pp_label (fd, 39., yoff, op_substr[1,3])
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call pp_draw (fd, 82., yoff+DY)
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call pp_draw (fd, 82., yoff)

	yoff = yoff + 2.5

	# print the statistics list
	# -------------------------
	call strcpy ("label '%8.1f'\n", dummy1, SZ_LINE)

	# if all observations are ACQ or ACQ/PEAK mode, skip to the end
	nstat = 0
	do n = 1, nobs {
	    if (strne (opmode[1,n], "ACQ") && strne (opmode[1,n], "ACQ/PEAK"))
		nstat = nstat + 1
	}
	if (nstat == 0) go to 10

	do n = 1, nobs {
	    if (n == 1) {
	        if (yoff > (BOTTOM-5.)) {
		    call pp_erase (fd)
		    call pp_banner (fd, str1, "", str2, stis, timetag, page)
		    yoff = 3
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Statistics List")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
		}
		call ostat_head (fd, yoff)
	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call pp_banner (fd, str1, "", str2, stis, timetag, page)
		yoff = 3
		call ostat_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # skip if it is ACQ or ACQ/PEAK mode
	    if (streq (opmode[1,n], "ACQ") || streq (opmode[1,n], "ACQ/PEAK"))
		next

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 7., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_label (fd, 25., yoff, detector[1,n])
	    call pp_label (fd, 31., yoff, opmode[1,n])

	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 43., yoff)
	    call fprintf (fd, dummy1)
		call pargr (exptime[n])

	    call pp_move (fd, 49., yoff)
	    call fprintf (fd, "label '%7d'\n")
		call pargi (npix[n])
	    call pp_move (fd, 54., yoff)
	    call fprintf (fd, "label '%7d'\n")
		call pargi (ngoodpix[n])
	    call pp_move (fd, 58., yoff)
	    call fprintf (fd, dummy1)
		call pargr (gmin[n])
	    call pp_move (fd, 63., yoff)
	    call fprintf (fd, dummy1)
		call pargr (gmax[n])
	    call pp_move (fd, 68., yoff)
	    call fprintf (fd, dummy1)
		call pargr (gmean[n])
	    call pp_move (fd, 73., yoff)
	    call fprintf (fd, "label '%5.1f'\n")
		call pargr (snrmin[n])
	    call pp_move (fd, 77., yoff)
	    call fprintf (fd, "label '%5.1f'\n")
		call pargr (snrmax[n])
	    call pp_move (fd, 81., yoff)
	    call fprintf (fd, "label '%5.1f'\n")
		call pargr (snrmean[n])
	    call fprintf (fd, "justify 3\n")

	    # if the target name is long, wrap it into two lines
	    if (targ2[1] != EOS) {
		yoff = yoff + DY
	        call pp_label (fd, 15., yoff, targ2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+DY)
	    call pp_draw (fd, 82., yoff+DY)
	    yoff = yoff + 1.
	}

	# draw the second of the double line
	call pp_move (fd, 0., yoff)
	call pp_draw (fd, 82., yoff)

	# close output file
10	call close (fd)
end

# produce observation list header for STIS

procedure oobs_head (fd, yoff)

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
	call pp_label (fd, 25., yoff, "Detector")

	call pp_label (fd, 31., yoff-DY/2., "Operating")
	call pp_label (fd, 31., yoff+DY/2., "Mode")
	call pp_label (fd, 39., yoff, "Aperture")
	call pp_label (fd, 48., yoff-DY/2., "Optical")
	call pp_label (fd, 48., yoff+DY/2., "Element")

	call pp_label (fd, 54., yoff-DY/2., "Cenwave")
	call pp_label (fd, 54., yoff+DY/2., "(A)")
	call pp_label (fd, 59., yoff-DY/2., "Total Exp")
	call pp_label (fd, 59., yoff+DY/2., "Time (s)")
	call pp_label (fd, 65., yoff-DY/2., "# of")
	call pp_label (fd, 65., yoff+DY/2., "Exp")

	call pp_label (fd, 69., yoff, "File")
	call pp_label (fd, 74., yoff-DY/2., "Quality Flags")
	call pp_label (fd, 74., yoff+DY/2., "Obs")
	call pp_label (fd, 77., yoff+DY/2., "Proc")
	call pp_label (fd, 80., yoff+DY/2., "Cal")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call pp_draw (fd, 82., yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call pp_draw (fd, 82., yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# produce optional parameter header for STIS

procedure ooption_head (fd, yoff)

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

	# Print Obervation List column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd,  7., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 25., yoff, "Detector")

	call pp_label (fd, 31., yoff-DY/2., "Operating")
	call pp_label (fd, 31., yoff+DY/2., "Mode")
	call pp_label (fd, 39., yoff, "Optional Parameters")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call pp_draw (fd, 82., yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call pp_draw (fd, 82., yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# produce Statistics List header for STIS

procedure ostat_head (fd, yoff)

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
	call fprintf (fd, "label '%sfIObservation Statistics'\n")
	    call pargstr ("\\")
	
	call fprintf (fd, "justify 3; expand %f\n")
	    call pargr (DY)
	yoff = yoff + 2.4

	# Print Obervation List column names
	call pp_label (fd,  0., yoff, "Visit-Exp#")
	call pp_label (fd,  7., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 25., yoff, "Detector")

	call pp_label (fd, 31., yoff-DY/2., "Operating")
	call pp_label (fd, 31., yoff+DY/2., "Mode")
	call pp_label (fd, 39., yoff-DY/2., "Total exp")
	call pp_label (fd, 39., yoff+DY/2., "Time (s)")

	call pp_label (fd, 45., yoff, "# pixels")
	call pp_label (fd, 50., yoff-DY/2., "# Good")
	call pp_label (fd, 50., yoff+DY/2., "pixels")

	call pp_label (fd, 55., yoff-DY/2., "Good")
	call pp_label (fd, 55., yoff+DY/2., "Min")
	call pp_label (fd, 60., yoff-DY/2., "Good")
	call pp_label (fd, 60., yoff+DY/2., "Max")
	call pp_label (fd, 65., yoff-DY/2., "Good")
	call pp_label (fd, 65., yoff+DY/2., "Mean")

	call pp_label (fd, 71., yoff-DY/2., "Min")
	call pp_label (fd, 71., yoff+DY/2., "S/N")
	call pp_label (fd, 75., yoff-DY/2., "Max")
	call pp_label (fd, 75., yoff+DY/2., "S/N")
	call pp_label (fd, 79., yoff-DY/2., "Mean")
	call pp_label (fd, 79., yoff+DY/2., "S/N")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call pp_draw (fd, 82., yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call pp_draw (fd, 82., yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# generate the optional parameter string

procedure ooption_string (ip, detector, mode, op_string)

pointer	ip		# input image pointer
char	detector[ARB]	# detector used
char	mode[ARB]	# observation mode
char	op_string[ARB]	# output option-parameter string

int	ival
real	rval
int	len
char	str[SZ_LINE]
char	str1[SZ_LINE], str2[SZ_LINE]

bool	streq()
bool	strne()
int	strncmp()
int	strlen()
int	imgeti()
real	imgetr()

begin

	# initialize the string
	op_string[1] = EOS
	
	# construct the option parameter string according to the
	# detector and operation mode

	## MAMA detector
	if (streq (detector, "FUV-MAMA") || streq (detector, "NUV-MAMA")) {

	    # ACQ/PEAKUP mode
	    if (streq (mode, "ACQ/PEAK")) {
		iferr (ival = imgeti(ip, "SIZAXIS2"))
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS2=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS2=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)
	    } else {

	        # ACCUM mode
	        if (strncmp (mode, "ACCUM", 5) == 0) {
		    iferr (ival = imgeti (ip, "BINAXIS1"))
		        call sprintf (str, SZ_OOPTION, "BINAXIS1=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "BINAXIS1=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (ival = imgeti (ip, "BINAXIS2"))
		        call sprintf (str, SZ_OOPTION, "BINAXIS2=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "BINAXIS2=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

	        # time-tag mode
	        } else if (streq (mode, "TIME-TAG")) {
		    iferr (ival = imgeti (ip, "BUFFTIME"))
		        call sprintf (str, SZ_OOPTION, "BUFF-TIME=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "BUFF-TIME=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)
	        }

		# common stuff
		iferr (call imgstr (ip, "PATTERN", str1, SZ_LINE)) {
		    call sprintf (str, SZ_OOPTION, "PATTERN=(N/A), ")
		    call strcpy ("NONE", str1, SZ_LINE)
		} else {
		    call sprintf (str, SZ_OOPTION, "PATTERN=%s, ")
		        call pargstr (str1)
		}
		call strcat (str, op_string, SZ_OOPTION)

		# if pattern is NONE, don't print the following keywords
	        if (strne (str1, "NONE")) {
		    iferr (ival = imgeti (ip, "NUMPOS"))
		        call sprintf (str, SZ_OOPTION, "NUM-POS=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "NUM-POS=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (rval = imgetr (ip, "STEPSIZE"))
		        call sprintf (str, SZ_OOPTION, "STEP-SIZE=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "STEP-SIZE=%0.1f, ")
		            call pargr (rval)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (rval = imgetr (ip, "BOXSCALE"))
		        call sprintf (str, SZ_OOPTION, "SCALING-FACTOR=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "SCALING-FACTOR=%0.1f, ")
		            call pargr (rval)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (call imgstr (ip, "PATTDRC", str2, SZ_LINE))
		        call sprintf (str, SZ_OOPTION, "PATTERN-DIRECTION=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "PATTERN-DIRECTION=%s, ")
		            call pargstr (str2)
		    }
		    call strcat (str, op_string, SZ_OOPTION)
		}

		iferr (ival = imgeti(ip, "SIZAXIS1"))
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS1=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS1=%d, ")
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

		iferr (ival = imgeti(ip, "CENTERA2"))
		    call sprintf (str, SZ_OOPTION, "CENTERAXIS2=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "CENTERAXIS2=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)
	    }

	} else if (streq (detector, "CCD")) {

	    # accum mode
	    if (strncmp (mode, "ACCUM", 5) == 0) {
		iferr (ival = imgeti (ip, "CRSPLIT"))
		    call sprintf (str, SZ_OOPTION, "CR-SPLIT=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "CR-SPLIT=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)

		iferr (ival = imgeti (ip, "CCDGAIN"))
		    call sprintf (str, SZ_OOPTION, "GAIN=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "GAIN=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)

		iferr (ival = imgeti (ip, "BINAXIS1"))
		    call sprintf (str, SZ_OOPTION, "BINAXIS1=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "BINAXIS1=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)

		iferr (ival = imgeti (ip, "BINAXIS2"))
		    call sprintf (str, SZ_OOPTION, "BINAXIS2=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "BINAXIS2=%d, ")
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

		iferr (call imgstr (ip, "PATTERN", str1, SZ_LINE)) {
		    call sprintf (str, SZ_OOPTION, "PATTERN=(N/A), ")
		    call strcpy ("NONE", str1, SZ_LINE)
		} else {
		    call sprintf (str, SZ_OOPTION, "PATTERN=%s, ")
		        call pargstr (str1)
		}
		call strcat (str, op_string, SZ_OOPTION)

		# if pattern is NONE, don't print the following keywords
	        if (strne (str1, "NONE")) {
		    iferr (ival = imgeti (ip, "NUMPOS"))
		        call sprintf (str, SZ_OOPTION, "NUM-POS=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "NUM-POS=%d, ")
		            call pargi (ival)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (rval = imgetr (ip, "STEPSIZE"))
		        call sprintf (str, SZ_OOPTION, "STEP-SIZE=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "STEP-SIZE=%0.1f, ")
		            call pargr (rval)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (rval = imgetr (ip, "BOXSCALE"))
		        call sprintf (str, SZ_OOPTION, "SCALING-FACTOR=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "SCALING-FACTOR=%0.1f, ")
		            call pargr (rval)
		    }
		    call strcat (str, op_string, SZ_OOPTION)

		    iferr (call imgstr (ip, "PATTDRC", str2, SZ_LINE))
		        call sprintf (str, SZ_OOPTION, "PATTERN-DIRECTION=(N/A), ")
		    else {
		        call sprintf (str, SZ_OOPTION, "PATTERN-DIRECTION=%s, ")
		            call pargstr (str2)
		    }
		    call strcat (str, op_string, SZ_OOPTION)
		}

	    # target acquisiton mode
	    } else if (streq (mode, "ACQ")) {
		iferr (call imgstr (ip, "ACQTYPE", str1, SZ_LINE))
		    call sprintf (str, SZ_OOPTION, "ACQTYPE=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "ACQTYPE=%s, ")
		        call pargstr (str1)
		}
		call strcat (str, op_string, SZ_OOPTION)

		iferr (call imgstr (ip, "CENTMETH", str1, SZ_LINE))
		    call sprintf (str, SZ_OOPTION, "DIFFUSE-CENTER=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "DIFFUSE-CENTER=%s, ")
		        call pargstr (str1)
		}
		call strcat (str, op_string, SZ_OOPTION)

		iferr (ival = imgeti (ip, "CHECKBOX"))
		    call sprintf (str, SZ_OOPTION, "CHECKBOX=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "CHECKBOX=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)

	    # ACQ/PEAKUP mode
	    } else if (streq (mode, "ACQ/PEAK")) {
		iferr (ival = imgeti(ip, "SIZAXIS2"))
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS2=(N/A), ")
		else {
		    call sprintf (str, SZ_OOPTION, "SIZEAXIS2=%d, ")
		        call pargi (ival)
		}
		call strcat (str, op_string, SZ_OOPTION)
	    } 
	}

	# strip the ending comma
	len = strlen (op_string)
	if (op_string[len-1] == ',') op_string[len-1] = EOS
end


procedure opp_title (root, raw, title1, title2)

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

	iferr (call imgstr (raw, "propttl1", title1, SZ_LINE)) {
	
		if (spt != NULL) {
	        	iferr (call imgstr (spt, "propttl1", title1, SZ_LINE))
	            	    title1[1] = EOS
		} else {
			    title1[1] = EOS
 		}
	}
        iferr (call imgstr (raw, "propttl2", title2, SZ_LINE)) {

		if (spt != NULL) {
	        	iferr (call imgstr (spt, "propttl2", title2, SZ_LINE))
	            	    title2[1] = EOS
		} else {
 	           	    title2[1] = EOS
		}
	}	

	# Close the SPT file
	call imunmap(spt)

end
