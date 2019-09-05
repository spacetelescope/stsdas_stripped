include	<imhdr.h>
include <imio.h>
include	"pplist.h"

define	SZ_HIST	4000

# produce target list and observation list for WFPC2

procedure upplist (tpin, tpimtype, output, selector, timetag, page)

pointer	tpin			# template of input files
char	output[SZ_FNAME]	# output igi script file name
pointer	tpimtype		# list of input file types (FITS or GEIS)
char	selector[SZ_LINE]	# output parts selector
char	timetag[SZ_LINE]	# time tag for the footnote
int	page			# page number counter for the footnote

char	imtype[SZ_FNAME]
int	fd
char	fname[SZ_FNAME]
char	root[SZ_FNAME]
pointer	shp, c0h
char	shp_ext[SZ_EXT]
char	c0h_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	visit[SZ_VISIT+1]
char	propid[SZ_LINE]
char	targ1[MAX_TARG], targ2[MAX_TARG]
#char	aper1[MAX_APER], aper2[MAX_APER]
bool	tfound
int	ntarg
real	yoff
int	nchar
int	n, k, nobs, ncom

# observation attributes
char	tltarg[SZ_TARG, MAX_OBS]
double	ra[MAX_OBS], dec[MAX_OBS]
char	desc[SZ_DESC, MAX_OBS]
char	linenum[SZ_LINENUM, MAX_OBS]
char	rootname[SZ_ROOT, MAX_OBS]
char	targ[SZ_TARG, MAX_OBS]
char	mode[SZ_UMODE, MAX_OBS]
char	aper[SZ_UAPER, MAX_OBS]
char	gain[SZ_UGAIN]
char	serial[SZ_USERIAL]
char	ser_gain[SZ_USERGAIN, MAX_OBS]
char	filter1[SZ_UFILT]
char	filter2[SZ_UFILT]
char	filtnam[SZ_UFILTNAM, MAX_OBS]
char	filtnam2[SZ_UFILTNAM, MAX_OBS]
bool	filt_split[MAX_OBS]
char	qual[SZ_LINE, MAX_OBS]
char	targext[SZ_LINE, MAX_OBS]
char	dummy[SZ_LINE,10]
real	exptime[MAX_OBS]
real	photplam[MAX_OBS]
char    first_name[SZ_LINE]             # PI's first name
char    last_name[SZ_LINE]              # PI's last name
char    title1[SZ_LINE], title2[SZ_LINE]        # proposal title
char    pdftitle[SZ_LINE]

# group specific parameters
int 	ngroups
real	detector[MAX_UGRPS, MAX_OBS]
real	photflam[MAX_UGRPS, MAX_OBS]
real 	backgrnd[MAX_UGRPS, MAX_OBS]
real 	goodmin[MAX_UGRPS, MAX_OBS]
real	goodmax[MAX_UGRPS, MAX_OBS]
real	maglim[MAX_UGRPS, MAX_OBS]
real	skysig[MAX_UGRPS, MAX_OBS]
real	dmin, dmax, xlim
real	hist[SZ_HIST]

int	calcheck[MAX_OBS]
char	ref[SZ_PED, SZ_EXT]
char 	ped[SZ_PED, SZ_EXT]
char 	cflags[SZ_LINE, SZ_EXT]
int	nref

int	upp_calflag()
pointer	immap()
int	open()
int	imtlen()
int	imtgetim()
real	imgetr()
double	imgetd()
bool	streq()
int	strmatch()
int	strlen()
int 	gf_gstfval()
int	access()
real	umag_limit()
bool	qual_check()


begin
        
    nobs = imtlen(tpin)
	tfound = false
	ntarg = 0
	
	# loop all root names
        do n = 1, nobs {

            # read the next input image name in the template list
            nchar = imtgetim (tpin, root, SZ_FNAME)

	    # construct necessary file name extensions
	    # (see 'lib/pp_roots.x' for list of possible imtypes)
	    nchar = imtgetim (tpimtype, imtype, SZ_FNAME)

	    if (streq(imtype,"fits")) {
	    	call strcpy ("_shf.fits[0]", shp_ext, SZ_EXT) 
	    	call strcpy ("_c0f.fits", c0h_ext, SZ_EXT) 
	    	call strcpy ("_d0f.fits", d0h_ext, SZ_EXT) 
	    } else {
	    	call strcpy (".shh", shp_ext, SZ_EXT) 
	    	call strcpy (".c0h", c0h_ext, SZ_EXT) 
	    	call strcpy (".d0h", d0h_ext, SZ_EXT) 
	    }

	    # construct file names
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (shp_ext, fname, SZ_FNAME)
	    shp = immap (fname, READ_ONLY, 0)

	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (c0h_ext, fname, SZ_FNAME)

	    if (access (fname, 0, 0) == NO) {
		# Try to find a d0h image to work from
		call strcpy (root, fname, SZ_FNAME)
		call strcat (d0h_ext, fname, SZ_FNAME)

		if (access (fname, 0, 0) == NO) {
			# if there is no d0h image or c0h image, exit
			c0h = NULL
			call error (1, "PPLIST: No images to process")
		} else {
			# otherwise, use the d0h image
			if(streq(imtype,"fits") )
				call strcat("[0]", fname, SZ_FNAME)
			c0h = immap (fname, READ_ONLY, 0)
		}
	    } else {
		# found a c0h image, so we use it...
		if(streq(imtype,"fits") )
			call strcat("[0]", fname, SZ_FNAME)
		c0h = immap (fname, READ_ONLY, 0)
	    }

	    # read target name
	    call imgstr (shp, "TARGNAME", targ[1,n], SZ_TARG)

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

		ra[ntarg] = imgetd (shp, "RA_TARG")
		dec[ntarg] = imgetd (shp, "DEC_TARG")
	        iferr (call imgstr (shp, "TARDESCR", desc[1,ntarg], SZ_DESC))
	     	    call strcpy ("(N/A)", desc[1,ntarg], SZ_DESC)
	    }

	    # read observation list related keywords
	    call imgstr (shp, "LINENUM", linenum[1,n], SZ_LINENUM)
	    call imgstr (shp, "ROOTNAME", rootname[1,n], SZ_ROOT)
	    call imgstr (c0h, "MODE", mode[1,n], SZ_UMODE)
	    call imgstr (c0h, "IMAGETYP", targext[1,n], SZ_LINE)

	    # read in SERIALS and ATODGAIN and concatenate into 1 string
	    call imgstr (c0h, "SERIALS", serial, SZ_USERIAL)
	    call imgstr (c0h, "ATODGAIN", gain, SZ_UGAIN)

	    call strcpy (serial, ser_gain[1,n], SZ_USERGAIN)
	    call strcat ("/",ser_gain[1,n], SZ_USERGAIN)
	    call strcat (gain, ser_gain[1,n], SZ_USERGAIN)


	    # read in aperture and filter info here
	    call imgstr (shp, "APER_1", aper[1,n], SZ_UAPER)
	    call imgstr (c0h, "FILTNAM1", filter1, SZ_UFILT)
	    iferr (call imgstr (c0h, "FILTNAM2", filter2, SZ_UFILT)) filter2[1] = EOS
	    call strcpy (filter1, filtnam[1,n], SZ_UFILTNAM)
	    if (filter2[1] != EOS) {
	    	call strcat (",", filtnam[1,n], SZ_UFILTNAM)
	    	call strcat (filter2, filtnam[1,n], SZ_UFILTNAM)
	    }
	    if (strlen(filtnam[1,n]) > MAX_FILT) {
		filt_split[n] = true
		call strcpy(filter1, filtnam[1,n], SZ_UFILTNAM)
		call strcpy(filter2, filtnam2[1,n], SZ_UFILTNAM)
	    } else {
		filt_split[n] = false
    	    }
	    exptime[n] = imgetr (c0h, "EXPTIME")
	    photplam[n] = imgetr (c0h, "PHOTPLAM")

	    if (streq(imtype,"geis")) {	    
	    	call pp_qual (root, qual[1,n], dummy[1,1], ncom)
	    } else {
		call strcat ("_pdq.fits",root,SZ_FNAME)
		call pp_qualfits (root, qual[1,n], dummy[1,1], ncom)
	    }
	
	    if (n == 1) {
	        call imgstr (shp, "LINENUM", visit, SZ_VISIT)
			# If LINENUM has a format of 'n.nnnn',
			# read in first two characters
			# and convert them from 'n.' to '0n'
			if(visit[2] == '.' ) {
				visit[2] = visit[1]
				visit[1] = '0'
			}
	        call imgstr (shp, "PROPOSID", propid, SZ_LINE)
                call imgstr (shp, "pr_inv_l", last_name, SZ_LINE)
                call imgstr (shp, "pr_inv_f", first_name, SZ_LINE)
                iferr (call imgstr (shp, "propttl1", title1, SZ_LINE))
                    title1[1] = EOS
                iferr (call imgstr (shp, "propttl2", title2, SZ_LINE))
                    title2[1] = EOS
	    }

	    # read in statistical keywords for each group
	   	# first determine the number of groups
#		call eprintf("IM_KERNEL = %d\n")
#			call pargi(IM_KERNEL(c0h))

	    # Determine the number of groups in the image
	    if (streq(imtype,"geis") ) {
		    ngroups = gf_gstfval (c0h, "GCOUNT")
	    } else {
		# For a 3-d FITS WFPC2 image
		    ngroups = IM_LEN(c0h, 3)
	    }

	    do k=1,ngroups {

		if (streq(imtype,"geis") ) {
		    call gf_opengr(c0h, k, dmin, dmax, 0)
		    detector[k,n] = imgetr (c0h, "DETECTOR")
		    backgrnd[k,n] = imgetr (c0h, "BACKGRND")
		    goodmin[k,n] =  imgetr (c0h, "GOODMIN")
		    goodmax[k,n] = imgetr (c0h, "GOODMAX")
		    photflam[k,n] = imgetr(c0h, "PHOTFLAM")

		    if (goodmin[k,n] == goodmax[k,n])
		    	call get_hist(c0h, 0.99, 0.01, hist, SZ_HIST, goodmin[k,n], goodmax[k,n])

		# End GEIS-specific image section
		} else {
		# We are working with FITS images, NOT GEIS
		    
		    # Get keyword values for this group only
		    call get_grinfo(fname, k, detector[k,n], backgrnd[k,n], goodmin[k,n], goodmax[k,n], photflam[k,n])
		    if (goodmin[k,n] == goodmax[k,n])
			call get_fithist(fname, k, hist, SZ_HIST, goodmin[k,n], goodmax[k,n])

		# End FITS-specific image section
		}

		if (goodmin[k,n] == goodmax[k,n]) {
			call eprintf ("UPPLIST: Blank Image\n")
		}

		if (strmatch(targext[1,n],"EXT") != 0) { 
		    maglim[k,n] = umag_limit(c0h, imtype, k, skysig[k,n], detector[k,n], photflam[k,n], exptime[n])
		} else {
		    maglim[k,n] = 0.
		    skysig[k,n] = 0.
		}

	    # Finished looping over groups...
	    }		

	    # determine the status of the calibration quality flag
	    calcheck[n] = upp_calflag (c0h, ref, ped, cflags, nref)

	    # close files
	    call imunmap (shp)
	    call imunmap (c0h)
	}

	# ---------------------------------------------
    # Start generating the output pages here
	# ---------------------------------------------    
	# open the output file
        fd = open (output, APPEND, TEXT_FILE)

	# print the cover page and the explanatory page
	# ---------------------------------------------
	if (strmatch(selector, "cover") != 0 || strmatch(selector,"all") != 0) {
		# print the banner
		# set page to -1 to prevent the page number from being printed
        call pp_banner (fd, "", "cover", "", "WFPC2", timetag, -1)
        # Set up document so that bookmarks are automatically visible
        call pp_pdfsetup(fd)

        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "Prop %s Cover Page")
            call pargstr(propid)
            
        call pp_pdfbook(fd, page, pdftitle)
         
		# Print the rest of the cover page
		call gen_cover (fd, propid, visit, last_name, first_name, title1, title2)        
        call pp_erase(fd)

        # advance the page number for the next page
        page = page + 1
	}
	# print the rest?
	if (strmatch(selector, "visit") == 0 && strmatch(selector, "all") == 0)
	    go to 10

	# print the target list
	call list_banner (fd, visit, propid, "WFPC2", yoff)

    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Target List")
        call pargstr(propid)
    call pp_pdfbook(fd, page, pdftitle)

	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, "WFPC2",
			yoff)

	xlim = 110

	# print observation list
	do n = 1, nobs {
	    if (n == 1) {
		    if (yoff > (BOTTOM-5.)) {
		        call pp_erase (fd)
  		        call list_banner (fd, visit, propid, "WFPC2", yoff)
		    }
		    call uobs_head (fd, yoff, xlim)
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Observation List")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
            page = page + 1
	    } else if (yoff > BOTTOM-1.) {
		    call pp_erase (fd)
		    call list_banner (fd, visit, propid, "WFPC2", yoff)
		    call uobs_head (fd, yoff, xlim)
                            
            page = page + 1
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 6., yoff, rootname[1,n])
	    call pp_label (fd, 17., yoff, targ1)
	    call pp_label (fd, 35., yoff, mode[1,n])
	    call pp_label (fd, 43., yoff, ser_gain[1,n])
	    call pp_label (fd, 53., yoff, aper[1,n])
	    call pp_label (fd, 63., yoff, filtnam[1,n])
	    call fprintf(fd,"justify 1\n")
	    call pp_rlabel (fd, 88., yoff, photplam[n])
	# print out exposure time here, with 1 only digit after the decimal
	    call pp_move (fd, 96., yoff)
	    call fprintf (fd, "label '%8.1f'\n")
		call pargr (exptime[n])
	    call fprintf (fd, "justify 2\n")

	    # do the quality flags
	    # first, let's see if the 'quality' keyword from the
	    # PDQ file say's 'OK' or 'NO-EVAL'
	    # otherwise, give an indication of an error
	    if (qual[1,n] != EOS) {
	        call pp_move (fd, 101., yoff+0.25)
	        if( qual_check(qual[1,n], "OK, NO-EVAL, NO-TLM, TM_GAP") ) {

		#call eprintf("Quality check: OK\n")
		    call fprintf (fd, "ptype 25 0; dot\n")
	        } else {

		#call eprintf("Quality check: Not OK!\n")
		    call fprintf (fd, "ptype 25 3; dot\n")
	        }

	    }
	    # Check the calibration quality information
	    # if there are no error messages created for the 
	    # 'calibration data quality summary', then indicate 'OK'
	    #
	    call pp_move (fd, 109., yoff+0.25)
	    if (calcheck[n] > 0) {
		call fprintf (fd, "ptype 25 3; dot\n")
	    } else {
		call fprintf (fd, "ptype 25 0; dot\n")
	    }

	    call fprintf (fd, "justify 3\n")
	    
	    # if the target name is long
	    if ( (targ2[1] != EOS) || filt_split[n]  )  {
		yoff = yoff + 0.75
	        call pp_label (fd, 17., yoff, targ2)
		if(filt_split[n]) call pp_label (fd, 63., yoff, filtnam2[1,n])
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+0.75)
	    call fprintf (fd, "draw %6.2f %6.2f\n")
		call pargr (xlim)
		call pargr (yoff+0.75)
	
	    yoff = yoff + 1.
	}

	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw %6.2f %6.2f\n")
	    call pargr (xlim)
	    call pargr (yoff)

	# flag caption
	call flag_caption (fd, yoff)

	#
	# Print out Statistics page 
	# start new page and print the banner
	#
	# reset 'yoff' to 0. for the new page (just for clarity)
	yoff=0.
	#
	call pp_erase (fd)
	call list_banner (fd, visit, propid, "WFPC2", yoff)

    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Image Statistics")
        call pargstr(propid)
    call pp_pdfbook(fd, page, pdftitle)
 
    page = page + 1

	# When the information becomes incorporated into the image headers,
	# place section here for printing out Special Requirements

	# print observation statistics section
	do n = 1, nobs {
	  do k=1, ngroups {
	    if (n == 1 && k == 1 && yoff < (BOTTOM-4.))
		call uobstat_head (fd, yoff)
	    # if line position is within 1 of the bottom, go to new page
	    if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, "WFPC2", yoff)
		call uobstat_head (fd, yoff)

        page = page + 1
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 6., yoff, rootname[1,n])
	    call pp_label (fd, 16., yoff, targ1)
	    call pp_label (fd, 32., yoff, filtnam[1,n])
	    
	    #
	    # Use 'pp_rlabel' to print out exptime as a real number
	    #
	    call fprintf(fd,"justify 1\n")

	    call pp_rlabel (fd, 50., yoff, exptime[n])
	    call fprintf(fd,"justify 3\n")

	    # put in spacers for POS TARG information here
	    call pp_label (fd, 53., yoff, "N/A")
	    call pp_label (fd, 58., yoff, "N/A")

	    # Put in group/chip specific information here
	    #
	    call pp_move(fd,67.,yoff)
	    call fprintf(fd,"label '%d'\n")
		call pargi(int(detector[k,n]))

	    call fprintf(fd,"justify 1\n")
	    call pp_rlabel (fd, 76., yoff, backgrnd[k,n])
	    # put in 'Sky Sigma' value
	    if(strmatch(targext[1,n],"EXT") != 0)  { 
	    	call pp_rlabel (fd, 84., yoff, skysig[k,n])
	    } else {
		call fprintf(fd,"justify 3\n")
		call pp_label(fd, 79., yoff, "N/A")
		call fprintf(fd, "justify 1\n")
	    }
	    call pp_rlabel (fd, 93., yoff, goodmin[k,n])
	    call pp_rlabel (fd, 101., yoff, goodmax[k,n])
	    # put in  "Limit Mag."
	    if(strmatch(targext[1,n],"EXT") != 0)  {	    
	    	call pp_rlabel (fd, 109., yoff, maglim[k,n])
	    } else {
		call fprintf(fd,"justify 3\n")
		call pp_label(fd, 104., yoff, "N/A")
	    }
	    call fprintf(fd,"justify 3\n")

    	    # if the target name is long
	    if ( (targ2[1] != EOS) || filt_split[n] ){
		yoff = yoff + 0.75
	        call pp_label (fd, 16., yoff, targ2)
	        if(filt_split[n]){
			 call pp_label (fd, 32., yoff, filtnam2[1,n])
	  	#	 call eprintf("filtnam1 = %s, filtnam2 = %s\n")
		#		call pargstr(filtnam[1,n])
		#		call pargstr(filtnam2[1,n])
		}
	    }

	  # end of ngroups loop
	  yoff = yoff + 1
	  }

	  yoff = yoff + 1
	# end of nobs loop
	}		

	# Close the output file
10	call close (fd)
end

# Produce an observation statistics list header for WFPC2

procedure uobstat_head (fd, yoff)

int fd
real yoff

real xlim

begin

	# set the limit for the line length
	xlim = 110.

	# initialize and title
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.01 0.98 0.05 0.95\n")
	call fprintf (fd, "limits 0. %6.2f %6.2f 0.\n")
	    call pargr (xlim)
	    call pargr (BOTTOM)
	call pp_move (fd, 51., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation Statistics'; expand 0.75; justify 3\n")
	    call pargstr ("\\")
	
	yoff = yoff + 2.

	# first line
	call pp_label (fd,  0., yoff, "Logsheet")
	call pp_label (fd, 44., yoff, "Exposure")
	call pp_label (fd, 71., yoff, "Modal")
	call pp_label (fd, 79., yoff, "Sky")
	call pp_label (fd, 103., yoff, "Limiting")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd, 0., yoff, "Line#")
	call pp_label (fd, 6., yoff, "Rootname")
	call pp_label (fd, 16., yoff, "Target Name")
	call pp_label (fd, 32., yoff, "Filter")
	call pp_label (fd, 44., yoff, "Time")
	call pp_label (fd, 53., yoff, "POSTARG")
	call pp_label (fd, 63., yoff, "Detector")
	call pp_label (fd, 71., yoff, "Sky")
	call pp_label (fd, 79., yoff, "Sigma")
	call pp_label (fd, 87., yoff, "Min DN")
	call pp_label (fd, 95., yoff, "Max DN")
	call pp_label (fd, 103., yoff, "Mag.")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call fprintf (fd, "draw %6.2f %6.2f\n")
		call pargr (xlim)
		call pargr (yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call fprintf (fd, "draw %6.2f %6.2f\n")
		call pargr (xlim)
		call pargr (yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5


end



# produce observation list header for WFPC2

procedure uobs_head (fd, yoff, xlim)

int	fd
real	yoff
real	xlim

begin

	# initialize and title
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. %6.2f %6.2f 1.\n")
	    call pargr (xlim)
	    call pargr (BOTTOM)
	call pp_move (fd, 51., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List'; expand 0.75; justify 3\n")
	    call pargstr ("\\")
	
	yoff = yoff + 2.

	# first line
	call pp_label (fd,  0., yoff, "Logsheet")
	call pp_label (fd, 35., yoff, "Operating")
	call pp_label (fd, 43., yoff, "Serials/")
	call pp_label (fd, 53., yoff, "Aperture")
		call fprintf (fd, "justify 1\n")
	call pp_label (fd, 96., yoff, "Exposure")
		call fprintf (fd, "justify 3\n")
	call pp_label (fd, 100., yoff, "Quality Flags")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd, 0., yoff, "Line#")
	call pp_label (fd, 6., yoff, "Rootname")
	call pp_label (fd, 17., yoff, "Target Name")
	call pp_label (fd, 35., yoff, "Mode")
	call pp_label (fd, 43., yoff, "Gain")
	call pp_label (fd, 53., yoff, "or FOV")
	call pp_label (fd, 63., yoff, "Filters")
	call pp_label (fd, 80., yoff, "Wavelength")
	call fprintf (fd, "justify 1\n")
	call pp_label (fd, 96., yoff, "(sec)")
	call fprintf (fd, "justify 2\n")
	call pp_label (fd, 101., yoff, "Obs")
	call pp_label (fd, 105., yoff, "Proc")
	call pp_label (fd, 109., yoff, "Cal")

	call fprintf (fd, "justify 3\n")
	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call fprintf (fd, "draw %6.2f %6.2f\n")	
		call pargr (xlim)
		call pargr (yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call fprintf (fd, "draw %6.2f %6.2f\n")
		call pargr (xlim)
		call pargr (yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end

# For each group determine the 'Sky Sigma' and return the value of
#	'Limiting Mag'
# Calculate mean/mode and sigma for each section using clip_mean()
#
define	NSTEPS	12
real procedure umag_limit (img, imtype, group, sigma, det, pflam, exp)

pointer	img
char	imtype[ARB]
int	group
real	sigma
real	maglimit, pflam, det, exp


pointer	vec
int	npix, delta
int	n
int	naxis
int	x[NSTEPS],y[NSTEPS]
real	im_mean, im_sig[NSTEPS]
real	sharp, cts, scale
int	xmin, xmax, ymin, ymax


data	x/100,300,500,700,700,700,700,500,300,100,100,100/
data	y/100,100,100,100,300,500,700,700,700,700,500,300/

real	clip_mean()
pointer	imgs2r()
pointer	imgs3r
real	amedr()
bool	streq()


begin
	
	delta = 10
	naxis = IM_LEN(img, 1)
 	scale = naxis/800.
	npix = (2*delta + 1) * (2*delta + 1)

	do n =1,12 {
		xmin = int(x[n]*scale) - 10
		ymin = int(y[n]*scale) - 10
		xmax = int(x[n]*scale) + 10
		ymax = int(y[n]*scale) + 10

		if (streq(imtype, "geis") ) {
			vec = imgs2r(img, xmin, xmax, ymin, ymax)
		} else {
			vec = imgs3r(img, xmin, xmax, ymin, ymax, group, group)
		}
		im_mean = clip_mean(vec, npix, im_sig[n])
	}	

	sigma = amedr(im_sig, NSTEPS)
#
# Values of sharp corrected 23 Jan 98. WJH
#
	if (det > 1.) 
		sharp = 9.
	else
		sharp = 15.	
			
	if (exp > 0.) {

		cts = 3. * sigma * sqrt(sharp) * pflam / exp

		if (cts > 0.)
			maglimit = -2.5 * log10(cts) - 21.10		
		else
			maglimit = 0.
	} else {
		maglimit = 0.
	}

	return(maglimit)
end

procedure get_grinfo(fname, group, det, back, gmin, gmax, photflam)

char	fname[SZ_FNAME]
int	group
real	det, back, gmin, gmax, photflam

pointer gtab, cptr
char	tabname[SZ_FNAME]
int	extnum

pointer	tbtopn()

begin
	# We want to access FITS extension '[1]' as a table
	extnum = 1

	# Create new filename with FITS extension of '[1]'
	call get_fitextn(fname, extnum, tabname)
	
	# Open table
	gtab = tbtopn(tabname, READ_ONLY, NULL)

	# Get information from each column here, from rownum = group
	# Consider adding a check to make sure the column is found	
	call tbcfnd (gtab, "DETECTOR", cptr, 1)
	call tbegtr (gtab, cptr, group, det)

	call tbcfnd (gtab, "BACKGRND", cptr, 1)
	call tbegtr (gtab, cptr, group, back)

	call tbcfnd (gtab, "GOODMIN", cptr, 1)
	call tbegtr (gtab, cptr, group, gmin)

	call tbcfnd (gtab, "GOODMAX", cptr, 1)
	call tbegtr (gtab, cptr, group, gmax)

	call tbcfnd (gtab, "PHOTFLAM", cptr, 1)
	call tbegtr (gtab, cptr, group, photflam)
	
	# Close table
	call tbtclo(gtab) 

end

procedure get_fitextn (fname, extnum, extname)

char	fname[ARB]		# Input file name with original FITS extension
char	extname[ARB]		# Output file name with new FITS extension
int	extnum			# New FITS extension number to be accessed

char	fitext[SZ_FNAME]
char	extstr[SZ_FNAME]
int	flen
int	nchar

int	strlen()
int	stridx()
int	itoc()

begin
	# Find first occurance of '[' in fname
	flen = stridx("[",fname)
	
	# If there is no extension in the given filename,
	#	start with the entire name
	if (flen == 0) {
		flen = strlen(fname)
	} else {
		flen = flen - 1
	}

	# Build table name
	# Copy over the file name without the FITS extension '[0]'
	call strcpy (fname, extname, flen)
	
	# Build extension string 
	call strcpy("[", fitext, SZ_FNAME)

	nchar = itoc (extnum, extstr, SZ_FNAME)
	call strcat (extstr, fitext, SZ_FNAME)

	call strcat ("]", fitext, SZ_FNAME)

	# Append extension string to filename
	call strcat(fitext, extname, SZ_FNAME)
	
end

procedure get_fithist(fname, group, hist, sizehist, gmin, gmax)

char	fname[ARB]
int	group
real	hist[ARB]
int	sizehist
real	gmin, gmax

char	fitname[SZ_FNAME]
pointer	foh

pointer	immap()

begin
	# Create file name for FITS file that accesses only one group
	# at a time
	call sprintf(fitname,SZ_FNAME,"%s[*,*,%d]")
		call pargstr(fname)
		call pargi(group)
		
	# Open this section
	foh = immap (fitname, READ_ONLY, 0)
	
	call get_hist(foh, 0.99, 0.01, hist, sizehist, gmin, gmax)

	# Close section
	call imunmap(foh)	
end
