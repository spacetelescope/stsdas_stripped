include	<imhdr.h>
include	"pplist.h"

# produce target list and observation list for FOC

procedure xpplist (tpin, tpimtype, output, selector, timetag, page)

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
char	c1h_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	visit[SZ_VISIT+1]
char	propid[SZ_LINE]
char	targ1[MAX_TARG], targ2[MAX_TARG]
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
char	filter1[SZ_XFILTNAM]
char	filter2[SZ_XFILTNAM]
char	filtnam1[SZ_XFILTNAM, MAX_OBS]
char	filtnam2[SZ_XFILTNAM, MAX_OBS]
bool	filt_split[MAX_OBS]
char	qual[SZ_LINE, MAX_OBS]
char	dummy[SZ_LINE,10]
real	exptime[MAX_OBS]
real	imgmax[MAX_OBS], maxrate[MAX_OBS]
real	imgmean[MAX_OBS], imgmeanrt[MAX_OBS]
int	naxis1, naxis2, im_min1, im_max1, im_min2, im_max2, npix
pointer	xsect
real	im_max, im_sig
bool	noaper
int	calcheck[MAX_OBS]
char    ref[SZ_PED, SZ_EXT]
char    ped[SZ_PED, SZ_EXT]
char    cflags[SZ_LINE, SZ_EXT]
int     nref

char	img_name[SZ_FNAME]
char    first_name[SZ_LINE]             # PI's first name
char    last_name[SZ_LINE]              # PI's last name
char    title1[SZ_LINE], title2[SZ_LINE]        # proposal title
char    pdftitle[SZ_LINE]

char	detector[SZ_LINENUM, MAX_OBS]
char	pxzoom[SZ_LINENUM, MAX_OBS]
char	pxformt[SZ_XFILTNAM, MAX_OBS]
real 	xlim

pointer	immap()
int	open()
int	imtlen()
int	imtgetim()
real	imgetr()
double	imgetd()
bool	streq()
int	strmatch()
bool	read_xfilt()
pointer	xpp_c0h()
real	clip_mean()
pointer	imgs2r()
real	ahivr()
int	xpp_calflag()
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

	    # construct necessary file name extensions
	    if (streq(imtype,"fits") ) {
		call strcpy ("_shf.fits[0]", shp_ext, SZ_EXT) 
		call strcpy ("_c0f.fits", c0h_ext, SZ_EXT) 
		call strcpy ("_c1f.fits", c1h_ext, SZ_EXT) 
		call strcpy ("_d0f.fits", d0h_ext, SZ_EXT) 
	    } else {
		call strcpy (".shh", shp_ext, SZ_EXT) 
		call strcpy (".c1h", c1h_ext, SZ_EXT) 
		call strcpy (".c0h", c0h_ext, SZ_EXT) 
		call strcpy (".d0h", d0h_ext, SZ_EXT) 
	    }

	    # construct file names
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (shp_ext, fname, SZ_FNAME)
	    shp = immap (fname, READ_ONLY, 0)
	
	    c0h = xpp_c0h (root, img_name, d0h_ext, c0h_ext, c1h_ext, imtype, c1h_ext)

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

	    # read in filter info here
	    # read_xfilt will always return at least NULL for filter2
	    filt_split[n] = read_xfilt(c0h, filter1, filter2)
		call strcpy(filter1,filtnam1[1,n], SZ_XFILTNAM)
		call strcpy(filter2,filtnam2[1,n], SZ_XFILTNAM)

	    exptime[n] = imgetr (c0h, "EXPTIME")
	    if (streq(imtype,"geis")) {	    
	    	call pp_qual (root, qual[1,n], dummy[1,1], ncom)
	    } else {
		call strcat ("_pdq.fits",root,SZ_FNAME)
		call pp_qualfits (root, qual[1,n], dummy[1,1], ncom)
	    }
	    #call pp_qual (root, qual[1,n], dummy[1,1], ncom)

	    # read in statistical keywords
	    call imgstr (shp, "CONFIG", detector[1,n], SZ_LINENUM)
	    #call imgstr (shp, "OPMODE", opmode[1,n], SZ_LINENUM)
	    call imgstr (c0h, "PXFORMT", pxzoom[1,n], SZ_LINENUM)
	    noaper = FALSE
	    iferr(call imgstr (shp, "APER_1", pxformt[1,n],SZ_XFILTNAM) ) {
		call strcpy("N/A", pxformt[1,n], SZ_XFILTNAM)	    
		noaper = TRUE
	    }

	    if(strmatch(pxzoom[1,n],"ZOOM") != 0 && !noaper ) {
		call strcat("z",pxformt[1,n],SZ_XFILTNAM)
	    }

	    # determine the status of the calibration quality flag
	    calcheck[n] = xpp_calflag (c0h, ref, ped, cflags, nref)
	
	    # Perform statistical calculations here...
	    #imgmax[n] = IM_MAX[c0h]

	    naxis1 = IM_LEN(c0h, 1)
	    naxis2 = IM_LEN(c0h, 2)

	    call det_xsection(naxis1, naxis2, im_min1, im_max1, im_min2, im_max2)
	    npix = (im_max1 - im_min1 + 1) * (im_max2 - im_min2 + 1)

	    xsect = imgs2r(c0h, im_min1, im_max1, im_min2, im_max2)
	    im_max = ahivr(Memr[xsect], npix)

	    imgmean[n] = clip_mean(xsect, npix, im_sig)
	    imgmax[n] = im_max

	    if (exptime[n] > 0.) {
	    	imgmeanrt[n] = imgmean[n] / exptime[n] * 1000.
	    	maxrate[n] = imgmax[n] / exptime[n]
	    } else {
		imgmeanrt[n] = 0.
		maxrate[n] = 0.
	    }

	    # close files
	    call imunmap (shp)
	    call imunmap (c0h)
	}

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	if (strmatch(selector, "cover") != 0 || strmatch(selector,"all") != 0) {
		# print the banner
		# set page to -1 to prevent the page number from being printed
		call pp_banner (fd, "", "cover", "", "FOC", timetag, -1) 
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
	call list_banner (fd, visit, propid, "FOC", yoff)

    # Insert PDF bookmark for this page here
    # First, build title string from rootname
    call sprintf(pdftitle, SZ_LINE, "Prop %s Target List")
        call pargstr(propid)
    call pp_pdfbook(fd, page, pdftitle)

	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, "FOC",
			yoff)

	xlim = 110

	# print observation list
	do n = 1, nobs {
	    if (n == 1) {
		    if (yoff > (BOTTOM-5.)) {
		        call pp_erase (fd)
  		        call list_banner (fd, visit, propid, "FOC", yoff)
		    }
		    call xobs_head (fd, yoff, xlim)
            # Insert PDF bookmark for this page here
            # First, build title string from rootname
            call sprintf(pdftitle, SZ_LINE, "Prop %s Observation List")
                call pargstr(propid)

            call pp_pdfbook(fd, page, pdftitle)
        
            page = page + 1
	    } else if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, "FOC", yoff)
		call xobs_head (fd, yoff, xlim)
            page = page + 1
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 8., yoff, rootname[1,n])
	    call pp_label (fd, 19., yoff, targ1)
	    #call pp_label (fd, 37., yoff, opmode[1,n])
	    call pp_label (fd, 42., yoff, detector[1,n])
	    call pp_label (fd, 52., yoff, pxformt[1,n])
	    call pp_label (fd, 67., yoff, filtnam1[1,n])
	# print out exposure time here, with 1 only digit after the decimal
	    call fprintf (fd, "justify 1\n")
	    call pp_rlabel (fd, 88., yoff, exptime[n])
	    call fprintf (fd, "justify 2\n")

	    # do the quality flags
	    # first, let's see if the 'quality' keyword from the
	    # PDQ file say's 'OK' or 'NO-EVAL'
	    # otherwise, give an indication of an error
	    if (qual[1,n] != EOS) {
	        call pp_move (fd, 93., yoff+0.25)
	        if( qual_check(qual[1,n], "OK; NO-EVAL, NO-TLM; TM_GAP") ) {

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
		call pp_move (fd, 104., yoff+0.25)
	    if (calcheck[n] >= 0){
		if (calcheck[n] > 0) {
			call fprintf (fd, "ptype 25 3; dot\n")
	    	} else {
			call fprintf (fd, "ptype 25 0; dot\n")
	    	}
	    }	
	    call fprintf (fd, "justify 3\n")
	    
	    # if the target name is long
	    if ( (targ2[1] != EOS) || filt_split[n] ){
		yoff = yoff + 0.75
	        call pp_label (fd, 19., yoff, targ2)
		if (filt_split[n] ) call pp_label (fd, 67., yoff, filtnam2[1,n])
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
	call list_banner (fd, visit, propid, "FOC", yoff)

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
	    if (n == 1 && yoff < (BOTTOM-4.))
		call xobstat_head (fd, yoff)
	    # if line position is within 1 of the bottom, go to new page
	    if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, "FOC", yoff)
		call xobstat_head (fd, yoff)
        page = page + 1
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 8., yoff, rootname[1,n])
	    call pp_label (fd, 19., yoff, targ1)
	    call pp_label (fd, 37., yoff, pxformt[1,n])
	    #
	    # Use 'pp_rlabel' to print out exptime as a real number
	    #
	    call fprintf(fd,"justify 1\n")

	    call pp_rlabel (fd, 58., yoff, exptime[n])
	#    call fprintf(fd,"justify 3\n")

	#    if (strsearch(pxzoom[1,n],"ZOOM") > 0 ) {
	#	call pp_label (fd, 57., yoff, "50X25")
	#    } else {	
	#	call pp_label (fd, 57., yoff, "25X25")
	#    }

	#   call fprintf(fd, "justify 1\n")

	    call pp_rlabel (fd, 70., yoff, imgmean[n])
	    call pp_rlabel (fd, 82., yoff, imgmeanrt[n])
	    call pp_rlabel (fd, 94., yoff, imgmax[n])
	    call pp_rlabel (fd, 106., yoff, maxrate[n])

	    call fprintf(fd, "justify 3\n")
	
    	    # if the target name is long
	    if (targ2[1] != EOS) {
		yoff = yoff + 0.75
	        call pp_label (fd, 19., yoff, targ2)
	    }


	  yoff = yoff + 1
	# end of nobs loop
	}		

10	call close (fd)
end

# Produce an observation statistics list header for FOC

procedure xobstat_head (fd, yoff)

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
	call pp_label (fd, 37., yoff, "Image")
	call pp_label (fd, 52., yoff, "Exposure")
	#call pp_label (fd, 57., yoff, "Pixel")
	call pp_label (fd, 75., yoff, "Backgd. Count")
	call pp_label (fd, 99., yoff, "Max Count")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd, 0., yoff, "Line#")
	call pp_label (fd, 8., yoff, "Rootname")
	call pp_label (fd, 19., yoff, "Target Name")
	call pp_label (fd, 37., yoff, "Format")
	call pp_label (fd, 55.5, yoff, "(sec)")
	#call pp_label (fd, 57., yoff, "Size")
	call pp_label (fd, 65., yoff, "Backgd.")
	call pp_move (fd, 77., yoff)
	call fprintf (fd, "label Rate x 10\n")
	call pp_move (fd, 83.5, yoff-0.15)
	call fprintf (fd, "expand 0.5; label -3; expand 0.75\n")
	call pp_label (fd, 87., yoff, "Max Count")
	call pp_label (fd, 103.5,yoff,"Rate")

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



# produce observation list header for FOC

procedure xobs_head (fd, yoff, xlim)

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
	#call pp_label (fd, 37., yoff, "Operating")

	call pp_label (fd, 52., yoff, "Image")
	call pp_label (fd, 81., yoff, "Exposure")
	call pp_label (fd, 92., yoff, "Quality Flags")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd, 0., yoff, "Line#")
	call pp_label (fd, 8., yoff, "Rootname")
	call pp_label (fd, 19., yoff, "Target Name")
	#call pp_label (fd, 36., yoff, "Mode")

	call pp_label (fd, 42., yoff, "Config.")
	call pp_label (fd, 52., yoff, "Format")
	call pp_label (fd, 67., yoff, "Filters")
	call pp_label (fd, 84.5, yoff, "(sec)")
	call pp_label (fd, 92., yoff, "Obs")
	call pp_label (fd, 97., yoff, "Proc")
	call pp_label (fd, 102., yoff, "Cal")

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


bool procedure read_xfilt (c0h, filt1, filt2)

int	c0h
char	filt1[ARB]
char	filt2[ARB]
bool	split
int	totlen
char	optcrly[3]

char	filter[SZ_XFILTNAM]

int	strsearch()
int	strlen()
bool	streq()

begin
	# initialize variables to reasonable values
	split = false
	call strcpy("", filt1,1)
	call strcpy("", filt2,1)
	call imgstr (c0h, "OPTCRLY", optcrly, 3)

	# read in first filter name
	call imgstr (c0h, "FILTNAM1", filter, SZ_UFILT)
	if (strsearch(filter,"CLEAR") == 0) 
		call strcpy (filter, filt1, SZ_XFILTNAM)


	call imgstr (c0h, "FILTNAM2", filter, SZ_UFILT)
	if (strsearch(filter,"CLEAR") == 0) {
		if (strlen(filt1) > 0) {
                	call strcat (",", filt1, SZ_XFILTNAM)
                	call strcat (filter, filt1, SZ_XFILTNAM)
		} else {
			call strcpy (filter, filt1, SZ_XFILTNAM)
		}
	}
  
	if (!streq(optcrly,"F48") ) {
		call imgstr (c0h, "FILTNAM3", filter, SZ_UFILT)
		if (strsearch(filter,"CLEAR") == 0) {
			totlen = strlen(filt1) + strlen(filter)
			if (totlen <= SZ_XFILTNAM){
				if (strlen(filt1) > 0) {
   		             		call strcat (",", filt1, SZ_XFILTNAM)
     		           		call strcat (filter, filt1, SZ_XFILTNAM)
				} else {
					call strcpy (filter, filt1, SZ_XFILTNAM)
				}
			} else {
				call strcpy(filter, filt2, SZ_XFILTNAM)
				split = true
			}
		}

		call imgstr (c0h, "FILTNAM4", filter, SZ_UFILT)
		if (strsearch(filter,"CLEAR") == 0){
			totlen = strlen(filt1) + strlen(filter)
			if (totlen <= SZ_XFILTNAM){
				if (strlen(filt1) > 0) {
   		             		call strcat (",", filt1, SZ_XFILTNAM)
     		           		call strcat (filter, filt1, SZ_XFILTNAM)
				} else {
					call strcpy (filter, filt1, SZ_XFILTNAM)
				}
			} else {
				if (!split ) {
					call strcpy(filter, filt2, SZ_XFILTNAM)
					split = true
				} else {
					call strcat (",", filt2, SZ_XFILTNAM)
     		           		call strcat (filter, filt2, SZ_XFILTNAM)
				}
			}
		}
	}
	if (strlen(filt1) == 0) {
		call strcpy("CLEAR",filt1,SZ_XFILTNAM)
	}
	return (split)

end

#procedure det_xsection (img, axis1, axis2, x1, x2, y1, y2)
procedure det_xsection (axis1, axis2, x1, x2, y1, y2)

#pointer	img
int	x1, x2, y1, y2
int	axis1, axis2
int	max1, max2, min1, min2

begin

	if (axis1 > XSECT_MAX && axis2 > XSECT_MAX) {
		x1 = XSECT_MIN
		x2 = XSECT_MAX
		y1 = XSECT_MIN
		y2 = XSECT_MAX

	} else {
		max1 = int(axis1/10.)
		max2 = int(axis2/10.)
		min1 = int(axis1 * 0.9)
		min2 = int(axis1 * 0.9)
		x1 = max (XIMG_BORD, max1)
		x2 = min (axis1 - XIMG_BORD, min1)

		y1 = max (XIMG_BORD, max2)
		y2 = min (axis2 - XIMG_BORD, min2)

		}


end

# produce the target/observation list banner
 
procedure blnk_banner (fd, propid, instru, yoff)
 
int     fd
char	propid[ARB]
char    instru[ARB]
real    yoff
 
begin
        call fprintf (fd, 
                "reset; fontset hard; vpage 0.0 1 0.05 0.98; expand 1.\n")
        call fprintf (fd, "location 0 1 0 1\n")
 
        # print the shading
        call fprintf (fd, 
                "!printf ('0 .94%sn1 .94%sn1 1%sn0 1%sn',> 'tmp$prop%s')\n")
            call pargstr ("\\")
            call pargstr ("\\")
            call pargstr ("\\")
            call pargstr ("\\")
            call pargstr (propid)
        call fprintf (fd, "data tmp$prop%s\n")
            call pargstr (propid)
        call fprintf (fd, "xcol c1; ycol c2; color 5; fillpat 2; polygon\n")
        call fprintf (fd, "color 1; fillpat 1\n")
        call fprintf (fd, "!delete tmp$prop%s verify-\n")
            call pargstr (propid)
        call fprintf (fd, "vmove 0.98 0.98; justify 1\n")
        call fprintf (fd, "label '%sfI%s'\n")
            call pargstr ("\\")
            call pargstr (instru)
 
        yoff = 3.
end
