# Do the FOC Spacecraft Performance, Pipeline Processing, and Exposure 
# Summaries.

include <tbset.h>
include <imhdr.h>
include	"xpp.h"

procedure xpp_obsum ()

char    rootname[SZ_FNAME]
char    output[SZ_FNAME]
char    ftype[SZ_FNAME]
int     page

int     fd
char    fname[SZ_FNAME]
char	pdqname[SZ_FNAME]
char    img_root[SZ_FNAME]
pointer	c0h, shh, jih
char    c0h_ext[SZ_EXT]
char    c1h_ext[SZ_EXT]
char    d0h_ext[SZ_EXT]
char    shh_ext[SZ_EXT]
char    img_ext[SZ_EXT]
char	jit_fname[SZ_FNAME]
char	img_name[SZ_FNAME]
char    linenum[SZ_LINENUM]
char    propid[SZ_LINE]
char	qual[SZ_LINE]
char	qualcom[SZ_LINE,10]
char	com1[SZ_LINE], com2[SZ_LINE]
real	x1, x2, y0, yoff, ycom, ylim
int	i, ncom
char    pdftitle[SZ_LINE]

char	ref[SZ_PED, MAX_EXT]
char 	ped[SZ_PED, MAX_EXT]
int	nref

pointer	immap()
int     open()
bool	streq()
int     clgeti()


pointer	get_jitname()
pointer	xpp_c0h()
real	xexp_summary()
#------------------------------------------------------------------------------
begin
        # read parameters
        call clgstr ("rootname", rootname, SZ_FNAME)
        call clgstr ("output", output, SZ_FNAME)
        call clgstr ("fits", ftype, SZ_FNAME)
        page = clgeti ("page")

        # construct necessary file name extensions
        if (streq(ftype,"fits")) {
            call strcpy ("_c0f.fits", c0h_ext, SZ_EXT)
            call strcpy ("_c1f.fits", c1h_ext, SZ_EXT)
            call strcpy ("_shf.fits[0]", shh_ext, SZ_EXT)
	    call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
        } else {
            call strcpy (".c0h", c0h_ext, SZ_EXT)
            call strcpy (".c1h", c1h_ext, SZ_EXT)
	    call strcpy (".d0h", d0h_ext, SZ_EXT) 
            call strcpy (".shh", shh_ext, SZ_EXT)
        }
 
        # construct file names
	c0h = xpp_c0h(rootname, img_name, d0h_ext, c0h_ext, c1h_ext, ftype, img_ext)

        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (shh_ext, fname, SZ_FNAME)
        shh = immap (fname, READ_ONLY, 0)

	
	jih = get_jitname(rootname, jit_fname)

        # read keywords
        call imgstr (shh, "ROOTNAME", img_root, SZ_FNAME)
	#call strcat (img_ext, img_root, SZ_FNAME)

        call imgstr (shh, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (shh, "PROPOSID", propid, SZ_LINE)

	# read in pedigree information from history records
	call pp_pedigree (c0h, ref, ped, nref)

        # open the output file
        fd = open (output, NEW_FILE, TEXT_FILE)
 
        # start a new page
        call pp_erase (fd)
        # Insert PDF bookmark for this page here
        # First, build title string from rootname
        call sprintf(pdftitle, SZ_LINE, "Calibration Summary")
        call pp_pdfbook(fd, page, pdftitle)
  
        # draw the banner
        call obs_banner (fd, linenum, img_root, propid, "FOC", yoff)
        call fprintf (fd, "reset; fontset hard\n")
	
	# draw in double lines for bottom section
	call fprintf (fd, "vpage 0 1 0 1; location 0 1 0 1;limits 0 1 0 1\n")
	call fprintf (fd, "move 0 0.30; draw 0.98 0.30\n")
	call fprintf (fd, "move 0 0.31; draw 0.98 0.31\n")

	# Jitter summary box
	call fprintf (fd, "vpage 0.0 0.65 0.77 0.92\n")
        call fprintf (fd, "limits 0 60 8 0\n")
        call fprintf (fd, "move 0 0; justify 3\n")
        call fprintf (fd, "label '%sfIHST Spacecraft Performance Summary'")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.75\n")
        call pp_label (fd,  0., 2.,"# Recenterings:")
        call pp_label (fd, 30., 2.,"# Losses of Locks:")
	call pp_fmtkw (fd, 15., 2., jih, "NRECENT", "%d")
	call pp_fmtkw (fd, 45., 2., jih, "NLOSSES", "%d")

        call pp_label (fd,  0., 3.,"V2 Jitter (RMS):")
        call pp_label (fd, 30., 3.,"V2 Jitter (PP):")
	call pp_fmtkw (fd, 15., 3., jih, "V2_RMS", "%0.1f")
	call pp_fmtkw (fd, 45., 3., jih, "V2_P2P", "%0.1f")

        call pp_label (fd,  0., 4.,"V3 Jitter (RMS):")
        call pp_label (fd, 30., 4.,"V3 Jitter (PP):")
	call pp_fmtkw (fd, 15., 4., jih, "V3_RMS", "%0.1f")
	call pp_fmtkw (fd, 45., 4., jih, "V3_P2P", "%0.1f")
	call fprintf (fd, "expand 0.7\n")
	
	if (streq(ftype,"geis")) {
		call pp_qual (rootname, qual, qualcom, ncom)
	} else {
		call strcpy(rootname, pdqname, SZ_FNAME)
		call strcat("_pdq.fits",pdqname,SZ_FNAME)
		call pp_qualfits (pdqname, qual, qualcom, ncom)
	}
	#call pp_qual (rootname, qual, qualcom, ncom)
	ycom = 5.
	do i = 1, ncom {
            call split_str (qualcom[1,i], com1, com2, 75)
            call pp_label (fd,  1., ycom+real(i), com1)
	    if (com2[1] != EOS) {
		ycom = ycom + 1.
            	call pp_label (fd,  5., ycom+real(i), com2)
	    }
	}

	# Calibration Data Quality Summary
	ylim = 8
	call xpipe_sum(fd, c0h, ylim)


	# Exposure Summary box
	call fprintf (fd, "vpage 0.65 1. 0.3 0.92\n")
	call fprintf (fd, "limits 0 30 30 0\n")
        call fprintf (fd, "move 0 0; draw 0 29\n")

	x1 = 2.
	x2 = 12.
	#y0 = 2.
	y0 = xexp_summary(fd, shh, c0h, x1, x2)

	#
	# Calibration Status Summary section
	#
	call xpp_calsum(fd, c0h, ref, ped, nref)

	#
	# set aside space for jitter plot here...
	#
	call xjit_plot(fd, jit_fname)

	# close images
	if (c0h != NULL) call imunmap (c0h)
	if (jih != NULL) call imunmap (jih)
	call imunmap (shh)

    # Finished with this page, advance page counter
    page = page + 1

    # Update the number of pages created
    call clputi("page", page)
    
        call close (fd)
	#call memlog("End of UPP_OBSUM...")
end


procedure xobssum (fd, x1, x2, y0, shh ,c0h)

int	fd
real	x1, x2, y0
pointer	shh, c0h

bool	filt_split
char	filter1[SZ_XFILTNAM]
char	filter2[SZ_XFILTNAM]
bool	POSTARG

bool	read_xfilt()

begin
	POSTARG = false

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "RA (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "ra_targ", "%0.2H")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Dec (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "dec_targ", "%0.1h")
	y0 = y0 + 1
	if (POSTARG) {
		call pp_label (fd, x1, y0, "X POSTARG:")
		call pp_fmtkw (fd, x2, y0, c0h, "xpostarg", "%s")
		y0 = y0 + 1
		call pp_label (fd, x1, y0, "Y POSTARG:")
		call pp_fmtkw (fd, x2, y0, c0h, "ypostarg", "%s")
		y0 = y0 + 1
	}
	call pp_label (fd, x1, y0, "V:")
	call pp_fmtkw (fd, x2, y0, shh, "MAG_V", "%0.2f")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "B-V:")
	call pp_fmtkw (fd, x2, y0, shh, "COL_B_V", "%0.2f")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Spec. Type:")
	call pp_fmtkw (fd, x2, y0, shh, "SP_TYPE", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Detector:")
	call pp_fmtkw (fd, x2, y0, shh, "CONFIG", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Filters:")
	filt_split = read_xfilt(c0h, filter1, filter2)
	call pp_label (fd, x2, y0, filter1)
	if (filt_split) {
		y0 = y0 + 1
		call pp_label (fd, x2, y0, filter2)
	}
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Aperture:")
	call pp_fmtkw (fd, x2, y0, shh, "aper_1", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Exp Time (sec):")
	call pp_fmtkw (fd, x2, y0, c0h, "exptime", "%0.1f")

	y0 = y0 + 2
	call pp_label (fd, x1, y0, "Rootname:")
	call pp_fmtkw (fd, x2, y0, shh, "rootname", "%s")
end



procedure get_xped (fd, x, y, refname, ref, ped, nref)
 
int     fd
real    x, y
char    refname[ARB]
char    ref[SZ_PED, ARB]
char    ped[SZ_PED, ARB]
int     nref
 
int     i
 
bool    streq()
 
begin
        do i = 1, nref {
            if (streq (refname, ref[1,i])) {
                call pp_label (fd, x, y, ped[1,i])
                break
            }
        }
end


procedure xpipe_sum(fd, c0h, ylim)

int	fd
pointer	c0h
real	ylim, ycom

int	calerr
char    ref[SZ_PED, SZ_EXT]
char    ped[SZ_PED, SZ_EXT]
char    cflags[SZ_LINE, SZ_EXT]
int     nref, i,j

int	xpp_calflag()

begin


	call fprintf (fd, "vpage 0.0 .65 0.3 0.45\n")
        call fprintf (fd, "limits 0 80 %f 0\n")
		call pargr (ylim)
	call fprintf (fd, "move 0 0; draw 80 0\n")
        call fprintf (fd, "move 0 0.5; justify 3\n")
	call fprintf (fd, "expand 1.\n")
        call fprintf (fd, "label '%sfIPipeline Processing and Calibration Data Quality Summary'\n")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.5\n")
	ycom = 0.5
	
	call read_xthrough(fd, c0h, ycom, ylim)

	#
	# Put in notes about calibration files used - determine flags
	#
	calerr =  xpp_calflag (c0h, ref, ped, cflags, nref)
	#calerr = 0
	j=1
	# ycom = 0.75
	if (calerr > 0) {
		# make sure we don't try to print out more messages
		# than we have space for...
	 	if (calerr > ((ylim-ycom)-2) ) calerr = (ylim-ycom) - 2
		do i = 1, calerr {
			call pp_label (fd, 44., 1.5+real(i*0.75), cflags[1,i])
	#		call eprintf ("%s\n")
	#		call pargstr(cflags[1,i])
		j = i
		}
	} else {
		call pp_label (fd, 44., 2.5, "No Anomalies.")
	}

	if (nref == 0)
		call pp_label (fd, 1., ycom+real((j-1)*0.75)+1.0, "No Calibration File Pedigree Information available.")

	call fprintf (fd, "expand 0.7\n")

end

real procedure xexp_summary (fd, shh, c0h, x1, x2)

int	fd
pointer	shh, c0h
real	x1,x2

real	y0
char	targ1[SZ_LINE], targ2[SZ_LINE]
char	targname[SZ_LINE]
char	date1[SZ_LINE], date2[SZ_LINE]

begin
	y0=2.

	# Headings.
	call fprintf (fd, "expand 1.\n")
	call fprintf (fd, 
		"move 15 0; justify 2; label '%sfIExposure Summary'\n")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.8\n")

	# Exposure Summary Values
	call fprintf (fd, "justify 3\n")
	call pp_label (fd, x1, y0, "Target Name:")
        call imgstr (shh, "TARGNAME", targname, SZ_LINE)
        call split_str (targname, targ1, targ2, 15)
        call pp_label (fd,  x2, y0, targ1)
        if (targ2[1] != EOS) {
            y0 = y0 + 1.
            call pp_label (fd,  x2, y0, targ2)
        }

	call xobssum (fd, x1, x2, y0, shh ,c0h)

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Date:")
	if (c0h != NULL) {
	    call imgstr (c0h, "date-obs", date1, SZ_LINE)
	    call date_str (date1, date2)
	    call pp_label (fd, x2, y0, date2)
	}
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Time:")
	call pp_fmtkw (fd, x2, y0, c0h, "time-obs", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Proposal:")
	call pp_fmtkw (fd, x2, y0, shh, "proposid", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "PI:")
	call pp_fmtkw (fd, x2, y0, shh, "pr_inv_l", "%s")

	return (y0)

end


procedure read_xthrough (fd, c0h, ycom, ylim)

int	fd
pointer	c0h
real	ycom, ylim

pointer	idb
pointer	recpt
char	dummy[81]
int	ualen

int	strmatch()
pointer	idb_open()
int	idb_nextcard()
bool	is_hist()

begin
	idb = idb_open (c0h, ualen)
	ycom = ycom + 2
	# read each card
	while (idb_nextcard(idb, recpt) != EOF) {
	    if (is_hist(recpt)) {
		call strcpy (Memc[recpt+8], dummy, 72)

		# if the history string contains "used:"
		# or "crfoccomp", print out the history line
		if ( (strmatch (dummy, "used:") != 0) || (strmatch(dummy, "crfoccomp") != 0) ) {
		   if (ycom <= ylim) {
		   	call pp_label (fd, 0., ycom, dummy)
		   	ycom = ycom + 0.75
		   }
		}
	    }
	}
	call idb_close (idb)
	
end


procedure xjit_plot(fd, jit_fname)

int	fd
char	jit_fname[SZ_FNAME]

real	jit_margin, left, right, bottom, top, left_label
pointer	tp
int	nrows
real	v2min, v2max, v3min, v3max
real	midrang, xmin, xmax, ymin, ymax, delv2, delv3

pointer	tbtopn()
int	tbpsta()
int	access()

begin

	call fprintf (fd, "vpage 0.0 .65 0.46 .77\n")

	# Add plot of jitter data here if present	
	# set up the margin between the data limits and plot axes
	jit_margin = 0.02
	left = 0.63
	right = 0.90
	bottom = 0.2
	top = 0.95
	left_label = left - 0.12	

	if (access(jit_fname,0,0) == YES) {
 		tp = tbtopn (jit_fname, READ_ONLY, 0)   # initialize and open the table 
  		nrows = tbpsta (tp, TBL_NROWS)          # how many rows?
	
		if (nrows > 1) {
			call calc_tlmir (tp, "si_v2_avg", nrows, v2min, v2max)
			call calc_tlmir (tp, "si_v3_avg", nrows, v3min, v3max)
		}
		call tbtclo(tp)
	}

	if (nrows > 1) {
		# determine limits of plot
		delv2 = (v2max - v2min) / 2.
		delv3 = (v3max - v3min) / 2.
		if (abs(delv2) >= abs(delv3)) {
			xmin = v2min
			xmax = v2max
			midrang = v3min + delv3
			ymin = midrang - abs(delv2)
			ymax = midrang + abs(delv2)
		} else {
			midrang = v2min + delv2
			xmin = midrang - abs(delv3)
			xmax = midrang + abs(delv3)
			ymin = v3min
			ymax = v3max
		}
	}
	if (access(jit_fname,0,0) == YES && nrows > 1) {
		# simply tdump the two columns, then use IGI to
		#determine limits and plot the data
		# Now, lets plot out the data...
		call fprintf(fd, "location %.3g %.3g %.3g %.3g\n")
			call pargr(left)
			call pargr(right)
			call pargr(bottom)
			call pargr(top)

		call fprintf(fd, "data %s\n")
			call pargstr(jit_fname)

		call fprintf(fd, "xcol si_v2_avg; ycol si_v3_avg\n")
		call fprintf(fd, "limits %.3g %.3g %.3g %.3g \n")
			call pargr(xmin)
			call pargr(xmax)
			call pargr(ymin)
			call pargr(ymax)

		call fprintf(fd,"margin %.3f\n")
			call pargr(jit_margin)
	#	call fprintf(fd, "fmtick %.3f\n")
		call fprintf(fd, "expand 0.6; box; expand 0.7; connect\n")
		call fprintf(fd, "xlabel 'Avg. V2 (arcsec)'\n")
		call fprintf(fd, "angle 90; vmove %.3g 0.5\n")
			call pargr(left_label)
		call fprintf(fd, "putlabel 5 'Avg. V3 (arcsec)'; angle 0\n")
	} else 
		call fprintf(fd, "vmove 0.2 0.85; putlabel 5 'No Jitter Data Available'\n")

end


procedure xpp_calsum(fd, c0h, ref, ped, nref)

int	fd
pointer	c0h
char    refname[SZ_LINE]
char    ref[SZ_PED, ARB]
char    ped[SZ_PED, ARB]
 
int     nref
real    xc1, xc2, xc3, xc4, xc5, xc6
real	y0

begin

	#
	# put in label here, resetting y0 for this section
	y0 = 0.
	call fprintf (fd, "vpage 0.0 0.98 0.05 0.3\n")
	call fprintf (fd, "limits 0 120 14 0\n")
	call fprintf (fd, "expand 1; justify 2; move 50 1\n")
	call fprintf (fd, "label '%sfICalibration Status Summary'")
		call pargstr ("\\")
	call fprintf (fd, "justify 3; expand 0.7\n")

	# Put in column headings
	y0 = 3 
	call pp_label (fd, 0., y0, "Switches and Flags")
	call pp_label (fd, 52., y0, "Reference Files and Tables")
	y0 = y0 + 1
	call fprintf (fd, "move 0 %f; draw 14 %f\n")
		call pargr (y0)
		call pargr (y0)
	call fprintf (fd, "move 52 %f; draw 72 %f\n")
		call pargr (y0)
		call pargr (y0)
	y0 = y0 + 1
	call pp_label (fd, 0., y0, "Keyword")
	call pp_label (fd, 12., y0, "Value")
	call pp_label (fd, 24., y0, "Calibration Step")
	call pp_label (fd, 52., y0, "Keyword")
	call pp_label (fd, 64., y0, "filename")
	call pp_label (fd, 81., y0, "Pedigree")
	y0 = y0 + 1
	# draw double line for underlining column headings
	call fprintf (fd, "move 0 %f; draw 120 %f\n")
		call pargr (y0)
		call pargr (y0)
	y0 = y0 + 0.25
	call fprintf (fd, "move 0 %f; draw 120 %f\n")
		call pargr (y0)
		call pargr (y0)
	y0 = y0 + 0.75
		
	# set up the starting positions for each column
	xc1 = 0
	xc2 = 12
	xc3 = 24
	xc4 = 52
	xc5 = 64
	xc6 = 81
	
	# ready to fill in the information	
	call pp_label (fd, xc1, y0, "BACCORR")
        call pp_keywd (fd, xc2, y0, c0h, "baccorr")
        call pp_label (fd, xc3, y0, "Background Subtraction")
	call pp_label (fd, xc4, y0, "BACHFILE")
        iferr (call imgstr (c0h, "bachfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "bachfile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "ITFCORR")
        call pp_keywd (fd, xc2, y0, c0h, "itfcorr")
        call pp_label (fd, xc3, y0, "ITF Correction")
	call pp_label (fd, xc4, y0, "ITFFILE")
        iferr (call imgstr (c0h, "itffile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "itffile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "PXLCORR")
        call pp_keywd (fd, xc2, y0, c0h, "pxlcorr")
        call pp_label (fd, xc3, y0, "Split Zoom Format Pixels")
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "UNICORR")
        call pp_keywd (fd, xc2, y0, c0h, "unicorr")
        call pp_label (fd, xc3, y0, "Uniform DE Correction")
	call pp_label (fd, xc4, y0, "UNIHFILE")
        iferr (call imgstr (c0h, "unihfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "unihfile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "WAVCORR")
        call pp_keywd (fd, xc2, y0, c0h, "wavcorr")
        call pp_label (fd, xc3, y0, "Compute Photometric Par.")
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "GEOCORR")
        call pp_keywd (fd, xc2, y0, c0h, "geocorr")
        call pp_label (fd, xc3, y0, "Geometric Correction")
	call pp_label (fd, xc4, y0, "GEOHFILE")
        iferr (call imgstr (c0h, "geohfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "geohfile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "SDECORR")
        call pp_keywd (fd, xc2, y0, c0h, "sdecorr")
        call pp_label (fd, xc3, y0, "Spectrograph DE Correction")
	call pp_label (fd, xc4, y0, "SDEHFILE")
        iferr (call imgstr (c0h, "sdehfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "sdehfile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

        call pp_label (fd, xc3, y0, "Blemish Correction")
	call pp_label (fd, xc4, y0, "BLMHFILE")
        iferr (call imgstr (c0h, "blmhfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "blmhfile")
        call get_xped  (fd, xc6, y0, refname, ref, ped, nref)


end
