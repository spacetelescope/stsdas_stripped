# Do the WFPC2 Spacecraft Performance, Pipeline Processing, and Exposure 
# Summaries.

include <imhdr.h>
include	"upp.h"

procedure upp_obsum ()

char    rootname[SZ_FNAME]
char    output[SZ_FNAME]
char    ftype[SZ_FNAME]
int     page

int     fd
char    fname[SZ_FNAME]
char    pdqname[SZ_FNAME]
char    img_root[SZ_FNAME]
pointer	c0h, shh, jih
#char    c0h_ext[SZ_EXT]
#char    d0h_ext[SZ_EXT]
#char    shh_ext[SZ_EXT]
#char    jih_ext[SZ_EXT]
char    linenum[SZ_LINENUM]
char    propid[SZ_LINE]
char	date1[SZ_LINE], date2[SZ_LINE]
char	qual[SZ_LINE]
char	qualcom[SZ_LINE,10]
char	com1[SZ_LINE], com2[SZ_LINE]
char	targname[SZ_LINE]
char	targ1[SZ_LINE], targ2[SZ_LINE]
real	x1, x2, y0, yoff, ycom, ylim
int	i, ncom
int 	chipnum, ngroups, apergrp
real 	dmin, dmax
real	skysig, maglim
char    pdftitle[SZ_LINE]

char 	cflags[SZ_LINE, MAX_EXT]
int	calerr
char	ref[SZ_PED, MAX_EXT]
char 	ped[SZ_PED, MAX_EXT]

int	nref

#pointer	immap()
int     open()
#int	access()
#bool	clgetb()
int     clgeti()
bool	streq()
int 	gf_gstfval()
#pointer	get_jitname()

int	upp_calflag()
#------------------------------------------------------------------------------
begin
        # read parameters
        call clgstr ("rootname", rootname, SZ_LINE)
        call clgstr ("output", output, SZ_LINE)
        call clgstr ("fits", ftype, SZ_LINE)
        page = clgeti ("page")
    
        # construct necessary file name extensions 
  	call upp_fname (rootname, c0h, shh, jih, ftype, fname)

	if (jih == NULL) call eprintf ("Couldn't find jitter file...\n")

        # read keywords
        call imgstr (shh, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (shh, "ROOTNAME", img_root, SZ_FNAME)
	call strlwr (img_root)
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
            call pargstr(rootname)
        call pp_pdfbook(fd, page, pdftitle)
 
        # draw the banner
        call obs_banner (fd, linenum, img_root, propid, "WFPC2", yoff)
        call fprintf (fd, "reset; fontset hard\n")
	
	# draw in double lines for bottom section
	call fprintf (fd, "vpage 0 1 0 1; location 0 1 0 1;limits 0 1 0 1\n")
	call fprintf (fd, "move 0 0.30; draw 0.98 0.30\n")
	call fprintf (fd, "move 0 0.31; draw 0.98 0.31\n")

	# Jitter summary box
	call fprintf (fd, "vpage 0.0 0.65 0.7 0.92\n")
        call fprintf (fd, "limits 0 60 12 0\n")
        call fprintf (fd, "move 0 11.9; draw 60 11.9\n")
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

	ycom = 6.
	if (jih == NULL) call pp_label (fd, 1., ycom, "Could not find JITTER file.")

	do i = 1, ncom {
            call split_str (qualcom[1,i], com1, com2, 75)
            call pp_label (fd,  1., ycom+real(i), com1)
	    if (com2[1] != EOS) {
		ycom = ycom + 1.
            	call pp_label (fd,  5., ycom+real(i), com2)
	    }
	}

        # Pipeline Processing Summary.
        call fprintf (fd, "vpage 0.0 .65 0.5 0.7\n")
        call fprintf (fd, "limits 0 60 10 0\n")
	call fprintf (fd, "move 0 9.9; draw 60 9.9\n")
        call fprintf (fd, "move 0 0; justify 3\n")
	call fprintf (fd, "expand 1.\n")
        call fprintf (fd, "label '%sfIPipeline Processing Summary'")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.7\n")
	call fprintf (fd, "move 1 2; label 'No Error Checks Made.'")

	# Calibration Data Quality Summary
	ylim = 12
	        call fprintf (fd, "vpage 0.0 .65 0.3 0.5\n")
        call fprintf (fd, "limits 0 80 %f 0\n")
		call pargr (ylim)
        call fprintf (fd, "move 0 0; justify 3\n")
	call fprintf (fd, "expand 1.\n")
        call fprintf (fd, "label '%sfICalibration Data Quality Summary'")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.7\n")
	ycom = 0.5
	#
	# Put in notes about calibration files used - determine flags
	#
	calerr =  upp_calflag (c0h, ref, ped, cflags, nref)
	if (calerr > 0) {
		# make sure we don't try to print out more messages
		# than we have space for...
		if (calerr > (ylim-2) ) calerr = ylim - 2
		do i = 1, calerr {
			call pp_label (fd, 1., ycom+real(i), cflags[1,i])
		}
	} else {
		call pp_label (fd, 1., ycom+1.0, "No Anomalies.")
	}

	if (nref == 0)
		call pp_label (fd, 1., ycom+real(i)+1., "No Calibration File Pedigree Information available.")

	# Exposure Summary box
	call fprintf (fd, "vpage 0.65 1. 0.3 0.92\n")
	call fprintf (fd, "limits 0 30 30 0\n")
        call fprintf (fd, "move 0 0; draw 0 29\n")

	x1 = 2.
	x2 = 12.
	y0 = 2.

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

	call uobssum (fd, x1, x2, y0, shh ,c0h)

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

### Determine what aperture was used and get stats for that chip
# first determine the number of groups in observation
	if (streq(ftype,"geis") ) {
		ngroups = gf_gstfval (c0h, "GCOUNT")
	} else {
	    # For a 3-d FITS WFPC2 image
		ngroups = IM_LEN(c0h, 3)
	}
	call det_uaper (c0h, shh, ngroups, chipnum, apergrp, fname, ftype)

# then print out stats here

	y0 = y0 + 2
	call fprintf (fd, "justify 2; move %f %f; label Chip %d Statistics\n")
		call pargr (x2)
		call pargr (y0)
		call pargi (chipnum)
	call fprintf (fd, "justify 3\n")
	y0 = y0 + 1
	#open group corresponding to aperture
	if (streq(ftype,"geis")) {
		call gf_opengr (c0h, apergrp, dmin, dmax, 0) 
		call upp_expmag(fd, c0h, skysig, maglim, ftype, apergrp)
	
		call pp_label (fd, x1, y0, "Sky:")
		call pp_fmtkw (fd, x2, y0, c0h, "backgrnd", "%0.2f")
		y0 = y0 + 1
		call pp_label (fd, x1, y0, "Sky Sigma:")
		#call pp_fmtkw (fd, x2, y0, c0h, "", "%0.2f")
		call pp_rlabel(fd, x2, y0, skysig)
		y0 = y0 + 1
		call pp_label (fd, x1, y0, "Minimum:")
		call pp_fmtkw (fd, x2, y0, c0h, "goodmin", "%0.2f")
		y0 = y0 + 1
		call pp_label (fd, x1, y0, "Maximum:")
		call pp_fmtkw (fd, x2, y0, c0h, "goodmax", "%0.2f")
		y0 = y0 + 1
		call pp_label (fd, x1, y0, "Limiting Mag:")
		#call pp_fmtkw (fd, x2, y0, c0h, "nxsteps", "%0.2f")
		call pp_rlabel (fd, x2, y0, maglim)
	} else {
		call fits_expmag(fd, x1, x2, y0, c0h, apergrp, ftype, fname) 
	}

	# make sure group is reset to 1
	# call gf_opengr (c0h, 1, dmin, dmax, 0)
	call upp_calsum(fd, c0h, ref, ped, nref)

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


procedure fits_expmag(fd, xp1, xp2, ypos, img, grpnum, ftype, fname)

pointer fd, img
real	xp1, xp2, ypos
char	fname[ARB]
char	ftype[ARB]
int	grpnum

real	det, back, gmin, gmax, photflam
real	skysig, maglim
real	exptime
char	targext[SZ_LINE]

int	strmatch()
real	imgetr()
real	umag_limit()

begin
	# Not a group parameter, so we can get it first
	exptime = imgetr (img, "EXPTIME")
	call imgstr (img, "IMAGETYP", targext[1], SZ_LINE)
		
	call get_grinfo(fname, grpnum, det, back, gmin, gmax, photflam)

	if (strmatch(targext[1],"EXT") != 0) 
		maglim = umag_limit(img, ftype, grpnum, skysig, det, photflam, exptime)
	else {
		maglim = 0.
		skysig = 0.
	}


	#call gf_opengr (c0h, apergrp, dmin, dmax, 0) 
	#call upp_expmag(fd, c0h, skysig, maglim, ftype, apergrp)

	call pp_label (fd, xp1, ypos, "Sky:")
	#call pp_fmtkw (fd, x2, y0, c0h, "backgrnd", "%0.2f")
	call pp_rlabel(fd, xp2, ypos, back)
	ypos = ypos + 1
	call pp_label (fd, xp1, ypos, "Sky Sigma:")
	#call pp_fmtkw (fd, x2, y0, c0h, "", "%0.2f")
	call pp_rlabel(fd, xp2, ypos, skysig)
	ypos = ypos + 1
	call pp_label (fd, xp1, ypos, "Minimum:")
	#call pp_fmtkw (fd, x2, y0, c0h, "goodmin", "%0.2f")
	call pp_rlabel (fd, xp2, ypos, gmin)
	ypos = ypos + 1
	call pp_label (fd, xp1, ypos, "Maximum:")
	#call pp_fmtkw (fd, x2, y0, c0h, "goodmax", "%0.2f")
	call pp_rlabel (fd, xp2, ypos, gmax)
	ypos = ypos + 1
	call pp_label (fd, xp1, ypos, "Limiting Mag:")
	#call pp_fmtkw (fd, x2, y0, c0h, "nxsteps", "%0.2f")
	call pp_rlabel (fd, xp2, ypos, maglim)

end



procedure uobssum (fd, x1, x2, y0, shh ,c0h)

int	fd
real	x1, x2, y0
pointer	shh, c0h

begin
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "RA (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "ra_targ", "%0.2H")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Dec (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "dec_targ", "%0.1h")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "X POSTARG:")
	#call pp_fmtkw (fd, x2, y0, c0h, "xpostarg", "%s")
	call pp_label (fd, x2, y0, "N/A")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Y POSTARG:")
	#call pp_fmtkw (fd, x2, y0, c0h, "ypostarg", "%s")
	call pp_label (fd, x2, y0, "N/A")
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



procedure get_uped (fd, x, y, refname, ref, ped, nref)
 
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


procedure upp_calsum(fd, c0h, ref, ped, nref)

pointer c0h
real	y0
int	fd

char	refname[SZ_LINE]
char	ref[SZ_PED, ARB]
char 	ped[SZ_PED, ARB]

int	nref
real	xc1, xc2, xc3, xc4, xc5, xc6

begin

	#
	# Calibration Status Summary section
	#
	# put in label here, resetting y0 for this section
	y0 = 0
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
	call pp_label (fd, xc1, y0, "MASKCORR")
        call pp_keywd (fd, xc2, y0, c0h, "maskcorr")
        call pp_label (fd, xc3, y0, "Mask Correction")
	call pp_label (fd, xc4, y0, "MASKFILE")
        iferr (call imgstr (c0h, "maskfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "maskfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "ATODCORR")
        call pp_keywd (fd, xc2, y0, c0h, "atodcorr")
        call pp_label (fd, xc3, y0, "A-to-D Correction")
	call pp_label (fd, xc4, y0, "ATODFILE")
        iferr (call imgstr (c0h, "atodfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "atodfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "BLEVCORR")
        call pp_keywd (fd, xc2, y0, c0h, "blevcorr")
        call pp_label (fd, xc3, y0, "Bias Level Correction")
	call pp_label (fd, xc4, y0, "BLEVFILE")
        iferr (call imgstr (c0h, "blevfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "blevfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "BIASCORR")
        call pp_keywd (fd, xc2, y0, c0h, "biascorr")
        call pp_label (fd, xc3, y0, "Bias Correction")
	call pp_label (fd, xc4, y0, "BIASFILE")
        iferr (call imgstr (c0h, "biasfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "biasfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "DARKCORR")
        call pp_keywd (fd, xc2, y0, c0h, "darkcorr")
        call pp_label (fd, xc3, y0, "Dark Correction")
	call pp_label (fd, xc4, y0, "DARKFILE")
        iferr (call imgstr (c0h, "darkfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "darkfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "FLATCORR")
        call pp_keywd (fd, xc2, y0, c0h, "flatcorr")
        call pp_label (fd, xc3, y0, "Flat Field Correction")
	call pp_label (fd, xc4, y0, "FLATFILE")
        iferr (call imgstr (c0h, "flatfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "flatfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "SHADCORR")
        call pp_keywd (fd, xc2, y0, c0h, "shadcorr")
        call pp_label (fd, xc3, y0, "Shaded Shutter Correction")
	call pp_label (fd, xc4, y0, "SHADFILE")
        iferr (call imgstr (c0h, "shadfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "shadfile")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)
	y0 = y0 + 1

	call pp_label (fd, xc1, y0, "DOPHOTOM")
        call pp_keywd (fd, xc2, y0, c0h, "dophotom")
        call pp_label (fd, xc3, y0, "Populate Photometry Keywords")
	call pp_label (fd, xc4, y0, "PHOTTAB")
        iferr (call imgstr (c0h, "phottab", refname, SZ_LINE)) refname[1] = EOS
        call pp_keywd (fd, xc5, y0, c0h, "phottab")
        call get_uped  (fd, xc6, y0, refname, ref, ped, nref)

end

procedure upp_expmag(fd, c0h, skysig, maglim, ftype, grpnum)

int	fd
pointer	c0h
real	skysig, maglim
char	ftype[ARB]
int	grpnum

real	detector, photflam, exptime
char	targext[SZ_LINE]

int	strmatch()
real	umag_limit()
real	imgetr()

begin

	call imgstr (c0h, "IMAGETYP", targext[1], SZ_LINE)

	detector = imgetr (c0h, "DETECTOR")
	photflam = imgetr(c0h, "PHOTFLAM")
	exptime = imgetr (c0h, "EXPTIME")

	if (strmatch(targext[1],"EXT") != 0) 
		maglim = umag_limit(c0h, ftype, grpnum, skysig, detector, photflam, exptime)
	else {
		maglim = 0.
		skysig = 0.
	}

end

procedure upp_fname (rootname, c0h, shh, jih, ftype, imgname)
char    rootname[SZ_FNAME]
pointer	c0h, shh, jih
char	ftype[ARB]

char    fname[SZ_FNAME]
char    imgname[SZ_FNAME]
char    c0h_ext[SZ_EXT]
char    d0h_ext[SZ_EXT]
char    shh_ext[SZ_EXT]


pointer	immap(), get_jitname()
int	access()
bool	streq()

begin

	if (streq(ftype,"fits")) {
  	    call strcpy ("_c0f.fits", c0h_ext, SZ_EXT)
    	    call strcpy ("_shf.fits[0]", shh_ext, SZ_EXT)
	    call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
        #    call strcpy ("_jif.fits", jih_ext, SZ_EXT)
        } else {
            call strcpy (".c0h", c0h_ext, SZ_EXT)
	    call strcpy (".d0h", d0h_ext, SZ_EXT) 
   	    call strcpy (".shh", shh_ext, SZ_EXT)
       }
 
        # construct file names
        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (c0h_ext, fname, SZ_FNAME)
	if (access (fname, 0, 0) == NO) {
		# Try to find a d0h image to work from
		call strcpy (rootname, fname, SZ_FNAME)
		call strcat (d0h_ext, fname, SZ_FNAME)
		if (access (fname, 0, 0) == NO) {
			# if there is no d0h image or c0h image, exit
			c0h = NULL
			call error (1, "UPP_IMAGE: No images to process")
		} else {
			if(streq(ftype,"fits"))
				call strcat("[0]",fname, SZ_FNAME)
			# otherwise, use the d0h image
			c0h = immap (fname, READ_ONLY, 0)
		}
	} else {
		# found a c0h image, so we use it...
		if(streq(ftype,"fits"))
			call strcat("[0]",fname, SZ_FNAME)
		c0h = immap (fname, READ_ONLY, 0)
	}
	call strcpy (fname, imgname, SZ_FNAME)
	
        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (shh_ext, fname, SZ_FNAME)
        shh = immap (fname, READ_ONLY, 0)

	jih = get_jitname(rootname, fname)

end
