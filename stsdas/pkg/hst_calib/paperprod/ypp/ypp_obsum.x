# Do the FOS Spacecraft Performance, Pipeline Processing, and Exposure 
# Summaries.

include <imhdr.h>
include <tbset.h>
include	"ypp.h"

procedure ypp_obsum ()

char    rootname[SZ_FNAME]
char    output[SZ_FNAME]
char    ftype[SZ_FNAME]

int     fd
char    fname[SZ_FNAME]
char    pdqname[SZ_FNAME]
char    rootid[SZ_FNAME]
pointer	d0h, shh, jih, c7h, c1h
char    d0h_ext[SZ_EXT]
char    c7h_ext[SZ_EXT]
char    c1h_ext[SZ_EXT]
char    shh_ext[SZ_EXT]
char	jit_fname[SZ_FNAME]
#char	command[SZ_LINE]
char    linenum[SZ_LINENUM]
char    propid[SZ_LINE]
char	qual[SZ_LINE]
char	qualcom[SZ_LINE,10]
char	com1[SZ_LINE], com2[SZ_LINE]
real	x1, x2, y0, yoff, ycom
char	grndmode[SZ_LINE]
int	i, ncom
double	rapid1, rapidl
real	dmin, dmax, readtime
int	gcount
real	darkmean, sct_val, exp_grp
bool	sct_exist
pointer	gtab, cptr

pointer	immap()
int     open()
int	access()
int	gf_gstval()
#double	imgetd()
char	strsearch()
real	ypp_mean()
real	ypp_fmean()
real	imgetr()
real	exp_sum()
pointer	get_jitname()
bool	streq()
pointer	tbtopn()

#------------------------------------------------------------------------------
begin
        # read parameters
        call clgstr ("rootname", rootname, SZ_LINE)
        call clgstr ("output", output, SZ_LINE)
	call clgstr ("fits", ftype, SZ_FNAME)

	if (streq(ftype, "geis") ) {
            call strcpy (".d0h", d0h_ext, SZ_EXT)
            call strcpy (".shh", shh_ext, SZ_EXT)
            call strcpy (".c7h", c7h_ext, SZ_EXT)
            call strcpy (".c1h", c1h_ext, SZ_EXT)
 	} else {
            call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
            call strcpy ("_shf.fits[0]", shh_ext, SZ_EXT)
            call strcpy ("_c7f.fits", c7h_ext, SZ_EXT)
            call strcpy ("_c1f.fits", c1h_ext, SZ_EXT)
	}	   
	
        # construct file names
        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (d0h_ext, fname, SZ_FNAME)
	if (access (fname, 0, 0) == NO) d0h = NULL
        else {
		call strcat("[0]", fname, SZ_FNAME)
		d0h = immap (fname, READ_ONLY, 0)
	}

        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (c7h_ext, fname, SZ_FNAME)
	if (access (fname, 0, 0) == NO) c7h = NULL
        else {
		call strcat("[0]", fname, SZ_FNAME)
		 c7h = immap (fname, READ_ONLY, 0)
	}
      	call strcpy (rootname, fname, SZ_FNAME)
        call strcat (c1h_ext, fname, SZ_FNAME)
	if (access (fname, 0, 0) == NO) c1h = NULL
        else {
		call strcat("[0]", fname, SZ_FNAME)
		c1h = immap (fname, READ_ONLY, 0)
	}
        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (shh_ext, fname, SZ_FNAME)
        shh = immap (fname, READ_ONLY, 0)

	jih = get_jitname(rootname, jit_fname)

        # read keywords
        call imgstr (shh, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (shh, "PROPOSID", propid, SZ_LINE)
        call imgstr (shh, "ROOTNAME", rootid, SZ_LINE)
	sct_exist = TRUE
	exp_grp = 0.
	if (c1h != NULL) {
		exp_grp = imgetr(c1h, "exposure")

		#determine the number of groups in the image
		if (streq(ftype,"geis") ){
			gcount = gf_gstval (c1h, "GCOUNT")
			call gf_opengr (c1h, gcount, dmin, dmax, 0)
		    	iferr(sct_val = imgetr (c1h, "SCT_VAL")) {
			    sct_exist = FALSE
			    sct_val = 0.
		    	}
		} else {
			gcount = IM_LEN(c1h, 2)
 		     	call strcpy (rootname, fname, SZ_FNAME)
  			call strcat (c1h_ext, fname, SZ_FNAME)
			call strcat ("[1]", fname, SZ_FNAME)
			gtab = tbtopn(fname, READ_ONLY, NULL)
			call tbcfnd (gtab, "SCT_VAL", cptr, 1)
			if (cptr != NULL) {
			    call tbegtr (gtab, cptr, gcount, sct_val)
			} else {
				sct_exist = FALSE
				sct_val = 0.
			}
			call tbtclo(gtab)
		}
		sct_val = sct_val * exp_grp
	}


        # open the output file
        fd = open (output, NEW_FILE, TEXT_FILE)
 
        # start a new page
        call pp_erase (fd)
 
        # draw the banner
        call obs_banner (fd, linenum, rootid, propid, "FOS", yoff)
        call fprintf (fd, "reset; fontset hard\n")

	# Jitter summary box
	call fprintf (fd, "vpage 0.0 .65 .75 .9\n")
        call fprintf (fd, "limits 0 60 6 0\n")
#        call fprintf (fd, "move 0 27; draw 60 27\n")
        call fprintf (fd, "move 0 0; justify 3\n")
        call fprintf (fd, "label '%sfIHST Spacecraft Performance Summary'")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.8\n")
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

	# Pipeline Summary Section
	call pipeline_sum(fd, sct_exist)

	# Exposure Summary box
	x1 = 2.
	x2 = 15.

	y0 = exp_sum(fd, shh, d0h, x1, x2)

	# Now add additional keywords here...

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Mean Dark Rate:")
	if (c7h != NULL ) {
		# determine how many groups are in this image
		# gcount = gf_gstval (c7h, "GCOUNT")
		if (streq(ftype,"geis") ){
			gcount = gf_gstval (c7h, "GCOUNT")
			darkmean = ypp_mean (c7h, gcount)
		} else {
			gcount = IM_LEN(c7h, 2)
			darkmean = ypp_fmean (c7h, gcount)
		}

		call sprintf (com1, SZ_LINE, "%.4g cts/s/pix")
			call pargr (darkmean)
		call pp_label (fd, x2, y0, com1)
	} else 
		call pp_label (fd, x2, y0, "N/A")

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Mean Dark Counts:")
	if (c7h != NULL ) {
		darkmean = darkmean * exp_grp
		call sprintf (com1, SZ_LINE, "%.4g cts/pix")
			call pargr (darkmean)
		call pp_label (fd, x2, y0, com1)
	} else 
		call pp_label (fd, x2, y0, "N/A")

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Mean Scat. Light:")
	if (sct_exist) { 
		call sprintf (com1, SZ_LINE, "%.4g cts/pix")
			 call pargr(sct_val)
		call pp_label (fd, x2, y0, com1)
	} else 
		call pp_label (fd, x2, y0, "N/A")

	#
	# check to see if we have a RAPID mode observation
	#
	call imgstr (d0h, "grndmode", grndmode, SZ_LINE)
	if ( strsearch (grndmode, "RAPID") != NULL) {
		# We do have a RAPID mode observation, so 
		# calculate READTIME and print out value
		y0= y0 + 1
 		call strcpy (rootname, fname, SZ_FNAME)
  		call strcat (d0h_ext, fname, SZ_FNAME)
		call strcat ("[1]", fname, SZ_FNAME)

		call ypp_calcrapid(d0h, ftype, fname, rapid1, rapidl)
		
		call pp_label (fd, x1, y0, "Readtime:")
		if (gcount > 1) {
		    readtime = ((rapidl - rapid1 ) / (gcount - 1)) * 3600. * 24.
		    call sprintf (com1, SZ_LINE, "%5.2f sec")
			call pargr(readtime) 
		   call pp_label (fd, x2, y0, com1)
		} else {
		   readtime = 0. 
		   call sprintf (com1, SZ_LINE, "Error")
			call pargr(readtime) 
		   call pp_label (fd, x2, y0, com1)
		}
	}

	# set aside space for jitter plot here...
		call jit_plot(fd, jit_fname)

	# close images
	if (d0h != NULL) call imunmap (d0h)
	if (c7h != NULL) call imunmap (c7h)
	if (c1h != NULL) call imunmap (c1h)
	if (jih != NULL) call imunmap (jih)
	call imunmap (shh)

        call close (fd)
end

procedure ypp_calcrapid (img, ftype, fname, r1, rl)

pointer	img
double	r1, rl
char	fname[ARB]
int	gcount
real	dmin, dmax
char	ftype[ARB]
pointer	gtab, cptr



int	gf_gstval()
double	imgetd()
bool	streq()
pointer	tbtopn()

begin 
	if (streq(ftype,"geis") ) {
		gcount = gf_gstval (img, "GCOUNT")

		call gf_opengr (img, 1, dmin, dmax, 0)
		r1 = imgetd (img, "FPKTTIME")
		call gf_opengr (img, gcount, dmin, dmax, 0)
		rl = imgetd (img, "FPKTTIME")
	} else {
		gcount = IM_LEN(img, 2)
		gtab = tbtopn(fname, READ_ONLY, NULL)
		call tbcfnd (gtab, "FPKTTIME", cptr, 1)
		if (cptr != NULL) {
		    call tbegtd (gtab, cptr, 1, r1)
		    call tbegtd (gtab, cptr, gcount, rl)
		} else {
		    r1 = 0.
		    rl = 0.
		}
		call tbtclo(gtab)
	}

end


procedure obssum (fd, x1, x2, y0, shh ,d0h)

int	fd
real	x1, x2, y0
char	aper_id[SZ_LINE]

pointer	shh, d0h

begin
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "RA (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "ra_targ", "%0.2H")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Dec (J2000):")
	call pp_fmtkw (fd, x2, y0, shh, "dec_targ", "%0.1h")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "X POSTARG:")
	call pp_fmtkw (fd, x2, y0, d0h, "xpostarg", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Y POSTARG:")
	call pp_fmtkw (fd, x2, y0, d0h, "ypostarg", "%s")

	y0 = y0 + 2
	call pp_label (fd, x1, y0, "Detector:")
	call pp_fmtkw (fd, x2, y0, d0h, "detector", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Grating:")
	call pp_fmtkw (fd, x2, y0, d0h, "fgwa_id", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Aperture:")
	iferr (call pp_fmtkw (fd, x2, y0, d0h, "aper_fov", "%s") ) {
		call imgstr (d0h, "aper_id", aper_id, SZ_LINE)
		call pp_label (fd, x2, y0, "Pre-COSTAR %s")
			call pargstr(aper_id)
	}

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Exp Time (sec):")
	call pp_fmtkw (fd, x2, y0, d0h, "exptime", "%0.1f")

	y0 = y0 + 2
	call pp_label (fd, x1, y0, "Rootname:")
	call pp_fmtkw (fd, x2, y0, shh, "rootname", "%s")
end

real procedure ypp_mean (img, grp)

pointer	img
int	grp
real	mean
real	sigma
real 	dmin, dmax
pointer	imgvec
int	xdim
int	gcount


pointer	imgl1r()
int	gf_gstval()

begin
		# open up the specified group
		gcount = gf_gstval (img, "GCOUNT")
		# make sure that specified group is legitimate,
		# otherwise, open just last group in image
		if (grp <= gcount) 
			call gf_opengr (img, grp, dmin, dmax, 0)
		else
			call gf_opengr (img, gcount, dmin, dmax, 0)

		# read the entire image into a vector
		xdim = IM_LEN(img, 1)
		imgvec = imgl1r(img)

		# perform statistics on vector to calculate mean
		call aavgr (Memr[imgvec], xdim, mean, sigma)

		# return the mean value
		return (mean)

end

real procedure ypp_fmean (img, grp)

pointer	img
int	grp
real	mean
real	sigma
pointer	imgvec
int	xdim

pointer	imgl2r()

begin

		# read the entire image into a vector
		xdim = IM_LEN(img, 1)
		imgvec = imgl2r(img, grp)

		# perform statistics on vector to calculate mean
		call aavgr (Memr[imgvec], xdim, mean, sigma)

		# return the mean value
		return (mean)

end


procedure pipeline_sum(fd, sct_exist)

int	fd
bool 	sct_exist

begin

        # Pipeline Processing Summary.
        call fprintf (fd, "vpage 0.0 .65 .0 .25\n")
        call fprintf (fd, "limits 0 60 15 0\n")
	call fprintf (fd, "move 0 0; draw 60 0\n")
        call fprintf (fd, "move 0 1; justify 3\n")
	call fprintf (fd, "expand 1.\n")
        call fprintf (fd, "label '%sfIPipeline Processing Summary'\n")
	    call pargstr ("\\")
	call fprintf (fd, "expand 0.8\n")
	if (!sct_exist) {
		call fprintf (fd, "move 0. 4.\n")
		call fprintf (fd, "label 'No SCT_VAL Keyword: This data should be reprocessed using ADDNEWKEYS task.' \n")
	}
end


real procedure exp_sum(fd, shh, d0h, x1, x2)

int	fd
pointer	shh, d0h

char	targname[SZ_LINE]
char	targ1[SZ_LINE], targ2[SZ_LINE]
real	x1, x2, y0
char	date1[SZ_LINE], date2[SZ_LINE]

begin

	call fprintf (fd, "vpage 0.65 1. 0.00 0.92\n")
	call fprintf (fd, "limits 0 30 35 0\n")
        call fprintf (fd, "move 0 0; draw 0 35\n")

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

	call obssum (fd, x1, x2, y0, shh ,d0h)

	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Date:")
	if (d0h != NULL) {
	    call imgstr (d0h, "date-obs", date1, SZ_LINE)
	    call date_str (date1, date2)
	    call pp_label (fd, x2, y0, date2)
	}
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Time:")
	call pp_fmtkw (fd, x2, y0, d0h, "time-obs", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Proposal:")
	call pp_fmtkw (fd, x2, y0, shh, "proposid", "%s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "PI:")
	call pp_fmtkw (fd, x2, y0, shh, "pr_inv_l", "%s")

	y0 = y0 + 2
	call pp_label (fd, x1, y0, "Ground Mode:")
	call pp_fmtkw (fd, x2, y0, d0h, "grndmode", "%15s")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Position Angle:")
	call pp_fmtkw (fd, x2, y0, d0h, "pa_aper", "%0.2f")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "First Channel:")
	call pp_fmtkw (fd, x2, y0, d0h, "fchnl", "%d")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "# of Channels:")
	call pp_fmtkw (fd, x2, y0, d0h, "nchnls", "%d")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "NXSTEPS:")
	call pp_fmtkw (fd, x2, y0, d0h, "nxsteps", "%d")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "Overscan:")
	call pp_fmtkw (fd, x2, y0, d0h, "overscan", "%d")
	y0 = y0 + 1
	call pp_label (fd, x1, y0, "YSTEPS:")
	call pp_fmtkw (fd, x2, y0, d0h, "ysteps", "%s")

	return (y0)

end



procedure jit_plot(fd, jit_fname)

int	fd
char	jit_fname[SZ_FNAME]

real	jit_margin, left, right, bottom, top
pointer	tp
int	nrows
real	v2min,v2max, v3min, v3max
bool	plot_jit

pointer	tbtopn()
int	tbpsta()
int	access()

begin


	call fprintf (fd, "vpage 0.0 .65 0.25 .75\n")

	# Add plot of jitter data here if present	
	# set up the margin between the data limits and plot axes
	jit_margin = 0.05
	left = 0.45
	right = 0.95
	bottom = 0.2
	top = 0.9	
	plot_jit = true

	if (access(jit_fname,0,0) == YES) {
 		tp = tbtopn (jit_fname, READ_ONLY, 0)   # initialize and open the table 
  		nrows = tbpsta (tp, TBL_NROWS)          # how many rows?

		if (nrows > 1) {
                        call calc_tlmir (tp, "si_v2_avg", nrows, v2min, v2max)
                        call calc_tlmir (tp, "si_v3_avg", nrows, v3min, v3max)

			if (v2min == 0. && v2max == 0. && v3min == 0. && v3max == 0.) plot_jit = false
                 } else { 
			plot_jit = false
		 }
		call tbtclo(tp)
	} else {
		plot_jit = false
	} 

	#if ( access(jit_fname,0,0) == YES && nrows > 1) {
	if (plot_jit) {
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
		call fprintf(fd, "limits; margin %.3f\n")
			call pargr(jit_margin)
	#	call fprintf(fd, "fmtick %.3f\n")
		call fprintf(fd, "box; connect\n")
		call fprintf(fd, "xlabel 'V2 (arcsec)'\n")
		call fprintf(fd, "angle 90; vmove 0.35 0.5\n")
		call fprintf(fd, "putlabel 5 'V3 (arcsec)'; angle 0\n")
	} else 
		call fprintf(fd, "vmove 0.2 0.85; putlabel 5 'No Guide Star Positions Available'\n")

end
