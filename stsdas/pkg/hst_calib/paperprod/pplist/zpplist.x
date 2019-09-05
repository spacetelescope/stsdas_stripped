include	"pplist.h"

# produce target list and observation list for GHRS

procedure zpplist (tpin, tpimtype, output)

pointer	tpin
pointer tpimtype                # list of input file types (FITS or GEIS)
char	output[SZ_FNAME]

int	fd
char    ftype[SZ_FNAME]
char	fname[SZ_FNAME]
char	root[SZ_FNAME]
pointer	shp, d0h, c1h, udl
char	shp_ext[SZ_EXT]
char	udl_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	d1h_ext[SZ_EXT]
char	c1h_ext[SZ_EXT]
char	visit[SZ_VISIT+1]
char	propid[SZ_LINE]
char	targ1[MAX_TARG], targ2[MAX_TARG]
bool	tfound
int	ntarg
real	yoff
real	wave1, wave2, expt
real	srchsize, steptime
int	nchar
int	n, k, nobs, ncom

# observation attributes
char	tltarg[SZ_TARG, MAX_OBS]
double	ra[MAX_OBS], dec[MAX_OBS]
char	desc[SZ_DESC, MAX_OBS]

char	linenum[SZ_LINENUM, MAX_OBS]
char	rootname[SZ_ROOT, MAX_OBS]
char	targ[SZ_TARG, MAX_OBS]
char	exptime[SZ_EXPT, MAX_OBS]

char	config[SZ_ZCONFIG, MAX_OBS]
char	opmode[SZ_ZMODE, MAX_OBS]
char	aper[SZ_ZAPER, MAX_OBS]
char	filter[SZ_ZFILT, MAX_OBS]
char	cwave[SZ_ZWAVE, MAX_OBS]

char    qual[SZ_LINE, MAX_OBS]
char    dummy[SZ_LINE,10]
char    dum3[3]

pointer	immap()
int	open()
int	imtlen()
int	imtgetim()
bool	imgetb()
real	imgetr()
double	imgetd()
bool	streq()
int	strmatch()

begin


        nobs = imtlen(tpin)
	tfound = false
	ntarg = 0

	# loop all root names
        do n = 1, nobs {

            # read the next input image name in the template list
            nchar = imtgetim (tpin, root, SZ_FNAME)

            # (see 'lib/pp_roots.x' for list of possible imtypes)
            nchar = imtgetim (tpimtype, ftype, SZ_FNAME)
		# construct necessary file name extensions
		if (streq(ftype,"fits")) {
		    call strcpy ("_shf.fits[0]", shp_ext, SZ_EXT) 
		    call strcpy ("_ulf.fits[0]", udl_ext, SZ_EXT) 
		    call strcpy ("_d0f.fits[0]", d0h_ext, SZ_EXT) 
		    call strcpy ("_d1f.fits[0]", d1h_ext, SZ_EXT) 
		    call strcpy ("_c1f.fits[0]", c1h_ext, SZ_EXT) 
		} else {
		    call strcpy (".shh", shp_ext, SZ_EXT) 
		    call strcpy (".ulh", udl_ext, SZ_EXT) 
		    call strcpy (".d0h", d0h_ext, SZ_EXT) 
		    call strcpy (".d1h", d1h_ext, SZ_EXT) 
		    call strcpy (".c1h", c1h_ext, SZ_EXT) 
		}

	    # construct file names
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (shp_ext, fname, SZ_FNAME)
	    shp = immap (fname, READ_ONLY, 0)

	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (udl_ext, fname, SZ_FNAME)
	    udl = immap (fname, READ_ONLY, 0)

	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (c1h_ext, fname, SZ_FNAME)
	    iferr (c1h = immap (fname, READ_ONLY, 0)) c1h = NULL

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

	    # read other observation list related keywords
	    call imgstr (shp, "LINENUM", linenum[1,n], SZ_LINENUM)
	    call imgstr (shp, "ROOTNAME", rootname[1,n], SZ_ROOT)
	    call imgstr (shp, "CONFIG", config[1,n], SZ_ZCONFIG)
	    call imgstr (udl, "OBSMODE", opmode[1,n], SZ_ZMODE)

	    # decide whether to use d0h or d1h
	    call strcpy (root, fname, SZ_FNAME)
	    call strcpy (opmode[1,n], dum3, 3)
	    if (streq(dum3, "ACQ"))
	    	call strcat (d1h_ext, fname, SZ_FNAME)
	    else
	    	call strcat (d0h_ext, fname, SZ_FNAME)
	    iferr (d0h = immap (fname, READ_ONLY, 0)) d0h = NULL

	    if (d0h != NULL) {
	        iferr (call imgstr (d0h, "APERTURE", aper[1,n], SZ_ZAPER))
		    call strcpy ("(N/A)", aper[1,n], SZ_ZAPER)
	    } else
		call strcpy ("(N/A)", aper[1,n], SZ_ZAPER)
	    call imgstr (shp, "SPEC_1", filter[1,n], SZ_ZFILT)
	    if (c1h != NULL) {
	    	wave1 = imgetr (c1h, "MINWAVE")
	    	wave2 = imgetr (c1h, "MAXWAVE")
		call sprintf (cwave[1,n], SZ_ZWAVE, "%d")
		    call pargi (nint((wave1+wave2)/2.))
	    } else
		call strcpy ("(N/A)", cwave[1,n], SZ_ZWAVE)
	    if (d0h != NULL) {
	        expt = imgetr (d0h, "EXPTIME")

		# calculate the exposure time for the .d1h file
	        if (streq(dum3, "ACQ")) {
		    if (imgetb (d0h, "RTBRIGHT")) {
			srchsize = imgetr (d0h, "SRCHSIZE") 
			steptime = imgetr (d0h, "STEPTIME") 
			expt = steptime * srchsize**2
		    }
		}
		call sprintf (exptime[1,n], SZ_EXPT, "%8.1f")
		    call pargr (expt)
	    } else
		call strcpy ("(N/A)", exptime[1,n], SZ_EXPT)

            if (streq(ftype,"geis")) {     
                call pp_qual (root, qual[1,n], dummy[1,1], ncom)
            } else {
                call strcat ("_pdq.fits",root,SZ_FNAME)
                call pp_qualfits (root, qual[1,n], dummy[1,1], ncom)
            }

	    if (n == 1) {
	        call imgstr (shp, "PEP_EXPO", visit, SZ_VISIT)
	        call imgstr (shp, "PROPOSID", propid, SZ_LINE)
	    }
		
	    # close files
	    call imunmap (shp)
	    call imunmap (udl)
	    if (d0h != NULL) call imunmap (d0h)
	    if (c1h != NULL) call imunmap (c1h)
	}

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# print the target list
	call list_banner (fd, visit, propid, "GHRS", yoff)
	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, "GHRS", 
			yoff)

	# print observation list
	do n = 1, nobs {
	    if (n == 1 && yoff < (BOTTOM-4.))
		call zobs_head (fd, yoff)
	    if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, "GHRS", yoff)
		call zobs_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 8., yoff, rootname[1,n])
	    call pp_label (fd, 17., yoff, targ1)
	    call pp_label (fd, 31., yoff, config[1,n])
	    call pp_label (fd, 37., yoff, opmode[1,n])
	    call pp_label (fd, 46., yoff, aper[1,n])
	    call pp_label (fd, 52., yoff, filter[1,n])
	    call pp_label (fd, 61., yoff, cwave[1,n])
	    call fprintf (fd, "justify 1\n")
	    call pp_label (fd, 72., yoff, exptime[1,n])
	    call fprintf (fd, "justify 3\n")

            # do the quality flags
            if (qual[1,n] != EOS) {
                call pp_move (fd, 75., yoff+0.25)
                if (strmatch(qual[1,n],"OK") != 0 || 
		    strmatch(qual[1,n],"NO-EVAL") != 0)
                    call fprintf (fd, "ptype 25 0\n")
                else
                    call fprintf (fd, "ptype 25 3\n")
                call fprintf (fd, "dot\n")
            }

	    # if the target name is long
	    if (targ2[1] != EOS) {
		yoff = yoff + 0.75
	        call pp_label (fd, 17., yoff, targ2)
	    }

	    # draw a line
	    call pp_move (fd, 0., yoff+0.75)
	    call fprintf (fd, "draw 82. %6.2f\n")
		call pargr (yoff+0.75)
	
	    yoff = yoff + 1.
	}

	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw 82. %6.2f\n")
	    call pargr (yoff)
 
        # flag caption
        call flag_caption (fd, yoff)

	call close (fd)
end

# produce observation list header for GHRS

procedure zobs_head (fd, yoff)

int	fd
real	yoff

begin
	
	# initialize and title
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 1. 82. %6.2f 1.\n")
	    call pargr (BOTTOM)
	call pp_move (fd, 41., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List'; expand 0.75\n")
	    call pargstr ("\\")
	
	yoff = yoff + 2.

	# first line
	call pp_label (fd,  3., yoff, "Logsheet")
	call pp_label (fd, 41., yoff, "Observation")
	call pp_label (fd, 55., yoff, "Spectral")
	call pp_label (fd, 63., yoff, "Wavelength")
	call pp_label (fd, 70., yoff, "Exposure")
	call pp_label (fd, 78., yoff, "Quality Flags")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd,  3., yoff, "Line#")
	call pp_label (fd, 11., yoff, "Rootname")
	call pp_label (fd, 21., yoff, "Target Name")
	call pp_label (fd, 33., yoff, "Config")
	call pp_label (fd, 41., yoff, "Mode")
	call pp_label (fd, 48., yoff, "Aperture")
	call pp_label (fd, 55., yoff, "Element")
	call pp_label (fd, 63., yoff, "(central)")
	call pp_label (fd, 70., yoff, "(sec)")
	call pp_label (fd, 75., yoff, "Obs")
	call pp_label (fd, 80., yoff, "Proc")

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
