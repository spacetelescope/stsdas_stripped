include	"pplist.h"

# produce target list and observation list for FOS

procedure ypplist (tpin, tpimtype, output)

pointer	tpin
char	output[SZ_FNAME]
pointer tpimtype                # list of input file types (FITS or GEIS)
#char    selector[SZ_LINE]       # output parts selector

char    imtype[SZ_FNAME]
int	fd
char	fname[SZ_FNAME]
char	root[SZ_FNAME]
pointer	shp, d0h
char	shp_ext[SZ_EXT]
char	d0h_ext[SZ_EXT]
char	visit[SZ_VISIT+1]
char	propid[SZ_LINE]
char	targ1[MAX_TARG], targ2[MAX_TARG]
char	aper1[MAX_APER], aper2[MAX_APER]
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
char	opmode[SZ_YMODE, MAX_OBS]
char	detector[SZ_YDET, MAX_OBS]
char	aper[SZ_YAPER, MAX_OBS]
char	aper_id[SZ_YAPER]
char	filter[SZ_YFILT, MAX_OBS]
char	dateobs[SZ_DATEOBS, MAX_OBS]
char	qual[SZ_LINE, MAX_OBS]
char	dummy[SZ_LINE,10]
real	exptime[MAX_OBS]

pointer	immap()
int	open()
int	imtlen()
int	imtgetim()
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

            # construct necessary file name extensions
            # (see 'lib/pp_roots.x' for list of possible imtypes)
            nchar = imtgetim (tpimtype, imtype, SZ_FNAME)

	    # construct necessary file name extensions
  	    if (streq(imtype, "fits") ) {
	    	call strcpy ("_shf.fits[0]", shp_ext, SZ_EXT) 
	    	call strcpy ("_d0f.fits[0]", d0h_ext, SZ_EXT) 
	    } else {
	    	call strcpy (".shh", shp_ext, SZ_EXT) 
	    	call strcpy (".d0h", d0h_ext, SZ_EXT) 
	    }
	    # construct file names
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (shp_ext, fname, SZ_FNAME)
	    shp = immap (fname, READ_ONLY, 0)

	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (d0h_ext, fname, SZ_FNAME)
	    d0h = immap (fname, READ_ONLY, 0)

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
	    call imgstr (shp, "OPMODE", opmode[1,n], SZ_YMODE)
	    call imgstr (d0h, "DETECTOR", detector[1,n], SZ_YDET)
	    iferr (call imgstr (d0h, "APER_FOV", aper[1,n], SZ_YAPER) ) {
		call imgstr (d0h, "APER_ID", aper_id, SZ_YAPER)
		call sprintf (aper[1,n], SZ_YAPER, "Pre-COSTAR %s")
			call pargstr(aper_id)
	    }
	    call imgstr (d0h, "FGWA_ID", filter[1,n], SZ_YFILT)
	    call imgstr (d0h, "DATE-OBS", dateobs[1,n], SZ_DATEOBS)
	    exptime[n] = imgetr (d0h, "EXPTIME")
            if (streq(imtype,"geis")) {     
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
	    call imunmap (d0h)
	}

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# print the target list
	call list_banner (fd, visit, propid, "FOS", yoff)
	call targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, "FOS", 
			yoff)

	# print observation list
	do n = 1, nobs {
	    if (n == 1 && yoff < (BOTTOM-4.))
		call yobs_head (fd, yoff)
	    if (yoff > BOTTOM-1.) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, "FOS", yoff)
		call yobs_head (fd, yoff)
	    }

	    call split_str (targ[1,n], targ1, targ2, MAX_TARG)
	    call split_str (aper[1,n], aper1, aper2, MAX_APER)

	    # fill in one line
	    call pp_label (fd, 0., yoff, linenum[1,n])
	    call pp_label (fd, 6., yoff, rootname[1,n])
	    call pp_label (fd, 15., yoff, targ1)
	    call pp_label (fd, 29., yoff, opmode[1,n])
	    call pp_label (fd, 38., yoff, detector[1,n])
	    call pp_label (fd, 45., yoff, aper1)
	    call pp_label (fd, 57., yoff, filter[1,n])
	    call pp_label (fd, 62.5, yoff, dateobs[1,n])
	    call fprintf (fd, "justify 1\n")
	    call pp_move (fd, 73., yoff)
	    call fprintf (fd, "label '%8.1f'\n")
		call pargr (exptime[n])
	    call fprintf (fd, "justify 3\n")

	    # do the quality flags
	    if (qual[1,n] != EOS) {
	        call pp_move (fd, 76., yoff+0.25)
		if (strmatch(qual[1,n],"OK") != 0 || 	
		    strmatch(qual[1,n],"NO-EVAL") != 0 ||
		    strmatch(qual[1,n],"NO_TLM") != 0)
		    call fprintf (fd, "ptype 25 0\n")
		else
		    call fprintf (fd, "ptype 25 3\n")
		call fprintf (fd, "dot\n")
	    }
	    
	    # if the target name is long
	    if (targ2[1] != EOS || aper2[1] != EOS) {
		yoff = yoff + 0.75
	        call pp_label (fd, 17., yoff, targ2)
	        call pp_label (fd, 45., yoff, aper2)
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

# produce observation list header for FOS

procedure yobs_head (fd, yoff)

int	fd
real	yoff

begin

	# initialize and title
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "vpage 0.05 0.95 0.05 0.95\n")
	call fprintf (fd, "limits 0. 82. %6.2f 1.\n")
	    call pargr (BOTTOM)
	call pp_move (fd, 41., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfIObservation List'; expand 0.75\n")
	    call pargstr ("\\")
	call fprintf(fd,"justify 3\n")
	
	yoff = yoff + 2.

	# first line
	call pp_label (fd,  0., yoff, "Logsheet")
	call pp_label (fd, 29., yoff, "Operating")
	call pp_label (fd, 57., yoff, "Spectral")
	call pp_label (fd, 69., yoff, "Exposure")
	call pp_label (fd, 75., yoff, "Quality Flags")

	yoff = yoff + 0.75
	
	# second line
	call pp_label (fd,  0., yoff, "Line#")
	call pp_label (fd, 6., yoff, "Rootname")
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 29., yoff, "Mode")
	call pp_label (fd, 38., yoff, "Detector")
	call pp_label (fd, 45., yoff, "Aperture")
	call pp_label (fd, 57., yoff, "Element")
	call pp_label (fd, 62., yoff, "Obs Date")
	call pp_label (fd, 71., yoff, "(sec)")
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
