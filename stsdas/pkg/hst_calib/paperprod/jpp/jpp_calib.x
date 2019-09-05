# generate the calibration summary page for ACS paper product

include "jpp.h"

define	SZ_KEYWD	9

procedure jpp_calib ()

char    root[SZ_FNAME]
char    prodext[SZ_FNAME]
char	fname[SZ_FNAME]			# image file name
char	trl[SZ_FNAME]			# trailer file
char	script[SZ_FNAME]		# igi script.
char    pedigree[SZ_LINE, NREF]		# pedigrees of reference files
char    name[SZ_KEYWD, NREF]		# calibration flag
char    caltext[SZ_LINE, NREF]		# calibration text
char	obstype[SZ_LINE]		# observation type
char	statflag[SZ_LINE]
char	errflag[SZ_LINE]
char    str[SZ_LINE]
pointer	im
int     fd              		# the output igi script file pointer
real	x1, x2, x3, x4, x5, x6, yoff
real	y1, y2
bool	bdum

pointer immap()
int     open()
bool	imgetb()

begin

	# get the input parameters
	call clgstr ("root", root, SZ_FNAME)
	call clgstr ("prodext", prodext, SZ_FNAME)
    call clgstr ("igi_output", script, SZ_FNAME)

    # build the necessary filenames...
	call strcpy (root, fname, SZ_FNAME)
    call strcat (prodext, fname, SZ_FNAME)
	call strcat ("[0]", fname, SZ_FNAME)
    
    call strcpy (root, trl, SZ_FNAME)
    call strcat (".tra", trl, SZ_FNAME)
	call strcat ("[0]", trl, SZ_FNAME)

	# open the input image
        im = immap (fname, READ_ONLY, 0)

        # open the output file
        fd = open (script, APPEND, TEXT_FILE)

	# tab locations
	x1 = 0
	x2 = x1+8
	x3 = x2+8
	x4 = x3+25
	x5 = x4+7
	x6 = x5+14

	y1 = 0.85
	y2 = 0.25

	# Start a new page.
        call fprintf (fd, "location 0 1 0 1\n")
        call fprintf (fd, "limits 0 80 42 0; vpage 0.05 0.95 0.05 0.87\n")

	# Headings
	yoff = 0
        call fprintf (fd, "expand 1.; justify 2\n")
        call pp_move (fd, 40., yoff)
        call fprintf (fd, "label '%sCalibration Status Summary'\n")
            call pargstr ("\\fB")
        call fprintf (fd, "expand 0.55; justify 3\n")

	yoff = yoff + 3
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sSwitches and Flags'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x4, yoff)
        call fprintf (fd, "label '%sReference Files and Tables'\n")
            call pargstr ("\\fB")

	yoff = yoff + y1
        call pp_move (fd, x1, yoff)
        call pp_draw (fd, x3, yoff)
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)

	yoff = yoff + y2
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sKeyword'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x2, yoff)
        call fprintf (fd, "label '%sValue'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x3, yoff)
        call fprintf (fd, "label '%sCalibration Step'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x4, yoff)
        call fprintf (fd, "label '%sKeyword'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x5, yoff)
        call fprintf (fd, "label '%sFile Name'\n")
            call pargstr ("\\fB")
        call pp_move (fd, x6, yoff)
        call fprintf (fd, "label '%sPedigree'\n")
            call pargstr ("\\fB")

	# draw a double line
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)
        yoff = yoff + 0.2
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# get pedigrees
	call jpp_pedigree (trl, pedigree)

	# get text
	call jpp_caltext (name, caltext)

	# read the observation type
        call imgstr (im, "OBSTYPE", obstype, SZ_LINE)

	# Print each row
	# ==============
	# STATFLAG
	yoff = yoff + y2
        iferr (bdum = imgetb (im, name[1,STATFLAG])) statflag[1] = EOS
	else {
            if (bdum)
                call strcpy ("T", statflag, SZ_LINE)
            else
                call strcpy ("F", statflag, SZ_LINE)
	}
        call pp_label (fd, x1, yoff, name[1,STATFLAG])
        call pp_label (fd, x2, yoff, statflag)
        call pp_label (fd, x3, yoff, caltext[1, STATFLAG])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# WRTERR
	yoff = yoff + y2
        iferr (bdum = imgetb (im, name[1,WRTERR])) errflag[1] = EOS
	else {
            if (bdum)
                call strcpy ("T", errflag, SZ_LINE)
            else
                call strcpy ("F", errflag, SZ_LINE)
	}
        call pp_label (fd, x1, yoff, name[1,WRTERR])
        call pp_label (fd, x2, yoff, statflag)
        call pp_label (fd, x3, yoff, caltext[1, WRTERR])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# DQICORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,DQICORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,DQICORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, DQICORR])

        iferr (call imgstr (im, "BPIXTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "BPIXTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, BPIXTAB])
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# ATODCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,ATODCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,ATODCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, ATODCORR])
 
        iferr (call imgstr (im, "ATODTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "ATODTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, ATODTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# BLEVCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,BLEVCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,BLEVCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, BLEVCORR])
 
        iferr (call imgstr (im, "CCDTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "CCDTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, CCDTAB])
        yoff = yoff + y1
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)

	yoff = yoff + y2
        iferr (call imgstr (im, "OSCNTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "OSCNTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, OSCNTAB])

        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# BIASCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,BIASCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,BIASCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, BIASCORR])
 
        iferr (call imgstr (im, "BIASFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "BIASFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, BIASFILE])

        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# CRCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,CRCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,CRCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, CRCORR])
 
        iferr (call imgstr (im, "CRREJTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "CRREJTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, CRREJTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# EXPSCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,EXPSCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,EXPSCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, EXPSCORR])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# SHADCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,SHADCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,SHADCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, SHADCORR])
 
        iferr (call imgstr (im, "SHADFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "SHADFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, SHADFILE])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# GLINCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,GLINCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,GLINCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, GLINCORR])
 
        iferr (call imgstr (im, "MLINTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "MLINTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, MLINTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# LFLGCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,LFLGCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,LFLGCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, LFLGCORR])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# DARKCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,DARKCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,DARKCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, DARKCORR])
 
        iferr (call imgstr (im, "DARKFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "DARKFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, DARKFILE])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)
  
	# FLATCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,FLATCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,FLATCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, FLATCORR])
 
        iferr (call imgstr (im, "PFLTFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "PFLTFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, PFLTFILE])

	yoff = yoff + y1
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)
	yoff = yoff + y2
        iferr (call imgstr (im, "DFLTFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "DFLTFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, DFLTFILE])

	yoff = yoff + y1
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)
	yoff = yoff + y2
        iferr (call imgstr (im, "LFLTFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "LFLTFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, LFLTFILE])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# PHOTCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,PHOTCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,PHOTCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, PHOTCORR])
 
        iferr (call imgstr (im, "PHOTTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "PHOTTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, PHOTTAB])

	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# RPTCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,RPTCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,RPTCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, RPTCORR])
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)
 
	# DITHCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,DITHCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,DITHCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, DITHCORR])
 
        iferr (call imgstr (im, "DITHTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "DITHTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, DITHTAB])

	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

    #
    # Done with calibration step summary
	# draw a double line
    #
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)
	yoff = yoff + 0.2
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

        # close images
        call imunmap (im)
 	call close (fd)
end


procedure jpp_caltext (name, text)

char    name[SZ_KEYWD, NREF]		# calibartion flag
char    text[SZ_LINE, NREF]		# calibartion text

begin
        call strcpy ("STATFLAG", name[1, STATFLAG], SZ_KEYWD)
        call strcpy ("WRTERR", name[1, WRTERR], SZ_KEYWD)
        call strcpy ("DQICORR",  name[1, DQICORR],  SZ_KEYWD)
        call strcpy ("ATODCORR", name[1, ATODCORR], SZ_KEYWD)
        call strcpy ("BLEVCORR", name[1, BLEVCORR], SZ_KEYWD)
        call strcpy ("BIASCORR", name[1, BIASCORR], SZ_KEYWD)
        call strcpy ("CRCORR",   name[1, CRCORR],   SZ_KEYWD)
        call strcpy ("EXPSCORR", name[1, EXPSCORR], SZ_KEYWD)
        call strcpy ("SHADCORR", name[1, SHADCORR], SZ_KEYWD)
        call strcpy ("GLINCORR", name[1, GLINCORR], SZ_KEYWD)
        call strcpy ("LFLGCORR", name[1, LFLGCORR], SZ_KEYWD)
        call strcpy ("DARKCORR", name[1, DARKCORR], SZ_KEYWD)
        call strcpy ("FLATCORR", name[1, FLATCORR], SZ_KEYWD)
        call strcpy ("PHOTCORR", name[1, PHOTCORR], SZ_KEYWD)
        call strcpy ("RPTCORR",  name[1, RPTCORR],  SZ_KEYWD)
        call strcpy ("DITHCORR",  name[1, DITHCORR],  SZ_KEYWD)

        call strcpy ("Calculate statistics", 
			text[1, STATFLAG], SZ_LINE)
        call strcpy ("Write out error array extension", 
			text[1, WRTERR], SZ_LINE)
        call strcpy ("Data quality initialization", 
			text[1, DQICORR], SZ_LINE)
        call strcpy ("Correct for A-to-D conversion errors", 
			text[1, ATODCORR], SZ_LINE)
        call strcpy ("Subtract bias level computed from overscan", 
			text[1, BLEVCORR], SZ_LINE)
        call strcpy ("Subtract bias image", 
			text[1, BIASCORR], SZ_LINE)
        call strcpy ("Combine observations to reject cosmic rays", 
			text[1, CRCORR], SZ_LINE)
        call strcpy ("Process individual observations after cr-rejection", 
			text[1, EXPSCORR], SZ_LINE)
        call strcpy ("Apply shutter shading correction", 
			text[1, SHADCORR], SZ_LINE)
        call strcpy ("Correct MAMA data for global detector non-linearities", 
			text[1, GLINCORR], SZ_LINE)
        call strcpy ("Flag pixels for local and global non-linearities", 
			text[1, LFLGCORR], SZ_LINE)
        call strcpy ("Subtract dark image", 
			text[1, DARKCORR], SZ_LINE)
        call strcpy ("Flat field data", 
			text[1, FLATCORR], SZ_LINE)
        call strcpy ("Populate photometric header keywords", 
			text[1, PHOTCORR], SZ_LINE)
        call strcpy ("Add individual repeat observations", 
			text[1, RPTCORR], SZ_LINE)
        call strcpy ("Process dithered images", 
			text[1, DITHCORR], SZ_LINE)
end
