# generate the calibration summary page for STIS paper product

include "opp.h"

define	SZ_KEYWD	9

procedure opp_calib ()

char	fname[SZ_FNAME]			# image file name
char	trl[SZ_FNAME]			# trailer file
char	script[SZ_FNAME]		# igi script.
char    pedigree[SZ_LINE, NREF]		# pedigrees of reference files
char    name[SZ_KEYWD, NREF]		# calibration flag
char    caltext[SZ_LINE, NREF]		# calibration text
char	obstype[SZ_LINE]		# observation type
char	statflag[SZ_LINE]
char    str[SZ_LINE]
pointer	im
int     fd              		# the output igi script file pointer
real	x1, x2, x3, x4, x5, x6, yoff
real	y1, y2
bool	bdum

pointer immap()
int     open()
bool	imgetb()
bool	streq()

begin

	# get the input parameters
	call clgstr ("rootname", trl, SZ_FNAME)
	call clgstr ("filename", fname, SZ_FNAME)
        call clgstr ("igi_output", script, SZ_FNAME)

	call strcat ("_trl.fits", trl, SZ_FNAME)
	call strcat ("[0]", fname, SZ_FNAME)

	# open the input image
        im = immap (fname, READ_ONLY, 0)

        # open the output file
        fd = open (script, APPEND, TEXT_FILE)

	# tab locations
	x1 = 0
	x2 = x1+8
	x3 = x2+8
	x4 = x3+21
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
	call opp_pedigree (trl, pedigree)

	# get text
	call opp_caltext (name, caltext)

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

	# DOPPCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,DOPPCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,DOPPCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, DOPPCORR])
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

	# LFLGCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,LFLGCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,LFLGCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, LFLGCORR])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# LORSCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,LORSCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,LORSCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, LORSCORR])
 
        iferr (call imgstr (im, "MOFFTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "MOFFTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, MOFFTAB])
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

	if (streq(obstype, "IMAGING") || streq(obstype, "SPECTROSCOPIC")) {
	    yoff = yoff + y1
            call pp_move (fd, x4, yoff)
            call pp_draw (fd, 80., yoff)
	    yoff = yoff + y2

            iferr (call imgstr (im, "WBIAFILE", str, SZ_LINE)) str[1] = EOS
            call pp_label (fd, x4, yoff, "WBIAFILE")
            call pp_label (fd, x5, yoff, str)
            call pp_label (fd, x6, yoff, pedigree[1, WBIAFILE])
	}
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
 
	# BACKCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,BACKCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,BACKCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, BACKCORR])
 
        iferr (call imgstr (im, "APDESTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "APDESTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, APDESTAB])
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

	if (streq(obstype, "IMAGING")) {
	    yoff = yoff + y1
            call pp_move (fd, x4, yoff)
            call pp_draw (fd, 80., yoff)
	    yoff = yoff + y2
            iferr (call imgstr (im, "APERTAB", str, SZ_LINE)) str[1] = EOS
            call pp_label (fd, x4, yoff, "APERTAB")
            call pp_label (fd, x5, yoff, str)
            call pp_label (fd, x6, yoff, pedigree[1, APERTAB])
	}
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# GEOCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,GEOCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,GEOCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, GEOCORR])
 
        iferr (call imgstr (im, "IDCTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "IDCTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, IDCTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# X2DCORR
        yoff = yoff + y2
        iferr (call imgstr (im, name[1,X2DCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,X2DCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, X2DCORR])
 
        iferr (call imgstr (im, "SDCTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "SDCTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, SDCTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# X1DCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,X1DCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,X1DCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, X1DCORR])
 
        iferr (call imgstr (im, "XTRACTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "XTRACTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, XTRACTAB])

	yoff = yoff + y1
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)
	yoff = yoff + y2
        iferr (call imgstr (im, "SPTRCTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "SPTRCTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, SPTRCTAB])
        yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# WAVECORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,WAVECORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,WAVECORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, WAVECORR])
 
        iferr (call imgstr (im, "LAMPTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "LAMPTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, LAMPTAB])
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# DISPCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,DISPCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,DISPCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, DISPCORR])
 
        iferr (call imgstr (im, "DISPTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "DISPTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, DISPTAB])

	yoff = yoff + y1
        call pp_move (fd, x4, yoff)
        call pp_draw (fd, 80., yoff)
	yoff = yoff + y2
        iferr (call imgstr (im, "INANGTAB", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "INANGTAB")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, INANGTAB])
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# HELCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,HELCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,HELCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, HELCORR])
	yoff = yoff + y1
        call pp_move (fd,  0., yoff)
        call pp_draw (fd, 80., yoff)

	# FLUXCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,FLUXCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,FLUXCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, FLUXCORR])

	if (streq(obstype, "SPECTROSCOPIC")) {
            iferr (call imgstr (im, "APERTAB", str, SZ_LINE)) str[1] = EOS
            call pp_label (fd, x4, yoff, "APERTAB")
            call pp_label (fd, x5, yoff, str)
            call pp_label (fd, x6, yoff, pedigree[1, APERTAB])

	    yoff = yoff + y1
            call pp_move (fd, x4, yoff)
            call pp_draw (fd, 80., yoff)
	    yoff = yoff + y2
            iferr (call imgstr (im, "PCTAB", str, SZ_LINE)) str[1] = EOS
            call pp_label (fd, x4, yoff, "PCTAB")
            call pp_label (fd, x5, yoff, str)
            call pp_label (fd, x6, yoff, pedigree[1, PCTAB])
	}
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
 
	# SGEOCORR
	yoff = yoff + y2
        iferr (call imgstr (im, name[1,SGEOCORR], str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x1, yoff, name[1,SGEOCORR])
        call pp_label (fd, x2, yoff, str)
        call pp_label (fd, x3, yoff, caltext[1, SGEOCORR])

        iferr (call imgstr (im, "SDSTFILE", str, SZ_LINE)) str[1] = EOS
        call pp_label (fd, x4, yoff, "SDSTFILE")
        call pp_label (fd, x5, yoff, str)
        call pp_label (fd, x6, yoff, pedigree[1, SDSTFILE])

	# draw a double line
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


procedure opp_caltext (name, text)

char    name[SZ_KEYWD, NREF]		# calibartion flag
char    text[SZ_LINE, NREF]		# calibartion text

begin
        call strcpy ("STATFLAG", name[1, STATFLAG], SZ_KEYWD)
        call strcpy ("DOPPCORR", name[1, DOPPCORR], SZ_KEYWD)
        call strcpy ("DQICORR",  name[1, DQICORR],  SZ_KEYWD)
        call strcpy ("LFLGCORR", name[1, LFLGCORR], SZ_KEYWD)
        call strcpy ("LORSCORR", name[1, LORSCORR], SZ_KEYWD)
        call strcpy ("GLINCORR", name[1, GLINCORR], SZ_KEYWD)
        call strcpy ("ATODCORR", name[1, ATODCORR], SZ_KEYWD)
        call strcpy ("BLEVCORR", name[1, BLEVCORR], SZ_KEYWD)
        call strcpy ("BIASCORR", name[1, BIASCORR], SZ_KEYWD)
        call strcpy ("CRCORR",   name[1, CRCORR],   SZ_KEYWD)
        call strcpy ("EXPSCORR", name[1, EXPSCORR], SZ_KEYWD)
        call strcpy ("DARKCORR", name[1, DARKCORR], SZ_KEYWD)
        call strcpy ("BACKCORR", name[1, BACKCORR], SZ_KEYWD)
        call strcpy ("FLATCORR", name[1, FLATCORR], SZ_KEYWD)
        call strcpy ("SHADCORR", name[1, SHADCORR], SZ_KEYWD)
        call strcpy ("PHOTCORR", name[1, PHOTCORR], SZ_KEYWD)
        call strcpy ("GEOCORR",  name[1, GEOCORR],  SZ_KEYWD)
        call strcpy ("X2DCORR",  name[1, X2DCORR],  SZ_KEYWD)
        call strcpy ("X1DCORR",  name[1, X1DCORR],  SZ_KEYWD)
        call strcpy ("WAVECORR", name[1, WAVECORR], SZ_KEYWD)
        call strcpy ("DISPCORR", name[1, DISPCORR], SZ_KEYWD)
        call strcpy ("HELCORR",  name[1, HELCORR],  SZ_KEYWD)
        call strcpy ("FLUXCORR", name[1, FLUXCORR], SZ_KEYWD)
        call strcpy ("RPTCORR",  name[1, RPTCORR],  SZ_KEYWD)
        call strcpy ("SGEOCORR", name[1, SGEOCORR], SZ_KEYWD)

        call strcpy ("Calculate statistics", 
			text[1, STATFLAG], SZ_LINE)
        call strcpy ("Correct Doppler-induced velocity shift", 
			text[1, DOPPCORR], SZ_LINE)
        call strcpy ("Data quality initialization", 
			text[1, DQICORR], SZ_LINE)
        call strcpy ("Flag pixels for local and global non-linearities", 
			text[1, LFLGCORR], SZ_LINE)
        call strcpy ("Convert MAMA data to Lo-Res before processing", 
			text[1, LORSCORR], SZ_LINE)
        call strcpy ("Correct for global detector non-linearities", 
			text[1, GLINCORR], SZ_LINE)
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
        call strcpy ("Subtract dark image", 
			text[1, DARKCORR], SZ_LINE)
        call strcpy ("Subtract background (sky and interorder)", 
			text[1, BACKCORR], SZ_LINE)
        call strcpy ("Flat field data", 
			text[1, FLATCORR], SZ_LINE)
        call strcpy ("Apply shutter shading correction", 
			text[1, SHADCORR], SZ_LINE)
        call strcpy ("Populate photometric header keywords", 
			text[1, PHOTCORR], SZ_LINE)
        call strcpy ("Perform geometric correction for imaging modes", 
			text[1, GEOCORR], SZ_LINE)
        call strcpy ("Rectify 2-D spectral image", 
			text[1, X2DCORR], SZ_LINE)
        call strcpy ("Perform 1-D spectral extraction", 
			text[1, X1DCORR], SZ_LINE)
        call strcpy ("Use wavecal to adjust wavelength zeropoint", 
			text[1, WAVECORR], SZ_LINE)
        call strcpy ("Apply dispersion solutions", 
			text[1, DISPCORR], SZ_LINE)
        call strcpy ("Convert to heliocentric wavelengths", 
			text[1, HELCORR], SZ_LINE)
        call strcpy ("Convert to absolute flux units", 
			text[1, FLUXCORR], SZ_LINE)
        call strcpy ("Add individual repeat observations", 
			text[1, RPTCORR], SZ_LINE)
        call strcpy ("Correct for small scale geometric distortions", 
			text[1, SGEOCORR], SZ_LINE)
end
