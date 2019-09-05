# Generate the calibration summary page for the NICMOS paper product

include	"npp.h"

procedure npp_calib (root, file, fd)

char	root[SZ_FNAME]		# file root name
int	file			# file type
int	fd			# the output igi script file pointer

pointer	im			# input file pointer
char	str[SZ_LINE]
real	x1, x2, x3, x4, x5, x6	# tab positions
real	yoff, y1, y2

pointer	immap()

begin

	# tab locations
	x1 = 0
	x2 = x1+8
	x3 = x2+8
	x4 = x3+19
	x5 = x4+8
	x6 = x5+15

	y1 = 0.9
	y2 = 0.3

	# Create the file names.
	call strcpy (root, str, SZ_FNAME)
	if (file == MOS)
	    call strcat ("_mos.fits[0]", str, SZ_FNAME)
	else if (file == CAL)
	    call strcat ("_cal.fits[0]", str, SZ_FNAME)
	else 
	    call strcat ("_raw.fits[0]", str, SZ_FNAME)

	im = immap (str, READ_ONLY, 0)

	# Start a new page.
	call fprintf (fd, "location 0 1 0 1\n")
	call fprintf (fd, "limits 0 80 32 0; vpage 0.05 0.95 0.05 0.87\n")

	# Headings.
	yoff = 0
	call fprintf (fd, "expand 1.; justify 2\n")
	call pp_move (fd, 40., yoff)
	call fprintf (fd, "label '%sCalibration Status Summary'\n")
	    call pargstr ("\\fB")
	call fprintf (fd, "expand 0.7; justify 3\n")

	yoff = yoff + 2
	call pp_move (fd, x1, yoff)
	call fprintf (fd, "label '%sSwitches and Flags'\n")
	    call pargstr ("\\fB")
	call pp_move (fd, x4, yoff)
	call fprintf (fd, "label '%sReference Files and Tables'\n")
	    call pargstr ("\\fB")

	# draw separating lines
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

	# Print each row
	# BIASCORR
	yoff = yoff + y2
	call imgstr (im, "BIASDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "BIASCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Wrapped pixel correction")
	call pp_label (fd, x4, yoff, "N/A")
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# ZOFFCORR
	yoff = yoff + y2
	call imgstr (im, "ZOFFDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "ZOFFCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Subtract MULTIACCUM zero read")
	call pp_label (fd, x4, yoff, "N/A")
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# MASKCORR
	yoff = yoff + y2
	call imgstr (im, "MASKDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "MASKCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Mask bad pixels")
	call pp_label (fd, x4, yoff, "MASKFILE")
	call imgstr (im, "MASKFILE", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "MASKPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# NOISCALC
	yoff = yoff + y2
	call imgstr (im, "NOISDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "NOISCALC")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Compute statistical errors")
	call pp_label (fd, x4, yoff, "NOISFILE")
	call imgstr (im, "NOISFILE", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "NOISPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# DARKCORR
	yoff = yoff + y2
	call imgstr (im, "DARKDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "DARKCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Dark subtraction")
	call pp_label (fd, x4, yoff, "DARKFILE")
	call imgstr (im, "DARKFILE", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "DARKPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# NLINCORR
	yoff = yoff + y2
	call imgstr (im, "NLINDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "NLINCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Linearity correction")
	call pp_label (fd, x4, yoff, "NLINFILE")
	call imgstr (im, "NLINFILE", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "NLINPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# FLATCORR
	yoff = yoff + y2
	call imgstr (im, "FLATDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "FLATCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Flat field correction")
	call pp_label (fd, x4, yoff, "FLATFILE")
	call imgstr (im, "FLATFILE", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "FLATPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# UNITCORR
	yoff = yoff + y2
	call imgstr (im, "UNITDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "UNITCORR")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Convert to count rates")
	call pp_label (fd, x4, yoff, "N/A")
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# PHOTCALC
	yoff = yoff + y2
	call imgstr (im, "PHOTDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "PHOTCALC")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Photometric calibration")
	call pp_label (fd, x4, yoff, "PHOTTAB")
	call imgstr (im, "PHOTTAB", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "PHOTPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# CRIDCALC
	yoff = yoff + y2
	call imgstr (im, "CRIDDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "CRIDCALC")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Identify cosmic ray hits")
	call pp_label (fd, x4, yoff, "N/A")
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# BACKCALC
	yoff = yoff + y2
	call imgstr (im, "BACKDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "BACKCALC")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Predict background")
	call pp_label (fd, x4, yoff, "BACKTAB")
	call imgstr (im, "BACKTAB", str, SZ_LINE)
	call pp_label (fd, x5, yoff, str)
	call imgstr (im, "BACKPDGR", str, SZ_LINE)
	call pp_label (fd, x6, yoff, str)
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# WARNCALC
	yoff = yoff + y2
	call imgstr (im, "WARNDONE", str, SZ_LINE)
	call pp_label (fd, x1, yoff, "WARNCALC")
	call pp_label (fd, x2, yoff, str)
	call pp_label (fd, x3, yoff, "Generate user warnings")
	call pp_label (fd, x4, yoff, "N/A")
	yoff = yoff + y1
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# ILLMCORR
	if (file == MOS) {
	    yoff = yoff + y2
	    call imgstr (im, "ILLMDONE", str, SZ_LINE)
	    call pp_label (fd, x1, yoff, "ILLMCORR")
	    call pp_label (fd, x2, yoff, str)
	    call pp_label (fd, x3, yoff, "Subtract background illumination")
	    call pp_label (fd, x4, yoff, "ILLMFILE")
	    call imgstr (im, "ILLMFILE", str, SZ_LINE)
	    call pp_label (fd, x5, yoff, str)
	    call imgstr (im, "ILLMPDGR", str, SZ_LINE)
	    call pp_label (fd, x6, yoff, str)
	    yoff = yoff + y1
	    call pp_move (fd,  0., yoff)
	    call pp_draw (fd, 80., yoff)
	}

	# draw a double line
	yoff = yoff + 0.2
	call pp_move (fd,  0., yoff)
	call pp_draw (fd, 80., yoff)

	# close the primary header
	call imunmap (im)
end
