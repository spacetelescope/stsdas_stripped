# generate the exposure summary for NICMOS paper product

include	<imhdr.h>

define	SZ_TARG	15

procedure npp_expsum (im, fd)

pointer	im			# input image pointer
int	fd			# output file pointer

real	x1, x2, yoff		# tab positions
int	camera			# camera used
real	imscale			# image scale
char	strval[SZ_LINE]
char	targ1[SZ_TARG+1]	# target name
char	targ2[SZ_TARG+1]	# target name
real	rval

int	imgeti()
real	imgetr()
double	imgetd()
bool	streq()

begin
	
	# redefines the window
        call fprintf (fd, "reset\n")
        call fprintf (fd, "fontset hard\n")
        call fprintf (fd, "vpage 0.65 1. 0.00 0.92\n")
        call fprintf (fd, "limits 0 30 35 0\n")
        call fprintf (fd, "move 0 0; draw 0 40\n")
	call fprintf (fd, "expand 0.7\n")

	# tab positions
	x1 = 16.
	x2 = 17.
	yoff = -1.
    
	# print OBS keywords
	yoff = yoff + 2.
        call imgstr (im, "ROOTNAME", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Rootname:", strval)

	yoff = yoff + 1.
        call imgstr (im, "DATE-OBS", strval, SZ_LINE)
	call date_str (strval, strval)
	call print_expsum (fd, x1, x2, yoff, "Obs. Date:", strval)

	yoff = yoff + 1.
        call imgstr (im, "TIME-OBS", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Obs. Time:", strval)

	# print TARGET keywords
	targ1[1] = EOS
	targ2[1] = EOS
	yoff = yoff + 2.
        call imgstr (im, "TARGNAME", strval, SZ_LINE)
	call split_str (strval, targ1, targ2, SZ_TARG)
	call print_expsum (fd, x1, x2, yoff, "Target Name:", targ1)
	if (targ2[1] != EOS) {
	    yoff = yoff + 1.
	    call print_expsum (fd, x1, x2, yoff, "", targ2)
	}

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2H")
	    call pargd (imgetd (im, "RA_TARG"))
	call print_expsum (fd, x1, x2, yoff, "R.A. (J2000):", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.1h")
	    call pargd (imgetd (im, "DEC_TARG"))
	call print_expsum (fd, x1, x2, yoff, "Dec. (J2000):", strval)

	# print IMAGING keywords
	yoff = yoff + 2.
	camera = imgeti (im, "CAMERA")
	call sprintf (strval, SZ_LINE, "%d")
	    call pargi (camera)
	call print_expsum (fd, x1, x2, yoff, "Camera Used:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%d")
	    call pargi (imgeti (im, "PRIMECAM"))
	call print_expsum (fd, x1, x2, yoff, "Prime Camera:", strval)

	yoff = yoff + 1.
        iferr (rval = imgetr (im, "NPFOCUSP")) 
	    strval[1] = EOS
	else {
	    call sprintf (strval, SZ_LINE, "%0.3f deg")
	        call pargr (rval)
	}
	call print_expsum (fd, x1, x2, yoff, "PAM Focus Position:", strval)

	yoff = yoff + 1.
        call imgstr (im, "APERTURE", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Aperture:", strval)

	yoff = yoff + 1.
        call imgstr (im, "OBSMODE", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Observation Mode:", strval)

	if (streq (strval, "MULTIACCUM")) {
            call imgstr (im, "SAMP_SEQ", strval, SZ_LINE)
	    yoff = yoff + 1.
	    call print_expsum (fd, x1, x2, yoff, "Samp Seq:", strval)
	}

	yoff = yoff + 1.
        call imgstr (im, "FILTER", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Spectral Element:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.3f sec")
	    call pargr (imgetr (im, "EXPTIME"))
	call print_expsum (fd, x1, x2, yoff, "Exp Time per Iteration:", 
			   strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%d")
	    call pargi (imgeti (im, "NUMITER"))
	call print_expsum (fd, x1, x2, yoff, "Number Iterations:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%d x %d")
	    call pargi (IM_LEN(im, 1))
	    call pargi (IM_LEN(im, 2))
	call print_expsum (fd, x1, x2, yoff, "Image Size:", strval)

	yoff = yoff + 1.
	if (camera == 1) imscale = 0.043
	else if (camera == 2) imscale = 0.075
	else if (camera == 3) imscale = 0.20
	call sprintf (strval, SZ_LINE, "%0.3f\"/pix")
	    call pargr (imscale)
	call print_expsum (fd, x1, x2, yoff, "Plate Scale:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f deg")
	    call pargr (imgetr (im, "ORIENTAT"))
	call print_expsum (fd, x1, x2, yoff, "Orientation:", strval)

	# print Pattern keywords
	yoff = yoff + 2.
        call imgstr (im, "PATTERN", strval, SZ_LINE)
	call print_expsum (fd, x1, x2, yoff, "Pattern Name:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f deg")
	    call pargr (imgetr (im, "PORIENT"))
	call print_expsum (fd, x1, x2, yoff, "Pattern Orient:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%d")
	    call pargi (imgeti (im, "NUMPOS"))
	call print_expsum (fd, x1, x2, yoff, "Number Positions:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f\"")
	    call pargr (imgetr (im, "DITHSIZE"))
	call print_expsum (fd, x1, x2, yoff, "Dither Size:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f\"")
	    call pargr (imgetr (im, "CHOPSIZE"))
	call print_expsum (fd, x1, x2, yoff, "Chop Size:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f\"")
	    call pargr (imgetr (im, "FOMXPOS"))
	call print_expsum (fd, x1, x2, yoff, "FOM X Offset:", strval)

	yoff = yoff + 1.
	call sprintf (strval, SZ_LINE, "%0.2f\"")
	    call pargr (imgetr (im, "FOMYPOS"))
	call print_expsum (fd, x1, x2, yoff, "FOM Y Offset:", strval)
end


procedure print_expsum (fd, x1, x2, y, label, strval)

int	fd		# output file pointer
real	x1, x2, y	# location of the label and its value
char	label[SZ_LINE]	# the label
char	strval		# header keyword value corresponding to the label

begin
	call fprintf (fd, "justify 7\n")
	call pp_label (fd, x1, y, label)
	call fprintf (fd, "justify 9\n")
	call pp_label (fd, x2, y, strval)
end
