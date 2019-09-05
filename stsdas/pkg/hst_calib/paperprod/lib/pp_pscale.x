# draw plate scale

procedure pp_pscale (fd, x, y, vsize, pscale, vscale)

int	fd
real	x, y		# location of the plate scale center
real	vsize		# the nominal "visual" size of the plate scale bar
real	pscale		# plate scale in arcsec/pixel
real	vscale		# visual size per pixel of the plotted image

real	r, label, bsize, size
char	label_text[SZ_LINE]
int	m

begin
	# figure out the bar size
	bsize = vscale / pscale		# in "v-unit"/arcsec
	r = vsize / bsize
	m = int(log10 (r*0.6))
	
	if (m == 0) {
	    label = 1.
	    if (r > 1.6 && r <= 3.2)
		label = label * 2.
	    else if (r > 3.2)
		label = label * 5.
	    call sprintf (label_text, SZ_LINE, "%1d arc sec")
	    	call pargi (nint(label))
	} else if (m == 1) {
	    label = 10.
	    if (r > 1.6 && r <= 3.2)
		label = label * 2.
	    else if (r > 3.2)
		label = label * 5.
	    call sprintf (label_text, SZ_LINE, "%2d arc sec")
	    	call pargi (nint(label))
	} else if (m == -1) {
	    label = 0.1
	    if (r > 1.6 && r <= 3.2)
		label = label * 2.
	    else if (r > 3.2)
		label = label * 5.
	    call sprintf (label_text, SZ_LINE, "%3.1f arc sec")
	    	call pargi (label)
	} else 
	    call error (1, "scale too large or too small")

	size = label * bsize

        call fprintf (fd, "reset\n")
        call fprintf (fd, "vpage 0. 1. 0. 1.\n")
        call fprintf (fd, "location 0. 1. 0. 1.\n")
        call fprintf (fd, "justify 2\n")

	call pp_move (fd, x-size/2., y)
        call fprintf (fd, "draw %5g %5g\n")
            call pargr (x-size/2.)
            call pargr (y-0.015)
        call fprintf (fd, "draw %5g %5g\n")
            call pargr (x+size/2.)
            call pargr (y-0.015)
        call fprintf (fd, "draw %5g %5g\n")
            call pargr (x+size/2.)
            call pargr (y)

        call fprintf (fd, "expand 0.7\n")
	call pp_label (fd, x, y+0.03, label_text)
end
