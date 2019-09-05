# draw the dither/chopping patterns for NICMOS

define	NMAX		40	# maximum number of positions
define	SZ_LABEL	3

procedure npp_dithchop (fd, pattern, npos, dithsize, chopsize)

int	fd		# output file pointer
char	pattern[SZ_LINE]# dither-chopping pattern
int	npos		# number of dither-chopping positions
real	dithsize	# dither size (in arc seconds)
real	chopsize	# chop size (in arc seconds)

real	x, x0, y0, y, y1, y2	# location of the number
real	xloc[NMAX], yloc[NMAX]	# location of the number
real	hsize		# relative size of the arrow head(s)
real	tab		# relative size between the number and the arrow
real	x1, x2		# X location of successive numbers
real	expand		# font size 
real	xlim, ylim	# X and Y limits
real	dx, dy		# spacing between successive numbers
int	nplt		# number of plotted positions
int	i, j, dum, block
real	xch1, xch2, ych
char	dummy[SZ_LINE]
char	istr[SZ_LABEL]

int	itoc()
int	strmatch()
bool	streq()
bool	strne()

begin
	expand = 0.7
	tab = 0.3
	hsize = 0.6

        call fprintf (fd, "reset; fontset hard; vpage 0. 1. 0.05 0.95\n")
        call fprintf (fd, "justify 5\n")
        call fprintf (fd, "expand %0.3f\n")
	    call pargr (expand)
	
	# X strip dithering case
	if (streq (pattern, "XSTRIP-DITH")) {
	    xlim = 100.
	    ylim = 30.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    y0 = ylim / 2.
	    if (npos > 15) nplt = 15
	    else nplt = npos
	    dx = 6.

	    do i = 1, nplt {
		x = 0.5 * xlim + (real(i) - real(nplt+1)/2.) * dx
		
		if (npos > 15 && i > 8) j = (npos - 15) + i
		else j = i
		
		# draw the ellipsis
	   	if (npos > 15 && i == 8) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-0.1*dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x+0.1*dx, y0)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		} else {
		    dum = itoc (j, istr, SZ_LABEL)
		    call pp_label (fd, x, y0, istr)
	 	}

		# connect the numbers with arrows
		if (i > 1) {
		    x2 = x
		    call npp_arrow (fd, x1, y0, x2, y0, tab, hsize, 1, 1.)
		}
		x1 = x
	    }

	# Y strip dithering case
	} else if (streq (pattern, "YSTRIP-DITH")) {
	    xlim = 50.
	    ylim = 70.

            call fprintf (fd, "location 0.6 0.85 0.05 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    x0 = xlim / 2.
	    if (npos > 15) nplt = 15
	    else nplt = npos
	    dy = 4.5

	    do i = 1, nplt {
		y = 0.5 * ylim + (real(i) - real(nplt+1)/2.) * dy
		
		if (npos > 15 && i > 8) j = (npos - 15) + i
		else j = i
		
		# draw the ellipsis
	   	if (npos > 15 && i == 8) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x0, y)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x0, y-0.1*dy)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x0, y+0.1*dy)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		} else {
		    dum = itoc (j, istr, SZ_LABEL)
		    call pp_label (fd, x0, y, istr)
	 	}

		# connect the numbers with arrows
		if (i > 1) {
		    y2 = y
		    call npp_arrow (fd, x0, y1, x0, y2, tab, hsize, 1, 1.)
		}
		y1 = y
	    }

	# square-wave dithering case
	} else if (streq (pattern, "SQUARE-WAVE-DITH")) {
	    xlim = 100.
	    ylim = 30.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    x0 = xlim / 2.
	    y0 = ylim / 2.
	    if (npos > 20) {
		block = (npos - 1) / 4 - 4
		nplt = npos - block * 4
	    } else 
		nplt = npos
	    dx = 7.

	    do i = 1, nplt {
		x = x0 + dx * (real((i-1)/2)-real(nplt)/4.)
		
		if (npos > 20 && i > 12) {
		    j = block * 4 + i
		    x = x0 + dx * (real((i+1)/2)-real(nplt)/4.)
		} else 
		    j = i
		
		# draw the ellipsis
	   	if (npos > 20 && i == 13) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x-dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx-0.1*dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx+0.1*dx, y0)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		dum = mod (i, 4)
		if (dum == 1 || dum == 0) dum = 1
		else dum = 0
		y = y0 + dx * (0.5 - dum)
		dum = itoc (j, istr, SZ_LABEL)
		call pp_label (fd, x, y, istr)

		# connect the numbers with arrows
		x2 = x
		y2 = y
		if (i > 1) {
		    if (npos > 20 && i == 13) dum = dum
		    else call npp_arrow (fd, x1, y1, x2, y2, tab, hsize, 1, 1.)
		}
		x1 = x
		y1 = y
	    }

	# spiral dithering case
	} else if (streq (pattern, "SPIRAL-DITH")) {
	    xlim = 50.
	    ylim = 50.

            call fprintf (fd, "location 0.5 0.9 0.37 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    if (npos > NMAX) {
		call printf ("Too many position points, reset it to 40\n")
		npos = 40
	    }

	    x0 = xlim * 0.5
	    y0 = ylim * 0.5
	    if (npos > 10) nplt = 10
	    else nplt = npos
	    dx = 5.5

	    tab = 0.35 - (10 - nplt) * 0.02

	    call npp_spiral (x0, y0, dx, 1, npos, 1, xloc, yloc)
	    do i = 1, npos {
		dum = itoc (i, istr, SZ_LABEL)
		call pp_label (fd, xloc[i], yloc[i], istr)

		# connect the numbers with arrows
		if (i > 1) {
		    call npp_arrow (fd, xloc[i-1], yloc[i-1], xloc[i], yloc[i], 
					tab, hsize, 1, 1.)
		}
	    }

	# X strip dither-chop case
	} else if (streq (pattern, "XSTRIP-DITH-CHOP")) {
	    xlim = 100.
	    ylim = 30.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    x0 = xlim / 2.
	    y0 = ylim / 2.
	    if (npos > 20) {
		block = (npos - 1) / 2 - 9
		nplt = npos - block * 2
	    } else 
		nplt = npos
	    dx = 6.
	    dy = 16.

	    do i = 1, nplt {
		x = x0 + dx * (real((i-1)/2)-real(nplt)/4.)
		
		if (npos > 20 && i > 10) {
		    j = block * 2 + i
		    x = x0 + dx * (real((i+1)/2)-real(nplt)/4.)
		} else 
		    j = i
		
		# draw the ellipsis
	   	if (npos > 20 && i == 11) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x-dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx-0.1*dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx+0.1*dx, y0)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		dum = mod (i, 2)
		y = y0 + dy * (0.5 - dum)
		dum = itoc (j, istr, SZ_LABEL)
		call pp_label (fd, x, y, istr)

		# connect the numbers with arrows
		x2 = x
		y2 = y
		if (i > 1) {
		    if (npos > 20 && i == 11) dum = dum
		    else call npp_arrow (fd, x1, y1, x2, y2, tab, hsize, 1, 1.)
		}

		# draw the chop size label
		if (i == 2) {
		    call npp_arrow (fd, x1-0.5*dx, y1, x2-0.5*dx, y2, 0.0, 
					hsize, 2, 2.)
                    call fprintf (fd, "expand 0.6\n")
		    call sprintf (dummy, SZ_LINE, "CHOP-SIZE=%0.2f\"")
	    	    	call pargr (chopsize)
		    call pp_label (fd, x1/2., y0, dummy)
		    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		x1 = x
		y1 = y
	    }

	# Y strip dither-chop case
	} else if (streq (pattern, "YSTRIP-DITH-CHOP")) {
	    xlim = 100.
	    ylim = 30.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    x0 = xlim / 2.
	    y0 = ylim / 2.
	    if (npos > 20) {
		block = (npos - 1) / 2 - 9
		nplt = npos - block * 2
	    } else 
		nplt = npos
	    dx = 6.
	    dy = 16.

	    # write it side way
            call fprintf (fd, "angle 270\n")

	    do i = 1, nplt {
		x = x0 + dx * (real((i-1)/2)-real(nplt)/4.)
		
		if (npos > 20 && i > 10) {
		    j = block * 2 + i
		    x = x0 + dx * (real((i+1)/2)-real(nplt)/4.)
		} else 
		    j = i
		
		# draw the ellipsis
	   	if (npos > 20 && i == 11) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x-dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx-0.1*dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx+0.1*dx, y0)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		dum = mod (i-1, 2)
		y = y0 + dy * (0.5 - dum)
		dum = itoc (j, istr, SZ_LABEL)
		call pp_label (fd, x, y, istr)

		# connect the numbers with arrows
		x2 = x
		y2 = y
		if (i > 1) {
		    if (npos > 20 && i == 11) dum = dum
		    else call npp_arrow (fd, x1, y1, x2, y2, tab, hsize, 1, 1.)
		}

		# draw the chop size label
		if (i == 2) {
		    call npp_arrow (fd, x1-0.5*dx, y1, x2-0.5*dx, y2, 0.0, 
					hsize, 2, 2.)
                    call fprintf (fd, "expand 0.6\n")
		    call sprintf (dummy, SZ_LINE, "CHOP-SIZE=%0.2f\"")
	    	    	call pargr (chopsize)
		    call pp_label (fd, x1-dx, y0, dummy)
		    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		x1 = x
		y1 = y
	    }
            call fprintf (fd, "angle 0\n")

	# spiral dither-chop case
	} else if (streq (pattern, "SPIRAL-DITH-CHOP")) {
	    xlim = 100.
	    ylim = 23.

            call fprintf (fd, "location 0.1 0.9 0.635 0.92\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    if (npos > NMAX) {
		call printf ("Too many position points, reset it to 40\n")
		npos = 40
	    }

	    y0 = ylim * 0.35
	    if (npos > 10) nplt = 10
	    else nplt = npos
	    dx = 5.

	    tab = 0.35 - (10 - nplt) * 0.02

	    # do the odd points
	    x0 = xlim * 0.3
	    xch1 = x0
	    call npp_spiral (x0, y0, dx, 1, npos, 2, xloc, yloc)
	    do i = 1, npos, 2 {
		dum = itoc (i, istr, SZ_LABEL)
		call pp_label (fd, xloc[i], yloc[i], istr)

		# connect the numbers with arrows
		if (i > 1) {
		    call npp_arrow (fd, xloc[i-2], yloc[i-2], xloc[i], yloc[i], 
					tab, hsize, 1, 1.)
		}
	    }

	    # do the even points
	    x0 = xlim * 0.7
	    xch2 = x0
	    call npp_spiral (x0, y0, dx, 2, npos, 2, xloc, yloc)
	    do i = 2, npos, 2 {
		dum = itoc (i, istr, SZ_LABEL)
		call pp_label (fd, xloc[i], yloc[i], istr)

		# connect the numbers with arrows
		if (i > 2) {
		    call npp_arrow (fd, xloc[i-2], yloc[i-2], xloc[i], yloc[i], 
					tab, hsize, 1, 1.)
		}
	    }

	    # draw the chop size label
	    ych = y0 + dx * 2. + 1.5
	    call npp_arrow (fd, xch1, ych, xch2, ych, 0.0, hsize, 2, 2.)
            call fprintf (fd, "expand 0.6\n")
	    call sprintf (dummy, SZ_LINE, "CHOP-SIZE=%0.2f\"")
	       	call pargr (chopsize)
	    call pp_label (fd, xlim*0.5, ych+1.5, dummy)
	    call fprintf (fd, "expand %0.3f\n")
		call pargr (expand)

	# square-wave dithering case
	} else if (streq (pattern, "SQUARE-WAVE-DITH")) {
	    xlim = 100.
	    ylim = 30.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, dithsize, expand)

	    x0 = xlim / 2.
	    y0 = ylim / 2.
	    if (npos > 20) {
		block = (npos - 1) / 4 - 4
		nplt = npos - block * 4
	    } else 
		nplt = npos
	    dx = 7.

	    do i = 1, nplt {
		x = x0 + dx * (real((i-1)/2)-real(nplt)/4.)
		
		if (npos > 20 && i > 12) {
		    j = block * 4 + i
		    x = x0 + dx * (real((i+1)/2)-real(nplt)/4.)
		} else 
		    j = i
		
		# draw the ellipsis
	   	if (npos > 20 && i == 13) {
                    call fprintf (fd, "ptype 25 3; expand 0.1\n")
		    call pp_move (fd, x-dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx-0.1*dx, y0)
                    call fprintf (fd, "dot\n")
		    call pp_move (fd, x-dx+0.1*dx, y0)
                    call fprintf (fd, "dot\n")
            	    call fprintf (fd, "expand %0.3f\n")
			call pargr (expand)
		}
		dum = mod (i, 4)
		if (dum == 1 || dum == 0) dum = 1
		else dum = 0
		y = y0 + dx * (0.5 - dum)
		dum = itoc (j, istr, SZ_LABEL)
		call pp_label (fd, x, y, istr)

		# connect the numbers with arrows
		x2 = x
		y2 = y
		if (i > 1) {
		    if (npos > 20 && i == 13) dum = dum
		    else call npp_arrow (fd, x1, y1, x2, y2, tab, hsize, 1, 1.)
		}
		x1 = x
		y1 = y
	    }

	# All chop cases (one-, two-, four-, eight-chop)
	} else if (strmatch (pattern, "-CHOP") != 0 &&
		   strmatch (pattern, "-DITH-CHOP") == 0) {
	    xlim = 100.
	    ylim = 24.

            call fprintf (fd, "location 0.1 0.9 0.65 0.9\n")
	    call ndc_box (fd, xlim, ylim, pattern, chopsize, expand)

	    # write the number of positions
            call fprintf (fd, "expand 0.6\n")
            call fprintf (fd, "justify 9\n")
	    call sprintf (dummy, SZ_LINE, "NUMPOS=%d")
	       	call pargi (npos)
	    call pp_label (fd, 0.01*xlim+1., 0.99*ylim-2., dummy)
            call fprintf (fd, "expand %0.3f\n")
	        call pargr (expand)
            call fprintf (fd, "justify 5\n")

	    x0 = xlim / 2.
	    y0 = ylim / 2.
	    dx = 9.

	    call pp_label (fd, x0, y0, "1")
	    x = x0 + dx
	    y = y0
	    call pp_label (fd, x, y, "2")
	    call npp_arrow (fd, x0, y0, x, y, tab, hsize, 2, 1.)
	    if (strne (pattern, "ONE-CHOP")) {
		x = x0 - dx
	        call pp_label (fd, x, y, "4")
	        call npp_arrow (fd, x0, y0, x, y, tab, hsize, 2, 1.)
	        if (strne (pattern, "TWO-CHOP")) {
		    x = x0
		    do i = 6, 8, 2 {
		   	y = y0 + (i-7) * dx
			dum = itoc (i, istr, SZ_LABEL)
	        	call pp_label (fd, x, y, istr)
	        	call npp_arrow (fd, x0, y0, x, y, tab, hsize, 2, 1.)
		    }
		}
	    }
	    if (streq (pattern, "EIGHT-CHOP")) {
		do i = 10, 16, 2 {
		    x = x0 + (abs(i-13) - 2) * dx
		    y = y0 + (1 - mod(i,4)) * dx
		    dum = itoc (i, istr, SZ_LABEL)
	            call pp_label (fd, x, y, istr)
	            call npp_arrow (fd, x0, y0, x, y, tab, hsize, 2, 1.)
		}
	    }
	    
	} else if (strne (pattern, "NONE")) {
	    call printf ("Illegal pattern '%s'\n")
		call pargstr (pattern)
	}

end

# produce the coordinate for each point in a spiral pattern

procedure npp_spiral (x0, y0, dist, n1, n2, dn, x, y)

real	x0, y0		# location of the plot center
real	dist		# distance between two consecutive points
int	n1, n2		# staring and ending indices of the points
int	dn		# increment of the indices
real	x[ARB], y[ARB]	# output locations of the points

int	n, j
real	xx, yy

begin

	do j = n1, n2, dn {
	    n = (j - n1) / dn + 1
	    if (j == n1) {
		x[j] = x0
		y[j] = y0
	    } else {
		if (n==2 || (n>=8 && n<=10) || (n>=22 && n<=26)) {
		    xx = 1.
		    yy = 0.
		} 
		if (n==3 || (n>=11 && n<=13) || (n>=27 && n<=31)) {
		    xx = 0.
		    yy = 1.
		} 
		if ((n>=4 && n<=5) || (n>=14 && n<=17) || (n>=32 && n<=37)) {
		    xx = -1.
		    yy = 0.
		}
		if ((n>=6 && n<=7) || (n>=18 && n<=21) || (n>=38 && n<=43)) {
		    xx = 0.
		    yy = -1.
		}

		x[j] = x[j-dn] + dist * xx
		y[j] = y[j-dn] + dist * yy
	    }
	}
end
	
# Draw an arrow between two points
	    
procedure npp_arrow (fd, x1, y1, x2, y2, tab, hsize, nheads, lweight)

int	fd
real	x1, y1		# location of the first target point
real	x2, y2		# location of the second target point
real	tab		# the gap between the target point and the arrow's
			# starting point, as fraction of the distance between
			# the two target points
real	hsize		# arrow head size
int	nheads		# number of arrow heads
real	lweight		# line width

real	dx, dy, xx1, xx2, yy1, yy2
real	head

begin
        call fprintf (fd, "lweight %0.2f\n")
	    call pargr (lweight)

	dx = x2 - x1
	dy = y2 - y1
	head = hsize / sqrt (dx**2+dy**2)
	xx1 = x1 + tab * dx
	yy1 = y1 + tab * dy
	xx2 = x2 - tab * dx
	yy2 = y2 - tab * dy

	call pp_move (fd, xx1, yy1)
        call pp_draw (fd, xx2, yy2)

	# draw the arrow head
        call pp_draw (fd, xx2-head*(dx-dy), yy2-head*(dy+dx))
	call pp_move (fd, xx2, yy2)
        call pp_draw (fd, xx2-head*(dx+dy), yy2-head*(dy-dx))

	# draw the second arrow head
	if (nheads == 2) {
	    call pp_move (fd, xx1, yy1)
            call pp_draw (fd, xx1+head*(dx+dy), yy1+head*(dy-dx))
	    call pp_move (fd, xx1, yy1)
            call pp_draw (fd, xx1+head*(dx-dy), yy1+head*(dy+dx))
	}

	# reset the line width
        call fprintf (fd, "lweight 1\n")
end

# define the limits, draw a box, and write the pattern name
	    
procedure ndc_box (fd, xlim, ylim, pattern, dcsize, expand)

int	fd
real	xlim, ylim
char	pattern[ARB]
real	dcsize		# dither/chop size (in arc seconds)
real	expand

real	x1, x2, y1, y2
real	margin
char	dummy[SZ_LINE]
char	dummy2[SZ_LINE]

bool	streq()
int	strmatch()

begin
	margin = 0.01
        call fprintf (fd, "limits 0 %0.2f 0 %0.2f\n")
	    call pargr (xlim)
	    call pargr (ylim)
	
	# draw the box
	x1 = margin * xlim
	x2 = xlim - x1
	y1 = margin * ylim
	y2 = ylim - y1
	call pp_move (fd, x1, y1)
	call pp_draw (fd, x2, y1)
	call pp_draw (fd, x2, y2)
	call pp_draw (fd, x1, y2)
	call pp_draw (fd, x1, y1)

	# write the pattern name, and dither/chop size
	if (strmatch (pattern, "-CHOP") != 0 &&
	    strmatch (pattern, "-DITH-CHOP") == 0) {
	    call sprintf (dummy, SZ_LINE, "CHOP-SIZE=%0.2f\"")
	} else {
	    call sprintf (dummy, SZ_LINE, "DITH-SIZE=%0.2f\"")
	}
	        call pargr (dcsize)
	call sprintf (dummy2, SZ_LINE, "PATTERN: %s")
	    call pargstr (pattern)
        call fprintf (fd, "justify 7\n")
	if (streq (pattern, "YSTRIP-DITH-CHOP")) {
            call fprintf (fd, "angle 270\n")
	    call pp_label (fd, x2+1., y1+1., dummy2)
            call fprintf (fd, "expand 0.6\n")
	    call pp_label (fd, x2-2., y1+1., dummy)
            call fprintf (fd, "angle 0\n")
	} else {
	    call pp_label (fd, x2-1., y2+1., dummy2)
            call fprintf (fd, "expand 0.6\n")
	    call pp_label (fd, x2-1., y2-2., dummy)
	}
        call fprintf (fd, "justify 5\n")
        call fprintf (fd, "expand %0.3f\n")
	    call pargr (expand)
end
