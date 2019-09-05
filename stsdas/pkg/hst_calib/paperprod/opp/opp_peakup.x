include <imhdr.h>

define	MAXCH	5		# maximum digits of the DN

# draw the peakup plot for STIS

procedure opp_peakup (im, fd, x1, x2, y1, y2)

pointer	im
int	fd
real	x1, x2, y1, y2

real	x3, y3
pointer	dn
pointer	xsum, ysum
real	x, y
real	xmin, xmax, ymin, ymax
int	xdim, ydim
int	i, j, indx, dummy
char	str[MAXCH]
real	dx, dy
real	size1, size2

pointer	imgs2r()
int	itoc()

begin

	x3 = x2 + (x2 - x1) * 0.3
	y3 = y2 + (y2 - y1) * 0.3

        call fprintf (fd, "fontset hard\n")
        call fprintf (fd, "justify 5\n")

	# get dimensions of the image
	xdim = IM_LEN(im, 1)
	ydim = IM_LEN(im, 2)

	# determine the font size
	size1 = 0.65
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size1)

	if (xdim <= 5)
	    size2 = size1 
	else
	    size2 = size1 * 5. / real(xdim)

	# allocate summation arrays
	call calloc (xsum, xdim, TY_REAL)
	call calloc (ysum, ydim, TY_REAL)

	# read the data
	dn = imgs2r (im, 1, xdim, 1, ydim)

	# spacing of the data on paper
	dx = (x2 - x1) / real(xdim)
	dy = (y2 - y1) / real(ydim)
	
	# plot the data
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size2)
	do j = 1, ydim {
	    indx = (j-1) * xdim 
	    y = y1 + dy * (j - 0.5)
	    do i = 1, xdim {
		x = x1 + dx * (i - 0.5)
		dummy = itoc (nint(Memr[dn+indx+i-1]), str, MAXCH)
		call pp_label (fd, x, y, str)
	    }
	}

	# pixel numbers on each axis
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size1)
        call fprintf (fd, "angle 0\n")
	call pp_label (fd, (x1+x2)/2., y1-0.05, "Pixel")
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size2)
	do i = 1, xdim {
	    x = x1 + dx * (i - 0.5)
	    dummy = itoc (i, str, MAXCH)
	    call pp_label (fd, x, y1-0.03, str)
	}
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size1)
        call fprintf (fd, "angle 90\n")
	call pp_label (fd, x1-0.05, (y1+y2)/2., "Pixel")
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size2)
	do j = 1, ydim {
	    y = y1 + dy * (j - 0.5)
	    dummy = itoc (j, str, MAXCH)
	    call pp_label (fd, x1-0.03, y, str)
	}

	# draw the title
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (size1)
        call fprintf (fd, "angle 0\n")
	call pp_label (fd, (x1+x2)/2., y3+0.03, "ACQ/PEAKUP Image")
	   	 
	# draw the box
	call pp_move (fd, x2, y1)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x2)
            call pargr (y3)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x1)
            call pargr (y3)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x1)
            call pargr (y1)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x3)
            call pargr (y1)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x3)
            call pargr (y2)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x1)
            call pargr (y2)
	
	# calculate the projected sum on each axis
	do j = 1, ydim {
	    Memr[ysum+j-1] = 0.
	    do i = 1, xdim {
		indx = (j-1) * xdim + i - 1
		Memr[ysum+j-1] = Memr[ysum+j-1] + Memr[dn+indx]
	    }
	    #call printf ("ysum(%d) = %0.1f\n")
		#call pargi(j)
		#call pargr(Memr[ysum+j-1])
	}
	do i = 1, xdim {
	    Memr[xsum+i-1] = 0.
	    do j = 1, ydim {
		indx = (j-1) * xdim + i - 1
		Memr[xsum+i-1] = Memr[xsum+i-1] + Memr[dn+indx]
	    }
	    #call printf ("xsum(%d) = %0.1f\n")
		#call pargi(i)
		#call pargr(Memr[xsum+i-1])
	}

	# plot the summation curves
	call alimr (Memr[xsum], xdim, xmin, xmax)
	call alimr (Memr[ysum], ydim, ymin, ymax)
	if (xdim > 1) {
	    do i = 1, xdim {
		x = x1 + dx * (i - 0.5)
		if (xmax == xmin) y = (y2+y3) / 2.
		else y = (y2+0.01) + (Memr[xsum+i-1]-xmin) * (y3-y2-0.02) /
			 (xmax-xmin)
		if (i == 1) call pp_move (fd, x, y)
		else {
        	    call fprintf (fd, "draw %0.3g %0.3g\n")
            	      	call pargr (x)
            		call pargr (y)
		}
	    }
	}
	if (ydim > 1) {
	    do i = 1, ydim {
		y = y1 + dy * (i - 0.5)
		if (ymax == ymin) x = (x2+x3) / 2.
		else x = (x2+0.01) + (Memr[ysum+i-1]-ymin) * (x3-x2-0.02) /
			 (ymax-ymin)
		if (i == 1) call pp_move (fd, x, y)
		else {
        	    call fprintf (fd, "draw %0.3g %0.3g\n")
            	      	call pargr (x)
            		call pargr (y)
		}
	    }
	}

	# free the memory
	call mfree (xsum, TY_REAL)
	call mfree (ysum, TY_REAL)
end
