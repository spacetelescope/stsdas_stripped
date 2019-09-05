# Use the CD matrix to draw compass

procedure pp_cdcompass (fd, x, y, size, cd)

int	fd		# output file pointer
real	x, y		# location where the compass will be drawn
real	size		# size of the compass
real    cd[2, 2]	# the CD matrix

real	shift		# the distance between the label letter from the compass
real	ratio		# aspect ration of the physical page
real	det		# determinant of the CD matrix
real	norm		# normalization factor
real	dx, dy

begin
	shift = 1.2
	
	# only works for landscape printout
	ratio = 11. / 8.5

	# reset the page
        call fprintf (fd, "reset\n")
        call fprintf (fd, "vpage 0. 1. 0. 1.\n")
        call fprintf (fd, "location 0. 1. 0. 1.\n")
        call fprintf (fd, "justify 5\n")

	# calculate the determinant of the CD matrix
	det = cd[1,1] * cd[2,2] - cd[1,2] * cd[2,1]

	if (det == 0.) {
	    call printf ("Singular CD matrix - no compass is generated.\n")
	    return
	}

	# draw the north direction
	dx = -cd[1,2] / det
	dy =  cd[1,1] / det
	norm = sqrt (dx**2 + dy**2)
	dx = dx * size / norm
	dy = dy * size * ratio / norm

	call pp_move (fd, x, y)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x+dx)
            call pargr (y+dy)

        call fprintf (fd, "expand 0.7\n")
	call pp_label (fd, x+shift*dx, y+shift*dy, "N")

	# draw the east direction
	dx =  cd[2,2] / det
	dy = -cd[2,1] / det
	norm = sqrt (dx**2 + dy**2)
	dx = dx * size / norm
	dy = dy * size * ratio / norm

	call pp_move (fd, x, y)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x+dx)
            call pargr (y+dy)

	call pp_label (fd, x+shift*dx, y+shift*dy, "E")
end
