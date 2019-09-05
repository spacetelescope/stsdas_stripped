include	<math.h>

# draw compass.  When mir_revr is false, it means the parity is OPPOSITE to the
# sky (i.e. if north is to the +y direction, east will be to the +x direction)

procedure pp_compass (fd, x, y, size, orientat, mir_revr)

int	fd			# output file pointer
real	x, y			# location where the compass will be drawn
real	size		# size of the compass
real    orientat	# orientation of the compass
real	shift		# the distance between the label letter from the compass
real	ratio		# aspect ration of the physical page
bool    mir_revr	# is the image mirror reversed?

real	theta, dx, dy

begin
	# the mir_revr has the reverse meaning
	mir_revr = !(mir_revr)

	# positive angle is counter clockwise
	if (mir_revr) orientat = 360. - orientat

	shift = 1.2
	
	# only works for landscape printout
	ratio = 11. / 8.5

        call fprintf (fd, "reset\n")
        call fprintf (fd, "vpage 0. 1. 0. 1.\n")
        call fprintf (fd, "location 0. 1. 0. 1.\n")
        call fprintf (fd, "justify 5\n")

	theta = (90. - orientat) / RADIAN
	dx = size * cos (theta)
	dy = size * sin (theta) * ratio

	call pp_move (fd, x, y)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x+dx)
            call pargr (y+dy)

        call fprintf (fd, "expand 0.7\n")
	call pp_label (fd, x+shift*dx, y+shift*dy, "N")

	if (mir_revr)
	    theta = (360. - orientat) / RADIAN
	else
	    theta = (180. - orientat) / RADIAN
	dx = size * cos (theta)
	dy = size * sin (theta) * ratio

	call pp_move (fd, x, y)
        call fprintf (fd, "draw %0.3g %0.3g\n")
            call pargr (x+dx)
            call pargr (y+dy)

	call pp_label (fd, x+shift*dx, y+shift*dy, "E")
end
