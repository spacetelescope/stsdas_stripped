# draw a gray scale bar

procedure pp_gsbar (fd, vleft, vright, vbottom, vtop, min, max, label_size, 
			negative)

int	fd
real	vleft, vright, vbottom, vtop	# the location of the gray scale bar
real	min, max	# data extremes represented by the gray scale bar
real	label_size	# the size of the label
bool	negative	# is the image negative (i.e. black represents bright 
			# instead of faint object)?

begin

	# if the min and max are the same, just leave the box blank
	if (min != max) {

	    # construct the Z buffer, only need max of 128 points
            call fprintf (fd, "zsection dev$pix[1:128,1]\n")
            call fprintf (fd, "zevaluate r\n")
            call fprintf (fd, "limits\n")

	    # decide whether to render it as negative or positive
	    if (negative)
                call fprintf (fd, "zrange 128. 1.\n")
	    else
                call fprintf (fd, "zrange 1. 128.\n")

	    # draw the gray scale bar at the specified location
            call fprintf (fd, "location %0.3f %0.3f %0.3f %0.3f\n")
	        call pargr (vleft)
	        call pargr (vright)
	        call pargr (vbottom)
	        call pargr (vtop)
            call fprintf (fd, "pixmap\n")
	}

	# draw a box around it
        call fprintf (fd, "vmove %0.3f %0.3f\n")
	    call pargr (vleft)
	    call pargr (vbottom)
        call fprintf (fd, "vdraw %0.3f %0.3f\n")
	    call pargr (vright)
	    call pargr (vbottom)
        call fprintf (fd, "vdraw %0.3f %0.3f\n")
	    call pargr (vright)
	    call pargr (vtop)
        call fprintf (fd, "vdraw %0.3f %0.3f\n")
	    call pargr (vleft)
	    call pargr (vtop)
        call fprintf (fd, "vdraw %0.3f %0.3f\n")
	    call pargr (vleft)
	    call pargr (vbottom)

        call fprintf (fd, "expand %0.3f\n")
	    call pargr (label_size)
        call fprintf (fd, "justify 2\n")
        call fprintf (fd, "vmove %0.3f %0.3f; label '%0.5g'\n")
	    call pargr (vleft)
	    call pargr (vbottom-0.01)
	    call pargr (min)
        call fprintf (fd, "vmove %0.3f %0.3f; label '%0.5g'\n")
	    call pargr (vright)
	    call pargr (vbottom-0.01)
	    call pargr (max)
end
