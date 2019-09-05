procedure flag_caption (fd, yoff)

int	fd
real	yoff

real	x0

begin
	x0 = 1.

	# flag caption
	yoff = yoff + 0.75
	call pp_label (fd, x0, yoff, "Quality flags:") 
	call pp_move (fd, x0+15., yoff+0.25)
	call fprintf (fd, "ptype 25 0\n")
	call fprintf (fd, "dot\n")
	call pp_label (fd, x0+16., yoff, " = OK") 
	call pp_move (fd, x0+25., yoff+0.25)
	call fprintf (fd, "ptype 25 3\n")
	call fprintf (fd, "dot\n")
	call pp_label (fd, x0+26., yoff, " = Not OK")
	call pp_label (fd, x0+35., yoff, "Blank = Unknown or file missing") 
end
