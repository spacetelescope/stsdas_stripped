# produce the target/observation list banner

procedure list_banner (fd, visit, propid, instru, yoff)

int	fd
char	visit[ARB]
char	propid[ARB]
char	instru[ARB]
real	yoff

begin
	call fprintf (fd, 
		"reset; fontset hard; vpage 0.0 1 0.05 0.98; expand 1.\n")
	call fprintf (fd, "location 0 1 0 1\n")

	# print the shading
	call fprintf (fd, 
		"!printf ('0 .94%sn1 .94%sn1 1%sn0 1%sn',> 'tmp$prop%s')\n")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr (propid)
	call fprintf (fd, "data tmp$prop%s\n")
	    call pargstr (propid)
	call fprintf (fd, "xcol c1; ycol c2; color 5; fillpat 2; polygon\n")
	call fprintf (fd, "color 1; fillpat 1\n")
	call fprintf (fd, "!delete tmp$prop%s verify-\n")
	    call pargstr (propid)

	call fprintf (fd, "limits 1. 80. 24. 1.\n")
	call fprintf (fd, "vmove 0.02 0.98; justify 3\n")
	call fprintf (fd, "label '%sfBVisit: %s'\n")
	    call pargstr ("\\")
	    call pargstr (visit)

	call fprintf (fd, "vmove 0.5 0.98; justify 2\n")
	call fprintf (fd, "label '%sfBProposal: %s'\n")
	    call pargstr ("\\")
	    call pargstr (propid)

	call fprintf (fd, "vmove 0.98 0.98; justify 1\n")
	call fprintf (fd, "label '%sfI%s'\n")
	    call pargstr ("\\")
	    call pargstr (instru)

	yoff = 3.
end
