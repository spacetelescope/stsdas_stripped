# produce the individual observation banner

procedure obs_banner (fd, linenum, root, propid, instru, yoff)

int	fd
char	linenum[ARB]
char	root[ARB]
char	propid[ARB]
char	instru[ARB]
real	yoff

char	rootupr[SZ_FNAME]
char	tmp_banner[SZ_FNAME]

begin
	call strcpy (root, rootupr, SZ_FNAME)
	call strupr (rootupr)

	call mktemp ("tmp$ban", tmp_banner, SZ_FNAME)

	call fprintf (fd, 
		"reset; fontset hard; vpage 0.0 1 0.05 0.98; expand 1.\n")
	call fprintf (fd, "location 0 1 0 1\n")

	# print the shading
	call fprintf (fd, 
		"!printf ('0 .94%sn1 .94%sn1 1%sn0 1%sn',> '%s')\n")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr (tmp_banner)
	call fprintf (fd, "data %s\n")
	    call pargstr (tmp_banner)
	call fprintf (fd, "xcol c1; ycol c2; color 5; fillpat 2; polygon\n")
	call fprintf (fd, "color 1; fillpat 1\n")
	call fprintf (fd, "!delete %s verify-\n")
	    call pargstr (tmp_banner)

	call fprintf (fd, "vmove 0.02 0.98; justify 3\n")
	call fprintf (fd, "label '%sfBLogsheet Line# %s'\n")
	    call pargstr ("\\")
	    call pargstr (linenum)

	call fprintf (fd, "vmove 0.3 0.98; justify 3\n")
	call fprintf (fd, "label '%sfBObservation: %s'\n")
	    call pargstr ("\\")
	    call pargstr (rootupr)

	call fprintf (fd, "vmove 0.85 0.98; justify 1\n")
	call fprintf (fd, "label '%sfBProposal: %s'\n")
	    call pargstr ("\\")
	    call pargstr (propid)

	call fprintf (fd, "vmove 0.98 0.98; justify 1\n")
	call fprintf (fd, "label '%sfI%s'\n")
	    call pargstr ("\\")
	    call pargstr (instru)

	call fprintf (fd, "vpage 0.05 0.95 0.05 0.93; expand 0.9\n")
end
