# Create the cover page for FOC/WFPC2/FOS/GHRS paper products

define	SZ_TITLE	40

procedure gen_cover(fd, propid, visit, last_name, first_name, title1, title2)

int	fd				# output file pointer
char	propid[ARB]			# proposal ID
char	visit[ARB]			# visit number(s)
char	last_name[ARB]			# PI's last name
char	first_name[ARB]			# PI's first name 
char	title1[ARB], title2[ARB]	# proposal title
#char	timetag[ARB]			# time tag for the footnote


char	title[SZ_LINE], titlen[SZ_TITLE,10]
char	visitn[SZ_TITLE,10]
int	i, nout
real	x1, x2, yoff

begin

	# print the banner
	# Do this in the calling routine
	#call pp_banner (fd, "", "cover", "", instr, timetag, page)

        # initialize the page
        call fprintf (fd, 
                "reset; fontset hard; vpage 0.0 1 0.05 0.98; expand 1.8\n")
        call fprintf (fd, "location 0 1 0 0.97\n")

        call fprintf (fd, "limits 1 80 20 1\n")

	x1 = 13.
	x2 = 14.
	yoff = 3.

        # Print cover page
        call fprintf (fd, "justify 4\n")
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sfBProposal:'\n")
            call pargstr ("\\")
        call fprintf (fd, "justify 6\n")
        call pp_move (fd, x2, yoff)
        call fprintf (fd, "label '%sfB%s'\n")
            call pargstr ("\\")
            call pargstr (propid)

        call fprintf (fd, "justify 4\n")
        call pp_move (fd, x1, yoff+1)
        call fprintf (fd, "label '%sfBVisit:'\n")
            call pargstr ("\\")
        call fprintf (fd, "justify 6\n")

	call split_strn (visit, visitn, SZ_TITLE, nout)

	# Added this check to avoid problems with IGI and Postscript output.
	# 'nout' needs to be at least one, so that yoff can be incremented.
	#    WJH 23 Jun 97
	if (nout == 0) nout = 1

	do i = 1, nout {
	    yoff = yoff + 1
            call pp_move (fd, x2, yoff)
            call fprintf (fd, "label '%sfB%s'\n")
                call pargstr ("\\")
                call pargstr (visitn[1,i])
	}

	yoff = yoff + 1

	# print PI's full name
	if (first_name[1] != EOS && last_name[1] != EOS)
	    call strcat (", ", last_name, SZ_LINE)
        call fprintf (fd, "justify 4\n")
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sfBPI:'\n")
            call pargstr ("\\")
        call fprintf (fd, "justify 6\n")
        call pp_move (fd, x2, yoff)
        call fprintf (fd, "label '%sfB%s%s'\n")
            call pargstr ("\\")
            call pargstr (last_name)
            call pargstr (first_name)

	# print the title(s)
        call fprintf (fd, "justify 4\n")
        call pp_move (fd, x1, yoff+1)
        call fprintf (fd, "label '%sfBTitle:'\n")
            call pargstr ("\\")

        call fprintf (fd, "justify 6\n")
	call strcat (title1, title, SZ_LINE)
	call strcat (title2, title, SZ_LINE)
	call split_strn (title, titlen, SZ_TITLE, nout)

	if (nout == 0) nout = 1

	do i = 1, nout {
            call pp_move (fd, x2, yoff+i)
            call fprintf (fd, "label '%sfB%s'\n")
                call pargstr ("\\")
                call pargstr (titlen[1,i])
	}

	# print the Explanatory Note Page
	# We do this in pplist already...

	# print the banner
	#call pp_banner (fd, "", "", "", "NICMOS", timetag, page)

	# print the text
	#call npp_expl (fd)
end
