# produce the paper product banner

procedure pp_banner (fd, visit, obs, prop, instru, timetag, page)

int	fd
char	visit[ARB]	# visit text (e.g. "Visit-Exp# : 12-30")
char	obs[ARB]	# observation text (e.g. "Observation: yabc1234")
char	prop[ARB]	# proposal text (e.g. "Proposal: 1234")
char	instru[ARB]	# instrument
char	timetag[ARB]	# time tag
int	page		# page number

char	str[SZ_LINE]
char	str1[SZ_LINE]
int	dum

int	itoc()
bool	streq()

begin

	# initialize the page
	if (streq (obs, "cover"))
	    obs[1] = EOS
	else
	    call fprintf (fd, "erase\n")
	call fprintf (fd, 
		"reset; fontset hard; vpage 0.0 1. 0.02 0.98; expand 1.\n")
	call fprintf (fd, "location 0 1 0 1\n")

	# print the shading
	call fprintf (fd, 
		"!printf ('0 .94%sn1 .94%sn1 1%sn0 1%sn',> 'tmp$PPBANNER')\n")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	    call pargstr ("\\")
	call fprintf (fd, "data tmp$PPBANNER\n")
	call fprintf (fd, "xcol c1; ycol c2; color 5; fillpat 2; polygon\n")
	call fprintf (fd, "color 1; fillpat 1\n")
	call fprintf (fd, "!delete tmp$PPBANNER verify-\n")

	# need this so the gray scale image will not mess up
	call fprintf (fd, "reset; fontset hard; vpage 0.0 1. 0.02 0.98\n")

	call fprintf (fd, "expand 1.2\n")

	# print the visit (or logsheet) text
	if (visit[1] != EOS) {
	    call fprintf (fd, "vmove 0.02 0.97; justify 6\n")
	    call fprintf (fd, "label '%sfB%s'\n")
	        call pargstr ("\\")
	        call pargstr (visit)
	}

	# print the observation text
	if (obs[1] != EOS) {
	    call fprintf (fd, "vmove 0.3 0.97; justify 6\n")
	    call fprintf (fd, "label '%sfB%s'\n")
	        call pargstr ("\\")
	        call pargstr (obs)
	}

	# print the Proposal text
	if (prop[1] != EOS) {
	    call fprintf (fd, "vmove 0.8 0.97; justify 4\n")
	    call fprintf (fd, "label '%sfB%s'\n")
	        call pargstr ("\\")
	        call pargstr (prop)
	}

	# print the instrument text
	if (instru[1] != EOS) {
            call fprintf (fd, "expand 2.0\n")
	    call fprintf (fd, "vmove 0.98 0.97; justify 4\n")
	    call fprintf (fd, "label '%sfI%s'\n")
	        call pargstr ("\\")
	        call pargstr (instru)
	}

	call strcpy ("Space Telescope Science Institute", str, SZ_LINE)

	# if the time tag is not NULL, add to the footnote
	if (timetag[1] != EOS) {
	    call strcat (",  ", str, SZ_LINE)
	    call strcat (timetag, str, SZ_LINE)
	}

	# if the page number is not INDEF, add to the footnote
	if (page >= 0) {
	    call strcat (",  Page ", str, SZ_LINE)
	    page = page + 1
	    dum = itoc (page, str1, SZ_LINE)
	    call strcat (str1, str, SZ_LINE)
	}
	call fprintf (fd, "vmove 1.00 0.00; justify 4; expand 0.4\n")
        call fprintf (fd, "label '%s'\n")
	    call pargstr (str) 

	call fprintf (fd, "location 0 1 0 1\n")
end
