# parse the full path name to get the (last) subdirectory name

procedure t_autopi()

char	dirname[SZ_LINE]	#i/o: directory name

int	len, i
char	dummy[SZ_LINE]

int	strlen()

begin
        call clgstr ("dirname", dirname, SZ_LINE)
	len = strlen (dirname)

	# if the last character is "]" (the VMS case), look for "[" or "." 
	# before it
	if (dirname[len] == ']') {
	    do i = len-1, 1, -1 {
		if (dirname[i] == '.' || dirname[i] == '[')
		    break
	    }

	# if the last character is "/" (the UNIX case), look for "/" before it
	} else if (dirname[len] == '/') {
	    do i = len-1, 1, -1 {
		if (dirname[i] == '/')
		    break
	    }
	} else {
	    i = 0
	    len = 1
	}

	call strcpy (dirname[i+1], dummy, len-i-1)
	call clpstr ("subdirname", dummy)
end
