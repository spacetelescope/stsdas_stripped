# cull the unique file root names from all input file names

define	MAX_OBS	2000		# maximum number of root names

procedure pp_roots ()

char	input[SZ_FNAME]		# input file list name
char	output[SZ_FNAME]	# output file name

char	fname[SZ_LINE]		# file name
char	root[SZ_LINE]		# file name
char	rootname[SZ_LINE,MAX_OBS]	# file name
int	infd, outfd		# input/output file pointer
char	str1[SZ_LINE]
int	len, nroots, i

int	open()
int	getline()
int	strlen()
bool	streq()

begin
	# read input, output file name
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	# open the input file
	infd = open (input, READ_ONLY, TEXT_FILE)

	nroots = 0

	while (getline(infd, fname) != EOF) {

	    # strip off the .??? or _???.fits suffix 
	    # (the last character is carridge return)
	    len = strlen (fname)
	    fname[len] = EOS
	    call strcpy (fname[len-5], str1, SZ_LINE)
	    if (streq (str1, ".fits")) {
		if (fname[len-9] == '_')
		    call strcpy (fname, root, len-10)
	    } else if (fname[len-4] == '.') {
		call strcpy (fname, root, len-5)
	    } else {
		next
	    }
	
	    # compile the unique root name list
	    if (nroots == 0) {
		nroots = nroots + 1
		call strcpy (root, rootname[1,nroots], SZ_LINE)
		outfd = open (output, NEW_FILE, TEXT_FILE)
		call roots_list (outfd, root)
	    } else {
		do i = 1, nroots {
		    if (streq (root, rootname[1,i])) go to 10
		}
		nroots = nroots + 1
		call strcpy (root, rootname[1,nroots], SZ_LINE)
		call roots_list (outfd, root)
10		next
	    }
	}

	# close the input/output file
	call close (infd)
	call close (outfd)
end

procedure roots_list (fd, root)

int	fd			# output file pointer
char	root[ARB]		# file name

pointer	ip			# input image pointer
char	fname[SZ_LINE]		# file name
int	proposid		# proposal ID
char	instrume[SZ_LINE]	# instrument
char	linenum[SZ_LINE]	# line number
char	piname[SZ_LINE]		# PI's name
char	str[SZ_LINE]
bool	fits			# Need to add a FITS extension to the filename?

pointer	immap()
int	imgeti()
int	access()

begin
	# look for _shf.fits, or .shh, or _spt.fits, or _wsp.fits file
	fits = true
	call sprintf (fname, SZ_LINE, "%s_shf.fits")
	    call pargstr (root)
	if (access(fname, 0, 0) == NO) {
	    fits = false
	    call sprintf (fname, SZ_LINE, "%s.shh")
	        call pargstr (root)
	    if (access(fname, 0, 0) == NO) {
		fits = true
	        call sprintf (fname, SZ_LINE, "%s_spt.fits")
	            call pargstr (root)
	        if (access(fname, 0, 0) == NO) {
	            call sprintf (fname, SZ_LINE, "%s_wsp.fits")
	                call pargstr (root)
	            if (access(fname, 0, 0) == NO) {
			#call printf ("Root name '%s' does not have SHP or SPT file\n")
	    		    #call pargstr (root)
			return
		    }
		}
	    }
	}
	    
	# to get the proposal ID, instrument, PI name, and linenum keywords
	if (fits) call strcat ("[0]", fname, SZ_LINE)
	ip = immap (fname, READ_ONLY, 0)
	proposid = imgeti (ip, "PROPOSID")
	call imgstr (ip, "INSTRUME", instrume, SZ_LINE)
	iferr (call imgstr (ip, "PR_INV_L", piname, SZ_LINE))
	    call strcpy (" ", piname, SZ_LINE)
	call imgstr (ip, "LINENUM", linenum, SZ_LINE)
	call imunmap (ip)

	# write to the output table
	call sprintf (str, SZ_LINE, "%10d\t%s\t%s\t\"%s\"\t\"%s%s\"\t%s\n")
	    call pargi (proposid)
	    call pargstr (instrume)
	    call pargstr (root)
	    call pargstr (piname)
	    if (linenum[2] == '.')
	 	call pargstr("v0")
	    else
	 	call pargstr("v")
	    call pargstr (linenum)
	    if (fits)
		call pargstr("fits")
	    else
		call pargstr("geis")
	call putline (fd, str)
end
