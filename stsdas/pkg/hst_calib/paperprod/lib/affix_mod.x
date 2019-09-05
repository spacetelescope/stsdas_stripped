# modify the prefix/suffix of file names in a list

procedure affix_mod ()

char	list1[SZ_FNAME]		# input file list name
char	list2[SZ_FNAME]		# output file list name
char	old_prefix[SZ_FNAME]	# old prefix name
char	new_prefix[SZ_FNAME]	# new prefix name
char	old_suffix[SZ_FNAME]	# old suffix name
char	new_suffix[SZ_FNAME]	# new suffix name

char	fname1[SZ_FNAME]		
char	fname2[SZ_FNAME]		
char	str1[SZ_LINE]
char	str2[SZ_LINE]
int	s_len, s_indx, p_indx, len1
int	i, j, k

int	fd1, fd2		# input/output file pointer

int	open()
int	getline()
int	strlen()
int	strmatch()

begin
	# read input and output list file names
	call clgstr ("list1", list1, SZ_LINE)
	call clgstr ("list2", list2, SZ_LINE)
	call clgstr ("old_prefix", old_prefix, SZ_LINE)
	call clgstr ("new_prefix", new_prefix, SZ_LINE)
	call clgstr ("old_suffix", old_suffix, SZ_LINE)
	call clgstr ("new_suffix", new_suffix, SZ_LINE)

	s_len = strlen(old_suffix)

	# open the input/output file
	fd1 = open (list1, READ_ONLY, TEXT_FILE)
	fd2 = open (list2, NEW_FILE, TEXT_FILE)

	while (getline(fd1, fname1) != EOF) {

	    # get rid of preceeding or trailing blanks
	    len1 = strlen (fname1)
	    do i = 1, len1 {
		if (fname1[i] != ' ') break
	    }
	    do j = len1-1, 1, -1 {
		if (fname1[j] != ' ') break
	    }

	    # if it is a line with blanks...
	    if (j < i)
	  	str1[1] = EOS
	    else {
	        do k = i, j
		    str1[k-i+1] = fname1[k]
	        str1[j-i+2] = EOS
	    }
	    
	    # determine the locations of the old prefix and suffix in the 
	    # input string
	    if (old_suffix[1] == EOS)
		s_indx = strlen (str1)
	    else {
	        s_indx = strmatch (str1, old_suffix)
	        if (s_indx > 0) 
		    s_indx = s_indx - s_len - 1
	        else 
		    s_indx = strlen (str1)
	    }
	    if (old_prefix[1] == EOS)
		p_indx = 1
	    else {
	        p_indx = strmatch (str1, old_prefix)
	        if (p_indx <= 0) p_indx = 1
	    }

	    # copy the infix (the central part which is without the old 
	    # prefix or suffix) to a buffer string str2
	    do k = p_indx, s_indx
		str2[k-p_indx+1] = str1[k]
	    str2[s_indx-p_indx+2] = EOS

	    # attach the new prefix and suffix to the infix
	    call sprintf (fname2, SZ_FNAME, "%s%s%s\n")
		call pargstr (new_prefix)
		call pargstr (str2)
		call pargstr (new_suffix)
	    call putline (fd2, fname2)
	}

	# close the input/output file
	call close (fd1)
	call close (fd2)
end
