# nor_fname -- extract portions of input image name
# This routine returns the names of the science file and shp header.
# The input name must not contain an image section specification.
# If the input contains a group number, that number will be appended
# to the name of the science file name but not the shp header name.
# The input may not be the same actual argument as either of
# the output names.
#
# Phil Hodge,  4-May-1992  Subroutine created.
# Phil Hodge,  2-Aug-1993  Add IMAGE to calling sequence.

procedure nor_fname (input, science, image, shp, maxch)

char	input[ARB]	# i: input image name
char	science[ARB]	# o: science file header name
char	image[ARB]	# o: name with no extension or default extension
char	shp[ARB]	# o: shp header name
int	maxch		# i: max length of each output string
#--
pointer sp
pointer cluster		# scratch for image name without group number
pointer root		# scratch for directory and root
pointer extension	# scratch for dot and extension
pointer group		# scratch for group number specification
pointer section		# scratch for image section string
pointer ldir		# scratch for logical directory name
pointer froot		# scratch for root name (without directory)
pointer message		# scratch for error message
int	len_cluster	# length of input name excluding group & section
int	len_ext		# length of extension
int	dirlen, rootlen	# length of directory and root of image name
int	flen		# = dirlen + rootlen
int	strlen(), fnldir(), fnroot()
bool	streq()
errchk	imgsection

begin
	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extension, SZ_FNAME, TY_CHAR)
	call salloc (group, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (froot, SZ_FNAME, TY_CHAR)
	call salloc (message, SZ_LINE, TY_CHAR)

	# Check for an image section.
	call imgsection (input, Memc[section], SZ_FNAME)
	if (Memc[section] != EOS) {
	    call sprintf (Memc[message], SZ_LINE,
			"`%s' contains an image section\n")
		call pargstr (input)
	    call error (1, Memc[message])
	}

	# Extract the group number.  The "cluster" consists of everything
	# except the group number (and the section, but there is none).
	call imgcluster (input, Memc[cluster], SZ_FNAME)
	len_cluster = strlen (Memc[cluster])
	call strcpy (input[len_cluster+1], Memc[group], maxch)

	# Extract the root name.  The input name up through flen includes
	# the logical directory and root but not the (optional) dot and
	# extension.
	dirlen  = fnldir (Memc[cluster], Memc[ldir], maxch)
	rootlen = fnroot (Memc[cluster], Memc[froot], maxch)
	flen = dirlen + rootlen
	if (flen > SZ_FNAME) {
	    call sprintf (Memc[message], SZ_LINE,
			"name too long:  `%s'\n")
		call pargstr (input)
	    call error (1, Memc[message])
	}
	call strcpy (input, Memc[root], flen)

	# Whatever remains after flen in Memc[cluster] is the extension.
	if (Memc[cluster+flen] == EOS) {
	    Memc[extension] = EOS		# no extension given
	} else if (Memc[cluster+flen] == '.') {
	    call strcpy (Memc[cluster+flen], Memc[extension], SZ_FNAME)
	} else {
	    call sprintf (Memc[message], SZ_LINE,
			"confused about extension in `%s'\n")
		call pargstr (input)
	    call error (1, Memc[message])
	}
	len_ext = strlen (Memc[extension])

	# Construct the names of the science file header and shp header.
	if (Memc[extension] == EOS) {

	    # science = root.d0h; image = input; shp = root.shh

	    call strcpy (Memc[root], science, maxch)
	    call strcat (".d0h", science, maxch)
	    call strcat (Memc[group], science, maxch)
	    call strcpy (input, image, maxch)
	    call strcpy (Memc[root], shp, maxch)
	    call strcat (".shh", shp, maxch)

	} else if (streq (Memc[extension], ".shh")) {

	    # science = root.d0h; image = root; shp = input (=root.shh)

	    call strcpy (Memc[root], science, maxch)
	    call strcat (".d0h", science, maxch)
	    call strcat (Memc[group], science, maxch)
	    call strcpy (Memc[root], image, maxch)
	    call strcpy (Memc[root], shp, maxch)
	    call strcat (".shh", shp, maxch)

	} else if (streq (Memc[extension], ".ulh")) {

	    call sprintf (Memc[message], SZ_LINE,
			"UDL file ignored:  `%s'\n")
		call pargstr (input)
	    call error (1, Memc[message])

	} else if (Memc[extension+len_ext-1] == 'd') {

	    call sprintf (Memc[message], SZ_LINE,
			"pixel file ignored:  `%s'\n")
		call pargstr (input)
	    call error (1, Memc[message])

	} else {

	    # science = input; image = ""; shp = root.shh

	    call strcpy (input, science, maxch)
	    image[1] = EOS
	    call strcpy (Memc[root], shp, maxch)
	    call strcat (".shh", shp, maxch)
	}

	call sfree (sp)
end
