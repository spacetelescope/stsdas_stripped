include <imio.h>

# CHK_PRINT -- Print the header keyword values that fail the check

procedure chk_print (im, keylist, command, title)

pointer	im		#  i: Image descriptor
char	keylist[ARB]	#  i: List of keywords to print
char	command[ARB]	# io: Expression used in check
bool	title		# io: Print title?
#--
int	ic
pointer	sp, keyword, value

int	strlen(), word_fetch()

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)

	# Print title if this is the first error found

	if (title)
	    call chk_title (im, title)

	# Truncate command to 64 characters

	if (strlen (command) > 60)
	    call strcat (" ...", command[60], SZ_COMMAND)

	# Print each keyword name, value, and associated command

	ic = 1
	while (word_fetch (keylist, ic, Memc[keyword], SZ_FNAME) > 0) {
	    iferr {
		call imgstr (im, Memc[keyword], Memc[value], SZ_FNAME)
	    } then {
		call chk_missing (im, Memc[keyword], title)

	    } else {
		call printf ("%-10s%-30s%-30s\n")
		call pargstr (Memc[keyword])
		call pargstr (Memc[value])
		call pargstr (command)
	    }
	}
	call sfree (sp)
end

# CHK_MISSING -- Print a message that a keyword was missing in the header

procedure chk_missing (im, keylist, title)

pointer	im		#  i: Image descriptor
char	keylist[ARB]	#  i: List of keyword names
bool	title		# io: Print title?
#--
int	ic
pointer	sp, keyword

int	word_fetch(), imaccf()

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	# Check to see if each keyword is missing

	ic = 1
	while (word_fetch (keylist, ic, Memc[keyword], SZ_FNAME) > 0) {
	    if (imaccf (im, Memc[keyword]) == NO) {
	
		# Print title if this is the first error found
		if (title)
		    call chk_title (im, title)

		# Print keyword missing message

		call printf ("%-10s%30w missing\n")
		call pargstr (Memc[keyword])
	    }
	}

end

# CHK_TITLE -- Print the image name as a title

procedure chk_title (im, title)

pointer	im		#  i: Image descriptor
bool	title		# io: Print title?
#--
int	offset
pointer	sp, file, ldir, root

int	fnldir()

begin
	call smark (sp)
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)

	if (title) {
	    title = false

	    # Remove the directory name from the root, if present

	    call imgcluster (IM_NAME(im), Memc[file], SZ_FNAME)
	    offset = fnldir (Memc[file], Memc[ldir], SZ_FNAME)
	    call strcpy (IM_NAME(im), Memc[file], SZ_FNAME)
	    root = file + offset

	    # Print the root name plus the image section

	    call printf ("#\n#%11t%-30s\n#\n")
	    call pargstr (Memc[root])
	}

	call sfree (sp)
end
