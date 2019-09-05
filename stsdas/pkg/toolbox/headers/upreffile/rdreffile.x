#* HISTORY *
#* B.Simon	28-Jun-92	Original
#* B.Simon	30-Oct-97	Rewritten for migration out of cdbs
#* B.Simon	15-Jul-98	Modified to read new getref files
#* B.Simon	11-Dec-98	Add source field to delta file

# RD_REFFILE -- Read a row from the structure into strings

int procedure rd_reffile (ref, key, value, file, oper, source, maxch)

int	ref		# i: reffile descriptor
char	key[ARB]	# o: header keyword
char	value[ARB]	# o: keyword value
char	file[ARB]	# o: file name
char	oper[ARB]	# o: operation to perform on keyword
char	source[ARB]	# o: source of change in dads database
int	maxch		# i: maximum length of output strings
#--
bool	more, title
int	ic, nc, status
pointer	sp, root, line, change

string	template  "*.d0h,*_raw.fits[0]"
string	badtitle  "Invalid input file: title line is incorrect"

bool	strne()
int	getline(), ctowrd()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (change, SZ_FNAME, TY_CHAR)

	# Read until we get a valid data line

	more = true
	while (more) {
	    # Read line, check for end of file

	    status = getline (ref, Memc[line])
	    if (status == EOF)
		break

	    # The title line starts with a comment character

	    title = Memc[line] == '#'
	    if (title) {
		ic = 2
	    } else {
		ic = 1
	    }

	    # Read each field by position. The change field is not
	    # used by this task, so it is thrown away

	    nc = ctowrd (Memc[line], ic, Memc[root], SZ_FNAME)
	    nc = ctowrd (Memc[line], ic, key, maxch)
	    nc = ctowrd (Memc[line], ic, value, maxch)
	    nc = ctowrd (Memc[line], ic, file, maxch)
	    nc = ctowrd (Memc[line], ic, oper, maxch)
	    nc = ctowrd (Memc[line], ic, Memc[change], maxch)
	    nc = ctowrd (Memc[line], ic, source, maxch)

	    # Check the title line to see if this is a valid reffile list

	    if (title) {
		if (strne (Memc[root], "ROOTNAME") || strne (key, "KEYWORD"))
		    call error (1, badtitle)
	    }

	    # Continue until a non-title and non-blank line is read

	    more = title || Memc[root] == EOS || key[1] == EOS
	}

	# Backwards compatibility for old versions of getref

	if (status != EOF && oper[1] == EOS) {
	    call opnroot (template, Memc[root], file, maxch)
	    call strcpy ("i", oper, maxch)
	}

	call sfree (sp)
	return (status)
end
