#* HISTORY *
#* B.Simon	28-Jun-92	Original
#* B.Simon	30-Oct-97	Modified for standalone version
#* B.Simon	15-Jul-98	Return image name instead of pointer

# OPNROOT -- Create file name from template and root name

procedure opnroot (template, root, file, maxch)

char	template[ARB]	# i: image template
char	root[ARB]	# i: observation root
char	file[ARB]	# o: observation file name
int	maxch		# i: max length of file name]
#--
char	star
int	ic, jc
pointer	sp, pattern, errmsg

data	star	/ '*' /
string	nostar   "Template name does not contain wildcard character (%s)"
string	badimage "Cannot find image corresponding to root name (%s)"

int	stridx(), word_fetch(), imaccess()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (pattern, SZ_PATHNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Substitute rootname for wildcard in template

	ic = 1
	file[1] = EOS

	while (word_fetch (template, ic, Memc[pattern], SZ_PATHNAME) > 0) {

	    jc = stridx (star, Memc[pattern])
	    if (jc == 0) {
		call sprintf (Memc[errmsg], SZ_LINE, nostar)
		call pargstr (Memc[pattern])

		call error (1, Memc[errmsg])
	    }

	    call strcpy (Memc[pattern], file, jc-1)
	    call strcat (root, file, maxch)
	    call strcat (Memc[pattern+jc], file, maxch)

	    # See if image can be found

	    if (imaccess (file, READ_ONLY) == YES)
		break

	    file[1] = EOS
	}

	if (file[1] == EOS) {
	    call sprintf (errmsg, SZ_LINE, badimage)
	    call pargstr (root)

	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
end
