#* HISTORY *
#* B.Simon	11-Dec-98	original

# CHECK_SOURCE -- Check the validity of a list of sources

procedure check_source (allsources, srclist)

char	allsources[ARB] # i: A list of all possible sources
char	srclist[ARB]	# i: list of sources
#--
int	ic, count
pointer	sp, source, errmsg

string	badsource   "Unrecognized value in source parameter: %s"
string	nosource    "Source parameter is blank"

int	word_fetch(), word_match()

begin
	call smark (sp)
	call salloc (source, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Check to see if each source is in a list of all validxs sources
	# Choke on first invalid value

	ic = 1
	count = 0

	while (word_fetch (srclist, ic, Memc[source], SZ_FNAME) > 0) {
	    if (word_match (Memc[source], allsources) == 0) {
		call sprintf (Memc[errmsg], SZ_LINE, badsource)
		call pargstr (Memc[source])
		call error (1, Memc[errmsg])

	    } else {
		count = count + 1
	    }
	}

	# A zero length list is not cool 'cause this task becomes a no-op

	if (count == 0)
	    call error (1, nosource)

	call sfree (sp)
end
