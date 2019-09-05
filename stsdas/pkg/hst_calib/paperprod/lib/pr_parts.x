# Determine which parts of the Paper Products to print

procedure pr_parts ()

char	input[SZ_FNAME]		# parts selections

int	strmatch()

begin
	# read the printout selection
	call clgstr ("input", input, SZ_FNAME)
	call strlwr (input)
	call clpstr ("output", input)

	# initialize all flags to false
	call clputb ("cover", false)
	call clputb ("visit", false)
	call clputb ("obs", false)

	# if "all" is in the selection string, set all output parameters to true
	if (strmatch (input, "all") != 0) {
	    call clputb ("cover", true)
	    call clputb ("visit", true)
	    call clputb ("obs", true)
	}

	# check for each possible selection value
	if (strmatch (input, "cover") != 0)
	    call clputb ("cover", true)

	if (strmatch (input, "visit") != 0)
	    call clputb ("visit", true)

	if (strmatch (input, "obs") != 0)
	    call clputb ("obs", true)
end
