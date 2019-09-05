# wh_prism -- which prism was specified
# This routine determines which prism was specified in the observation
# mode string and returns the prism name as "fuv96", "nuv96",
# "fuv48", "nuv48", or "userprism".  These names correspond to
# parameter names in the dispfiles pset.
#
# Phil Hodge, 31-Aug-1993  Subroutine created.

procedure wh_prism (obsmode, prism, maxch)

char	obsmode[ARB]	# i: observation mode
char	prism[ARB]	# o: parameter name identifying prism used
int	maxch		# i: max size of prism string
#--
pointer sp
pointer omode		# scratch for a copy of obsmode
int	sz_obsmode	# length of obsmode string
int	relay
int	strlen()
int	strsearch()

begin
	call smark (sp)
	sz_obsmode = strlen (obsmode)
	call salloc (omode, sz_obsmode, TY_CHAR)

	# Copy the obsmode to a local buffer, and convert to lower case.
	call strcpy (obsmode, Memc[omode], sz_obsmode)
	call strlwr (Memc[omode])

	# Find out which relay was specified.
	if (strsearch (Memc[omode], "f/96") > 0) {
	    relay = 96
	} else if (strsearch (Memc[omode], "f/48") > 0) {
	    relay = 48
	} else {
	    call strcpy ("userprism", prism, maxch)	# not FOC
	    return
	}

	# Is it far UV or near UV?
	if (strsearch (Memc[omode], "prism1") > 0 ||
	    strsearch (Memc[omode], "fuvop") > 0) {

	    if (relay == 96)
		call strcpy ("fuv96", prism, maxch)
	    else if (relay == 48)
		call strcpy ("fuv48", prism, maxch)

	} else if (strsearch (Memc[omode], "prism2") > 0 ||
	    strsearch (Memc[omode], "nuvop") > 0) {

	    if (relay == 96)
		call strcpy ("nuv96", prism, maxch)
	    else if (relay == 48)
		call strcpy ("nuv48", prism, maxch)

	} else {
	    call strcpy ("userprism", prism, maxch)	# not FOC
	}
end
