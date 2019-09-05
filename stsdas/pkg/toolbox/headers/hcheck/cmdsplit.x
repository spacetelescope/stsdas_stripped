include "hcheck.h"

# CMDSPLIT -- Split a command into keyword and expression strings

procedure cmdsplit (im, igroup, command, keystart, cmdstart, show, isgroup)

pointer	im		#  i: Image descriptor
int	igroup		#  i: Group number
char	command[ARB]	# io: Command line
int	keystart	#  o: Start of keyword substring
int	cmdstart	#  o: Start of command substring
int	show		#  o: When should keyword be printed?
bool	isgroup		#  o: Keyword contains a group parameter
#--
char	comment
int	ic, jc
pointer	sp, keyword

data	comment  / '#' /

string	noexpress  "No expression following when"

bool	streq()
int	stridx(), word_fetch(), gf_gfind()

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	# Strip comments from command line

	ic =stridx (comment, command)
	if (ic > 0)
	    command[ic] = EOS

	# Set output variables to default values

	keystart = 1
	cmdstart = 0
	show = NEVER
	isgroup = false

	# Find location of "when" in command and split the line there

	ic = 1
	jc = 0
	while (word_fetch (command, ic, Memc[keyword], SZ_FNAME) > 0) {
	    if (jc > 0 && streq (Memc[keyword], "when")) {
		command[jc] = EOS
		cmdstart = ic
		break
	    }

	    jc = ic
	    if (gf_gfind (im, Memc[keyword]) > 0)
		isgroup = true
	}

	if (cmdstart == 0) {
	    # If no "when" was found it's either all or nothing

	    if (jc == 0) {
		show = NEVER
		cmdstart = 1
	    } else {
		show = ALWAYS
		cmdstart = jc
	    }

	} else {
	    # If a "when" was found, see if the next word is "missing"
	    # Checking for the existence of a parameter does not 
	    # require its value, so isgroup is set to false

	    if (word_fetch (command, ic, Memc[keyword], SZ_FNAME) == 0)
		call error (SYNTAX, noexpress)

	    if (streq (Memc[keyword], "missing")) {
		show = IF_MISSING
		isgroup = false
	    } else {
		show = IF_TRUE
	    }
	}

	# Override the value of show if this is a non-group parameter
	# in a group other than the first

	if (igroup > 1 && ! isgroup)
	    show = NEVER

	call sfree (sp)
end

