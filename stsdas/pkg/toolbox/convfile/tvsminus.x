# tvs_minus -- convert - to \- in file name
# This routine replaces each minus sign in the input string with \- and
# then copies to the output string.  The input and output strings may be
# the same.
#
# Phil Hodge, 10-Oct-1990  Subroutine created.

procedure tvs_minus (in, out, maxch)

char	in[ARB]		# i: input string
char	out[ARB]	# o: output string
int	maxch		# i: maximum size of output string
#--
pointer sp
pointer scr		# scratch for result before copying to out
int	n_minus		# number of minus signs in string
int	sz_scr		# size to allocate for scr
int	ip, op		# indexes in in & out
char	lastch		# last character copied

begin
	n_minus = 0
	sz_scr = 0
	do ip = 1, maxch {
	    if (in[ip] == EOS)
		break
	    if (in[ip] == '-')
		n_minus = n_minus + 1
	    sz_scr = sz_scr + 1
	}
	if (n_minus <= 0) {
	    call strcpy (in, out, maxch)
	    return
	}

	# Allow space for the back slashes.
	sz_scr = max (SZ_LINE, sz_scr + n_minus)

	call smark (sp)
	call salloc (scr, sz_scr, TY_CHAR)

	op = 0				# note:  zero indexed
	lastch = ' '
	do ip = 1, maxch {

	    if (in[ip] == '-' && lastch != '\\') {
		Memc[scr+op] = '\\'
		op = op + 1
	    }
	    Memc[scr+op] = in[ip]
	    lastch = in[ip]
	    op = op + 1
	    if (in[ip] == EOS)
		break
	}

	call strcpy (Memc[scr], out, maxch)

	call sfree (sp)
end
