# Split one long string into several "readable parts"

procedure split_strn (instr, outstr, outlen,    nout)

char	instr[ARB]		# input string
char	outstr[outlen, ARB]	# parts of the output string
int	outlen			# (maximum) length of the output substrings

int	nout			# output: number of output strings

int	i, j, len, len0
int	lensub

int	strlen()
#------------------------------------------------------------------------------
begin
	len0 = strlen (instr)
	len = len0
	j = 0
	nout = 0
	while (len > outlen) {

	    # look for a separator (blank, -, /, or _)
	    do i = j+outlen, j+2, -1 {
	     	if (instr[i] == ' ' || instr[i] == '-' || instr[i] == '/' ||
		    instr[i] == '_') {
		    lensub = i
		    break
		}
	    }

	    # if the separator is at the beginning of the input string, 
	    # forget about the separator.
	    nout = nout + 1
	    if (i <= j+2) {
		lensub = outlen+j
	    }

	    # copy the parts to the output strings
	    call strcpy (instr[j+1], outstr[1,nout], lensub-j)
	    j = lensub
	    len = len0 - lensub
	}

	# copy the rest of the string
	if (len > 0) {
	    nout = nout + 1
	    call strcpy (instr[j+1], outstr[1,nout], outlen)
	}
end
