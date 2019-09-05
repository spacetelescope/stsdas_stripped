# Split one long string into two "readable parts"

procedure split_str (instr, outstr1, outstr2, outlen)

char	instr[ARB]	# input string
char	outstr1[ARB]	# first part of the output string
char	outstr2[ARB]	# second part of the output string
int	outlen		# (maximum) length of the output substrings

int	i
int	lensub

int	strlen()
#------------------------------------------------------------------------------
begin
	    if (strlen(instr) > outlen) {

	        # look for a separator (blank, -, /, or _)
		do i = outlen, 2, -1 {
	     	    if (instr[i] == ' ' || instr[i] == '-' || instr[i] == '/' ||
			instr[i] == '_') {
			lensub = i
			break
		    }
		}

		# if the separator is at the beginning of the input string, 
		# forget about the separator.
		if (i <= 2) lensub = outlen

		# copy the parts to the output strings
	    	call strcpy (instr, outstr1, lensub)
	        call strcpy (instr[lensub+1], outstr2, outlen)
	    } else {
	    	call strcpy (instr, outstr1, outlen)
	        outstr2[1] = EOS
	    }
end
