define	BLANK		' '

#* HISTORY *
#* B.Simon	06-Mar-89	Original
#* B.Simon	12-Nov-97	Version for siaf task

# This procedure removes trailing whitespace from a string.
# Whitespace is defined to be any character with an ascii value less 
# than or equal to that of the blank.

# SIAF_TRIM -- Remove trailing whitespace from a string

procedure siaf_trim (str)

char	str[ARB]	# u: String to be modified
#--
int	ic, jc

begin

	jc = 0
	for (ic = 1; str[ic] != EOS; ic = ic + 1) {
	    if (str[ic] > BLANK)
		jc = ic
	}
	str[jc+1] = EOS

end
