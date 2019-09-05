# Copyright restrictions apply - see stsdas$copyright.stsdas 

# PIXPARSE -- Parse the cursor command
#
# This procedure breaks the command read from the image cursor into words.
# Words are separated by whitespace (any character <= a blank) or an 
# equals sign, if this is the second and third word. The number of words on
# the line is returned in nword. Words after nword are set to the empty 
# string. 
#
# B.Simon	03-Jul-90	Original

procedure pixparse (command, word, nword, maxword)

pointer	command		# i: Cursor command
pointer	word[ARB]	# o: Words in the command
int	nword		# o: Number of words in command
int	maxword		# i: Maixmum number of words
#--
bool	inword, delim
pointer	ch
int	iword

begin
	iword = 0
	inword = false

	for (ch = command; Memc[ch] != EOS; ch = ch + 1) {
	    delim = Memc[ch] <= ' ' || (Memc[ch] == '=' && iword == 2)

	    if (inword) {
		if (delim) {
		    inword = false
		    Memc[ch] = EOS
		}

	    } else {
		if (! delim) {
		    iword = iword + 1
		    if (iword > maxword)
			break

		    inword = true
		    word[iword] = ch
		}
	    }
	}

	nword = iword
	do iword = nword+1, maxword
	    word[iword] = ch

end

