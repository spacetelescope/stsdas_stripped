include	<ctype.h>

# CHECK_EQ -- Check two strings for equality without regard to case

bool procedure check_eq (oldstr, newstr)

char	oldstr[ARB]	# i: Original string
char	newstr[ARB]	# i: Replacement string
#--
int	ic, diff, casediff

begin
	casediff = 'a' - 'A'

	for (ic = 1; oldstr[ic] != EOS && newstr[ic] != EOS; ic = ic + 1) {
	    if (oldstr[ic] != newstr[ic]) {
		if (IS_ALPHA(oldstr[ic]) && IS_ALPHA(newstr[ic])) {

		    diff = abs (newstr[ic] - oldstr[ic])
		    if (diff != casediff)
			break

		} else {
		    break
		}
	    }
	}

	return (oldstr[ic] == EOS && newstr[ic] == EOS)
end
