include <ctype.h>

# Copyright restrictions apply - see stsdas$copyright.stsdas 

# STRSAME -- Case insensitive comparison of two strings

bool procedure strsame (str1, str2)

char	str1[ARB]	# i: First string
char	str2[ARB]	# i: Second string
#--
int	ic, diff

begin
	do ic = 1, ARB {
	    diff = str1[ic] - str2[ic]
	    if (str1[ic] == EOS || str2[ic] == EOS) {
		break
	    } else if (diff != 0) {
		if (abs (diff) != ('a' - 'A')) {
		    break
		} else if (! IS_ALPHA(str1[ic])) {
		    break
		} else if (! IS_ALPHA(str2[ic])) {
		    break
		}
	    }
	}

	return (diff == 0)
end
