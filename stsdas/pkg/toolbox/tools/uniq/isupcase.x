include <ctype.h>

# IS_UPCASE -- Check to see if all letters in string are in upper case
#
# B.Simon	19-Mar-91	First Code
bool procedure is_upcase (str)

char	str[ARB]	# i: Sring
#--
int	ic

begin
	for (ic = 1; str[ic] != EOS; ic = ic + 1) {
	    if (IS_LOWER(str[ic]))
		return (false)
	}

	return (true)
end
