#  strtor -- convert a string of real numbers into a real array 
#
#  Description:
#  ------------
#  If the input string is blank(s), this routine will return 0.  If there are
#  characters other than digits, decimal point, comma, semi-colon, colon, or
#  slash in the input string, this routine will issue an error message.
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Sep-1993  J.-C. Hsu		Design and coding
#  08-Aug-1995  J.-C. Hsu		Avoid illegal string
#------------------------------------------------------------------------------
int procedure strtor (str, arr)

char	str[SZ_LINE]		# input: input character string
real	arr[ARB]

int	ip			# index of the string to be searched
real	rval
int	n
char	text[SZ_LINE]

int	ctor()
#==============================================================================
begin
	n = 0
	ip = 1

	while (str[ip] != EOS) {
	    if (ctor(str, ip, rval) == 0) {
		if (str[ip] == ',' || str[ip] == ';' || str[ip] == '/')
		    ip = ip + 1

		# add the following to detect illegal string, JCHsu 8/8/95
		else if (str[ip] == EOS)
		    ip = ip
		else {
		    call sprintf (text, SZ_LINE, "illegal input string '%s'")
			call pargstr (str)
		    call error (1, text)
		}
	    } else {
		n = n + 1
		arr[n] = rval
	    }
	}
	return (n)
end
