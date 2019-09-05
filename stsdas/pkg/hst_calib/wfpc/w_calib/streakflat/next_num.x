include	<ctype.h>

#  NEXT_NUM --  Get the next group of digits and convert them to an integer
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

int procedure next_num (str, ip, ival)

char	str[SZ_LINE]		# input: input character string
int	ip			# input: index of the string to be searched
int	ival			# output: numerical value of the digit group

int	ip_start, k, lp, nchar
char	tmp[SZ_LINE]

int	ctoi()
#==============================================================================
begin

	ival = 0
	while (!(IS_DIGIT(str[ip])) && str[ip] != EOS)
	    ip = ip + 1
	ip_start = ip
	while (IS_DIGIT(str[ip]))
	    ip = ip + 1

	k = ip - ip_start
	lp = 1
	if (k != 0) {
	    call strcpy (str[ip_start], tmp, k)
	    nchar = ctoi (tmp, lp, ival)
	}
	return (k)
end
