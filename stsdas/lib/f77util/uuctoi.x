include <iraf77.h>
include	<ctype.h>

# UUCTOI -- Simple character to integer (decimal radix).

procedure uuctoi (f77str, ips, ipe, ival, istat)

%	character*(*)	f77str
char	str[SZ_LINE]	# decimal encoded numeric string
int	ips		# starting index in string (input)
int	ipe		# position of last character translated (output)
int	ival		# decoded integer value (output)
int	istat		# Error code

bool	neg
int	sum, ip
int	ip_start
int	strncmp()

begin
	istat = ER_OK
	call f77upk (f77str, str, SZ_LINE)
	ip = ips
	while (IS_WHITE (str[ip]))
	    ip = ip + 1
	ip_start = ip

	# Check for "INDEF".
	if (str[ip] == 'I')
	    if (strncmp (str[ip], "INDEF", 5) == 0)
		if (!IS_ALNUM (str[ip+5])) {
		    ival = INDEFI
		    ip = ip + 5
		    ipe = ip
		    return
		}

	neg = (str[ip] == '-')
	if (neg)
	    ip = ip + 1

	sum = 0
	while (IS_DIGIT (str[ip])) {
	    sum = sum * 10 + TO_INTEG (str[ip])
	    ip = ip + 1
	}

	if (neg)
	    ival = -sum
	else
	    ival = sum

	ipe = ip
	return
end
