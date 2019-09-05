include	<ctype.h>

# BASECHAR -- Take the number modulo the given base and convert it to a
# lower case character in that base. Base must be 36 or less.
#
# B.Simon	28-Aug-87	First Code

procedure basechar (num, base, outstr, ip)

int	num		# i : Number to be converted
int	base	  	# i : Base of number (Hexadecimal=16)
char	outstr[ARB]	# o : String to hold digit
int	ip		# io: Location of digit in string

int	digit

begin
	digit = mod (num, base)
	if (digit < 10)
	    outstr[ip] = TO_DIGIT(digit)
	else
	    outstr[ip] = digit - 10 + 'a'
	ip = ip + 1
end	
