include	<mach.h>
include	<ctype.h>

# BASEDIGIT -- Take the number modulo the input base. Convert this to a
# string of digits with leading zeroes.
#
# B.Simon	28-Aug-87	First Code

procedure basedigit (num, base, outstr, ip)

int	num		# i : Number to be converted
int	base		# i : Base of number (Hexadecimal = 16)
char	outstr[ARB]	# o : String to hold digit
int	ip		# io: Location of digit in string

int	digit, tbase
pointer	sp, jp, tmpstr

begin
	# Allocate a temporary string to hold the characters
                
	call smark (sp)
	call salloc (tmpstr, MAX_DIGITS, TY_CHAR)

	# Create the characters in reverse order

	digit = mod (num, base)
	tbase = base
	jp = tmpstr - 1
	while (tbase > 0) {
	    jp = jp + 1
	    Memc[jp] = TO_DIGIT(mod (digit, 10))
	    digit = digit / 10
	    tbase = tbase / 10
	} 

	# Copy the characters to the output string in correct order

	while (jp >= tmpstr) {
	    outstr[ip] = Memc[jp]
	    ip = ip + 1
	    jp = jp - 1
	}

	call sfree (sp)
end
