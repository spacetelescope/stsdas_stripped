include <ctype.h>
include <mach.h>		# for EPSILOND & NDIGITS_DP
include "precess.h"

define	MIN_PREC	4	# minimum precision to display
define	MIN_FIELD_WIDTH	15	# minimum w, as in %w.d for the format

# pr_get_fmt -- get print format for ra & dec
# This routine constructs print formats for ra & dec that are appropriate
# for the given conditions, which include the units and whether the output
# should be in hms, dms format.  The level of precision for the output is
# determined by the number of digits entered by the user in the input string,
# i.e. the string containing the unprecessed ra & dec.
#
# Phil Hodge, 31-Jul-1990  Subroutine created.
# Phil Hodge, 22-Aug-1991  Add 1 to ra_prec if hours but not hms_format.

procedure pr_get_fmt (buf, ra_index, dec_index, ra_units, hms_format,
		ra_fmt, dec_fmt, maxch)

char	buf[ARB]	# i: string containing unprecessed ra & dec
int	ra_index	# i: starting index of ra in buf
int	dec_index	# i: starting index of dec in buf
int	ra_units	# i: units for ra
bool	hms_format	# i: should output be in hms, dms format?
char	ra_fmt[maxch]	# o: print format for ra
char	dec_fmt[maxch]	# o: print format for dec
int	maxch		# i: size of format strings
#--
int	ra_prec		# number of digits after the decimal for ra
int	dec_prec	# ditto for dec
int	precision	# max of ra, dec precision
int	width		# field width
int	pr_precision()

begin
	ra_prec = pr_precision (buf[ra_index])
	dec_prec = pr_precision (buf[dec_index])
	if (ra_units == PREC_HOURS)
	    ra_prec = ra_prec - 1

	precision = max (ra_prec, dec_prec, MIN_PREC)

	width = max (precision + 7, MIN_FIELD_WIDTH)

	if (hms_format) {
	    if (ra_units == PREC_HOURS) {
		ra_prec = precision - 3
		dec_prec = precision - 4
	    } else if (ra_units == PREC_DEGREES) {
		ra_prec = precision - 4
		dec_prec = precision - 4
	    } else {
		ra_prec = precision
		dec_prec = precision
	    }
	} else {
	    if (ra_units == PREC_HOURS)
		ra_prec = precision + 1
	    else
		ra_prec = precision

	    dec_prec = precision
	}

	call strcpy ("%", ra_fmt, maxch)
	call strcpy ("%", dec_fmt, maxch)

	call sprintf (ra_fmt[2], maxch-1, "%d.%d")
	    call pargi (width)
	    call pargi (ra_prec)
	call sprintf (dec_fmt[2], maxch-1, "%d.%d")
	    call pargi (width)
	    call pargi (dec_prec)

	if (hms_format) {
	    call strcat ("h", ra_fmt, maxch)
	    call strcat ("h", dec_fmt, maxch)
	} else {
	    call strcat ("f", ra_fmt, maxch)
	    call strcat ("f", dec_fmt, maxch)
	}
end

# pr_precision -- get precision
# This function returns the number of digits after the decimal point
# that were given for a number in the input string.  The precision is
# determined as follows.  The actual numerical value is gotten from the
# string using ctod.  Then a one is added to (or subtracted from) the
# last digit in the string.  That modified value is gotten using ctod.
# The difference between those values is the accuracy, and the precision
# (after the decimal point) is the log10 of one divided by the difference.

int procedure pr_precision (buf)

char	buf[ARB]	# i: string containing a numerical value
#--
char	word[SZ_FNAME]	# local char buffer
char	e_char		# 'e'
double	v1, v2		# actual value from buf; next value
double	diff		# abs (v1 - v2)
double	logdiff		# negative of base 10 log of diff
int	ip, nchar
int	length		# length of word in input buf
int	digit		# value of last digit in buf
int	precision	# number of digits after the decimal
int	ctowrd(), ctod(), stridx(), strlen()

begin
	ip = 1
	if (ctowrd (buf, ip, word, SZ_FNAME) < 1)
	    call error (1, "pr_precision:  bad input")

	# Get the numerical value.
	ip = 1
	nchar = ctod (word, ip, v1)

	# Find the position of the last digit in the string.  Note that
	# the very last character could be '.' or ':' rather than a digit.
	# If there is an exponent, take the last digit before the 'e'.

	e_char = 'e'
	length = stridx (e_char, word)		# check for 'e'
	if (length == 1)
	    return (0)
	else if (length > 1)
	    length = length - 1			# last digit before exponent
	else
	    length = strlen (word)		# no exponent

	while (length > 1) {
	    if (IS_DIGIT(word[length]))
		break
	    else
		length = length - 1
	}
	if ( ! IS_DIGIT(word[length]) )
	    return (0)

	# Add or subtract 1 from the last digit in the word.
	digit = TO_INTEG (word[length])
	if (digit < 9)
	    digit = digit + 1
	else
	    digit = digit - 1
	word[length] = TO_DIGIT (digit)

	# Get the value of the modified number.
	ip = 1
	nchar = ctod (word, ip, v2)

	diff = abs (v1 - v2)
	if (diff < EPSILOND)
	    return (NDIGITS_DP)

	logdiff = -log10 (diff)
	precision = int(logdiff)
	if (precision < 0)
	    precision = 0
	else if (logdiff - precision > 0.1)
	    precision = precision + 1

	return (precision)
end
