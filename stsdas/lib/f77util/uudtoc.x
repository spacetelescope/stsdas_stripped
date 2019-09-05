include <iraf77.h>
include	<ctype.h>
include	<printf.h>

# UUDTOC -- Format and output a floating point number, in any of the formats
# F,E,G,H, or M (H and M are hours-minutes-seconds and minutes-seconds formats,
# respectively).

procedure uudtoc (dval, decpl, fmt, width, f77str, istat)

double	dval			# number to be output
%	character*(*)	f77str
int	decpl			# number of decimal places or precision
int	fmt			# format (ascii equivalent f,e,g,h,m ...)
int	width			# field width
int	istat			# Error code

int	maxch			# size of the output string
char	outstr[SZ_LINE]		# output string
int	format			# format type (feghm)
int	nch
int	op
double	val
int	dtoc3(), ltoc(), gstrcpy(), len()
define	output {outstr[op]=$1;op=op+1;if(op>maxch)goto 10}

begin
	call f77upk (f77str, outstr, SZ_LINE)
%	maxch = len (f77str)
	istat = ER_OK
	# If HMS format is not desired, simply call DTOC3.  Control also returns
	# to this point in the event of overflow.

	if (IS_UPPER (fmt))
	    format = TO_LOWER (fmt)
	else
	    format = fmt

	if (format == FMT_FIXED || format == FMT_EXPON ||
	    format == FMT_GENERAL || IS_INDEFD(dval)) {

10	    nch = dtoc3 (dval, outstr, maxch, decpl, format, width)
	    call f77pak (outstr, f77str, SZ_LINE)	
	    return 
	}

	format = FMT_GENERAL					# for retries

	# HMS format is implemented using calls to DTOC3, LTOC.  Use zero
	# fill to get two chars for the second and third fields, if necessary.
	# The second field is omitted for "m" format.  No whitespace is
	# permitted in an HMS (or other) number.

	if (dval < 0.0 && long (dval) == 0)
	    op = gstrcpy ("-0", outstr, maxch) + 1
	else
	    op = ltoc (long(dval), outstr, maxch) + 1
	output (':')						# "+/-nnn:..."

	val = abs (dval)
	val = val - long (val)					# abs fraction

	if (fmt == FMT_HMS) {					# "...nn:..."
	    val = val * 60.0
	    if (long(val) < 10)
		output ('0')
	    op = op + ltoc (long(val), outstr[op], maxch-op+1)
	    output (':')
	    val = val - long (val)
	}

	val = val * 60.0					# "...nn.nnn"
	if (long(val) < 10)
	    output ('0')

	if (decpl <= 0)						# no decimal?
	    op = op + ltoc (long(val), outstr[op], maxch-op+1)
	else
	    op = op + dtoc3 (val, outstr[op], maxch-op+1, decpl, FMT_FIXED, ARB)

	# If the HMS format does not fit, go try a more compact format.
	if (op-1 > abs(width) || op > maxch)
	    goto 10
	call f77pak (outstr, f77str, SZ_LINE)
	return 
end
