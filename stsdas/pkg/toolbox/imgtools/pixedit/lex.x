include	<ctype.h>
include	"lex.h"

# LEX_FORMAT lexically analyze a format string

int procedure lex_format (str, pad, width, dec, type)

char	str[ARB]	# i: string to be analyzed
int	pad		# o: type of padding
int	width		# o: field width
int	dec		# o: number of decimal places
int	type		# o: data type
#--
int	ic, junk, index

string	fmt_chars  "xdefgh"

int	lex_number(), stridx()

begin
	ic = 1
	if (str[ic] == '%')
	    ic = ic + 1

	if (str[ic] == '-') {
	    ic = ic + 1
	    pad = -1
	} else if (str[ic] == '0') {
	    ic = ic + 1
	    pad = 0
	} else {
	    pad = 1
	}

	if (lex_number (str, ic, width) == LEX_ERROR)
	    return (LEX_ERROR)

	if (str[ic] == '.') {
	    ic = ic + 1
	    junk = lex_number (str, ic, dec)
	}

	index = stridx (str[ic], fmt_chars)
	if (index > 0) {
	    type = fmt_chars[index]
	    return (LEX_OKAY)
	} else {
	    type = EOS
	    return (LEX_ERROR)
	}

end

# LEX_NUMBER -- Lexically analyze a string containing a number

int procedure lex_number (str, ic, value)

char	str[ARB]	# i: string to be analyzed
int	ic		# i: first character of integer
int	value		# o: resulting value
#--
int	status

begin
	value = 0
	status = LEX_ERROR

	while (IS_DIGIT(str[ic])) {
	    value = 10 * value + TO_INTEG(str[ic])
	    status = LEX_MORE
	    ic = ic + 1
	}

	    
	if (status == LEX_MORE && str[ic] == EOS)
	    status = LEX_OKAY

	return (status)
end

# LEX_STRING -- Lexically analyze a string with several alternatives

int procedure lex_string (choices, str, ic, value)

char	choices[ARB]	# i: possible legal values
char	str[ARB]	# i: string to be analyzed
int	ic		# u: first character of integer
int	value		# o: resulting value
#--
int	jc, junk, status
pointer	sp, temp

int	ctowrd(), strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	jc = ic
	junk = ctowrd (str, ic, Memc[temp], SZ_LINE)

	if (str[jc] == EOS) {
	    status = LEX_OKAY
	} else {
	    status = LEX_MORE
	}

	value = strdic (Memc[temp], Memc[temp], SZ_LINE, choices)

	if (value == 0) { 
	    ic = jc
	    status = LEX_ERROR
	}

	call sfree (sp)
	return (status)

end

# LEX_YORN -- Lexically analyze a yes or no result

int procedure lex_yorn (str, ic, value)

char	str[ARB]	# i: string to be analyzed
int	ic		# u: first character of integer
int	value		# o: resulting value
#--
int	jc, status
pointer	sp, temp

int	lex_string()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	call strcpy (str[ic], Memc[temp], SZ_LINE)
	call strlwr (Memc[temp])
	jc = 1

	status = lex_string ("|no|yes|", Memc[temp], jc, value)
	value = value - 1

	ic = ic + jc - 1
	call sfree (sp)

	return (status)
end
