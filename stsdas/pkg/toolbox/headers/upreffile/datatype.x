include	<lexnum.h>

# DATATYPE -- Determine the data type of a character string value
#
# B.Simon	29-Apr-99	Based on similar code in ttools$copyone

int procedure datatype (value)

char	value[ARB]	# i: Character string value
#--
int	ic, dtype, nchar, ndigit
pointer	sp, uvalue

bool	streq()
int	nowhite(), lexnum()

begin
	# Convert value to upper case with no whitespace

	call smark (sp)
	call salloc (uvalue, SZ_LINE, TY_CHAR)

	nchar = nowhite (value, Memc[uvalue], SZ_LINE)
	call strupr (Memc[uvalue])

	# Determine if value is a number

	ic = 1
	switch (lexnum (Memc[uvalue], ic, ndigit)) {
	case LEX_OCTAL :
	    dtype = TY_INT
	case LEX_DECIMAL :
	    dtype = TY_INT
	case LEX_HEX :
	    dtype = TY_CHAR
	case LEX_REAL :
	    dtype = TY_DOUBLE
	case LEX_NONNUM :
	    dtype = TY_CHAR
	}

	# Check number of digits parsed against length of string
	# if it is shorter, we have a character string that starts 
	# with a digit

	if (ndigit != nchar)
	    dtype = TY_CHAR

	# Determine if string is boolean

	switch (Memc[uvalue]) {
	case 'T':
	    if (streq (Memc[uvalue],"T") || streq (Memc[uvalue],"TRUE"))
		dtype = TY_BOOL
	case 'F':
	    if (streq (Memc[uvalue],"F") || streq (Memc[uvalue],"FALSE"))
		dtype = TY_BOOL
	}

	call sfree (sp)
	return (dtype)
end

