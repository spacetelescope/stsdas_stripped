%{

include <ctype.h>
include "calcnoise.h"

define	YYMAXDEPTH	64
define	YYOPLEN		1
define	yyparse		parsenoise

define	BLANK		' '

# Tokens generated by xyacc have been moved calcnoise.h

%L
include "compnoise.com"

%}

%token		N_WRONG N_LPAR N_RPAR 
%token		N_READ N_TIME
%token		N_DONE N_SWAP N_NUM
%token		N_FLUX N_SQRT N_LOG

%left		N_MAG
%left		N_ADD N_SUB
%left		N_MUL N_DIV
%right		N_EXP
%right		N_NEG

%%

satement :	N_DONE {
		    # Blank expression
		    call add_pcode (N_DONE)
		    return (OK)
		}
	|	expr N_DONE {
		    # End of expression; successful parse
		    if (Memi[$1] != NULL) 
			call add_const (Memr[Memi[$1]])

		    call add_pcode (N_DONE)
		    return (OK)
		}
	|	error {
		    # Error exit
		    return (ERR)
		}
	;

expr	:	N_NUM {
		    call salloc (Memi[$$], 1, TY_REAL)
		    Memr[Memi[$$]] = Memr[Memi[$1]]
		}
	|	N_FLUX {
		    # Push a variable
		    call add_pcode (N_FLUX)
		    Memi[$$] = NULL
		}
	|	N_READ {
		    call salloc (Memi[$$], 1, TY_REAL)
		    Memr[Memi[$$]] = readout
		}
	|	N_TIME {
		    call salloc (Memi[$$], 1, TY_REAL)
		    Memr[Memi[$$]] = time
		}
	|	N_SUB expr %prec N_NEG {
		    # Negation
		    if (Memi[$1] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = - Memr[Memi[$1]]
		    } else {
			call add_pcode (N_NEG)
			Memi[$$] = NULL
		    }
		}
	|	expr N_EXP expr {
		    # Exponentiation
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = Memr[Memi[$1]] ** Memr[Memi[$3]]

		    } else {
			if (Memi[$1] != NULL) {
			    call add_const (Memr[Memi[$1]])
			    call add_pcode (N_SWAP)
			}

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_EXP)
			Memi[$$] = NULL
		    }
		}
	|	expr N_MUL expr {
		    # Multiplication
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = Memr[Memi[$1]] * Memr[Memi[$3]]

		    } else {
			if (Memi[$1] != NULL)
			    call add_const (Memr[Memi[$1]])

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_MUL)
			Memi[$$] = NULL
		    }
		}
	|	expr N_DIV expr {
		    # Division
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = Memr[Memi[$1]] / Memr[Memi[$3]]

		    } else {
			if (Memi[$1] != NULL) {
			    call add_const (Memr[Memi[$1]])
			    call add_pcode (N_SWAP)
			}

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_DIV)
			Memi[$$] = NULL
		    }
		}
	|	expr N_ADD expr {
		    # Addition
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = Memr[Memi[$1]] + Memr[Memi[$3]]

		    } else {
			if (Memi[$1] != NULL)
			    call add_const (Memr[Memi[$1]])

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_ADD)
			Memi[$$] = NULL
		    }
		}
	|	expr N_SUB expr {
		    # Subtraction
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = Memr[Memi[$1]] - Memr[Memi[$3]]

		    } else {
			if (Memi[$1] != NULL) {
			    call add_const (Memr[Memi[$1]])
			    call add_pcode (N_SWAP)
			}

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_SUB)
			Memi[$$] = NULL
		    }
		}
	|	expr N_MAG expr {
		    # Magnitude (sqrt (a**2 + b**2))
		    if (Memi[$1] != NULL && Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = sqrt (Memr[Memi[$1]] ** 2 + 
					       Memr[Memi[$3]] ** 2  )

		    } else {
			if (Memi[$1] != NULL)
			    call add_const (Memr[Memi[$1]])

			if (Memi[$3] != NULL)
			    call add_const (Memr[Memi[$3]])

			call add_pcode (N_MAG)
			Memi[$$] = NULL
		    }
		}
	|	N_SQRT N_LPAR expr N_RPAR {
		    # Square root
		    if (Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = sqrt (Memr[Memi[$3]])
		    } else {
			call add_pcode (N_SQRT)
			Memi[$$] = NULL
		    }
		}
	|	N_LOG N_LPAR expr N_RPAR {
		    # Natural logarithm
		    if (Memi[$3] != NULL) {
			call salloc (Memi[$$], 1, TY_REAL)
			Memr[Memi[$$]] = alog (Memr[Memi[$3]])
		    } else {
			call add_pcode (N_LOG)
			Memi[$$] = NULL
		    }
		}
	|	N_LPAR expr N_RPAR {
		    # Parenthesized expression, no-op
		    if (Memi[$2] != NULL) {
		    	call salloc (Memi[$$], 1, TY_REAL)
		    	Memr[Memi[$$]] = Memr[Memi[$2]]
		    } else {
			Memi[$$] = NULL
		    }
		}
	|	N_WRONG {
		    call eprintf ("Unrecognized token\n")
		    return (ERR)
		}
	;

%%

#* HISTORY *
#* B.Simon	30-Mar-95	original
#* B.Simon	30-May-95	added nread

# COMPNOISE -- Compile a noise expression into pseudocode

procedure compnoise (expr, exptime, nread, pcode, maxcode)

char	expr[ARB]	# i: noise expression
real	exptime		# i: exposure time
int	nread		# i: number of readouts
int	pcode[ARB]	# o: pseudocode instructions
int	maxcode		# i: length of instruction array
#--
include "compnoise.com"

int	len, debug
pointer	sp, code, line, ch

data	debug	/ NO /

int	strlen(), parsenoise()

extern	lexnoise

begin
	# Allocate memory for temporary arrays

	len = strlen (expr) 

	call smark (sp)
	call salloc (code, maxcode, TY_INT)
	call salloc (line, len, TY_CHAR)

	# Initialize parsing common block

	icode = code
	ncode = maxcode
	readout = nread
	time = exptime

	# Parse expression to produce pseudocode

	ch = line
	call strcpy (expr, Memc[line], len)

	if (parsenoise (ch, debug, lexnoise) == ERR) {
	    call eprintf ("%s\n%*t^\n")
	    call pargstr (Memc[line])
	    call pargi (ch-line)

	    call error (1, "Syntax error in noise expression")
	}

	# Clean up and return pseudocode array

	call amovi (Memi[code], pcode, maxcode)
	call sfree (sp)

end

# LEXNOISE -- Lexical analyzer for noise expression

int procedure lexnoise (ch, value)

pointer	ch		# u: Pointer to current char in expression
pointer	value		# i: Pointer to current token value
#--
include "compnoise.com"

char	token[SZ_TOKEN]
int	ic, nc, type, itype, functype[5], symtype[7]
real	const

string	funclist  "n,t,x,sqrt,log"
string	symlist   "()+-*/&"

data	functype  / N_READ, N_TIME, N_FLUX, N_SQRT, N_LOG /
data	symtype   / N_LPAR, N_RPAR, N_ADD, N_SUB, N_MUL, N_DIV, N_MAG /

int	ctor(), stridx(), word_match()

begin
	# Skip over leading white space 

	while (Memc[ch] <= BLANK) {
	    if (Memc[ch] == EOS)
		break

	    ch = ch + 1
	}

	# Determine token type from leading character

	if (Memc[ch] == '.' || IS_DIGIT(Memc[ch])) {
	    # Token is a number

	    ic = 1
	    nc = ctor (Memc[ch], ic, const)
	    nc = min (nc, SZ_TOKEN)
	    call strcpy (Memc[ch], token, nc)

	    type = N_NUM

	    call salloc (Memi[value], 1, TY_REAL)
	    Memr[Memi[value]] = const

	} else if (IS_ALPHA(Memc[ch])) {
	    # Token is a variable or function name

	    for (ic = 0; ic < SZ_TOKEN; ic = ic + 1) {
		if (! IS_ALNUM(Memc[ch+ic]))
		    break

		token[ic+1] = Memc[ch+ic]
	    }

	    token[ic+1] = EOS
	    nc = ic

	    # Determine token type by consulting list

	    call strlwr (token)
	    itype = word_match (token, funclist)

	    if (itype == 0) {
		type = N_WRONG
	    } else {
		type = functype[itype]
	    }

	    Memi[value] = NULL

	} else {
	    # Token is an operator
	    # Get symbol type from list

	    itype = stridx (Memc[ch], symlist)
	    if (itype == 0) {
		if (Memc[ch] == EOS) {
		    type = N_DONE
		} else {
		    type = N_WRONG
		}

	    } else {
		type = symtype[itype]
	    }

	    # Distinguish between multiplication and exponentiation

	    nc = 1
	    if (Memc[ch] == '*') {
		if (Memc[ch+1] == '*') {
		    type = N_EXP
		    nc = 2
		}
	    }

	    Memi[value] = NULL
	}

	ch = ch + nc
	return (type)
end

procedure add_const (const)

real	const		# i: real constant to be added to code
#--
include "compnoise.com"

char	token[SZ_TOKEN]
int	ic

begin
	call add_pcode (N_NUM)

	call sprintf (token, SZ_TOKEN, "%g")
	call pargr (const)

	ic = 0
	repeat {
	    if (ncode == 0) 
		call error (1, "Noise expression too complex")

	    ic = ic + 1
	    Memi[icode] = token[ic]

	    icode = icode + 1
	    ncode = ncode - 1
	} until (token[ic] == EOS)

end

procedure add_pcode (pcode)

int	pcode		# i:pseudocode instruction
#--
include "compnoise.com"

begin
	if (ncode == 0) 
	    call error (1, "Noise expression too complex")

	Memi[icode] = pcode
	icode = icode + 1
	ncode = ncode - 1

end

