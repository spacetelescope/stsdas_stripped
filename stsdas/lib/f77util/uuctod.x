include <iraf77.h>
include	<ctype.h>
include	<chars.h>
include	<lexnum.h>

define	OCTAL		8
define	DECIMAL		10
define	HEX		16


# UUGTOD -- General character string to double precision real.  Any legal
# number, e.g., integer, floating point, complex, or character constant,
# is decoded and returned as a double.

procedure uuctod (f77str, ips, ipe, odval, istat)

%	character*(*)	f77str	
char	str[SZ_LINE]		# input string
int	ips			# pointer into input string
int	ipe			# index of last character translated
double	odval			# output double
int	istat			# Error code

char	ch
double	dval
complex	xval
long	lval
int	ip_save, radix, nchars, vtype, ip
int	ctox(), cctoc(), ctod(), gctol(), lexnum()

begin

	istat = ER_OK
	call f77upk (f77str, str, SZ_LINE)
	vtype = TY_DOUBLE				# val to be returned
	ip = ips
	while (IS_WHITE (str[ip]))
	    ip = ip + 1

	ip_save = ip
	ch = str[ip]					# first nonwhite

	if (ch == '(') {				# complex number?
	    if (ctox (str, ip, xval) <= 0) {
		istat = ER_NOTNUMBER
		ipe = ip
		return 					# not a number
	    } else
		vtype = TY_COMPLEX

	} else if (ch == SQUOTE || ch == ESCAPE) {
	    if (cctoc (str, ip, ch) <= 0) {		# character constant?
		istat = ER_NOTNUMBER
		ipe = ip
		return
	    } else
		dval = ch

	} else {				# determine type of number
	    switch (lexnum (str, ip, nchars)) {
	    case LEX_OCTAL:
		radix = OCTAL
	    case LEX_DECIMAL:
		radix = DECIMAL
	    case LEX_HEX:
		radix = HEX
	    case LEX_REAL:
		radix = TY_REAL
	    default:
		istat = ER_NOTNUMBER
		return
	    }

	    if (radix == TY_REAL) 		# perform the conversion
		nchars = ctod (str, ip, dval)
	    else {
		nchars = gctol (str, ip, lval, radix)
		dval = lval
		if (IS_INDEFL (lval))
		    dval = INDEFD
	    }
	}

	if (vtype == TY_COMPLEX) {
	    odval = xval
	    if (IS_INDEFX (xval))
		odval = INDEFD
	} else
	    odval = dval

	if (nchars == 0)
	   istat = ER_NOTNUMBER
	if (nchars == 0 && IS_INDEFD (odval))
	   istat = ER_OVFNUMBER
	ipe = ip
	return
end
