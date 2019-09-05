include <iraf77.h>

# UUCTOR -- Character to real.  The number of characters converted to produce
# the output number is returned as the function value (0 is returned if the
# input cannot be interpreted as a number).

procedure uuctor (f77str, ips, ipe, rval, istat)

%	character*(*)	f77str
char	str[SZ_LINE]		# input string to be decoded
int	ips			# first character to be used in string
int	ipe			# last character translated (output)
real	rval			# decoded real value (output)
int	istat			# Error code

int	nchars, ip
double	dval
int	ctod()

begin
	istat = ER_OK
	call f77upk (f77str, str, SZ_LINE)
	ip = ips
	nchars = ctod (str, ip, dval)
	ipe = ip
	if (IS_INDEFD(dval))
	    rval = INDEFR
	else
	    rval = dval

	if (nchars == 0)
	   istat = ER_NOTNUMBER
	
	return
end
