###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	real procedure poly(ord, coef, x)
#		int	ord
#		real	coef[11]
#		real	x

#  Description:	POLY evaluates a polynomial

#  Arguments:	int	ord		Number of coefficients
#		real	coef[11]	Array of coefficients
#		real	x		Point of evaluation

#  Returns:	the value of the polynomial at x

#  Notes:

#  History:	June	1987	Gerard Kriss

###########################################################################

real procedure poly(ord, coef, x)

int	ord
real	coef[11]
real	x

int	i
real	xx, y

begin
	xx = 1.
	y = coef[1]
	for ( i = 2; i <= ord; i = i + 1)
	{
		xx = x * xx
		y = y + xx * coef[i]
	}
	return ( y )
end
