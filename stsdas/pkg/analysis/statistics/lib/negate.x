#* HISTORY*
#* B.Simon	14-Aug-92	original

# NEGATE -- Negate the values in a one dimensional array in place

procedure negate (a, n)

double	a[ARB]		# u: array to be negated
int	n		# i: length of array
#--
int	i

begin
	do i = 1, n
	    a[i] = - a[i]

end
