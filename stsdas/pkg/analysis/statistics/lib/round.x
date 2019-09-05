# ROUND -- Round a double precision value to given number of digits

double procedure round (value, digits)

double	value		# i: value to be rounded
int	digits		# i: number of digits
#--
double	v

begin
	if (value > 0.0)
	    v = aint (value * 10.0 ** digits + 0.5)
	else
	    v = aint (value * 10.0 ** digits - 0.5)

	return (v * 10.0 ** (- digits))
end
