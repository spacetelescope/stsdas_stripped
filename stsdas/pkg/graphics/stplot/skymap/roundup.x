double procedure round_up (x, y)

#  round_up -- Round X up to nearest whole multiple of Y.

double 	x	# Value to be rounded
double 	y	# Multiple X is to be rounded up in

double 	z
double 	r

begin
	if (x < 0.0)
	    z = 0.0
	else
	    z = y

	r = y * double (int ((x + z) / y))

	return (r)
end
