int	procedure negint (x)
#
#  This returns int(x) if x is positive, int(x) - 1 if x is negative
#
real	x
int	ival

begin
	ival = int (x)
	if (x < 0.0) ival = ival - 1
	return(ival)
end
