# linterp -- linear interpolation
# This replaces spec.x and invsens.x.

real procedure linterp (lambda, x, y, nspec, lastm)

real	lambda		# i: wavelength of interpolation
real	x[ARB]		# i: independent variable
real	y[ARB]		# i: dependent variable
int	nspec		# i: number of points in spec array
int	lastm		# io: current value of index into arrays
#--
real	specval		# return value of interpolated spectrum

begin
	lastm = max (lastm, 1)
	lastm = min (lastm, nspec)

	while (x[lastm] < lambda  && lastm < nspec)
	   lastm = lastm + 1

	while (x[lastm-1] > lambda && lastm > 2)
	   lastm = lastm - 1

	if (lambda == x[lastm-1])
	    return (y[lastm-1])

	specval = y[lastm-1] + (y[lastm] - y[lastm-1]) / 
	          (x[lastm] - x[lastm-1]) * (lambda - x[lastm-1])

	return specval
end
