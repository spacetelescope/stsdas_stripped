#---------------------------------------------------------------------------
.help wp 23Feb95 source
.ih
NAME
.nf
w2p	-- Wavelength to pixel conversion.
p2w	-- Pixel to wavelength conversion.
.fi
.endhelp
#---------------------------------------------------------------------------
real procedure w2p (w, warray, n)

double	w			# I:  Wavelength to find.
double	warray[n]		# I:  Wavelength array.
int	n			# I:  Size of array.

# Declarations.
int	high			# Pixel of greater wavelength.
int	low			# Pixel of lesser wavelength.
int	mid			# Mid pixel of current search range.
int	np			# Pixels in the search range.
real	p			# Closest pixel to specified wavelength.

begin
	# Now find the two pixels the specified wavelength is between.
	np = n
	if (warray[1] < warray[np]) {
	    if (w < warray[1] || w > warray[np])
		call error (1, "wavelength is outside of array")
	    low = 1
	    high = np
	} else {
	    if (w > warray[1] || w < warray[np])
		call error (1, "wavelength is outside of array")
	    low = np
	    high = 1
	}
	while (np > 2) {
	    mid = (low + high) / 2
	    if (warray[mid] < w)
		low = mid
	    else
		high = mid
	    np = abs (high - low) + 1
	}

	# Linearly interpolate for the subpixel.
	p = low + ((w - warray[low]) / (warray[high] - warray[low]))

	# That's all folks.
	return (p)
end
#---------------------------------------------------------------------------
# End of w2p
#---------------------------------------------------------------------------
double procedure p2w (p, warray, n)

real	p			# I:  Pixel to find wavelength for.
double	warray[n]		# I:  Wavelength solution.
int	n			# I:  Size of array.

# Declarations
int	ip1, ip2		# Bracketting pixels.
double	w			# Wavelength.

begin
	if (p < 1 || p > n)
	    call error (1, "p2w: pixel not in wavelength solution")

	ip1 = p
	ip2 = min (ip1+1, n)
        if (ip1 == ip2)
            w = warray[ip1]
        else
            w = (p - ip1) * (warray[ip2] - warray[ip1]) + warray[ip1]

        return (w)
end
#---------------------------------------------------------------------------
# End of p2w
#---------------------------------------------------------------------------
