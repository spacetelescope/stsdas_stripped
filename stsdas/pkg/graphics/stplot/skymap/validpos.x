int procedure valid_pos (ra, ranull, dec, decnull, mag, magnull,
	faint, bright, ramin, decmin, ramax, decmax)

double	ra, dec
real	mag
bool	ranull, decnull, magnull
real	faint, bright			# Magnitude limits
double	ramin, decmin, ramax, decmax	# Coordinate limits

begin
	if (ranull || decnull || magnull)
	    # Null column element
	    return (NO)

	if (IS_INDEFD(ra) || IS_INDEFD(dec) || IS_INDEFR(mag))
	    # Indefinite value
	    return (NO)

	if (ramin > ramax) {
	    # Range straddles 0
	    if (ra < ramin && ra > ramax)
		return (NO)
	} else {
	    if (ra < ramin || ra > ramax)
	    # RA is outside limits
		return (NO)
	}

	if (dec < decmin || dec > decmax)
	    # Dec is outside limits
	    return (NO)

#	if (!IS_INDEF(faint) && mag > faint)
#	    # Magnitude is fainter than limit
#	    return (NO)

#	if (!IS_INDEF(bright) && mag < bright)
#	    # Magnitude is brighter than limit
#	    return (NO)
	
	# Only then 
	return (YES)
end
