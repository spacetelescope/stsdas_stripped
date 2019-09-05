# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
#  bitson -- get the integer value specified by staring and ending bit indices

#  Description -- the bits are numbered from the least significant bit (bit 0)
#  to the most signicant bit.  

int procedure bitson (inbyte, firstbit, lastbit)

				##  inputs:
int	inbyte			# input byte or short word
int	firstbit		# staring bit index
int	lastbit			# ending bit index

				## local:
int	b1, b2, mask

int	andi()
#---------------------------------------------------------------------------
begin
	# if the starting and ending bit indices are out of order, reverse them
	if (firstbit >= lastbit) {
	    b1 = firstbit
	    b2 = lastbit
	} else {
	    b1 = lastbit
	    b2 = firstbit
	}

	# if the bit indices are negative, return zero
	if (b1 < 0)
	    return (0)

	mask = 2**(b1+1) - 2**b2
	return (andi(inbyte, mask) / 2**b2)
end
