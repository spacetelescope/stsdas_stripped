include	"mka2d.h"

#  mka2d_map -- Determine the mapping of observed DN to the actual DN
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  04-Oct-1991  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure mka2d_map (errors, mapping)

real	errors[ARB]		# input: errors for each bit
real	mapping[ARB]		# output: the correct DN's

int	i, bit, n1, power
real	upper, last

int	andi()
#==============================================================================
begin
	last = 0.

	# loop for all possible DN's
	do i = 0, SZ_ATOD-1 {
	    upper = 0.

	    # determine the upper limit of the actual DN may be mapped to 
	    # each observed DN
	    n1 = 0
	    do bit = 0, NBITS-1 {
		power = 2 ** bit
		if (andi(i+1, power) > 0) {
		    upper = upper + power
		    n1 = n1 + 1

		    # only add the error of the lowest bit because when the
		    # incoming analog voltage is subtracted, the subtracted 
		    # amount is precise (i.e. without the associated bit error)
		    if (n1 == 1)
			upper = upper + errors[bit+1]
		}	
	    }
	    if (upper < last)

		# for DN's not mapped, set them to a dummy number
		mapping[i+1] = i + 0.5
	    else {
		mapping[i+1] = (last+upper) / 2.
		last = upper
	    }
	}
end
