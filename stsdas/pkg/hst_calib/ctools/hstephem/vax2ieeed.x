include	<mach.h>

#  vax2ieeed -- Convert a vax format double precision to ieee format on any
#		machine
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  29-Jun-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure vax2ieeed (vax, ieee, n)

char	vax[ARB]		# input: vax format data 
double	ieee[ARB]		# output: ieee format data 
int	n			# input: how many double precision numbers to
				# convert
 
int	i, j
int	dums1, dums2, dums3
int	ch1[2], ch2[2], ch3[2]
#==============================================================================
begin

	# loop each double precision number
	do i = 1, n {

	    do j = 1, 2
		call bytmov (vax[1+(i-1)*4], 1+(j-1)*4, ch1[j], 1, 4)

	    # when bytmov to or from integer data type, need to swap bytes
	    # since we count bytes from left to right here
	    # (on the other hand, bits are always countd from right to left)
	    if (BYTE_SWAP4 == YES)
		call bswap4 (ch1[1], 1, ch1[1], 1, 8)

	    # swap byte pairs, this is because VAX's floating point data is 
	    # arranged as 21436587, from most to least significant byte
	    call bswap2 (ch1[1], 1, ch2[1], 1, 8)
	    
	    # copy the mantissa by bit shitfting
	    call bitmov (ch2[1], 4, ch3[1], 1, 20)

	    call bitmov (ch2[1], 1, ch3[2], 30, 3)
	    call bitmov (ch2[2], 4, ch3[2], 1, 29)

	    # copy the sign bit 
	    call bitmov (ch2[1], 32, ch3[1], 32, 1)

	    # now, deal with the exponent
	    dums1 = 0
	    call bitmov (ch2[1], 24, dums1, 1, 8)

	    # there is no need to swap bytes for integers when adding a constant
	    #if (BYTE_SWAP4 == YES)
	        #call bswap4 (dums1, 1, dums2, 1, 4)
	    #else
		dums2 = dums1

	    # the bias is 894 (=1024-128-2)
	    dums2 = dums2 + 894

	    #if (BYTE_SWAP4 == YES)
	        #call bswap4 (dums2, 1, dums3, 1, 4)
	    #else
		dums3 = dums2

	    call bitmov (dums3, 1, ch3[1], 21, 11)

	    # when bytmov to or from integer data type, need to swap bytes
	    if (BYTE_SWAP4 == YES)
		call bswap4 (ch3[1], 1, ch3[1], 1, 8)
	    call bytmov (ch3[1], 1, ieee[i], 1, 8)
	}
end
