# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
define	ORDER	3
define	NCJ	8

#  temppoly -- Calculate the temperature by polynomial evaluation
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  05-Jul-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure temppoly (indata, junction, temperature)

						## input
int	indata
int	junction
						## output
real	temperature
						## local
real	coeff[ORDER+1, NCJ]
real	x
int	i

include	"temppoly.h"
#==============================================================================
begin
	temperature = 0.
	x = real(indata)

	do i = 1, ORDER+1
	    temperature = temperature + coeff[i, junction] * x**(i-1)
end
