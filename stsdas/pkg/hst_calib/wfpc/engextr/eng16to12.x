# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
#  eng16to12 -- decode a 16 bits integer to 12 bits integer by overlapping
#               the second and third nibble.

int procedure eng16to12 (indata, status)

				## input:
int	indata			# input integer
				## output:
bool	status			# error status

int	intval			# integer value after the "compression"
int	nibble[4]

int	andi()
#---------------------------------------------------------------------------
begin
	nibble[1] = andi(indata, 0Fx)
	nibble[2] = andi(indata, 0F0x)/10x
	nibble[3] = andi(indata, 0F00x)/100x
	nibble[4] = andi(indata, 0F000x)/1000x

	status = (nibble[2] == nibble[3])
	if (status)
	    intval = nibble[1] + nibble[2]*10x + nibble[4]*100x
	else 
	    intval = 0

	return (intval)
end
