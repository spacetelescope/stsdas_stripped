#  DQVAL --	Evaluate the DQF bits

procedure dqval (badbits)

#  Calling arguments:
int	badbits			# Flagged DQF bits

#  Local variables:
bool	bit[16]			# Bit codes for DQF flags
int	i			# Loop index
int	nbits			# number of bits defined

#  Function used:
bool	clgetb()		# Get BOOL cl value

begin
	bit[1] = clgetb ("bit1")           
	bit[2] = clgetb ("bit2")           
	bit[3] = clgetb ("bit3")           
	bit[4] = clgetb ("bit4")           
	bit[5] = clgetb ("bit5")           
	bit[6] = clgetb ("bit6")           
	bit[7] = clgetb ("bit7")           
	bit[8] = clgetb ("bit8")           
	bit[9] = clgetb ("bit9")           
	bit[10] = clgetb ("bit10")           
	bit[11] = clgetb ("bit11")           
	bit[12] = clgetb ("bit12")           
	bit[13] = clgetb ("bit13")           
	bit[14] = clgetb ("bit14")           
	bit[15] = clgetb ("bit15")           
	bit[16] = clgetb ("bit16")           
 
        # nbits cannot be larger than 16 because the DQF is in short integer
        nbits = 16
        badbits = 0
 
        do i = 1, nbits
            if (bit[i]) badbits = badbits + 2**(i-1)
end
