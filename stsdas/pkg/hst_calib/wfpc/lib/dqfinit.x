#  DQFINIT --	Initialize the DQF bits and datafile extensions.  
#
#  Date         Author                  Description
#  ----         ------                  -----------
#  01-Sep-1991  RAShaw                  coding
#  05-Jul-1995  J.-C. Hsu               Add bit 8 and 9 definitions
#  20-Oct-1995  J.-C. Hsu               Add bit 10 and 11 definitions
#------------------------------------------------------------------------------
procedure dqfinit (badbits, datextn, dqfextn)

#  Calling arguments:
int	badbits			# Flagged DQF bits
char	datextn[SZ_FNAME]	# Filename extension for images
char	dqfextn[SZ_FNAME]	# Filename extension for DQFs

#  Local variables:
bool	bit[16]			# Bit codes for DQF flags
int	i			# Loop index
int	nbits			# number of bits defined

#  Function used:
bool	clgetb()		# Get BOOL cl value
#------------------------------------------------------------------------------
begin
	bit[1] = clgetb ("rsbit")           # Reed-Solomon error
	bit[2] = clgetb ("calbit")          # Calibration file defect
	bit[3] = clgetb ("defbit")          # Permanent camera defect
	bit[4] = clgetb ("satbit")          # Saturated pixel
	bit[5] = clgetb ("misbit")          # Missing data
        bit[6] = clgetb ("genbit")          # Generic bad pixel
	bit[7] = clgetb ("ovrlapbit")       # Overlap region 
	bit[8] = clgetb ("crbit")           # Cosmic Ray hit
	bit[9] = clgetb ("trapbit")         # Trap column
	bit[10] = clgetb ("hpbit")          # "unfixable" hot pixel
	bit[11] = clgetb ("fixhpbit")       # "fixed" hot pixel

	# nbits cannot be larger than 16 because the DQF is in short integer
	nbits = 11
	badbits = 0

	do i = 1, nbits {
	    if (bit[i]) badbits = badbits + 2**(i-1)
	}

	call clgstr ("datextn", datextn, SZ_FNAME)
        call clgstr ("dqfextn", dqfextn, SZ_FNAME)
end
