#  FRACD -- Calculate fraction of a day including AM/PM consideration from hour
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  09-Dec-1994  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure fracd (hour, instr, fraday)

double	hour
char	instr[ARB]
double	fraday			# output: fraction of a day

int	strsearch()
#==============================================================================
begin
	# if hour spec is missing in the input string
	if (hour == INDEF) fraday = 0.d0

	# search for AM/PM pattern
	else {
	    if (strsearch (instr, "am") != 0 || 
	    	strsearch (instr, "a.m") != 0 ||
	    	strsearch (instr, "a. m") != 0) {
		if (hour < 1.d0 || hour >= 13.d0)
	    	    call printf ("CAUTION: unusual input hour, unexpected answer may result\n")
		hour = mod (hour, 12.d0)
	    } else if (strsearch (instr, "pm") != 0 || 
	    	       strsearch (instr, "p.m") != 0 ||
	    	       strsearch (instr, "p. m") != 0) {
		if (hour < 1.d0 || hour >= 13.d0)
	    	    call printf ("CAUTION: unusual input hour, unexpected answer may result\n")
	    	hour = mod (hour, 12.d0)
		hour = hour + 12.d0
	    }

	    # calculate fraction of the day
	    fraday = hour / 24.d0
	}
end
