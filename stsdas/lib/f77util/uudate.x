# UUDATE -- Utility routine to get the calendar date.
#
# Modifications
#  Lucy Willard 2-June-1989  comment out old_time declaration
procedure uudate (date)

# This procedure returns the time calendar date in a 22 character 
# buffer substring.
# The format is "DDD hh:mm:ss dd-mmm-yy".
# Where DDD represent the day of the week.

%	character*(*)	date
#long	old_time	# input time in seconds
long	time		# output time
char	outstr[SZ_LINE]
int	clktime()

begin

	time = clktime(0)
	call cnvtime (time, outstr, SZ_LINE)
	call f77pak (outstr, date, SZ_LINE)

end
