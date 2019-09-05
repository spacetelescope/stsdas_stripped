# UUCLKT -- Utility routine to get clock time in seconds.

procedure uuclkt (old_time, time)

# This procedure returns difference in seconds in  an integer argument,
# between the clock time and old_time. If old_time is zero it returns the
# number of seconds since midnight 1-jan-1980.

int	old_time	# input time in seconds
int	time		# output time
int	clktime()

begin

	time = clktime(old_time)

end
