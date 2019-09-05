include	<time.h>
define	SZ_ID		9

# MAKEID -- Make an identifier from the current time
#
# B.Simon	27-Aug-87	First Code

procedure makeid (id, maxch)

char	id[ARB]		# o: Identifier
int	maxch		# i: Maximum length of id

int	ip, time[LEN_TMSTRUCT]
long	sec, oldsec
pointer	sp, tmpid

data	oldsec	/0/

long	clktime()

begin
	# Allocate storage space for temporary id

	call smark (sp)
	call salloc (tmpid, SZ_ID+1, TY_CHAR)

	# Get the current time. Make sure two seconds elapse between
	# invocations of this routine

	sec = clktime (long(0))
	if (sec - oldsec < 2) {
	    call tsleep (2)
	    sec = clktime (long(0))
	}
	oldsec = sec
	call brktime (sec, time)

	# Create an identifier with the format YMDHHMMS
	#
	#   Y denotes the year (1-z, where 1981 = 1)
	#   M denotes the month of the year (1-c, where January = 1)
	#   D denotes the day (1-v, where first day of month = 1)
	#   HH denotes the hour (00-23)
	#   MM denotes the minutes (00-59)
	#   S  denotes (seconds/2)  (0 - t)
                                              
	ip = 1
	call basechar (TM_YEAR(time)-1980, 36, Memc[tmpid], ip)
	call basechar (TM_MONTH(time), 36, Memc[tmpid], ip)
	call basechar (TM_MDAY(time), 36, Memc[tmpid], ip)
	call basedigit (TM_HOUR(time), 60, Memc[tmpid], ip)
	call basedigit (TM_MIN(time), 60, Memc[tmpid], ip)
	call basechar (TM_SEC(time)/2, 36, Memc[tmpid], ip)
	Memc[tmpid+ip-1] = EOS

	# Copy the identifier and free allocated memory

	call strcpy (Memc[tmpid], id, maxch)
	call sfree (sp)
end
