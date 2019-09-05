# fmessg -- write a message passed as a Fortran string

procedure fmessg (f77str)

%      character*(*) f77str
#--
char	message[SZ_LINE]

begin
	call f77upk (f77str, message, SZ_LINE)
	call logmsg (message)
end
