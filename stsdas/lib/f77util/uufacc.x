include <iraf77.h>

# This procedure determines whether the specified file exists.

procedure uufacc (f77nam, file_exists, istatus)

#					  (I) the file name
%      character*(*) f77nam
bool	file_exists			# (O) true if the file exists
int	istatus				# integer status return

char	filename[SZ_LINE]		# the file name as an SPP string
int	access()

begin
	istatus = ER_OK
	call f77upk (f77nam, filename, SZ_LINE)
	file_exists = (access (filename, 0, 0) == YES)
end
