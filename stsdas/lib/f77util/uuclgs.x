include	<iraf77.h>

# UUCLGS -- Get a scalar character string from the CL.
#  The parameter is NOT list structure.

procedure uuclgs (f77par, f77buf, istat)

% 	character*(*) f77par
% 	character*(*) f77buf
int	istat

char	buf[SZ_LINE]

char 	parnam[SZ_FNAME]
errchk	clgstr

begin
	istat = ER_OK
	# Convert character string to SPP string
	call f77upk (f77par, parnam, SZ_FNAME)
	iferr (call clgstr (parnam, buf, SZ_LINE)) {	 # simple string
	      istat = ER_CLNOTFND
	      return
	}
	# Be sure the user gets a blank in case of an error
%       f77buf = ' '
	if (istat == ER_OK)
	   # convert string back to FORTRAN
	   call f77pak (buf, f77buf, SZ_LINE)
	return

end
