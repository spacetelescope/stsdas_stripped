include	<iraf77.h>

# UCLPS? -- Put a scalar into the CL.

procedure uclpsd (f77par, buf, istat)

% 	character*(*) f77par
double	buf
int	istat

char 	parnam[SZ_FNAME]
# PIXEL   clput$t()

begin
	istat = ER_OK
	# Convert character string to SPP string
	call f77upk (f77par, parnam, SZ_FNAME)
	# put the parameter
	iferr ( call clputd (parnam, buf) )
	   istat = ER_CLPUT
	return

end
