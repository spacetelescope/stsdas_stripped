# FINDNULL -- Find the location of the last null in a column

procedure findnull (nullflag, nrow, irow)

bool	nullflag[ARB]	# i: array of null flags
int	nrow		# i: total number of null flags
int	irow		# i: index of first true null flag
#--

begin
	for (irow = nrow; irow > 0; irow = irow - 1) {
	    if (nullflag[irow])
		break
	}
end
