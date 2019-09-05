# si_reverse - Reverse a set of vectors.
#
# History
#   14Mar91 - Create by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_reverse (x, y, npts)

real x[npts], y[npts]  # IO: The vectors to reverse.
int  npts              # I:  Number of points in the vectors.

# Declarations
real tx, ty            # Temporary storage.

int i, j               # Index
int mid_point          # The midpoint of the vectors.

begin
	mid_point = npts / 2
	for (i = 1; i <= mid_point; i = i + 1) {
	    tx = x[i]
	    ty = y[i]
	    j = npts - i + 1
	    x[i] = x[j]
	    y[i] = y[j]
	    x[j] = tx
	    y[j] = ty
	}
end
#---------------------------------------------------------------------------
# End of si_reverse
#---------------------------------------------------------------------------
