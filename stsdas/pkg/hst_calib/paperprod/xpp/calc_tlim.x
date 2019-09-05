# CALC_TLIM
# 14 Apr 1997 - WJH
# This task was written to find the valid max and min values of a 
#    JITTER table column, or any other table with less than 28 columns.  
# This task was based on the method used in TSTAT, without a lot of
#    the extra checks which were deemed unnecessary for these purposes.
#
define	JIT_COLS	28

procedure calc_tlimr (tp, colname, nrows, colmin, colmax)

pointer	tp			# I: table pointer
real	colmin, colmax		# O: column min and max values
int	nrows			# I: number of rows in column
char	colname[ARB]		# I: name of column 

pointer colptr
int	row
real	val, tmpmin, tmpmax

begin
	call tbcfnd(tp, colname, colptr, JIT_COLS)

	call tbegtr (tp, colptr, 1, val)
	if (!IS_INDEFR(val)) {
		# initialize values of min and max
		tmpmin = val
		tmpmax = val
	} else {
		tmpmin = 0.
		tmpmax = 0.
	}
	row = 2

	while (row <= nrows) {
		call tbegtr (tp, colptr, row, val)
		if (!IS_INDEFR(val)) {
			# check for min and max values here
			if (val < tmpmin) tmpmin = val
			if (val > tmpmax) tmpmax = val
		}

		row = row + 1
	}
	colmin = tmpmin
	colmax = tmpmax
end
