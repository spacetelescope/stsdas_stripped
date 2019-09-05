include "pls.h"
define	SZ_MAXCOLS	20
# PLS_INPUT_TABLE -- Procedure to read a table with position information
# from the image subset chose. This information comes from a program that would
# read from an image display the position of known objects.

procedure pls_input_table (tp, pn, nlines, ncolumns, x, y)

pointer tp			# table descriptor
pointer pn			# pointer to objects structure
int	nlines			# number of objects in table
int	ncolumns		# number of columns in table
real	x[nlines], y[nlines]	# object pixel position

pointer	sp, cp
bool	nullflag
int	tbcnum()
int	i, k, ncol

begin

	call smark (sp)
	call salloc (cp, ncolumns, TY_INT)

	do k = 1, ncolumns
	   Memi[cp+k-1] = tbcnum (tp, k)

	ncol = 1
	nullflag = false
	do i = 1, nlines {
	   k = i - 1
	   # Read x_ref column
	   call tbrgtr (tp, Memi[cp], x[i], nullflag, ncol, i)
	   # Read y_ref column
	   call tbrgtr (tp, Memi[cp+1], y[i], nullflag, ncol, i)
	   # Read rigth ascension column
	   call tbrgtd (tp, Memi[cp+2], Memd[PRA(pn)+k], nullflag, ncol, i)
	   # Read declination column
	   call tbrgtd (tp, Memi[cp+3], Memd[PDEC(pn)+k], nullflag, ncol, i)
	   # Read magitud column
	   call tbrgtr (tp, Memi[cp+4], Memr[PMAG(pn)+k], nullflag, ncol, i)
	   # Read color column
	   call tbrgtr (tp, Memi[cp+5], Memr[PCOL(pn)+k], nullflag, ncol, i)
	   # Read associated flag
	   call tbrgti (tp, Memi[cp+6], Memi[POFLAG(pn)+k], nullflag, ncol, i)
	}	
	call sfree (sp)
end
