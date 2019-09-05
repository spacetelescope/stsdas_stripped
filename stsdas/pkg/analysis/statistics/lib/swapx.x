#* HISTORY *
#* B.Simon	19-Nov-92	original

# SWAPX -- Swap two rows in the independent variable array (x)

procedure swapx (x, idx, jdx, nvar, ntot)

double	x[nvar,ntot]	# u: Independent variable array
int	idx		# i: First row index
int	jdx		# i: Second row index
int	nvar		# i: Number of columns
int	ntot		# i: Number of rows
#--
pointer	sp, xtemp

begin
	# Allocate memory for work array

	call smark (sp)
	call salloc (xtemp, nvar, TY_DOUBLE)

	# Swap rows

	call amovd (x[1,idx], Memd[xtemp], nvar)
	call amovd (x[1,jdx], x[1,idx], nvar)
	call amovd (Memd[xtemp], x[1,idx], nvar)

	call sfree (sp)
end
