#* HISTORY *
#* B.Simon	19-Nov-92	original

# BJSORT -- Sort procedure for Buckley James test

procedure bjsort (ind, x, y, z, ncol, ntot)

int	ind[ntot] 	# u: indicator of censoring
double	x[ncol,ntot]	# u: independent variable array
double	y[ntot]		# u: dependent variable array
double	z[ntot]		# u: adjusted dependent variable (sort key)
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
#--
double	ytemp, ztemp
int	idx, jdx, kdx, itemp
pointer	sp, xtemp, index

begin
	if (ntot <= 1)
	    return

	# Allocate memory for work arrays

	call smark (sp)
	call salloc (xtemp, ncol, TY_DOUBLE)
	call salloc (index, ntot, TY_INT)

	# Call routine to produce sorted index

	call unisort (ind, z, 1, 1, ntot, Memi[index])

	# Sort the censor and data arrays according to the index
	# The algorithm used is taken from Knuth's Sorting and Searching p.595

	do idx = 1, ntot {
	    if (Memi[index+idx-1] != idx) {
		ytemp = y[idx]
		ztemp = z[idx]
		itemp = ind[idx]
		call amovd (x[1,idx], Memd[xtemp], ncol)
		jdx = idx

		while (Memi[index+jdx-1] != idx) {
		    kdx = Memi[index+jdx-1]
		    y[jdx] = y[kdx]
		    z[jdx] = z[kdx]
		    ind[jdx] = ind[kdx]
		    call amovd (x[1,kdx], x[1,jdx], ncol)
		    Memi[index+jdx-1] = jdx
		    jdx = kdx
		}

		y[jdx] = ytemp
		z[jdx] = ztemp
		ind[jdx] = itemp
		call amovd (Memd[xtemp], x[1,jdx], ncol)
		Memi[index+jdx-1] = jdx
	    }
	}

	call sfree (sp)
end

# CXSORT -- Sort procedure for cox hazard test

procedure cxsort (ind, x, y, ncol, ntot)

int	ind[ntot] 	# u: indicator of censoring
double	x[ncol,ntot]	# u: independent variable array
double	y[ntot]		# u: dependent variable array (sort key)
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
#--
double	ytemp
int	idx, jdx, kdx, itemp
pointer	sp, xtemp, index

begin
	if (ntot <= 1)
	    return

	# Allocate memory for work arrays

	call smark (sp)
	call salloc (xtemp, ncol, TY_DOUBLE)
	call salloc (index, ntot, TY_INT)

	# Call routine to produce sorted index

	call unisort (ind, y, 1, 1, ntot, Memi[index])

	# Sort the censor and data arrays according to the index

	do idx = 1, ntot {
	    if (Memi[index+idx-1] != idx) {
		ytemp = y[idx]
		itemp = ind[idx]
		call amovd (x[1,idx], Memd[xtemp], ncol)
		jdx = idx

		while (Memi[index+jdx-1] != idx) {
		    kdx = Memi[index+jdx-1]
		    y[jdx] = y[kdx]
		    ind[jdx] = ind[kdx]
		    call amovd (x[1,kdx], x[1,jdx], ncol)
		    Memi[index+jdx-1] = jdx
		    jdx = kdx
		}

		y[jdx] = ytemp
		ind[jdx] = itemp
		call amovd (Memd[xtemp], x[1,jdx], ncol)
		Memi[index+jdx-1] = jdx
	    }
	}

	call sfree (sp)
end

# KM_SORT -- Sort procedure for kmestimate

procedure km_sort (ind, x, jcol, ncol, ntot)

int	ind[ntot] 	# u: indicator of censoring
double	x[ncol,ntot]	# u: data array
int	jcol		# i: which column to sort on
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
#--
int	idx, jdx, kdx, itemp
pointer	sp, xtemp, index

begin
	if (ntot <= 1)
	    return

	# Allocate memory for work arrays

	call smark (sp)
	call salloc (xtemp, ncol, TY_DOUBLE)
	call salloc (index, ntot, TY_INT)

	# Call routine to produce sorted index

	call unisort (ind, x, jcol, ncol, ntot, Memi[index])

	# Sort the censor and data arrays according to the index

	do idx = 1, ntot {
	    if (Memi[index+idx-1] != idx) {
		itemp = ind[idx]
		call amovd (x[1,idx], Memd[xtemp], ncol)
		jdx = idx

		while (Memi[index+jdx-1] != idx) {
		    kdx = Memi[index+jdx-1]
		    ind[jdx] = ind[kdx]
		    call amovd (x[1,kdx], x[1,jdx], ncol)
		    Memi[index+jdx-1] = jdx
		    jdx = kdx
		}

		ind[jdx] = itemp
		call amovd (Memd[xtemp], x[1,jdx], ncol)
		Memi[index+jdx-1] = jdx
	    }
	}

	call sfree (sp)
end

# SP_SORT -- Sort procedure for spearman's rho test

procedure sp_sort (ind, indx, xf, xx, jcol, ntot)

int	ind[ntot] 	# u: indicator of censoring
int	indx[2,ntot]	# u: separate censor array
double	xf[2,ntot]	# u: rank array
double	xx[2,ntot]	# u: data array
int	jcol		# i: which column of xx to sort on (1 or 2)
int	ntot		# i: number of data values
#--
double	xft[2], xxt[2]
int	idx, jdx, kdx, icol, idxt, indxt[2]
pointer	sp, index

begin
	if (ntot <= 1)
	    return

	# Allocate memory for work arrays

	call smark (sp)
	call salloc (index, ntot, TY_INT)

	# Call routine to produce sorted index

	call unisort (ind, xx, jcol, 2, ntot, Memi[index])

	# Sort the arrays according to the index

	do idx = 1, ntot {
	    if (Memi[index+idx-1] != idx) {
		idxt = ind[idx]	
		do icol = 1, 2 {
		    xxt[icol] = xx[icol,idx]
		    xft[icol] = xf[icol,idx]
		    indxt[icol] = indx[icol,idx]
		}
		jdx = idx

		while (Memi[index+jdx-1] != idx) {
		    kdx = Memi[index+jdx-1]
		    ind[jdx] = ind[kdx]
		    do icol = 1, 2 {
			xx[icol,jdx] = xx[icol,kdx]
			xf[icol,jdx] = xf[icol,kdx]
			indx[icol,jdx] = indx[icol,kdx]
		    }
		    Memi[index+jdx-1] = jdx
		    jdx = kdx
		}

		ind[jdx] = idxt
		do icol = 1, 2 {
		    xx[icol,jdx] = xxt[icol]
		    xf[icol,jdx] = xft[icol]
		    indx[icol,jdx] = indxt[icol]
		}
		Memi[index+jdx-1] = jdx
	    }
	}

	call sfree (sp)
end

# TS_SORT -- Sort procedure for two sided tests

procedure ts_sort (ind, ista, x, jcol, ncol, ntot)

int	ind[ntot] 	# u: indicator of censoring
int	ista[ntot]	# u: indicator of group
double	x[ncol,ntot]	# u: data array
int	jcol		# i: which column to sort on
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
#--
int	idx, jdx, kdx, itemp, jtemp
pointer	sp, xtemp, index

begin
	if (ntot <= 1)
	    return

	# Allocate memory for work arrays

	call smark (sp)
	call salloc (xtemp, ncol, TY_DOUBLE)
	call salloc (index, ntot, TY_INT)

	# Call routine to produce sorted index

	call unisort (ind, x, jcol, ncol, ntot, Memi[index])

	# Sort the censor, group  and data arrays according to the index

	do idx = 1, ntot {
	    if (Memi[index+idx-1] != idx) {
		itemp = ind[idx]
		jtemp = ista[idx]
		call amovd (x[1,idx], Memd[xtemp], ncol)
		jdx = idx

		while (Memi[index+jdx-1] != idx) {
		    kdx = Memi[index+jdx-1]
		    ind[jdx] = ind[kdx]
		    ista[jdx] = ista[kdx]
		    call amovd (x[1,kdx], x[1,jdx], ncol)
		    Memi[index+jdx-1] = jdx
		    jdx = kdx
		}

		ind[jdx] = itemp
		ista[jdx] = jtemp
		call amovd (Memd[xtemp], x[1,jdx], ncol)
		Memi[index+jdx-1] = jdx
	    }
	}

	call sfree (sp)
end

# UNICOMP -- Comparison routine for unisort

int procedure unicomp (row1, row2)

int	row1		# i: index to first row to compare
int	row2		# i: index to second row to compare
#--
int	icol, maxcol
pointer	iptr, xptr
common	/ usort /	iptr, xptr, icol, maxcol

double	dif
int	order
pointer	pos1, pos2

begin
	pos1 = xptr + (row1 - 1) * maxcol + (icol - 1)
	pos2 = xptr + (row2 - 1) * maxcol + (icol - 1)
	dif = Memd[pos1] - Memd[pos2]

	# If two rows have the same value, the censored row is placed last

	if (dif != 0.0) {
	    order = sign (1.0D0, dif)

	} else {
	    pos1 = iptr + row1 - 1
	    pos2 = iptr + row2 - 1

	    if (Memi[pos1] == 0) {
		if (Memi[pos2] == 0) {
		    order = 0
		} else {
		    order = 1
		}
	    } else {
		if (Memi[pos2] == 0) {
		    order = -1
		} else {
		    order = 0
		}
	    }
	}

	return (order)
end

# UNISORT -- Sorting routine used by statistics package

procedure unisort (ind, x, jcol, ncol, ntot, index)

int	ind[ntot] 	# i: indicator of censoring
double	x[ncol,ntot]	# i: data array
int	jcol		# i: which column to sort on
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
int	index[ntot]	# i: index array giving sorted order of data
#--
int	icol, maxcol
pointer	iptr, xptr
common	/ usort /	iptr, xptr, icol, maxcol

int	idx

int	locva()
int     unicomp()  # Comparison routine for unisort
extern	unicomp

begin
	# Initialize the index array

	do idx = 1, ntot
	    index[idx] = idx

	# Initialize common block used by quick sort comparison routine

	icol = jcol
	maxcol = ncol
	iptr = (locva (ind) - locva (Memc)) / SZ_INT + 1
	xptr = (locva (x) - locva (Memc)) / SZ_DOUBLE + 1 

	# Call the quick sort routine to produce a sorted index

	call qsort (index, ntot, unicomp)

end
