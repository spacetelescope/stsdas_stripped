include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	10-Nov-92	revised version of get_bj_data

# BJDATA -- Data input routine for EM method linear regression

int procedure bjdata (tp, section, ind, x, y, nvar, ntot)

pointer	tp		# i: input table descriptor
char	section[ARB]	# i: table section (contains column names)
int	ind[ARB]	# o: censor indicator
double	x[nvar,ntot]	# o: independent variable values
double	y[ARB]		# o: dependent variable values
int	nvar		# o: number of regression coefficients
int	ntot		# i: number of data values
#--
int	ic, status, junk, ncol, icol, irow
pointer	sp, cp, temp, nullflag, cname

string	nosect   "No section arguments"
string	badsect  "Wrong number of section arguments"
string	badcol   "Column not found"
string	nullval  "Null value in column"

int	word_count(), word_fetch()

begin
	status = OK

	# Allocate dynamic memory for temporary variables

	call smark (sp)
	call salloc (temp, ntot, TY_DOUBLE)
	call salloc (nullflag, ntot, TY_BOOL)
	call salloc (cname, SZ_COLNAME, TY_CHAR)

	# Check number of arguments in section

	ncol = word_count (section)
	nvar = ncol - 2

	if (ncol == 0 ) {
	    call errdata (nosect, section, tp, status)

	} else if (ncol < 3) {
	    call errdata (badsect, section, tp, status)
	}

	# Read column containing censor values

	ic = 1
	junk = word_fetch (section, ic, Memc[cname], SZ_COLNAME)
	call tbcfnd (tp, Memc[cname], cp, 1)

	if (cp == NULL) {
	    call errdata (badcol, Memc[cname], tp, status)

	} else {
	    call tbcgti (tp, cp, ind, Memb[nullflag], 1, ntot)
	    
	    call findnull (Memb[nullflag], ntot, irow)
	    if (irow > 0)
		call errdata (nullval, Memc[cname], tp, status)
	}

	# Read column containing independent variable values

	do icol = 1, nvar {
	    if (word_fetch (section, ic, Memc[cname], SZ_COLNAME) <= 0)
		break

	    call tbcfnd (tp, Memc[cname], cp, 1)
	    
	    if (cp == NULL) {
		call errdata (badcol, Memc[cname], tp, status)
		
	    } else {
		call tbcgtd (tp, cp, Memd[temp], Memb[nullflag], 1, ntot)

		do irow = 1, ntot
		    x[icol,irow] = Memd[temp+irow-1]

		call findnull (Memb[nullflag], ntot, irow)
		if (irow > 0)
		    call errdata (nullval, Memc[cname], tp, status)
	    }
	}

	# Read column containing dependent variable values

	junk = word_fetch (section, ic, Memc[cname], SZ_COLNAME)
	call tbcfnd (tp, Memc[cname], cp, 1)

	if (cp == NULL) {
	    call errdata (badcol, Memc[cname], tp, status)

	} else {
	    call tbcgtd (tp, cp, y, Memb[nullflag], 1, ntot)
	    
	    call findnull (Memb[nullflag], ntot, irow)
	    if (irow > 0)
		call errdata (nullval, Memc[cname], tp, status)
	}

	call sfree (sp)
	return (status)

end
