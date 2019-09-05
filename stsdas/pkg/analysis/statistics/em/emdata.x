include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	22-Sep-92	revised version of get_km_data

# EMDATA -- Data input routine for EM method linear regression

int procedure emdata (tp, section, ind, x, y, y2, nvar, ntot)

pointer	tp		# i: input table descriptor
char	section[ARB]	# i: table section (contains column names)
int	ind[ARB]	# o: censor indicator
double	x[ntot,nvar]	# o: independent variable values
double	y[ARB]		# o: dependent variable values
double	y2[ARB]		# o: dependent variable upper limits
int	nvar		# o: number of regression coefficients
int	ntot		# i: number of data values
#--
bool	bounds
double	one, zero
int	ic, status, junk, ncol, icol, irow
pointer	sp, cp, nullflag, cname

data	one, zero  / 1.0, 0.0 /

string	nosect   "No section arguments"
string	badsect  "Wrong number of section arguments"
string	badcol   "Column not found"
string	nullval  "Null value in column"

int	word_count(), word_fetch()

begin
	status = OK

	# Allocate dynamic memory for temporary variables

	call smark (sp)
	call salloc (nullflag, ntot, TY_BOOL)
	call salloc (cname, SZ_COLNAME, TY_CHAR)

	# Check number of arguments in section

	ncol = word_count (section)
	nvar = ncol - 1

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

	# Check for presence of bounded data

	bounds = false
	do irow = 1, ntot {
	    if (ind[irow] == 5) {
		nvar = nvar - 1
		bounds = true
		break
	    }
	}

	# Fill first column of independent variable with ones

	call amovkd (one, x[1,1], ntot)

	# Read column containing independent variable values

	do icol = 2, nvar {
	    if (word_fetch (section, ic, Memc[cname], SZ_COLNAME) <= 0)
		break

	    call tbcfnd (tp, Memc[cname], cp, 1)
	    
	    if (cp == NULL) {
		call errdata (badcol, Memc[cname], tp, status)
		
	    } else {
		call tbcgtd (tp, cp, x[1,icol], Memb[nullflag], 1, ntot)
		
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

	# Read column containing upper limit of dependent variable

	if (! bounds) {
	    call amovkd (zero, y2, ntot)

	} else {
	    junk = word_fetch (section, ic, Memc[cname], SZ_COLNAME)
	    call tbcfnd (tp, Memc[cname], cp, 1)
	    
	    if (cp == NULL) {
		call errdata (badcol, Memc[cname], tp, status)
		
	    } else {
		call tbcgtd (tp, cp, y2, Memb[nullflag], 1, ntot)
		
		call findnull (Memb[nullflag], ntot, irow)
		if (irow > 0)
		    call errdata (nullval, Memc[cname], tp, status)
	    }
	}

	call sfree (sp)
	return (status)

end
