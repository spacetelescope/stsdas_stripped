include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	06-Aug-92	revised version of get_km_data

# TKMDATA -- Data input routine for Schmitt's linear regression

int procedure tkmdata (tp, section, ind, x, y, ntot)

pointer	tp		# i: input table descriptor
char	section[ARB]	# i: table section (contains column names)
int	ind[ARB]	# o: censor indicator
double	x[ARB]		# o: independent variable values
double	y[ARB]		# o: dependent variable values
int	ntot		# i: number of data values
#--
int	ic, status, junk, icol, irow
pointer	sp, cp, nullflag, cname

string	nosect   "No section arguments"
string	badsect  "Wrong number of section arguments"
string	badcol   "Column not found"
string	badsign  "File contains mixed censor indicators"
string	nullval  "Null value in column"

int	word_count(), word_fetch()

begin
	status = OK

	# Allocate dynamic memory for temporary variables

	call smark (sp)
	call salloc (nullflag, ntot, TY_BOOL)
	call salloc (cname, SZ_COLNAME, TY_CHAR)

	# Check number of arguments in section

	icol = word_count (section)
	if (icol == 0 ) {
	    call errdata (nosect, section, tp, status)

	} else if (word_count (section) != 3) {
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

	junk = word_fetch (section, ic, Memc[cname], SZ_COLNAME)
	call tbcfnd (tp, Memc[cname], cp, 1)

	if (cp == NULL) {
	    call errdata (badcol, Memc[cname], tp, status)

	} else {
	    call tbcgtd (tp, cp, x, Memb[nullflag], 1, ntot)
	    
	    call findnull (Memb[nullflag], ntot, irow)
	    if (irow > 0)
		call errdata (nullval, Memc[cname], tp, status)
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
