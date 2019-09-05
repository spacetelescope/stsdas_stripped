include <tbset.h>

# KS_GET_DATA -- Read data from table for KS test

procedure ks_get_data (infile, dp, npts)

char	infile[ARB]
pointer	dp
int	npts
#--
int	ip, op
pointer	sp, tp, cp, file, colname, errmsg, nullflag

string	nosect   "Filename does not include column (%s)"
string	notfound "Column not found (%s)"

int	tbpsta()
pointer	tbtopn()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (colname, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Open table and column

	call asparse (infile, Memc[file], SZ_FNAME, Memc[colname], SZ_FNAME)

	If (Memc[colname] == EOS) {
	    call sprintf (Memc[errmsg], SZ_LINE, nosect)
	    call pargstr (infile)
	    call error (1, Memc[errmsg])
	}

	tp = tbtopn (Memc[file], READ_ONLY, NULL)
	call tbcfnd (tp, Memc[colname], cp, 1)

	if (cp == NULL) {
	    call sprintf(Memc[errmsg], SZ_LINE, notfound)
	    call pargstr(infile)
	    call error (1, Memc[errmsg])
	}

	# Read data from column

	npts = tbpsta (tp, TBL_NROWS)
	call salloc (nullflag, npts, TY_BOOL)
	call malloc (dp, npts, TY_REAL)
	call tbcgtr (tp, cp, Memr[dp], Memb[nullflag], 1, npts)
	call tbtclo (tp)

	# Test for undefined values, discarding the undefined values

	op = 0
	do ip = 0, npts-1 {
	    Memr[dp+op] = Memr[dp+ip]
	    if (! Memb[nullflag+ip])
		op = op + 1
	}
	npts = op

	call sfree (sp)

end
