include	<tbset.h>

#* HISTORY *
#* B.Simon	09-Mar-89	Original
#* B.Simon	19-Jun-92	Rewritten to produce an sdas table
#* B.Simon	12-Nov-97	Modified to remove cdbs dependence

# READAPER -- Read an aperture record from the SIAF file

procedure readaper (fd, tp, tag, list, fmt)

int	fd		# i: The file descriptor of the SIAF file
pointer	tp		# i: The output table descriptor
char	tag[ARB]	# i: Record identification tag
char	list[ARB]	# i: List of database column names
char	fmt[ARB]	# i: Record format
#--
int	cp, nrow, ilist, ifmt, clen, junk, ic
pointer	sp, cname, cfmt, cval, record, errmsg, field

string	notfound "Expected %s record not found: %s"
string	nocolumn "readaper: table column not found (%s)"

int	word_fetch()
int	getline(), tbpsta(), gstrcpy(), ctoi()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (cfmt, SZ_FNAME, TY_CHAR)
	call salloc (cval, SZ_LINE, TY_CHAR)
	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Read the record and check the tag field

	if (getline (fd, Memc[record]) == EOF) {
	    call sprintf (Memc[errmsg], SZ_LINE, notfound)
		call pargstr (tag)
		call pargstr ("<End of file>")
	    call error (1, Memc[errmsg])
	}

	if (Memc[record+71] != tag[1] || Memc[record+72] != tag[2]) {
	    call siaf_trim (Memc[record])
	    call sprintf (Memc[errmsg], SZ_LINE, notfound)
		call pargstr (tag)
		call pargstr (Memc[record+70])
	    call error (1, Memc[errmsg])
	}

	# Read the column values from the record into the database

	ifmt = 1
	ilist = 1
	field = record
	nrow = tbpsta (tp, TBL_NROWS)

	while (word_fetch (fmt, ifmt, Memc[cfmt], SZ_FNAME) > 0) {

	    # Extract the next field from the record

	    ic = 1
	    junk = ctoi (Memc[cfmt+1], ic, clen)
	    field = field + gstrcpy (Memc[field], Memc[cval], clen)
	    junk = word_fetch (list, ilist, Memc[cname], SZ_FNAME)
	    
	    # Copy it to the output table

	    call tbcfnd (tp, Memc[cname], cp, 1)
	    if (cp == NULL) {
		call sprintf (Memc[errmsg], SZ_LINE, nocolumn)
		call pargstr (Memc[cname])
		call error (1, Memc[errmsg])	
		
	    }
	    
	    call tbeptt (tp, cp, nrow, Memc[cval])
		
	}

	call sfree (sp)

end
