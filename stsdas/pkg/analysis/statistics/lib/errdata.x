#* HISTORY *
#* B.Simon	06-Aug-92	original

# ERRDATA -- Print error message for error in input data

procedure errdata (msg, column, tp, status)

char	msg[ARB]	# i: error message
char	column[ARB]	# i: column name or section
pointer	tp		# i: table descriptor
int	status		# u: current error status
#--
pointer	sp, fname

string	errtitle  "*** File not processed because of the following error ***\n"
string	errfmt    "%s: %s[%s]\n"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call tbtnam (tp, Memc[fname], SZ_FNAME)

	if (status == OK) {
	    status = ERR
	    call eprintf (errtitle)
	}

	call eprintf (errfmt)

	call pargstr (msg)
	call pargstr (Memc[fname])
	call pargstr (column)

	call sfree (sp)
end
