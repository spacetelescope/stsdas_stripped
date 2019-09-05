include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	01-Sep-92	extracted from get_ts_data

# TSTITLE -- Print title information for two sided tests

procedure tstitle (tp, section, ifirst, isecond, start)

pointer	tp		# i: table descriptor
char	section[ARB]	# i: table section (contains column names)
int	ifirst		# i: first group to use
int	isecond		# i: second group to use
bool	start		# u: first time this routine was called?
#--
pointer	sp, fname, title, cencol, varcol, groupcol
int	ic, junk

int	word_fetch()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (cencol, SZ_COLNAME, TY_CHAR)
	call salloc (varcol, SZ_COLNAME, TY_CHAR)
	call salloc (groupcol, SZ_COLNAME, TY_CHAR)

	# Print title if this is the first time this routine was called

	if (start) {
	    start = false

	    call printf ("\n")
	    call printf ("*** Two Sample test *** \n")
	    call printf ("\n")
	}

	# Read information from table

	call tbtnam (tp, Memc[fname], SZ_FNAME)

	iferr (call tbhgtt(tp, "title", Memc[title], SZ_LINE))
	    Memc[title] = EOS

	# Parse section for column names

	ic = 1
	junk = word_fetch (section, ic, Memc[cencol], SZ_COLNAME)
	junk = word_fetch (section, ic, Memc[varcol], SZ_COLNAME)
	junk = word_fetch (section, ic, Memc[groupcol], SZ_COLNAME)

	# Print summary information

	if (Memc[title] != EOS) {
	    call printf ("Title:  %s\n")
	    call pargstr (Memc[title])
	    call printf ("\n")
	}
	
	call printf ("Data file:  %s\n")
	call pargstr (Memc[fname])

	call printf ("Columns used: Censor Indicator:  %s\n")
	call pargstr (Memc[cencol])

	call printf ("                      Variable:  %s\n")
	call pargstr (Memc[varcol])

	call printf ("               Group Indicator:  %s\n")
	call pargstr (Memc[groupcol])

	call printf ("                   Groups used:  %5d and %5d\n")
	call pargi (ifirst)
	call pargi (isecond)
	
	call printf ("\n")
	call sfree (sp)
end
