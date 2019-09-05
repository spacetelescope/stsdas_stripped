include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	19-Nov-92	extracted from get_cx_data

# CXTITLE -- Print title information for Buckley-James linear regression

procedure cxtitle (tp, section, nvar, first)

pointer	tp		# i: table descriptor
char	section[ARB]	# i: table section (contains column names)
int	nvar		# i: number of independent variables
bool	first		# u: first time this routine was called?
#--
pointer	sp, fname, title, colname
int	ic, icol, junk

string	header  " *** Correlation Test by Cox Proportional Hazard Model ***\n"

int	word_fetch()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	# Print header if this is the first time this routine was called

	if (first) {
	    first = false

	    call printf ("\n")
	    call printf (header)
	    call printf ("\n")
	}

	# Read information from table

	call tbtnam (tp, Memc[fname], SZ_FNAME)

	iferr (call tbhgtt(tp, "title", Memc[title], SZ_LINE))
	    Memc[title] = EOS

	# Print summary information

	if (Memc[title] != EOS) {
	    call printf ("Title:  %s\n")
	    call pargstr (Memc[title])
	    call printf ("\n")
	}
	
	call printf ("Data file:  %s\n")
	call pargstr (Memc[fname])

	ic = 1
	junk = word_fetch (section, ic, Memc[colname], SZ_COLNAME)

	call printf ("Columns used: Censor Indicator:  %s\n")
	call pargstr (Memc[colname])

	do icol = 2, nvar+1 {
	    junk = word_fetch (section, ic, Memc[colname], SZ_COLNAME)

	    if (icol == 2) {
		call printf ("       Independent Variable(s):  %s\n")
	    } else {
		call printf ("%33t %s\n")
	    }

	    call pargstr (Memc[colname])
	}

	junk = word_fetch (section, ic, Memc[colname], SZ_COLNAME)

	call printf ("            Dependent Variable:  %s\n")
	call pargstr (Memc[colname])
	
	call printf ("\n")
	call sfree (sp)
end
