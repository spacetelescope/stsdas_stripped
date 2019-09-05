include <tbset.h>

#* HISTORY *
#* B.Simon	23-Nov-92	adapted from Asurv 1.2

# SP_TITLE -- Print title information for Spearman's rho

procedure sp_title (tp, section, first)

pointer	tp		# i: table descriptor
char	section[ARB]	# i: table section (contains column names)
bool	first		# u: first time this routine was called?
#--
pointer	sp, fname, title, cencol, indcol, depcol
int	ic, junk

string	header  " *** Correlation test by Spearman`s rho ***\n"

int	word_fetch()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (cencol, SZ_COLNAME, TY_CHAR)
	call salloc (indcol, SZ_COLNAME, TY_CHAR)
	call salloc (depcol, SZ_COLNAME, TY_CHAR)

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

	# Parse section for column names

	ic = 1
	junk = word_fetch (section, ic, Memc[cencol], SZ_COLNAME)
	junk = word_fetch (section, ic, Memc[indcol], SZ_COLNAME)
	junk = word_fetch (section, ic, Memc[depcol], SZ_COLNAME)

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
	call printf ("          Independent Variable:  %s\n")
	call pargstr (Memc[indcol])
	call printf ("            Dependent Variable:  %s\n")
	call pargstr (Memc[depcol])
	
	call printf ("\n")
	call sfree (sp)
end
