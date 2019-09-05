#* HISTORY *
#* B.Simon	30-Jun-92	Original
#* B.Simon	30-Oct-97	Modified for migration out of cdbs
#* B.Simon	15-Jul-98	Added is_reffile check

# GETQUAL -- Get the data quality file keyword and name

bool procedure getqual (key, file, maxch)

char	key[ARB]	# u: keyword
char	file[ARB]	# u: filename
int	maxch		# i: declared string length
#--
char	refkey[63,2], qualkey[63,2]
int	len, icode, iword
pointer	sp, root, ext

string	codestr  "uw"
string	urefs    "blevfile,biasfile,darkfile,flatfile"
string	uqual    "blevdfil,biasdfil,darkdfil,flatdfil"
string	wrefs    "blevfile,biasfile,preffile,purgfile,darkfile,flatfile"
string	wqual    "blevdfil,biasdfil,prefdfil,purgdfil,darkdfil,flatdfil"

equivalence (refkey[1,1], urefs)
equivalence (refkey[1,2], wrefs)
equivalence (qualkey[1,1], uqual)
equivalence (qualkey[1,2], wqual)

bool	is_reffile()
int	fnroot(), fnextn(), strlen(), stridx(), word_match()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	# See if this is an instrument with associated data quality file

	if (! is_reffile (file)) {
	    icode = 0

	} else if (fnroot (file, Memc[root], SZ_FNAME) == 0) {
	    icode = 0

	} else if (fnextn (file, Memc[ext], SZ_FNAME) == 0) {
	    icode = 0

	} else {
	    len = strlen (Memc[root])
	    icode = stridx (Memc[root+len-1], codestr)
	}

	# See if the input keyword has an associated data quality file

	if (icode == 0) {
	    iword = 0

	} else {
	    iword = word_match (key, refkey[1,icode])
	    if (iword > 0) {
		call word_find (iword, qualkey[1,icode], key, maxch)
		Memc[ext] = 'b'

		call sprintf (file, maxch, "%s.%s")
		call pargstr (Memc[root])
		call pargstr (Memc[ext])
	    }
	}

	call sfree (sp)
	return (iword > 0)

end
