#* HISTORY *
#* B.Simon	19-Jun-92	original
#* B.Simon	12-Nov-97	modified to remove cdbs dependencies

define	LEN_FMLSTRUCT	2

define	FML_INDEX	Memi[$1]	# Index to character in format list
define	FML_LISTPTR	Memi[$1+1]	# Address of format list

define	FML_LIST	Memc[FML_LISTPTR($1)]

# CLS_FMTLIST -- Release memory used by format list

procedure cls_fmtlist (fp)

pointer	fp		# i: format list descriptor
#--

begin
	call mfree (FML_LISTPTR(fp), TY_CHAR)
	call mfree (fp, TY_STRUCT)
end

# OPN_FMTLIST -- Allocate and initialize a format list

pointer	procedure opn_fmtlist (fmtlist)

char	fmtlist[ARB]	# i: list of names and formats
#--
int	len
pointer	fp

int	strlen()

begin
	call malloc (fp, LEN_FMLSTRUCT, TY_STRUCT)

	FML_INDEX(fp) = 1
	len = strlen (fmtlist)
	call malloc (FML_LISTPTR(fp), len+1, TY_CHAR)
	call strcpy (fmtlist, FML_LIST(fp), len)

	return (fp)
end

# RD_FMTLIST -- Read and interpret the next item in format list

int procedure rd_fmtlist (fp, dtype, name, maxch)

pointer	fp		# i: format list descriptor
int	dtype		# o: data type of item
char	name[ARB]	# o: name of item
int	maxch		# i: declared length of name
#--
char	fmtchar
int	fmttype[5]
int	ic, nc, idx, junk
pointer	sp, item

data	fmtchar  / ':' /
data	fmttype  / TY_CHAR, TY_BOOL, TY_INT, TY_REAL, TY_DOUBLE /

string	fmtcode   "cbird"
string	badformat "Warning: unrecognized format, ch32 assumed (%s)\n"

int	stridx(), ctoi(), word_fetch()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (item, SZ_LINE, TY_CHAR)

	# Initialize output variables

	dtype = 0
	name[1] = EOS

	# Extract item from list

	nc = word_fetch (FML_LIST(fp), FML_INDEX(fp), Memc[item], SZ_LINE)

	# Break item into name and format string
	# Translate format string into SDAS table type

	if (nc > 0) {

	    ic = stridx (fmtchar, Memc[item])
	    if (ic == 0) {
		call strcpy (Memc[item], name, maxch)

	    } else {
		call strcpy (Memc[item], name, ic-1)
		idx = stridx (Memc[item+ic], fmtcode)

		if (idx > 0) {
		    dtype = fmttype[idx]

		    if (idx == 1) {
			ic = ic + 2
			if (Memc[item+ic+1] == 'h')
			    ic = ic + 1

			junk = ctoi (Memc[item], ic, dtype)
			dtype = - dtype
		    }
		}
	    }

	    # Use default type if type could not be parsed

	    if (dtype == 0) {
		call eprintf (badformat)
		call pargstr (Memc[item])
		
		dtype = -32
	    }
	}

	call sfree (sp)
	return (nc)
end
