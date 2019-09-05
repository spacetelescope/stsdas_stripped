include	<tbset.h>
include	"line.h"

# Column definitions.
define	LINES			1
define	STRENGTH		2
define	N_COLS			2

# Memory management.
define	Colname			Memc[colname+($1-1)*(SZ_COLNAME+1)]
define	Colptr			Memi[colptr+$1-1]
define	Nullflag		Memb[nullflag]
define	Sx			Memc[sx]

#---------------------------------------------------------------------------
.help read_lines 10Apr95 source
.ih
NAME
wid_ll_rtab -- Read lines from table.
.endhelp
#---------------------------------------------------------------------------
pointer procedure wid_ll_rtab (fname)

char	fname[ARB]		# I:  File name of table to read.

# Declarations.
pointer	colname			# Column names.
pointer	colptr			# Column pointers.
pointer	ll			# Line List Object.
pointer	wid_ll_alloc()		# Allocate the line list.
int	nlines			# Number of lines in table.
pointer	nullflag		# NULL indicators.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
pointer	t			# Table descriptor.
int	tbpsta()		# Get table parameters.
pointer	tbtopn()		# Open a table.

errchk	malloc
errchk	salloc, sfree, smark
errchk	tbcfnd, tbcgtd, tbpsta, tbtclo, tbtopn

begin
	# Open the table.
	t = tbtopn (fname, READ_ONLY, NULL)
	
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1)*N_COLS, TY_CHAR)
	call salloc (colptr, N_COLS, TY_POINTER)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Get column names from user.
	call clgstr ("lines_col", Colname(LINES), SZ_COLNAME)
	call clgstr ("strength_col", Colname(STRENGTH), SZ_COLNAME)

	# Find the columns.  Only LINES has to be there, other columns
	# are optional.
	call tbcfnd (t, Colname(1), Colptr(1), N_COLS)
	if (Colptr(LINES) == NULL) {
	    call sprintf (Sx, SZ_LINE, "no column '%s' giving line positions")
	    call pargstr (Colname(LINES))
	    call error (1, Sx)
	}

	# Read the lines.
        nlines = tbpsta (t, TBL_NROWS)
	ll = wid_ll_alloc (nlines)
	call malloc (nullflag, nlines, TY_BOOL)
	call tbcgtd (t, Colptr(LINES), LL_WAVE(ll,1), Nullflag, 1, nlines)

	# Read the Strengths.
	if (Colptr(STRENGTH) != NULL)
	    call tbcgtr (t, Colptr(STRENGTH), LL_INTP(ll,1), Nullflag,
			 1, nlines)

	# That's all folks.
	LL_N(ll) = nlines
	call mfree (nullflag, TY_BOOL)
	call sfree (sp)
	call tbtclo (t)
	return (ll)
end
#---------------------------------------------------------------------------
# End of wid_ll_rtab
#---------------------------------------------------------------------------
