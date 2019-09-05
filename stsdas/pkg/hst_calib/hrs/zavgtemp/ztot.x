include	<error.h>
include	<tbset.h>
include	"zt.h"

# Memory management.
define	A		Memd[a+$1-1]
define	Colname		Memc[colname+(SZ_COLNAME+1)*($1-1)]
define	Colunits	Memc[colunits+(SZ_COLUNITS+1)*($1-1)]
define	Colfmt		Memc[colfmt+(SZ_COLFMT+1)*($1-1)]
define	Datatype	Memi[datatype+$1-1]
define	Lendata		Memi[lendata+$1-1]

#---------------------------------------------------------------------------
.help ztot.x 3Apr 95 source
.ih
NAME
ztot.x -- Write out accumulated temperatures.
.endhelp
#---------------------------------------------------------------------------
pointer procedure zt_ot_alloc (k, fname)

pointer	k			# I:  Keys object.
char	fname[ARB]		# I:  Table to open.

# Declarations.
pointer	ot			# Output table object.
int	tbtacc()		# Check for table existance.

errchk	zt_ot_create, zt_ot_open

begin
	# Create the output table object.
	call malloc (ot, ZT_OT_SZ, TY_STRUCT)
	ZT_OT_NCOLS(ot) = ZT_K_NKEY(k)
	call malloc (ZT_OT_COL_PTR(ot), ZT_K_NKEY(k), TY_POINTER)

	# Create or append to the table.
	if (tbtacc (fname) == YES)
	    call zt_ot_open (ot, k, fname)
	else
	    call zt_ot_create (ot, k, fname)

	# That's all folks.
	return (ot)
end
#---------------------------------------------------------------------------
# End of zt_ot_alloc
#---------------------------------------------------------------------------
procedure zt_ot_create (ot, k, fname)

pointer	ot			# I:  Keys output table object.
pointer	k			# I:  Keys object.
char	fname[ARB]		# I:  Table to create.

# Declarations.
pointer	colfmt			# Formats.
pointer	colname			# column names.
pointer	colunits		# Units.
int	cp			# Character pointer.
pointer	datatype		# Data type.
int	i			# Generic.
pointer	lendata			# Length of data.
pointer	sp			# Stack pointer
pointer	tbtopn()		# Open a table.
int	word_fetch()		# Get next word from string.

errchk	salloc, sfree, smark
errchk	tbcdef, tbtopn

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * ZT_OT_NCOLS(ot), TY_CHAR)
	call salloc (colunits, (SZ_COLUNITS+1) * ZT_OT_NCOLS(ot), TY_CHAR)
	call salloc (colfmt, (SZ_COLFMT+1) * ZT_OT_NCOLS(ot), TY_CHAR)
	call salloc (datatype, ZT_OT_NCOLS(ot), TY_INT)
	call salloc (lendata, ZT_OT_NCOLS(ot), TY_INT)
	
	# Open table.
	ZT_OT(ot) = tbtopn (fname, NEW_FILE, NULL)

	# Create the column definitions.
	i = 1
	cp = 1
	while (word_fetch (ZT_K_KEY(k), cp, Colname(i), SZ_COLNAME) > 0) {
	    call strcpy ("", Colunits(i), SZ_COLUNITS)
	    Datatype(i) = TY_DOUBLE
	    call strcpy ("", Colfmt(i), SZ_COLFMT)
	    Lendata(i) = 1
	    i = i + 1
	}

	call tbcdef (ZT_OT(ot), ZT_OT_COL(ot,1), Colname(1), Colunits(1),
		     Colfmt(1), Datatype(1), Lendata(1), ZT_OT_NCOLS(ot))

	ZT_OT_NROWS(ot) = 0

	# That's all folks.
	call tbtcre (ZT_OT(ot))
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of zt_ot_create
#---------------------------------------------------------------------------
procedure zt_ot_open (ot, k, fname)

pointer	ot			# I:  Keys Output table object.
pointer	k			# I:  Keys object.
char	fname[ARB]		# I:  Name of table to open.

# Declarations
pointer	colname			# Column names.
int	cp			# Character pointer.
int	i			# Generic.
pointer	sp			# Stack pointer.
int	tbpsta()		# Get table parameter.
pointer	tbtopn()		# Open a table.
int	word_fetch()		# Get next word from string.

errchk	salloc, sfree, smark

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * ZT_OT_NCOLS(ot), TY_CHAR)

	# Create the column definitions.
	i = 1
	cp = 1
	while (word_fetch (ZT_K_KEY(k), cp, Colname(i), SZ_COLNAME) > 0)
	    i = i + 1

	# Open table.
	ZT_OT(ot) = tbtopn (fname, READ_WRITE, NULL)

	# Find the columns.  For any column that is not present, define
	# a new column.
	call tbcfnd (ZT_OT(ot), Colname(1), ZT_OT_COL(ot,1), ZT_OT_NCOLS(ot))
	do i = 1, ZT_OT_NCOLS(ot)
	    if (ZT_OT_COL(ot,i) == NULL)
		call tbcdef (ZT_OT(ot), ZT_OT_COL(ot,i), Colname(i),
			     "", "", TY_DOUBLE, 1, 1)
	
	# Get number of rows in table.
	ZT_OT_NROWS(ot) = tbpsta (ZT_OT(ot), TBL_NROWS)

	# That's all folks
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of zt_ot_open
#---------------------------------------------------------------------------
procedure zt_ot_free (ot)

pointer	ot			# IO: Keys output table object; NULL on return

errchk	mfree, tbtclo

begin
	call tbtclo (ZT_OT(ot))
	call mfree (ZT_OT_COL_PTR(ot), TY_POINTER)
	call mfree (ot, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of zt_ot_free
#---------------------------------------------------------------------------
procedure zt_ot_wtab (ot, k)

pointer	ot			# I:  Keys output table object.
pointer	k			# I:  Keys object.

# Declarations.
pointer	a			# Average.
int	i			# Generic.

errchk	malloc, mfree, tbrptd

begin
	# Average the results.
	call malloc (a, ZT_K_NKEY(k), TY_DOUBLE)
	do i = 1, ZT_K_NKEY(k) {
	    if (ZT_K_NA(k,i) != 0)
		A(i) = ZT_K_A(k,i) / ZT_K_NA(k,i)
	    else
		A(i) = INDEFI
	}
	
	ZT_OT_NROWS(ot) = ZT_OT_NROWS(ot) + 1
	call tbrptd (ZT_OT(ot), ZT_OT_COL(ot,1), A(1),
		     ZT_OT_NCOLS(ot), ZT_OT_NROWS(ot))

	# That's all folks.
	call mfree (a, TY_DOUBLE)
end
#---------------------------------------------------------------------------
# End of zt_ot_wtab
#---------------------------------------------------------------------------
