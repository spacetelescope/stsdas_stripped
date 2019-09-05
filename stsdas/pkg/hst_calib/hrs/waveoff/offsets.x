include	<tbset.h>

# The offset table descriptor.
define	Table		Memi[cwoff]		# Table descriptor.
define	Col_id_ptr	Memi[cwoff+1]		# Column descriptors.
define	Col_id		Memi[Col_id_ptr+$1-1]
define	Last_row	Memi[cwoff+2]		# Current last row.
define	Replace		Memi[cwoff+3]		# YES to replace rows.
define	SZ_CWOFF	4

# Define what the columns are.
define	ColFmt		Memc[colfmt+(($1-1)*(SZ_COLFMT+1))]
define	ColLen		Memi[collen+$1-1]
define	ColName		Memc[colname+(($1-1)*(SZ_COLNAME+1))]
define	ColType		Memi[coltype+$1-1]
define	ColUnits	Memc[colunits+(($1-1)*(SZ_COLUNITS+1))]
define	NAME_COL	1
define	WOFF_COL	2
define	OFFSET_COL	3
define	SOFF_COL	4
define	VALUE_COL	5
define	N_COLUMNS	5

# General memory.
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help offsets Nov93 source
.ih
NAME
offsets -- Manage the offset table.
.endhelp
#---------------------------------------------------------------------------
pointer procedure wo_alloc_offsets (file)

char	file[ARB]		# I:  The table name to open.

pointer	colfmt			# Column formats.
pointer	collen			# Length of columns.
pointer	colname			# The column names.
pointer	coltype			# Column types.
pointer	colunits		# The units for the columns.
pointer	cwoff			# Offset table descriptor.
int	i			# Generic.
pointer	sp			# Stack pointer.
bool	streq()			# Strings equal?
pointer	sx			# Generic.
int	tbpsta()		# Get table information.
pointer	tbtopn()		# Open a table.

errchk	malloc, salloc, smark, tbtcre, tbtopn

begin
	# Allocate descriptor.
	call malloc (cwoff, SZ_CWOFF, TY_STRUCT)
	call malloc (Col_id_ptr, N_COLUMNS, TY_INT)

	# Allocate general memory.
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * N_COLUMNS, TY_CHAR)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Get column names from parameters.
	call strcpy ("file", ColName(NAME_COL), SZ_COLNAME)
	call strcpy ("poffset", ColName(OFFSET_COL), SZ_COLNAME)
	call strcpy ("correlation", ColName(VALUE_COL), SZ_COLNAME)
	call strcpy ("woffset", ColName(WOFF_COL), SZ_COLNAME)
	call strcpy ("soffset", ColName(SOFF_COL), SZ_COLNAME)
	
	# Open the table.
	ifnoerr (Table = tbtopn (file, READ_WRITE, NULL)) {

	    # Check for existance of columns.  If they don't exist, error.
	    call tbcfnd (Table, ColName(1), Col_id(1), N_COLUMNS)
	    do i = 1, N_COLUMNS
		if (Col_id(i) == NULL) {
		    call eprintf ("ERROR: Column %s doesn't exist in table %s\n")
		    call pargstr (ColName(i))
		    call pargstr (file)
		    call eprintf ("    For a table that already exists, the specified columns must be present.\n")
		    call error (1, "wo_alloc_offsets: Cannot use existing table")
		}

	    # Get last row.
	    Last_row = tbpsta (Table, TBL_NROWS)
	    
	} else {
	    Table = tbtopn (file, NEW_FILE, NULL)
	    call tbtcre (Table)

	    # Define the default column descriptors.
	    call salloc (colunits, (SZ_COLUNITS+1) * N_COLUMNS, TY_CHAR)
	    call strcpy ("", ColUnits(NAME_COL), SZ_COLUNITS)
	    call strcpy ("pixels", ColUnits(OFFSET_COL), SZ_COLUNITS)
	    call strcpy ("correlation[0-1]", ColUnits(VALUE_COL), SZ_COLUNITS)
	    call strcpy ("", ColUnits(WOFF_COL), SZ_LINE)
	    call strcpy ("sample", ColUnits(SOFF_COL), SZ_COLUNITS)

	    call salloc (colfmt, (SZ_COLFMT+1) * N_COLUMNS, TY_CHAR)
	    if (streq (file, "STDOUT" )) {
		call strcpy ("%-30c", ColFmt(NAME_COL), SZ_COLFMT)
		call strcpy ("%7.4g", ColFmt(OFFSET_COL), SZ_COLFMT)
		call strcpy ("%7.4g", ColFmt(VALUE_COL), SZ_COLFMT)
		call strcpy ("%7.4g", ColFmt(WOFF_COL), SZ_COLFMT)
		call strcpy ("%7.4g", ColFmt(SOFF_COL), SZ_COLFMT)
		call printf ("# %7c                      %7c %7c %7c %7c\n")
		do i = 1, N_COLUMNS
		    call pargstr (ColName(i))
	    } else {
		call strcpy ("", ColFmt(NAME_COL), SZ_COLFMT)
		call strcpy ("", ColFmt(OFFSET_COL), SZ_COLFMT)
		call strcpy ("", ColFmt(VALUE_COL), SZ_COLFMT)
		call strcpy ("", ColFmt(WOFF_COL), SZ_COLFMT)
		call strcpy ("", ColFmt(SOFF_COL), SZ_COLFMT)
	    }
	    
	    call salloc (coltype, N_COLUMNS, TY_INT)
	    ColType(NAME_COL) = -1 * SZ_PATHNAME
	    ColType(OFFSET_COL) = TY_DOUBLE
	    ColType(VALUE_COL) = TY_DOUBLE
	    ColType(WOFF_COL) = TY_DOUBLE
	    ColType(SOFF_COL) = TY_DOUBLE

	    call salloc (collen, N_COLUMNS, TY_INT)
	    do i = 1, N_COLUMNS
		ColLen(i) = 1

	    call tbcdef (Table, Col_id(1), ColName(1), ColUnits(1),
			 ColFmt(1), ColType(1), ColLen(1), N_COLUMNS)
	    do i = 1, N_COLUMNS
		if (Col_id(i) == NULL) {
		    call eprintf ("ERROR: Could not create column %s in new table %s\n")
		    call pargstr (ColName(i))
		    call pargstr (file)
		    call eprintf ("    Check disk quota, file protections, existance of file, or anything\n")
		    call eprintf ("    else that would prevent a new file from being created.\n")
		    call error (1, "wo_alloc_offsets: could not open table")
		}

	    Last_row = 0
	}

	# Setup replacement.
	Replace = NO

	# That's all folks.
	call sfree (sp)
	return (cwoff)
end
#---------------------------------------------------------------------------
# end of wo_alloc_offsets
#---------------------------------------------------------------------------
procedure wo_free_offsets (cwoff)

pointer	cwoff			# IO: The offset descriptor, NULL on return.

begin
	call tbtclo (Table)
	call mfree (Col_id_ptr, TY_INT)
	call mfree (cwoff, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wo_free_offsets
#---------------------------------------------------------------------------
procedure wo_add_offsets (cwoff, file, offset, value, woffset, soffset)

pointer	cwoff			# I:  The offset descriptor.
char	file[ARB]		# I:  File name.
double	offset			# I:  The offset.
double	value			# I:  Correlation value.
double	woffset			# I:  Offset in wavelength.
double	soffset			# I:  Offset in samples.

int	r, row			# Row to modify.
pointer	sp			# Stack pointer.
bool	streq()			# Strings equal?
pointer	sx			# Generic string pointer.

begin
	call smark (sp)
	call salloc (sx, max(SZ_LINE, SZ_PATHNAME), TY_CHAR)
	row = 0
	
	# If replacing, look for a row with a matching file name.  The
	# first match will be replaced.
	if (Replace == YES) {
	    do r = 1, Last_row {
		call tbegtt (Table, Col_id(NAME_COL), r, Sx, SZ_PATHNAME)
		if (streq (Sx, file)) {
		    row = r
		    break
		}
	    }
	}

	# If a row has not been found, just append to the table.
	if (row == 0)
	    row = Last_row +1

	# Add the information to the table.
	call tbeptt (Table, Col_id(NAME_COL), row, file)
	call tbeptd (Table, Col_id(OFFSET_COL), row, offset)
	call tbeptd (Table, Col_id(VALUE_COL), row, value)
	call tbeptd (Table, Col_id(WOFF_COL), row, woffset)
	call tbeptd (Table, Col_id(SOFF_COL), row, soffset)

	# That's all folks.
	if (row > Last_row)
	    Last_row = row
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wo_add_offsets
#---------------------------------------------------------------------------
