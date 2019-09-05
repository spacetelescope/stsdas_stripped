procedure chart_stars (star_str, catalog, colnames, minrow, maxrow,
	faint, bright, ramin, decmin, ramax, decmax)

#  23 August 1991 fixed uninitialized array index (col --> RA_COL). ZGL

include <tbset.h>
include	"skymap.h"

pointer	star_str			# Catalog data structure pointer
char	catalog[ARB]			# Catalog table name
char	colnames[SZ_COLNAME,NUM_COLS]	# Column names
int	minrow, maxrow			# Range of table rows
real	faint, bright			# Magnitude limits
double	ramin, decmin, ramax, decmax	# Coordinate limits

pointer	ctp				# Catalog table pointer
pointer	colp[NUM_COLS]			# Column pointers
pointer	sp, errmsg
pointer	colunits[NUM_COLS]		# Column units
pointer	isnull
int	numrows, newnum
int	namsiz

pointer	tbtopn()
int	tbpsta(), select_stars(), tbcigi()

begin
	if (catalog[1] == EOS) {
	    # No catalog specified
	    NUM_CAT_VALS(star_str) = 0
	    return
	}

	# Open the catalog table
	ctp = tbtopn (catalog, READ_ONLY, 0)

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Find the table columns
	call tbcfnd (ctp, colnames, colp, NUM_COLS)

	numrows = tbpsta (ctp, TBL_NROWS)

	if (IS_INDEFI(minrow))
	    minrow = 1
	if (IS_INDEFI(maxrow))
	    maxrow = numrows

	numrows = maxrow - minrow + 1

	# Allocate the null flags buffer
	call malloc (isnull, numrows*NUM_COLS, TY_BOOL)

	if (colp[RA_COL] <= 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, 
		"R.A. column (%s) not found in catalog table (%s).")
		call pargstr (colnames[1,RA_COL])
		call pargstr (catalog)
	    call error (0, Memc[errmsg])
	}

	# Find R.A. column units
	call salloc (colunits[RA_COL], SZ_COLUNITS, TY_CHAR)
	call tbcigt (colp[RA_COL], TBL_COL_UNITS, 
	    Memc[colunits[RA_COL]], SZ_COLUNITS)
	
	call malloc (CAT_RA_P(star_str), numrows, TY_DOUBLE)

	call tbcgtd (ctp, colp[RA_COL], 
	    CAT_RA_VAL(star_str), NULL_COL(isnull,RA_COL,numrows), 
	    minrow, maxrow)

	if (colp[DEC_COL] <= 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, 
		"R.A. column (%s) not found in catalog table (%s).")
		call pargstr (colnames[1,DEC_COL])
		call pargstr (catalog)
	    call error (0, Memc[errmsg])
	}

	call malloc (CAT_DEC_P(star_str), numrows, TY_DOUBLE)

	call tbcgtd (ctp, colp[DEC_COL], 
	    CAT_DEC_VAL(star_str), NULL_COL(isnull,DEC_COL,numrows), 
	    minrow, maxrow)

	# Find Dec. column units
	call salloc (colunits[DEC_COL], SZ_COLUNITS, TY_CHAR)
	call tbcigt (colp[DEC_COL], TBL_COL_UNITS, 
	    Memc[colunits[DEC_COL]], SZ_COLUNITS)

	# Magnitudes
	if (colp[MAG_COL] <= 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, 
		"Magnitudes column (%s) not found in catalog table (%s).")
		call pargstr (colnames[1,MAG_COL])
		call pargstr (catalog)
	    call error (0, Memc[errmsg])
	}

	call malloc (CAT_MAG_P(star_str), numrows, TY_REAL)

	call tbcgtr (ctp, colp[MAG_COL], 
	    CAT_MAG_VAL(star_str), NULL_COL(isnull,MAG_COL,numrows), 
	    minrow, maxrow)

	# Object names
	if (colnames[1,NAM_COL] == EOS) {
	    # No object name column specified
	    NAME_COL_EX(star_str) = NO
	    namsiz = 0
	} else if (colp[NAM_COL] <= 0) {
	    # Specified object name column not found in catalog
	    NAME_COL_EX(star_str) = NO
	    namsiz = 0
	} else {
	    # Size of object name column
	    namsiz = tbcigi (colp[NAM_COL], TBL_COL_DATATYPE)
	    if (namsiz < 0)
		# Character column;  use allocated table column size
		namsiz = -namsiz
	    else
		# Numeric column;  use formatted size plus a bit
		namsiz = tbcigi (colp[NAM_COL], TBL_COL_FMTLEN) + 4

	    NAME_COL_EX(star_str) = YES
	}

	CAT_NAME_SIZE(star_str) = namsiz
	call malloc (CAT_NAM_P(star_str), (namsiz+1)*numrows, TY_CHAR)

	if (namsiz > 0)
	    call tbcgtt (ctp, colp[NAM_COL], 
		CAT_NAME(star_str), NULL_COL(isnull,NAM_COL,numrows), 
		namsiz, minrow, maxrow)
	else
	    call amovkc (EOS, CAT_NAME(star_str), numrows)

	# Object class 
	call malloc (CAT_CLS_P(star_str), numrows, TY_INT)

	if (colnames[1,CLS_COL] == EOS) {
	    # No column name specified
	    CLASS_COL_EX(star_str) = NO
	    call amovki (0, CAT_CLASS(star_str), numrows)

	} else if (colp[CLS_COL] <= 0) {
	    # Column not found in catalog
	    CLASS_COL_EX(star_str) = NO
	    call amovki (0, CAT_CLASS(star_str), numrows)

	} else {
	    CLASS_COL_EX(star_str) = YES
	    call tbcgti (ctp, colp[CLS_COL], 
		CAT_CLASS(star_str), NULL_COL(isnull,CLS_COL,numrows), 
		minrow, maxrow)
	}

	# Allocate the catalog row number column buffer
	call malloc (CAT_ROW_P(star_str),   numrows, TY_INT)
	call malloc (VALID_OBJ_P(star_str), numrows, TY_BOOL)

	# Make sure coordinates are in radians
	call coord_units (CAT_RA_VAL(star_str),  numrows,
	    Memc[colunits[RA_COL]])
	call coord_units (CAT_DEC_VAL(star_str), numrows,
	    Memc[colunits[DEC_COL]])

	# Select only rows with all valid columns in range
	newnum = select_stars (CAT_TBL_ROW(star_str), 
	    CAT_RA_VAL(star_str), CAT_DEC_VAL(star_str), 
	    CAT_MAG_VAL(star_str), CAT_CLASS(star_str),
	    CAT_NAME(star_str), namsiz, Memb[isnull], numrows, 
	    faint, bright, ramin, ramax, decmin, decmax)

	if (newnum > 0) {
	    numrows = newnum
	    call realloc (CAT_ROW_P(star_str), numrows, TY_INT)
	    call realloc (CAT_RA_P(star_str),  numrows, TY_DOUBLE)
	    call realloc (CAT_DEC_P(star_str), numrows, TY_DOUBLE)
	    call realloc (CAT_MAG_P(star_str), numrows, TY_REAL)
	    call realloc (CAT_CLS_P(star_str), numrows, TY_INT)
	    call realloc (CAT_NAM_P(star_str), (namsiz+1)*numrows, TY_CHAR)
	    NUM_CAT_VALS(star_str) = numrows
	} else
	    call error (0, "No stars selected from catalog")

	call mfree (isnull, TY_BOOL)
	call sfree (sp)
end
