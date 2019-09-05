include	<tbset.h>
include	<math.h>

define	NUM_COLS	12
define	NUM_IN_COLS	5
define	OUT_COLS_SUB	4
define	OUT_COLS_ALL	11
define	SZ_PLTID	4
define	HRSTODEG	($1*15)		# Convert hours (RA) to degrees

procedure t_extgst ()

#  EXTGST -- Extract guide star records from a list of guide star tables
#  into a single output table.  Select the records based on an input range
#  of coordinates and magnitude.

#  5/6/92  Add allcols option to copy all table columns for selected rows.  ZGL
#  6/2/92  Make sure output table contains all columns.  ZGL

pointer	sp, tblist, itname, otname
pointer	tblp				# Table file name template pointer
pointer	itp				# Input table pointer
pointer	otp				# Output table pointer
pointer	otcp[NUM_COLS]			# Output column descriptors
char	colnames[SZ_COLNAME,NUM_COLS]	# Column names
int	inrows, outrow			# Number of rows in table
pointer	colvals[NUM_IN_COLS]		# Column data
double	ra1, ra2			# R. A. limits
double	dec1, dec2			# Declination limits
real	mag1, mag2			# Magnitude limits
string	itlparm	"intablist"		# Input table list parameter name
string	otbparm	"outable"		# Output table list parameter name
int	old
bool	onestar				# Select stars from one plate only
char	plateid[SZ_PLTID]		# Select stars from this plate
bool	allcols				# Copy all columns?

pointer	fntopn(), tbtopn()
int	fntgfn()
bool	clgetb()

begin
	call smark (sp)
	call salloc (tblist, SZ_LINE, TY_CHAR)

	# Find list of table names
	call clgstr (itlparm, Memc[tblist], SZ_LINE)
	call salloc (itname, SZ_FNAME, TY_CHAR)

	# Output table (extracted stars)
	call salloc (otname, SZ_FNAME, TY_CHAR)
	call clgstr (otbparm, Memc[otname], SZ_LINE)
	otp = tbtopn (Memc[otname], NEW_FILE, 0)
	call tbtcre (otp)

	# Copy all table columns?
	allcols = clgetb ("allcols")

	# Define table columns
	if (allcols)
	    call otadefc (otp, colnames, otcp)
	else
	    call otdefc (otp, colnames, otcp)

	# Open input table file name teplate
	tblp = fntopn (Memc[tblist])

	inrows = 0
	outrow = 0

	# Find field limits
	call fldlim (ra1, ra2, dec1, dec2, mag1, mag2)
	call printf ("%00.0h %00.0h %00.0h %00.0h  %6.1f %6.1f \n")
	    call pargd (ra1/15.0)
	    call pargd (ra2/15.0)
	    call pargd (dec1)
	    call pargd (dec2)
	    call pargr (mag1)
	    call pargr (mag2)

	# Use stars from one plate only?
	onestar = clgetb ("onestar")
	
	if (onestar)
	    # Select stars from this plate only
	    call clgstr ("plateid", plateid, SZ_PLTID)

	else
	    plateid[1] = EOS

	while (fntgfn (tblp, Memc[itname], SZ_FNAME) != EOF) {
	    # For each input table in the list
#	    call printf ("Reading table:  %s\n")
#		call pargstr (Memc[itname])

	    itp = tbtopn (Memc[itname], READ_ONLY, 0)

	    # Read columns from input table
	    call gintab (itp, Memc[itname], colnames[1,2], colvals, inrows,
		onestar, plateid)

	    old = outrow
	    # Copy rows from input table within field to output table
	    call gstrcp (Memd[colvals[1]], Memd[colvals[2]],
		Memr[colvals[3]], Memi[colvals[4]], Memc[colvals[5]],
		inrows, 
		ra1, ra2, dec1, dec2, mag1, mag2,
		otp, otcp, outrow, onestar, plateid,
		itp, allcols, colnames)
 
	    call printf ("%6d stars selected from region %s\n")
		call pargi (outrow - old)
		call pargstr (Memc[itname])

	    # Close the input table
	    call tbtclo (itp)
	}
 
	call printf ("%6d stars written to table:  %s\n")
	    call pargi (outrow)
	    call pargstr (Memc[otname])

	call clputi ("nstars", outrow)

	call mfree (colvals[1], TY_DOUBLE)
	call mfree (colvals[2], TY_DOUBLE)
	call mfree (colvals[3], TY_REAL)
	call mfree (colvals[4], TY_INT)
	call mfree (colvals[5], TY_CHAR)

	call tbtclo (otp)
	call fntcls (tblp)
	call sfree (sp)
end


procedure otdefc (otp, colname, otcp)

# OTDEFC -- Define the input and output table columns and allocate the
# output table columns.

pointer	otp				# Output table pointer
pointer	otcp[NUM_COLS]			# Output table column descriptors
char	colname[SZ_COLNAME,NUM_COLS]	# Column names

char	colunits[SZ_COLUNITS,NUM_COLS]	# Column units
char	colfmt[SZ_COLFMT,NUM_COLS]	# Column formats
int	cdtype[NUM_COLS]		# Column data types
int	lendata[NUM_COLS]		# Character sizes

begin
	# Column names (input and output)
	call strcpy ("RA_HRS",   colname[1,1], SZ_COLNAME)	# O
	call strcpy ("RA_DEG",   colname[1,2], SZ_COLNAME)	# I/O
	call strcpy ("DEC_DEG",  colname[1,3], SZ_COLNAME)	# I/O
	call strcpy ("MAG",      colname[1,4], SZ_COLNAME)	# I/O
	call strcpy ("GSC_ID",   colname[1,5], SZ_COLNAME)	# I
	call strcpy ("PLATE_ID", colname[1,6], SZ_COLNAME)	# I

	# Output column units
	call strcpy ("hours",      colunits[1,1], SZ_COLUNITS)
	call strcpy ("degrees",    colunits[1,2], SZ_COLUNITS)
	call strcpy ("degrees",    colunits[1,3], SZ_COLUNITS)
	call strcpy ("magnitudes", colunits[1,4], SZ_COLUNITS)

	# Output column formats
	call strcpy ("%11.2h", colfmt[1,1], SZ_COLUNITS)
	call strcpy ("%11.1h", colfmt[1,2], SZ_COLUNITS)
	call strcpy ("%11.1h", colfmt[1,3], SZ_COLUNITS)
	call strcpy ("%6.1f",  colfmt[1,4], SZ_COLUNITS)

	# Column data types
	cdtype[1] = TY_DOUBLE
	cdtype[2] = TY_DOUBLE
	cdtype[3] = TY_DOUBLE
	cdtype[4] = TY_REAL

	# Character column size (ignore)
	lendata[1]  = 0
	lendata[2]  = 0
	lendata[3]  = 0
	lendata[4]  = 0

	# Define the output table columns
	call tbcdef (otp, otcp, colname, colunits,
	    colfmt, cdtype, lendata, OUT_COLS_SUB)
end


procedure otadefc (otp, colname, otcp)

# OTDEFC -- Define the input and output table columns and allocate the
# output table columns.

pointer	otp				# Output table pointer
pointer	otcp[NUM_COLS]			# Output table column descriptors
char	colname[SZ_COLNAME,NUM_COLS]	# Column names

char	colunits[SZ_COLUNITS,NUM_COLS]	# Column units
char	colfmt[SZ_COLFMT,NUM_COLS]	# Column formats
int	cdtype[NUM_COLS]		# Column data types
int	lendata[NUM_COLS]		# Character sizes

begin
	# Column names (input and output)
	call strcpy ("RA_HRS",   colname[1,1],  SZ_COLNAME)	# O
	call strcpy ("RA_DEG",   colname[1,2],  SZ_COLNAME)	# I/O
	call strcpy ("DEC_DEG",  colname[1,3],  SZ_COLNAME)	# I/O
	call strcpy ("MAG",      colname[1,4],  SZ_COLNAME)	# I/O
	call strcpy ("GSC_ID",   colname[1,5],  SZ_COLNAME)	# I/O
	call strcpy ("PLATE_ID", colname[1,6],  SZ_COLNAME)	# I/O
	call strcpy ("POS_ERR",  colname[1,7],  SZ_COLNAME)	# O
	call strcpy ("MAG_ERR",  colname[1,8],  SZ_COLNAME)	# O
	call strcpy ("MAG_BAND", colname[1,9],  SZ_COLNAME)	# O
	call strcpy ("CLASS",    colname[1,10], SZ_COLNAME)	# O
	call strcpy ("MULTIPLE", colname[1,11], SZ_COLNAME)	# O

	# Output column units
	call strcpy ("hours",      colunits[1,1],  SZ_COLUNITS)
	call strcpy ("degrees",    colunits[1,2],  SZ_COLUNITS)
	call strcpy ("degrees",    colunits[1,3],  SZ_COLUNITS)
	call strcpy ("magnitudes", colunits[1,4],  SZ_COLUNITS)
	call strcpy ("",           colunits[1,5],  SZ_COLUNITS)
	call strcpy ("",           colunits[1,6],  SZ_COLUNITS)
	call strcpy ("degrees",    colunits[1,7],  SZ_COLUNITS)
	call strcpy ("magnitudes", colunits[1,8],  SZ_COLUNITS)
	call strcpy ("",           colunits[1,9],  SZ_COLUNITS)
	call strcpy ("",           colunits[1,10], SZ_COLUNITS)
	call strcpy ("",           colunits[1,11], SZ_COLUNITS)

	# Output column formats
	call strcpy ("%11.2h", colfmt[1,1],  SZ_COLUNITS)
	call strcpy ("%11.1h", colfmt[1,2],  SZ_COLUNITS)
	call strcpy ("%11.1h", colfmt[1,3],  SZ_COLUNITS)
	call strcpy ("%6.1f",  colfmt[1,4],  SZ_COLUNITS)
	call strcpy ("%5d",    colfmt[1,5],  SZ_COLUNITS)
	call strcpy ("%-4s",   colfmt[1,6],  SZ_COLUNITS)
	call strcpy ("%5.1f",  colfmt[1,7],  SZ_COLUNITS)
	call strcpy ("%4.2f",  colfmt[1,8],  SZ_COLUNITS)
	call strcpy ("%2d",    colfmt[1,9],  SZ_COLUNITS)
	call strcpy ("%1d",    colfmt[1,10], SZ_COLUNITS)
	call strcpy ("%-1s",   colfmt[1,11], SZ_COLUNITS)

	# Column data types
	cdtype[1]  = TY_DOUBLE
	cdtype[2]  = TY_DOUBLE
	cdtype[3]  = TY_DOUBLE
	cdtype[4]  = TY_REAL
	cdtype[5]  = TY_INT
	cdtype[6]  = -4
	cdtype[7]  = TY_REAL
	cdtype[8]  = TY_REAL
	cdtype[9]  = TY_INT
	cdtype[10] = TY_INT
	cdtype[11] = -1

	# Character column size (ignored)
	lendata[1]  = 0
	lendata[2]  = 0
	lendata[3]  = 0
	lendata[4]  = 0
	lendata[5]  = 0
	lendata[6]  = 0
	lendata[7]  = 0
	lendata[8]  = 0
	lendata[9]  = 0
	lendata[10] = 0
	lendata[11] = 0

	# Define the output table columns
	call tbcdef (otp, otcp, colname, colunits,
	    colfmt, cdtype, lendata, OUT_COLS_ALL)
end


procedure fldlim (ra1, ra2, dec1, dec2, mag1, mag2) 

#  FLDLIM -- Find the coordinates of the corners of a field from the
#  plate center and size.

double	ra1, ra2		# R. A. limits
double	dec1, dec2		# Declination limits
real	mag1, mag2		# Magnitude limits

double	ra, dec			# Coordinates of field center in degrees
real	width			# Width of field in degrees
real	mag
double	cosdec

double	clgetd()
real	clgetr()

begin
	# Coordinate limits in degrees (as in tables)
	ra    = HRSTODEG (clgetd ("ra"))
	dec   = clgetd ("dec")
	width = clgetr ("width")

	# Magnitude limits
	mag1 = clgetr ("mag1")
	mag2 = clgetr ("mag2")
	if (!IS_INDEF(mag1) && !IS_INDEF(mag2) && mag1 > mag2) {
	    # Make sure the order is correct
	    mag  = mag1
	    mag1 = mag2
	    mag2 = mag
	}

	# Find field corners
	dec1 = dec - width / 2.0
	if (dec1 <= -90.0) {
	    # South pole in field
	    dec1 = -90.0
	    dec2 = dec + width / 2.0
	    ra1  = 0.0
	    ra2  = 360.0
	    return
	}

	dec2 = dec + width / 2.0
	if (dec2 >= +90.0) {
	    # North pole in field
	    dec2 = +90.0
	    dec1 = dec - width / 2.0
	    ra1  = 0.0
	    ra2  = 360.0
	    return
	}

	if (dec > 0.0)
	    # North
	    cosdec = cos (DEGTORAD (dec2))
	else
	    # South
	    cosdec = cos (DEGTORAD (dec1))

	ra1 = ra - (0.5 * width / cosdec)

	if (ra1 < 0)
	    ra1 = ra1 + 360.0

	ra2 = ra + (0.5 * width / cosdec)
	if (ra2 > 360.0)
	    ra2 = ra2 - 360.0
end


procedure gintab (itp, itname, colname, colvals, buffsize, onestar, plateid)

# GINTAB -- Open the input table, find the table columns, and read the
# columns.

pointer	itp					# Input table pointer
char	itname[ARB]				# Input table name
char	colname[SZ_COLNAME,NUM_COLS]		# Column names
pointer	colvals[NUM_IN_COLS]			# Column value pointers
int	buffsize				# Column size
bool	onestar					# Select stars from one plate?
char	plateid[SZ_PLTID]			# Select stars from this plate

pointer	sp, errmsg
pointer	itcp[NUM_COLS]				# Column descriptors
int	col
int	inrows
pointer	nullval

int	tbpsta()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Find the input table columns
	call tbcfnd (itp, colname, itcp, NUM_IN_COLS)

	do col = 1, NUM_IN_COLS
	    if (itcp[col] <= 0) {
		call sprintf (Memc[errmsg], SZ_LINE,
		    "Could not find column: %s in table: %s")
		    call pargstr (colname[1,col])
		    call pargstr (itname)
		call error (0, Memc[errmsg])
	    }

	# Number of rows in table
	inrows = tbpsta (itp, TBL_NROWS)

	if (buffsize == 0) {
	    # Allocate column buffers for the first time
	    call malloc (colvals[1], inrows, TY_DOUBLE)	#RA_DEG
	    call malloc (colvals[2], inrows, TY_DOUBLE)	#DEC_DEG
	    call malloc (colvals[3], inrows, TY_REAL)	#MAG
	    # Allocate the GSC ID column
	    call malloc (colvals[4], inrows, TY_INT)
	    # Allocate the PLATE ID column
	    call malloc (colvals[5], (SZ_PLTID+1)*inrows, TY_CHAR)

	} else if (buffsize < inrows) {
	    # Resize the column buffers
	    call realloc (colvals[1], inrows, TY_DOUBLE)
	    call realloc (colvals[2], inrows, TY_DOUBLE)
	    call realloc (colvals[3], inrows, TY_REAL)
	    # Allocate the GSC ID column
	    call realloc (colvals[4], inrows, TY_INT)
	    # Allocate the PLATE ID column
	    call realloc (colvals[5], (SZ_PLTID+1)*inrows, TY_CHAR)
	}

	# Null flag buffer
	call malloc (nullval, inrows, TY_BOOL)

	buffsize = inrows

	# Get the column values
	# Right ascension (degrees)
	call tbcgtd (itp, itcp[1], Memd[colvals[1]],
	    Memb[nullval], 1, buffsize)

	# Declination
	call tbcgtd (itp, itcp[2], Memd[colvals[2]],
	    Memb[nullval], 1, buffsize)

	# Magnitude
	call tbcgtr (itp, itcp[3], Memr[colvals[3]],
	    Memb[nullval], 1, buffsize)

	# GSC ID
	call tbcgti (itp, itcp[4], Memi[colvals[4]],
	    Memb[nullval], 1, buffsize)

	# Plate ID
	call tbcgtt (itp, itcp[5], Memc[colvals[5]],
	    Memb[nullval], SZ_PLTID, 1, buffsize)

	call sfree (sp)
	call mfree (nullval, TY_BOOL)
end


procedure gstrcp (ra, dec, mag, gscid, pltid, inrows, 
	ra1, ra2, dec1, dec2, mag1, mag2, 
	otp, otcp, outrow, onestar, plateid,
	itp, allcols, colnames)

# Copy rows from input table within field to output table

double	ra[ARB]				# Right ascension values
double	dec[ARB]			# Declination values
real	mag[ARB]			# Magnitude values
int	gscid[ARB]			# GSC ID
char	pltid[SZ_PLTID,ARB]		# Plate ID
int	inrows				# Number of input rows
double	ra1, ra2			# R. A. limits
double	dec1, dec2			# Declination limits
real	mag1, mag2			# Magnitude limits
pointer	otp				# Output table descriptor
pointer	otcp[NUM_COLS]			# Output column descriptors
int	outrow				# Last output table row written
bool	onestar				# Select stars from one plate only
char	plateid[SZ_PLTID]		# Select stars from this plate only
pointer	itp				# Input table descriptor
bool	allcols				# Copy all columns?
char	colnames[SZ_COLNAME,NUM_COLS]	# Column names

int	row
int	curid
pointer	itcp[7]				# Input column pointers

double	coord[2]
bool	strne()

begin
	curid = 0

	if (allcols) {
	    # Find the input table columns
	    call tbcfnd (itp, colnames[1,5], itcp, 7)
	}

	do row = 1, inrows {
#	    call printf ("%4s %5d %5d %00.0h %00.0h %6.1f\n")
#		call pargstr (pltid[1,row])
#		call pargi (gscid[row])
#		call pargi (row)
#		call pargd (ra[row]/15.0)
#		call pargd (dec[row])
#		call pargr (mag[row])

	    if (onestar) {
		# Use stars from one plate only
		if (plateid[1] == EOS)
		    # First copy of this star only
		    if (curid == gscid[row])
			next
		    else
			curid = gscid[row]

		else if (strne (plateid, pltid[1,row]))
		    # Not the specified plate
		    next
	    }

	    # Check the R.A. range
	    if (ra1 < ra2) {
		# 0 R.A. not in the region
		if (ra[row] < ra1 || ra[row] > ra2)
		    # Outside R.A. limits
		    next
	    } else {
		# 0 R.A. not in the region
		if (ra[row] < ra1 && ra[row] > ra2)
		    # Outside R.A. limits
		    next
	    }

	    if (dec1 > dec[row] || dec2 < dec[row])
		# Outside dec limits
		next

	    if (!IS_INDEF(mag1) || !IS_INDEF(mag2)) {
		# Mag limits
		if (mag1 > mag[row] || mag2 < mag[row])
		    # Outside mag limits
		    next
	    }
	
	    # The object is in the selected field and mag range
	    outrow = outrow + 1

	    # Write the output table row
	    coord[1] = ra[row]
	    coord[2] = dec[row]
	    call tbrptd (otp, otcp[1], ra[row]/15.0, 1, outrow)
	    call tbrptd (otp, otcp[2], coord, 2, outrow)
	    call tbrptr (otp, otcp[4], mag[row], 1, outrow)

	    if (allcols) {
		# Copy the remaining columns
		call tbrcsc (itp, otp, itcp, otcp[5], row, outrow, 7)
	    }

	    call printf ("%4s %5d %5d %00.0h %00.0h %6.1f\n")
		call pargstr (pltid[1,row])
		call pargi (gscid[row])
		call pargi (row)
		call pargd (ra[row]/15.0)
		call pargd (dec[row])
		call pargr (mag[row])
	}
end
