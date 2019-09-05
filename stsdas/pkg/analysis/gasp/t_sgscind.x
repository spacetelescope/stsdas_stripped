include <tbset.h>
include <math.h>

## 1/28/93  Change GSC pathname to lower case.
## 5/11/93  Fix bug causing regions just below R.A. 0h (23h:mm:ss) to
#  be ignored.  The index table has the upper limit as 0 rather than
#  24 and the test was assuing 24.  Curiously, the test was there in
#  the case of regions straddling 0h, but not with the upper limit
#  equal to 0h.


define  NUM_COLS        13              # Number of table columns
define  DIR_SLEN        5               # Size of directory name
define  NUM_DIR         24              # Number of dec zone dis
define  DEC_ZONES       (NUM_DIR/2)     # Number of dec zones
define  ZONE_SIZE       (90.0/DEC_ZONES)# Width of dec zones
define  HRSTODEG        ($1*15)         # Convert hours (RA) to degrees
define  NORTH           1
define  SOUTH           -1
define  LAST_NORTH      5259


procedure t_sgscind ()

#  SGSCIND -- Search the Guide Star Catalog index table for fields
#  in the specified range of coordinates and magnitudes.  Build a
#  list containing the pathnames of the files on the CD-ROM.

pointer sp, indtab, tablist, cdrom_n, cdrom_s
pointer tp                      # Index table pointer
pointer cdv[NUM_COLS]           # Column descriptors
int     tbl                     # Output FITS table list descriptor
double  ra, dec                 # Coordinates of field center
real    width                   # Width of field in degrees
double  ra1, ra2, dec1, dec2    # Coordinate limits
int     nrgn                    # Number of regions found

int     open(), prtrgn()
pointer tbtopn()
double  clgetd()
real    clgetr()

begin
	call smark (sp)

	call salloc (indtab, SZ_FNAME, TY_CHAR)
	call salloc (cdrom_n, SZ_FNAME, TY_CHAR)
	call salloc (cdrom_s, SZ_FNAME, TY_CHAR)
	call clgstr ("index", Memc[indtab], SZ_FNAME)
	call clgstr ("cdrom_north", Memc[cdrom_n], SZ_FNAME)
	call clgstr ("cdrom_south", Memc[cdrom_s], SZ_FNAME)

	# Open the index table
	tp = tbtopn (Memc[indtab], READ_ONLY, 0)

	# Column descriptors
	call gcolds (tp, cdv)

	# Find the user's field
	ra    = HRSTODEG (clgetd ("ra"))
	dec   = clgetd ("dec")
	width = clgetr ("width")
	call crdlim (ra, dec, width, ra1, ra2, dec1, dec2)

	# Table list file
	call salloc (tablist, SZ_FNAME, TY_CHAR)
	call clgstr ("tablist", Memc[tablist], SZ_FNAME)
	tbl = open (Memc[tablist], NEW_FILE, TEXT_FILE)

#       ndr = clgeti ("north")

	# Find the G.S. region path and file names
	nrgn = prtrgn (tp, cdv, ra1, ra2, dec1, dec2, tbl,
		Memc[cdrom_n], Memc[cdrom_s])

	call clputi ("nregions", nrgn)

	call close (tbl)
	call sfree (sp)
end


procedure gcolds (tp, cdv)

#  GCOLDS -- Find the columns in the index table

pointer tp                      # Index table descriptor
pointer cdv[NUM_COLS]           # Column pointers

pointer sp, cnstr, errmsg
int     col
char    colpar[8,NUM_COLS]

begin
	call smark (sp)
	call salloc (cnstr, SZ_COLNAME+1, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call strcpy ("region",   colpar[1,1],  8)
	call strcpy ("rahlow",   colpar[1,2],  8)
	call strcpy ("ramlow",   colpar[1,3],  8)
	call strcpy ("raslow",   colpar[1,4],  8)
	call strcpy ("rahhi",    colpar[1,5],  8)
	call strcpy ("ramhi",    colpar[1,6],  8)
	call strcpy ("rashi",    colpar[1,7],  8)
	call strcpy ("decsilow", colpar[1,8],  8)
	call strcpy ("decdlow",  colpar[1,9],  8)
	call strcpy ("decmlow",  colpar[1,10], 8)
	call strcpy ("decsihi",  colpar[1,11], 8)
	call strcpy ("decdhi",   colpar[1,12], 8)
	call strcpy ("decmhi",   colpar[1,13], 8)

	do col = 1, NUM_COLS {
	    # For each defined column
	    # Get the column name
	    call clgstr (colpar[1,col], Memc[cnstr], SZ_COLNAME)
	    call tbcfnd (tp, Memc[cnstr], cdv[col], 1)

#           call printf ("%4d  %s\n")
#               call pargi (col)
#               call pargstr (Memc[cnstr])

	    if (cdv[col] <= 0) {
		call sprintf (Memc[errmsg], SZ_LINE,
		    "Could not find column %s")
		    call pargstr (Memc[cnstr])
		call error (0, Memc[errmsg])
	    }
	}

	call sfree (sp)
end


procedure crdlim (ra, dec, width, ra1, ra2, dec1, dec2)

#  CRDLIM -- find the coordinates of the corners of a field from the
#  plate center and size.

double  ra, dec                 # Coordinates of region center
real    width                   # Size of region in degrees
double  ra1, ra2, dec1, dec2    # Coordinates of region corners
double  cosdec

begin
	dec1 = dec - width / 2.0
	if (dec1 <= -90.0) {
	    # South pole
	    dec1 = -90.0
	    dec2 = dec + width / 2.0
	    ra1  = 0.0
	    ra2  = 360.0
	    return
	}

	dec2 = dec + width / 2.0
	if (dec2 >= +90.0) {
	    # North pole
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

	call eprintf ("%00.0h %00.0h %00.0h --> \
%00.0h %00.0h %00.0h %00.0h\n")
	    call pargd (ra/15)
	    call pargd (dec)
	    call pargr (width)
	    call pargd (ra1/15)
	    call pargd (ra2/15)
	    call pargd (dec1)
	    call pargd (dec2)
end


int procedure prtrgn (tp, cdv, ra1, ra2, dec1, dec2, tbl,
		cdrom_n, cdrom_s)

#  PRTRGN -- Search the index table to find the region identifiers
#  whose coordinate limits overlap the specified field.  Writes to
#  STDOUT commands for the cdrom command to extract the regions files.

pointer tp                      # Index table descriptor
pointer cdv[NUM_COLS]           # Column descriptors
double  ra1, ra2                # Right ascension limits in hours
double  dec1, dec2              # Declination limits in degrees
int     tbl                     # Table file handle
char    cdrom_n[SZ_FNAME]       # Mount point of North CDROM
char    cdrom_s[SZ_FNAME]       # Mount point of South CDROM
#int    ndr                     # Drive number for N. hemisphere CD

int     numrows                 # Number of rows in the table
int     row
double  ralow, rahi
double  declow, dechi
int     regnum
bool    null
int     zone
pointer sp, root, path
char    zdir[DIR_SLEN,NUM_DIR]
int     nrgn
int     hemsph

int     tbpsta(), fzone()
double  rdira(), rdidec()

begin
	call smark (sp)
	call salloc (root, SZ_LINE, TY_CHAR)
	call salloc (path, SZ_LINE, TY_CHAR)

	# Initialize the directory name for each zone
	call initzd (zdir)

#       if (ndr == 0) {
#           call strcat ("disk0", north, 5)
#           call strcat ("disk1", south, 5)
#       } else {
#           call strcat ("disk1", north, 5)
#           call strcat ("disk0", south, 5)
#       }

	# Number of rows in the index table
	numrows = tbpsta (tp, TBL_NROWS)

	hemsph = 0
	nrgn   = 0

	do row = 1, numrows {
	    # For each row in the index table

	    # Declination range of the GS region
	    # Note:  southern dechi and declow are reversed
	    dechi = rdidec (tp, row, cdv[11], cdv[12], cdv[13])
	    if (dechi > 0 && dechi < dec1)
		# North
		next
	    else if (dechi < 0 && dechi > dec2)
		# South
		next

	    # Limit of GS region closer to equator
	    declow = rdidec (tp, row, cdv[8],  cdv[9],  cdv[10])

	    if (declow > 0) {
		# North
		if (declow > dec2)
		    next
	    } else if (declow < 0) {
		# South
		if (declow < dec1)
		    next
	    } else {
		# Lower limit of GS region ON equator
		if (dechi > 0) {
		    # North
		    if (dechi < dec1 || declow > dec2)
			next
		} else if (dechi < 0) {
		    # South
		    if (dechi > dec2 || declow < dec1)
			next
		}
	    }

	    # Right ascension range of the GS region
	    if (ra1 < ra2) {
		# 0 R.A. not in region

		ralow = rdira (tp, row, cdv[2], cdv[3], cdv[4])
		if (ralow > ra2)
		    next

		rahi  = rdira (tp, row, cdv[5], cdv[6], cdv[7])

		if (ralow > rahi)
		    rahi = rahi + 360.0

		if (rahi < ra1)
		    next

	    } else {
		# 0 R.A. in region
		ralow = rdira (tp, row, cdv[2], cdv[3], cdv[4])
		rahi  = rdira (tp, row, cdv[5], cdv[6], cdv[7])

		if (ralow > rahi)
		    rahi = rahi + 360.0

		if ((ralow > ra2) && (rahi < ra1))
		    next
	    }

	    # Region number
	    call tbrgti (tp, cdv[1], regnum, null, 1, row)

	    # Zone number => directory name
	    zone = fzone (declow, dechi)

	    # Build the file name
	    call sprintf (Memc[root], SZ_LINE, "%04d")
		call pargi (regnum)

	    if (hemsph != NORTH && regnum <= LAST_NORTH) {
		# Read the northern hemisphere disk (Volume 1)
		hemsph = NORTH
##              call printf ("%s\n")
##                  call pargstr (north)
	    # Build the path name
	    call strcat (cdrom_n, Memc[path], SZ_FNAME)
	    call sprintf (Memc[path], SZ_LINE, "/gsc/%s/")
		call pargstr (zdir[1,zone])


##          call printf ("/cdrom/GSC1/gsc/%s/")
##              call pargstr( zdir[1,zone])

	    } else if (hemsph != SOUTH && regnum > LAST_NORTH) {
		# Read the southern hemisphere disk (Volume 2)
		hemsph = SOUTH
##              call printf ("%s\n")
##                  call pargstr (south)
	    # Build the path name
	    call strcat (cdrom_s, Memc[path], SZ_FNAME)
	    call sprintf (Memc[path], SZ_LINE, "/gsc/%s/")
		call pargstr (zdir[1,zone])

##          call printf ("/cdrom/GSC2/gsc/%s/")
##              call pargstr( zdir[1,zone])

	    }

	    # Write the command line
#           call printf ("!cp %s%s.gsc gsc%s.fits\n")
#               call pargstr (Memc[path])
#               call pargstr (Memc[root])
#               call pargstr (Memc[root])

	    if (hemsph == NORTH) {
		# Write the table list line
		call fprintf (tbl, "%s/gsc/%s %s\n")
		    call pargstr (cdrom_n)
		    call pargstr (zdir[1,zone])
		    call pargstr (Memc[root])
	    } else if (hemsph == SOUTH) {
		# Write the table list line
		call fprintf (tbl, "%s/gsc/%s %s\n")
		    call pargstr (cdrom_s)
		    call pargstr (zdir[1,zone])
		    call pargstr (Memc[root])
	    }
	    nrgn = nrgn + 1

	    call eprintf ("%5d %6d %00.0h %00.0h %00.0h %00.0h \
%d %d %d\n")
		call pargi (nrgn)
		call pargi (row)
		call pargd (ralow/15)
		call pargd (rahi/15)
		call pargd (declow)
		call pargd (dechi)
		call pargi (hemsph)
		call pargi (regnum)
		call pargi (zone)
	}

	call sfree (sp)

	return (nrgn)
end


double procedure rdira (tp, row, hcol, mcol, scol)

#  RDIRA -- Returns R.A. in degrees from the G.S. index table

pointer tp                      # Index table descriptor
int     row                     # Table row number
pointer hcol                    # Column descriptor for hours
pointer mcol                    # Column descriptor for minutes
pointer scol                    # Column descriptor for seconds

int     hrs                     # Hours of RA
int     min                     # Minutes of RA
real    sec                     # Seconds of RA
bool    null                    # Null column?
double  ra

begin
	# Hours of Right Ascension
	call tbrgti (tp, hcol, hrs, null, 1, row)
	if (null)
	    return (INDEFD)

	# Minutes of R. A.
	call tbrgti (tp, mcol, min, null, 1, row)
	if (null)
	    return (INDEFD)

	# Seconds of R. A.
	call tbrgtr (tp, scol, sec, null, 1, row)
	if (null)
	    return (INDEFD)

	ra = double (hrs) + double (min) / 60 + double (sec) / 3600

	# R. A. in degrees
	ra = HRSTODEG (ra)

	return (ra)
end


double procedure rdidec (tp, row, sgncol, dcol, mcol)

#  RDIDEC -- Returns the declination in decimal degrees from the G.S.
#  index table.  This is converted from degrees, minutes, and seconds
#  columns.

pointer tp                      # Index table descriptor
int     row                     # Table row number
pointer sgncol                  # Column descriptor for sign
pointer dcol                    # Column descriptor for degrees
pointer mcol                    # Column descriptor for minutes

char    sign[4]                 # Sign of Dec
int     deg                     # Degrees of Dec
real    min                     # Minutes of Dec
bool    null                    # Null column?
double  dec
char    minus
data    minus /'-'/

int     stridx()

begin
	# Declination sign
	call tbrgtt (tp, sgncol, sign, null, 4, 1, row)
	if (null)
	    return (INDEFD)

	# Degrees of declination
	call tbrgti (tp, dcol, deg, null, 1, row)
	if (null)
	    return (INDEFD)

	# Minutes of declination
	call tbrgtr (tp, mcol, min, null, 1, row)
	if (null)
	    return (INDEFD)

	dec = double (deg) + double (min) / 60

	if (stridx (minus, sign) != 0)
	    # Negative declination
	    dec = -dec

	return (dec)
end


int procedure fzone (declow, dechi)

#  FZONE -- Find the zone number from the range of declinations in the
#  region.

double  declow, dechi           # Limits of declination of field

double  dec
int     zone

begin
	dec = (declow + dechi) / 2.0
	zone = int (dec / ZONE_SIZE) + 1
	if (dec < 0)
	    zone = (DEC_ZONES + 2) - zone

	return (zone)
end


procedure initzd (zdir)

## 1/28/93  Change GSC pathname to lower case.

char    zdir[DIR_SLEN,NUM_DIR]

begin
	call strcpy ("n0000", zdir[1,1],  DIR_SLEN)
	call strcpy ("n0730", zdir[1,2],  DIR_SLEN)
	call strcpy ("n1500", zdir[1,3],  DIR_SLEN)
	call strcpy ("n2230", zdir[1,4],  DIR_SLEN)
	call strcpy ("n3000", zdir[1,5],  DIR_SLEN)
	call strcpy ("n3730", zdir[1,6],  DIR_SLEN)
	call strcpy ("n4500", zdir[1,7],  DIR_SLEN)
	call strcpy ("n5230", zdir[1,8],  DIR_SLEN)
	call strcpy ("n6000", zdir[1,9],  DIR_SLEN)
	call strcpy ("n6730", zdir[1,10], DIR_SLEN)
	call strcpy ("n7500", zdir[1,11], DIR_SLEN)
	call strcpy ("n8230", zdir[1,12], DIR_SLEN)

	call strcpy ("s0000", zdir[1,13], DIR_SLEN)
	call strcpy ("s0730", zdir[1,14], DIR_SLEN)
	call strcpy ("s1500", zdir[1,15], DIR_SLEN)
	call strcpy ("s2230", zdir[1,16], DIR_SLEN)
	call strcpy ("s3000", zdir[1,17], DIR_SLEN)
	call strcpy ("s3730", zdir[1,18], DIR_SLEN)
	call strcpy ("s4500", zdir[1,19], DIR_SLEN)
	call strcpy ("s5230", zdir[1,20], DIR_SLEN)
	call strcpy ("s6000", zdir[1,21], DIR_SLEN)
	call strcpy ("s6730", zdir[1,22], DIR_SLEN)
	call strcpy ("s7500", zdir[1,23], DIR_SLEN)
	call strcpy ("s8230", zdir[1,24], DIR_SLEN)
end
