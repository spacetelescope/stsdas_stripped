include <ctype.h>
include <math.h>
include <mach.h>
include <tbset.h>
include <imhdr.h>
include <error.h>

define	NUM_COLS	5
define	NUM_COEFF	7
define	MAX_COEFF	7
define	ARCSEC_PER_RADIAN	(RADIAN*3600)
define	RADTOSEC		($1*ARCSEC_PER_RADIAN)
define	SECTORAD		($1/ARCSEC_PER_RADIAN)

procedure t_intrep()

#  INTREP -- Reposition initial guesses for guide star positions on an
#  image.  Allow the user to select each marked star and specify the
#  improved position.  A fit is then performed to a section of the image
#  around the position to refine it further.

.help
	c	Show the star closest to the cursor location
	d	Delete a star (mark as not valid)
	f	Plate solution
	l	List the stars
	p	Find the peak of the star inside the search box
	q	Quit
	s	Select a star to move
	u	Undelete a star (mark as valid)
	:	Commands with arguments, see below

	:b size	Set the search box size in pixels
	:d row	Delete the listed star by number
	:D	Delete all stars
	:f file	Write the fit coefficients to a file
	:n num	Set the number of fit coefficients
	:s file	Write the valid coordinates to a file
	:u row	Restore the listed star by number
	:U	Undelete all stars
.endhelp

real	wx, wy			# WCS coords of cursor
int	wcs			# WCS number
int	key			# Keystroke
char	command[SZ_LINE]	# : command
pointer	tabname			# Table name
pointer	imgname			# Input image name
pointer	tp			# Table descriptor
pointer	cp[NUM_COLS]		# Column descriptors
pointer	dp[NUM_COLS]		# Column data pointers
int	nrows			# Number of rows in table
int	row			# Row in table
pointer	im			# Image descriptor
pointer	sp, errmsg, ttabnam
int	ttd
int	junk
int	boxsize			# Box size for search
int	new_def
real	px, py			# Centered pixel coordinates
int	ip
double	racen, deccen		# Equatorial coords of plate center
double	xcrpix, ycrpix		# Reference pixel
double	pltscl			# Plate scale (arcsec/mm)
double	xpxsiz, ypxsiz		# Pixel size (microns)
double	xcoeff[NUM_COEFF], ycoeff[NUM_COEFF]
int	ncoeff, def_nc
double	xchisq, ychisq		# Reduced che square
double	cdmtrx[4]		# CD matrix
double	ra, dec			# Current equatorial coordinates

string	validpar	"valid"
string	tabnampar	"gsctab"
string	imgnampar	"image"
string	boxsizpar	"boxsize"
string	xcrpixpar	"xcrpix"
string	ycrpixpar	"ycrpix"
string	racenpar	"racen"
string	deccenpar	"deccen"
string	pltsclpar	"pltscl"
string	xpxsizpar	"xpxsize"
string	ypxsizpar	"ypxsize"
string	soloutpar	"solout"

string	keysfile	"gasp$intrep.key"
string  underterm	"Solution is underdetermined. Set number of \
coeffients with :n\n"

int	clgcur(), clgeti(), ctoi(), ctowrd()
int	open(), access(), wtxtab(), acount()
pointer	tbtopn(), immap()
bool	streq(), clgetb()
double	clgetd()

begin
	call smark (sp)

	call salloc (tabname, SZ_FNAME, TY_CHAR)
	call salloc (ttabnam, SZ_FNAME, TY_CHAR)
	call salloc (imgname, SZ_FNAME, TY_CHAR)

	call clgstr (tabnampar, Memc[tabname], SZ_FNAME)
	call clgstr (imgnampar, Memc[imgname], SZ_FNAME)

	# Open the catalog table
	tp = tbtopn (Memc[tabname], READ_WRITE, 0)

	# Read the catalog table
	call gstrdc (tp, cp, dp, nrows)

	if (nrows <= 0) {
	    call salloc (errmsg, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmsg], SZ_LINE,
		"No stars in gsc table:  %s")
		call pargstr (Memc[tabname])
	    call error (0, Memc[errmsg])
	}

	# Initialize the validity flags
	switch (clgeti (validpar)) {
	case -1:
	    # Set all stars to invalid
	    call amovki (NO, Memi[dp[5]], nrows)
	    call tbcpti (tp, cp[5], Memi[dp[5]], 1, nrows)
	case 1:
	    # Set all stars to valid
	    call amovki (YES, Memi[dp[5]], nrows)
	    call tbcpti (tp, cp[5], Memi[dp[5]], 1, nrows)
	}
	
	# Open the input image
	im = immap (Memc[imgname], READ_ONLY, 0)

	# Find the default box size for search
	boxsize = clgeti (boxsizpar)

	# Find the plate parameters
	xcrpix = clgetd (xcrpixpar)
	ycrpix = clgetd (ycrpixpar)
	racen  = clgetd (racenpar)
	deccen = clgetd (deccenpar)
	xpxsiz = clgetd (xpxsizpar)
	ypxsiz = clgetd (ypxsizpar)
	pltscl = clgetd (pltsclpar)

	def_nc = NUM_COEFF
	ncoeff = 0

	while (clgcur ("coord", wx, wy, wcs, key, command, SZ_LINE) != EOF) {
	    if (key == 'q')
		break

	    # Find the star (row) nearest this pixel
	    call fndgsc (wx, wy, Memr[dp[3]], Memr[dp[4]], nrows,
		px, py, row)

	    if (row <= 0) { 
		call printf ("No object found\n")
		next
	    }

	    switch (key) {
	    case 'c':
		# Display the coordinates of the catalog star nearest
		# the cursor
		if (ncoeff > 0) {
		    # There is a fit;
		    # Find the equatorial coordinates of the selected
		    # star based on the plate solution.
		    call rpixtoeq (px, py, racen, deccen, xcrpix, ycrpix,
			xpxsiz, ypxsiz, xcoeff, ycoeff, ncoeff,
			ra, dec)
		    call printf ("%d %.0f %.0f;  %.2f %.2f --> %011.2h %011.1h\n")
			call pargi (row)
			call pargr (wx)
		    	call pargr (wy)
			call pargr (px)
			call pargr (py)
			call pargd (ra/15.0)
			call pargd (dec)
		} else {
		    # Just print the peaked pixel coordinates
		    call printf ("No plate solution\n")
		    call printf ("%6d (%6.0f,%6.0f) --> (%6.2f,%6.2f)\n")
			call pargi (row)
			call pargr (wx)
			call pargr (wy)
			call pargr (px)
			call pargr (py)
		}

	    case 'd':
		# Mark star as deleted
		Memi[dp[5]+row-1] = NO
		call tbrpti (tp, cp[5], NO, 1, row)

		call printf ("%6d (%6.1f,%6.1f)  Deleted\n")
		    call pargi (row)
		    call pargr (px)
		    call pargr (py)

	    case 'f':
		# Fit the data and print the coefficients
		ncoeff = def_nc

		if (ncoeff > acount (YES, Memi[dp[5]], nrows)) {
		    call printf (underterm)

		} else {
		    call pltsol (Memd[dp[1]], Memd[dp[2]], 
				 Memr[dp[3]], Memr[dp[4]], Memi[dp[5]], nrows,
				 racen, deccen, xcrpix, ycrpix, pltscl, xpxsiz,
				 ypxsiz, xcoeff, ycoeff, ncoeff, xchisq, 
				 ychisq, cdmtrx)

		    call pixeqv (Memr[dp[3]], Memr[dp[4]], Memi[dp[5]], nrows,
				 racen, deccen, xcrpix, ycrpix, xpxsiz, ypxsiz,
				 xcoeff, ycoeff, ncoeff)
		}

	    case 'l':
		# List the stars in the table
		call lstgst (im, Memd[dp[1]], Memd[dp[2]], Memr[dp[3]],
		    Memr[dp[4]], Memi[dp[5]], nrows)

	    case 'p':
		# Display the centered coordinates of the star at the
		# cursor position without changing anything.
		call imcntr (im, wx, wy, boxsize, px, py)

		if (ncoeff > 0) {
		    # There is a fit;
		    # Find the equatorial coordinates at the cursor based on
	   	    # the plate solution and print the results
		    call rpixtoeq (px, py, racen, deccen, xcrpix, ycrpix,
			xpxsiz, ypxsiz, xcoeff, ycoeff, ncoeff,
			ra, dec)
		    call printf ("%.0f %.0f;  %.2f %.2f --> %011.2h %011.1h\n")
			call pargr (wx)
		    	call pargr (wy)
			call pargr (px)
			call pargr (py)
			call pargd (ra/15.0)
			call pargd (dec)
		} else {
		    # Just print the peaked pixel coordinates
		    call printf ("No plate solution\n")
		    call printf ("%.0f %.0f;  %.2f %.2f\n")
			call pargr (wx)
		    	call pargr (wy)
			call pargr (px)
			call pargr (py)
		}

	    case 's':
		# Select and reposition a star
		call printf ("%6d (%6.1f,%6.1f) --> (%6.1f,%6.1f):  ")
		    call pargi (row)
		    call pargr (wx)
		    call pargr (wy)
		    call pargr (px)
		    call pargr (py)
		call printf ("Should be?")

		# Read the cursor again
		junk = clgcur ("coord", wx, wy, wcs, key, command, SZ_LINE)

		# Refine the position from the input image
		call imcntr (im, wx, wy, boxsize, px, py)

		call printf ("\n       (%6.1f,%6.1f) --> (%6.1f,%6.1f)\n")
		    call pargr (wx)
		    call pargr (wy)
		    call pargr (px)
		    call pargr (py)

		# Replace the pixel coordinate values with
		# the peaked cursor position
		Memr[dp[3]+row-1] = px
		Memr[dp[4]+row-1] = py

		# Set this star to valid
		Memi[dp[5]+row-1] = YES

		# Replace the table values
		call tbcptr (tp, cp[3], px, row, row)
		call tbcptr (tp, cp[4], py, row, row)
		call tbrpti (tp, cp[5], YES, 1, row)

	    case 'u':
		# Mark a star as valid (undelete)
		Memi[dp[5]+row-1] = YES
		call tbrpti (tp, cp[5], YES, 1, row)

		call printf ("%6d (%6.1f,%6.1f)  UnDeleted\n")
		    call pargi (row)
		    call pargr (px)
		    call pargr (py)

	    case 'w':
		# Find the equatorial coordinates at the cursor based on
		# the plate solution and print the results
		if (ncoeff > 0) {
		    # There is a fit
		    call rpixtoeq (wx, wy, racen, deccen, xcrpix, ycrpix,
			xpxsiz, ypxsiz, xcoeff, ycoeff, ncoeff,
			ra, dec)
		    call printf ("%.0f %.0f --> %011.2h %011.1h\n")
			call pargr (wx)
			call pargr (wy)
			call pargd (ra/15.0)
			call pargd (dec)
		} else
		    call printf ("No plate solution\n")

	    case '?':
		# Page the keys help
		call pagefiles (keysfile)

	    case ':':
		# Colon command
		for (ip = 1;  IS_WHITE(command[ip]);  ip = ip + 1)
		    # Skip white space
		    ;

		switch (command[ip]) {
		case 'b':
		    # Change search box size
		    ip = ip + 1
		    if (ctoi (command, ip, new_def) > 0) {
			if (new_def == 2 * (new_def / 2))
			    # Even;  we want odd
			    new_def = new_def + 1
			boxsize = new_def
		    }

		    call printf ("Search box:  %d\n")
			call pargi (boxsize)

		case 'n':
		    # Set the number of fit coefficients
		    ip = ip + 1
		    if (ctoi (command, ip, new_def) > 0) 
			def_nc = new_def

		    call printf ("Number of fit coefficients:  %d\n")
			call pargi (def_nc)


		case 'd':
		    # Mark star in specified row as deleted
		    ip = ip + 1
		    if (ctoi (command, ip, row) > 0) {
			Memi[dp[5]+row-1] = NO
			call tbrpti (tp, cp[5], NO, 1, row)

			call printf ("%6d (%6.1f,%6.1f)  Deleted\n")
			    call pargi (row)
			    call pargr (Memr[dp[3]+row-1])
			    call pargr (Memr[dp[4]+row-1])
		    }

		case 'D':
		    # Mark all stars as deleted
		    call amovki (NO, Memi[dp[5]], nrows)
		    call tbcpti (tp, cp[5], Memi[dp[5]], 1, nrows)
		    call printf ("All stars deleted\n")

		case 'f':
		    # Write the fit coefficients to a file useful as
		    # input to xyeq
		    if (ncoeff <= 0) {
			call printf ("No plate solution\n")
			break
		    }
		    ip = ip + 1
		    junk = ctowrd (command, ip, Memc[ttabnam], SZ_FNAME)

		    if (junk <= 0 || streq (Memc[ttabnam], "STDOUT"))
			ttd = STDOUT
		    else if (access (Memc[ttabnam], 0, TEXT_FILE) == YES)
			ttd = open (Memc[ttabnam], APPEND, TEXT_FILE)
		    else
			ttd = open (Memc[ttabnam], NEW_FILE, TEXT_FILE)

		    call wrcoef (ttd, xcrpix, ycrpix, racen, deccen,
			xpxsiz, ypxsiz, pltscl, xcoeff, ycoeff, ncoeff)

		    if (ttd != STDOUT) {
			call close (ttd)
			call printf ("Fit coefficients written to %s\n")
			    call pargstr (Memc[ttabnam])
		    }

		case 'u':
		    # Mark star in specified row as valid
		    ip = ip + 1
		    if (ctoi (command, ip, row) > 0) {
			Memi[dp[5]+row-1] = YES
			call tbrpti (tp, cp[5], YES, 1, row)

			call printf ("%6d (%6.1f,%6.1f)  UnDeleted\n")
			    call pargi (row)
			    call pargr (Memr[dp[3]+row-1])
			    call pargr (Memr[dp[4]+row-1])
		    }

		case 'U':
		    # Mark all stars as valid
		    call amovki (YES, Memi[dp[5]], nrows)
		    call tbcpti (tp, cp[5], Memi[dp[5]], 1, nrows)
		    call printf ("All stars UnDeleted\n")

		case 's':
		    # Write the text table of valid stars
		    ip = ip + 1
		    junk = ctowrd (command, ip, Memc[ttabnam], SZ_FNAME)

		    if (junk <= 0 || streq (Memc[ttabnam], "STDOUT"))
			ttd = STDOUT
		    else if (access (Memc[ttabnam], 0, TEXT_FILE) == YES)
			ttd = open (Memc[ttabnam], APPEND, TEXT_FILE)
		    else
			ttd = open (Memc[ttabnam], NEW_FILE, TEXT_FILE)

		    junk = wtxtab (ttd, Memd[dp[1]], Memd[dp[2]],
			Memr[dp[3]], Memr[dp[4]], Memi[dp[5]], nrows)

		    if (ttd != STDOUT) {
			call close (ttd)
			call printf ("%d valid stars written to %s\n")
			    call pargi (junk)
			    call pargstr (Memc[ttabnam])
		    }
		}
	    }
	}

	if (clgetb (soloutpar))
	    # Write the equatorial and pixel coordinates for the
	    # full plate solution task
	    junk = wtxtab (STDOUT, Memd[dp[1]], Memd[dp[2]],
		Memr[dp[3]], Memr[dp[4]], Memi[dp[5]], nrows)

	call imunmap (im)
	call tbtclo (tp)
	call mfree (dp[1], TY_DOUBLE)
	call mfree (dp[2], TY_DOUBLE)
	call mfree (dp[3], TY_REAL)
	call mfree (dp[4], TY_REAL)
	call mfree (dp[5], TY_INT)
	call sfree (sp)
end


procedure gstrdc (tp, cp, dp, nrows)

#  GSTRDC -- Read the GSC table columns for catalog coordinates and
#  pixel coordinates.

pointer	tp				# Table pointer
pointer	cp[NUM_COLS]			# Column descriptors
pointer	dp[NUM_COLS]			# Data pointers
int	nrows				# Number of table rows

pointer	errmsg, null
pointer	sp
int	col

char	colname[SZ_COLNAME,NUM_COLS]	# Column names

string	racol	"RA_DEG"
string	deccol	"DEC_DEG"
string	xcol	"x_pix"
string	ycol	"y_pix"
string	valcol	"valid"

int	tbpsta()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Assign the column names
	call strcpy (racol,  colname[1,1], SZ_COLNAME)
	call strcpy (deccol, colname[1,2], SZ_COLNAME)
	call strcpy (xcol,   colname[1,3], SZ_COLNAME)
	call strcpy (ycol,   colname[1,4], SZ_COLNAME)
	call strcpy (valcol, colname[1,5], SZ_COLNAME)

	# Find the table columns
	call tbcfnd (tp, colname, cp, NUM_COLS)

	do col = 1, NUM_COLS {
	    if (cp[col] <= 0) {
		call sprintf (Memc[errmsg], SZ_LINE,
		    "Could not find column %s")
		    call pargstr (colname[1,col])
		call error (0, Memc[errmsg])
	    }
	}

	# Number of rows in catalog table
	nrows = tbpsta (tp, TBL_NROWS)

	# Allocate the column buffers
	call malloc (dp[1], nrows, TY_DOUBLE)
	call malloc (dp[2], nrows, TY_DOUBLE)
	call malloc (dp[3], nrows, TY_REAL)
	call malloc (dp[4], nrows, TY_REAL)
	call malloc (dp[5], nrows, TY_INT)
	call malloc (null,  nrows, TY_BOOL)

	# Read the columns
	# R.A.
	call tbcgtd (tp, cp[1], Memd[dp[1]], Memb[null], 1, nrows)

	# Dec.
	call tbcgtd (tp, cp[2], Memd[dp[2]], Memb[null], 1, nrows)

	# X pixel
	call tbcgtr (tp, cp[3], Memr[dp[3]], Memb[null], 1, nrows)

	# Y pixel
	call tbcgtr (tp, cp[4], Memr[dp[4]], Memb[null], 1, nrows)

	# Valid?
	call tbcgti (tp, cp[5], Memr[dp[5]], Memb[null], 1, nrows)

	call mfree (null, TY_BOOL)
	call sfree (sp)
end


procedure fndgsc (wx, wy, xpix, ypix, nrows, px, py, row)

# FNDGSC -- Find the catalog object nearest the pixel (cursor) position

real	wx, wy				# Input pixel (cursor) coordinates
real	xpix[nrows], ypix[nrows]	# Catalog pixel coordinates
int	nrows				# Number of catalog coordinates
int	row				# Output selected catalog row
real	px, py				# Pixel coordinates of found object

int	cr
real	dx, dy
real	d, dmin

begin
	dmin = MAX_REAL
	row  = 0

	do cr = 1, nrows {
	    if (IS_INDEF(xpix[cr]) || IS_INDEF(xpix[cr]))
		next

	    dx = wx - xpix[cr]
	    dy = wy - ypix[cr]
	    d = sqrt (dx * dx + dy * dy)

	    if (d < dmin) {
		dmin = d
		row = cr
	    }
	}

	if (row > 0) {
	    px = xpix[row]
	    py = ypix[row]

	} else {
	    px = INDEF
	    py = INDEF
	}
end


procedure imcntr (im, xstart, ystart, boxsize, xcntr, ycntr)

# IMCNTR -- Find the center of a star image given approximate coords.  Uses
# Mountain Photometry Code Algorithm as outlined in Stellar Magnitudes from
# Digital Images.

# This is stolen from noao.proto.imcntr

pointer	im
real	xstart, ystart
int	boxsize
real	xcntr, ycntr

int	x1, x2, y1, y2, half_box
int	ncols, nrows, nx, ny, try
real	xinit, yinit
pointer	bufptr, sp, x_vect, y_vect
int	imgs2r()

begin
	half_box = (boxsize - 1) / 2
	xinit = xstart
	yinit = ystart

	# Mark region to extract - use box size
	ncols = IM_LEN (im, 1)
	nrows = IM_LEN (im, 2)
	try = 0

	repeat {
	    x1 = amax1 (xinit - half_box, 1.0) +0.5
	    x2 = amin1 (xinit + half_box, real(ncols)) +0.5
	    y1 = amax1 (yinit - half_box, 1.0) +0.5
	    y2 = amin1 (yinit + half_box, real(nrows)) +0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # Extract region around center
	    bufptr = imgs2r (im, x1, x2, y1, y2)

	    # Collapse to two 1-D arrays
	    call smark (sp)
	    call salloc (x_vect, nx, TY_REAL)
	    call salloc (y_vect, ny, TY_REAL)

	    call aclrr (Memr[x_vect], nx)
	    call aclrr (Memr[y_vect], ny)

	    # Sum all rows
	    call mpc_rowsum (Memr[bufptr], Memr[x_vect], nx, ny)

	    # Sum all columns
	    call mpc_colsum (Memr[bufptr], Memr[y_vect], nx, ny)

	    # Find centers
	    call mpc_getcenter (Memr[x_vect], nx, xcntr)
	    call mpc_getcenter (Memr[y_vect], ny, ycntr)
	    call sfree (sp)

	    # Check for INDEF centers.
	    if (IS_INDEFR(xcntr) || IS_INDEFR(ycntr)) {
		xcntr = xinit
		ycntr = yinit
		break
	    }

	    # Add in offsets
	    xcntr = xcntr + x1
	    ycntr = ycntr + y1

	    try = try + 1
	    if (try == 1) {
		if ((abs(xcntr-xinit) > 1.0) || (abs(ycntr-yinit) > 1.0)) {
		    xinit = xcntr
		    yinit = ycntr
		}
	    } else
		break
	}
end


# ROWSUM -- Sum all rows in a raster

procedure mpc_rowsum (v, row, nx, ny)

int	nx, ny
real	v[nx,ny]
real	row[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		row[j] = row[j] + v[j,i]
end


# COLSUM -- Sum all columns in a raster.

procedure mpc_colsum (v, col, nx, ny)

int	nx, ny
real	v[nx,ny]
real	col[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		col[j] = col[j] + v[i,j]
end


# GETCENTER -- Compute center of gravity of array.

procedure mpc_getcenter (v, nv, vc)

real	v[ARB]
int	nv
real	vc

int	i
real	sum1, sum2, sigma, cont

begin
	# Assume continuum level is at endpoints
	# Compute first moment
	sum1 = 0.0
	sum2 = 0.0

	call aavgr (v, nv, cont, sigma)

	do i = 1, nv
	    if (v[i] > cont) {
	        sum1 = sum1 + (i-1) * (v[i] - cont)
	        sum2 = sum2 + (v[i] - cont)
	    }

	# Determine center
	if (sum2 == 0.0)
	    vc = INDEFR
	else
	    vc = sum1 / sum2
end


procedure lstgst (im, ra, dec, x, y, valid, nvals)

#  LSTGST -- Print the table of guide star equatorial and pixel coordinates.

pointer	im				# Image descriptor
double	ra[ARB], dec[ARB]		# World coords
real	x[ARB], y[ARB]			# Pixel coords
int	valid[ARB]			# Valid?
int	nvals				# Number of values

int	i
int	xsize, ysize

begin
	xsize = IM_LEN(im, 1)
	ysize = IM_LEN(im, 2)

	call printf ("        RA (hrs)    Dec (deg)      X      Y    Valid\n")

	do i = 1, nvals {
	    if (x[i] > 0 && x[i] <= xsize &&
		y[i] > 0 && y[i] <= ysize) {
		call printf ("%6d %011.2h %011.1h %6.1f %6.1f  %b\n")
		    call pargi (i)
		    call pargd (ra[i]/15.0)
		    call pargd (dec[i])
		    call pargr (x[i])
		    call pargr (y[i])
		    call pargi (valid[i])
	    }
	}
end


int procedure wtxtab (ttd, ra, dec, x, y, valid, num)

#  WTXTAB -- Write a text table of guide star equatorial and pixel
#  coordinates used as input to the plate solution task.

int	ttd				# Output file descriptor
double	ra[ARB], dec[ARB]		# World coordinates
real	x[ARB], y[ARB]			# Pixel coordinates
int	valid[ARB]			# Valid position?
int	num				# Number of coordinates

int	i, j

begin
	j = 0
	do i = 1, num {
	    if (valid[i] == NO)
		next

	    call fprintf (ttd, "%8d  %011.2h %011.1h %011.1h %8.2f %8.2f\n")
		call pargi (i)
		call pargd (ra[i]/15.0)
		call pargd (ra[i])
		call pargd (dec[i])
		call pargr (x[i])
		call pargr (y[i])

	    j = j + 1
	}

	return (j)
end


procedure rpixtoeq (px, py, r_cen, d_cen, x_cen, y_cen,
	xpxsiz, ypxsiz, xcoeff, ycoeff, ncoeff,
	ra, dec)

# PIXTOEQ -- Transform pixel to equatorial coordinates using plate
# solution coefficients.

real	px, py				# Pixel coordinates
double	r_cen, d_cen			# Reference coordinates (degrees)
double	x_cen, y_cen			# Reference pixel
double	xpxsiz, ypxsiz			# Pixel size (microns)
double	xcoeff[ARB], ycoeff[ARB]	# Plate solution coefficients
int	ncoeff				# Number of coefficients
double	ra, dec				# Equatorial coordinates in degrees

double	x, y				# Plate coords (mm)
double	xi, eta				# Seconds of arc from the plate center


begin
	# Coordinates in mm from reference pixel
#	x = (x_cen - double (px)) * xpxsiz / 1.0d3
	x = (double (px) - x_cen) * xpxsiz / 1.0d3
	y = (double (py) - y_cen) * ypxsiz / 1.0d3

	xi = -xcoeff[1] * x +
	     xcoeff[2] * y +
	     xcoeff[3] +
	     xcoeff[4] * x * x +
	     xcoeff[5] * x * y +
	     xcoeff[6] * y * y +
	     xcoeff[7] * (x * x + y * y)

	eta = ycoeff[1] * y +
	      ycoeff[2] * x +
	      ycoeff[3] +
	      ycoeff[4] * y * y +
	      ycoeff[5] * y * x +
	      ycoeff[6] * x * x +
	      ycoeff[7] * (y * y + x * x)

	# Find r.a. and dec.
	call stdeqc (r_cen, d_cen, xi, eta, ra, dec)
end


procedure wrcoef (ttd, xcrpix, ycrpix, racen, deccen, xpxsiz, ypxsiz,
	pltscl, xcoeff, ycoeff, ncoeff)

#  WRCOEF -- Write the plate solution coefficients in the format used by
#  the xyeq task.

int	ttd				# Output file descriptor
double	xcrpix, ycrpix			# Reference pixel
double	racen, deccen			# Reference coordinates
double	xpxsiz, ypxsiz			# Pixel size in microns
double	pltscl				# Plate scale arcsec/mm
double	xcoeff[ARB], ycoeff[ARB]	# Fit coefficients
int	ncoeff				# Number of coefficients

pointer	sp, id, cfnam
int	coef

begin
	call smark (sp)
	call salloc (id, SZ_LINE, TY_CHAR)
	call salloc (cfnam, SZ_LINE, TY_CHAR)

	# Comment header
	call sysid (Memc[id], SZ_LINE)
	call fprintf (ttd, "#Astrometric solution file %s\n#\n")
	    call pargstr (Memc[id])

	# Reference pixel
	call fprintf (ttd, "CRPIX1\t%g\nCRPIX2\t%g\n")
	    call pargd (xcrpix)
	    call pargd (ycrpix)

	# Reference coordinates
	call fprintf (ttd, "CRVAL1\t%g\nCRVAL2\t%g\n")
	    call pargd (racen)
	    call pargd (deccen)

	call fprintf (ttd, "XPIXELSZ\t%g\nYPIXELSZ\t%g\n")
	    call pargd (xpxsiz)
	    call pargd (ypxsiz)

	call fprintf (ttd, "PLATESCL\t%g\n")
	    call pargd (pltscl)

	# Write the coefficients
	call strcpy ("XCOEFF", Memc[cfnam], SZ_LINE)
	do coef = 1, ncoeff {
	    call fprintf (ttd, "%s%d\t%g\n")
		call pargstr (Memc[cfnam])
		call pargi (coef)
		call pargd (xcoeff[coef])
	}

	call strcpy ("YCOEFF", Memc[cfnam], SZ_LINE)
	do coef = 1, ncoeff {
	    call fprintf (ttd, "%s%d\t%g\n")
		call pargstr (Memc[cfnam])
		call pargi (coef)
		call pargd (ycoeff[coef])
	}

	call sfree (sp)
end


procedure stdeqc (r_cen, d_cen, xi, eta, ra, dec)

#  STDEQC -- Compute RA and Dec from Standard coordinates,
#  seconds of arc from the reference coordinates.

double	r_cen, d_cen			# Reference coords (degrees)
double	xi, eta				# Standard coords (arcsec)
double	ra, dec				# Equatorial coords (degrees)

double	xi_rad, eta_rad			# Standard coords (radians)
double	cr_rad, cd_rad
double	numer, denom

begin
	# To radians
	xi_rad  = SECTORAD(xi)
	xi_rad  = xi / 57.295779513082320877d0 / 3.6d3
	eta_rad = SECTORAD(eta)
	eta_rad = eta / 57.295779513082320877d0 / 3.6d3
	cr_rad  = DEGTORAD(r_cen)
	cr_rad  = r_cen / 57.295779513082320877d0
	cd_rad  = DEGTORAD(d_cen)
	cd_rad  = d_cen / 57.295779513082320877d0

	# Find RA
	numer = xi_rad / dcos (cd_rad)
	denom = 1.0d0 - eta_rad * dtan (cd_rad)
	ra    = datan2 (numer, denom) + cr_rad

	if (ra < 0.0)
	    ra = ra + TWOPI

	# Find Dec
	numer = dcos (ra - cr_rad) * (eta_rad + dtan (cd_rad))
#	numer = dcos (cr_rad - ra) * (eta_rad + dtan (cd_rad))
	denom = 1.0d0 - eta_rad * dtan (cd_rad)
	dec   = datan2 (numer, denom)

	call trsteq (cr_rad, cd_rad, xi, eta, ra, dec)

	# To degrees
	ra  = RADTODEG(ra)
	dec = RADTODEG(dec)
end


procedure pltsol (ra, dec, px, py, valid, npts,
	rcen, dcen, xcrpix, ycrpix, pltscl, xpxsiz, ypxsiz,
	xcoeff, ycoeff, ncoeff, xchisq, ychisq, cdmtrx)

#  PLTSOL -- Compute a plate solution from equatorial and pixel
#  coordinates of guide stars.

double	ra[ARB], dec[ARB]		# Eq. coords in degrees
real	px[ARB], py[ARB]		# Pixel coords
int	valid[ARB]			# Valid coord?
int	npts				# Number of data points
double	rcen, dcen			# Eq. coords of center (degrees)
double	xcrpix, ycrpix			# Referene pixel
double	pltscl				# Plate scale (arcsec/mm)
double	xpxsiz, ypxsiz			# Pixel size (microns)
double	cdmtrx[4]			# CD matrix
double	xcoeff[ARB]			# Fit coefficients
double	ycoeff[ARB]			# Fit coefficients
int	ncoeff				# Number of coefficients in fit
double	xchisq, ychisq			# Reduced chi square

pointer	sp
pointer	x, y				# Rectangular coords in mm
pointer	xi, eta
pointer	xpm, ypm			# Plate model
pointer	wgt				# Weights
pointer	u, v, w

begin
	if (npts == 0)
	    call error (0, "No data points;  can't fit")

	# Initialize coefficients
	call amovkd (double (0), xcoeff, ncoeff)
	call amovkd (double (0), ycoeff, ncoeff)

	call smark (sp)

	call salloc (x,   npts, TY_DOUBLE)
	call salloc (y,   npts, TY_DOUBLE)
	call salloc (xi,  npts, TY_DOUBLE)
	call salloc (eta, npts, TY_DOUBLE)
	call salloc (wgt, npts, TY_REAL)
	call salloc (xpm, npts*MAX_COEFF, TY_DOUBLE)
	call salloc (ypm, npts*MAX_COEFF, TY_DOUBLE)
	call salloc (u,   npts*ncoeff, TY_DOUBLE)
	call salloc (v,   ncoeff*ncoeff, TY_DOUBLE)
	call salloc (w,   ncoeff, TY_DOUBLE)

	# Convert pixel coords to mm from reference pixel
	call pxtomm (px, py, xcrpix, ycrpix, xpxsiz, ypxsiz, 
	    Memd[x], Memd[y], npts)

	# Convert the integer valid flag to floating weights
	call achtir (valid, Memr[wgt], npts)

	# Convert equatorial coordinates to xi, eta (arcsec. from center)
	call teqxev (rcen, dcen, ra, dec, Memd[xi], Memd[eta], npts)

	# Build the normal equations (equations of condition)
	call inormeq (Memd[x], Memd[y], Memd[xpm], Memd[ypm],
	    npts, MAX_COEFF)

	# Solve
	call fitsvd (Memd[xpm], Memd[xi], Memr[wgt], npts, xcoeff,
	    ncoeff, Memd[u], Memd[v], Memd[w], xchisq)
	xchisq = xchisq / double (npts - ncoeff - 1)

	call fitsvd (Memd[ypm], Memd[eta], Memr[wgt], npts, ycoeff,
	    ncoeff, Memd[u], Memd[v], Memd[w], ychisq)
	ychisq = ychisq / double (npts - ncoeff - 1)

	# CD matrix
	# deg/px    arcsec/mm   micron
	cdmtrx[1] = xcoeff[1] * xpxsiz / 3.6d6
	cdmtrx[2] = xcoeff[2] * ypxsiz / 3.6d6
	cdmtrx[3] = ycoeff[2] * ypxsiz / 3.6d6
	cdmtrx[4] = ycoeff[1] * xpxsiz / 3.6d6

	call sfree (sp)

	call printf ("Reference pixel: %.1f %.1f at:  %011.2h %011.1h\n")
	    call pargd (xcrpix)
	    call pargd (ycrpix)
	    call pargd (rcen/15.0)
	    call pargd (dcen)

	call printf ("Pixel size:  %6g X %6g microns\n")
	    call pargd (xpxsiz)
	    call pargd (ypxsiz)

	call printf ("Plate scale: %6g arcsec/mm\n")
	    call pargd (pltscl)

	call printf ("CD matrix:\t%8g\t%8g\n\t\t%8g\t%8g\n\n")
	    call pargd (cdmtrx[1])
	    call pargd (cdmtrx[2])
	    call pargd (cdmtrx[3])
	    call pargd (cdmtrx[4])

	call printf ("Coefficients of fit:\n")
	call printf ("\t\t Xi\t\t Eta\n")
	call printf ("Chi square\t%8g\t%8g\n")
	    call pargd (xchisq)
	    call pargd (ychisq)
	call printf ("Constant\t%8g\t%8g\n")
	    call pargd (xcoeff[3])
	    call pargd (ycoeff[3])
	call printf ("X\t\t%8g\t%8g\n")
	    call pargd (xcoeff[1])
	    call pargd (ycoeff[1])
	call printf ("Y\t\t%8g\t%8g\n")
	    call pargd (xcoeff[2])
	    call pargd (ycoeff[2])

	if (ncoeff < 7)
	    return

	call printf ("X**2\t\t%8g\t%8g\n")
	    call pargd (xcoeff[4])
	    call pargd (ycoeff[4])
	call printf ("XY\t\t%8g\t%8g\n")
	    call pargd (xcoeff[5])
	    call pargd (ycoeff[5])
	call printf ("Y**2\t\t%8g\t%8g\n")
	    call pargd (xcoeff[6])
	    call pargd (ycoeff[6])
	call printf ("X**2+Y**2\t%8g\t%8g\n")
	    call pargd (xcoeff[7])
	    call pargd (ycoeff[7])
end


procedure pxtomm (px, py, xcrpix, ycrpix, xpxsiz, ypxsiz, 
	x, y, npts)

#  PXTOMM -- Convert pixel coords to mm from reference pixel for
#  an array of stars

real	px[ARB], py[ARB]		# Pixel coordinates
double	xcrpix, ycrpix			# Reference pixel
double	xpxsiz, ypxsiz			# Pixel size (microns)
double	x[ARB], y[ARB]			# mm from reference
int	npts				# Number of points

int	i

begin
	do i = 1, npts {
	    #mm                                microns
#	    x[i] = (xcrpix - double (px[i])) * xpxsiz / 1.0d3
	    x[i] = (double (px[i]) - xcrpix) * xpxsiz / 1.0d3
	    y[i] = (double (py[i]) - ycrpix) * ypxsiz / 1.0d3
	}
end


procedure teqxev (rcen, dcen, ra, dec, xi, eta, npts)

#  TEQXEV -- Convert equatorial coordinates to standard coordinates
#  (arcsec from the reference coordinates) for an array of stars.

double	rcen, dcen			# Eq. coords of center in degrees
double	ra[ARB], dec[ARB]		# Eq. coords in degrees
double	xi[ARB], eta[ARB]		# Eq. coords in arcsec from reference
int	npts				# Number of data points

double	rcr, dcr			# Eq. coords of center in radians
int	i
double	rs, ds
double	xir, etar
double	div
double	diff

begin
	# Convert equatorial coordinates of center to radians
	rcr = DEGTORAD(rcen)
	rcr = rcen / 57.295779513082320877d0
	dcr = DEGTORAD(dcen)
	dcr = dcen / 57.295779513082320877d0
	diff = rcr - rs

	do i = 1, npts {
	    rs  = DEGTORAD(ra[i])
	    rs  = ra[i] / 57.295779513082320877d0
	    ds  = DEGTORAD(dec[i])
	    ds  = dec[i] / 57.295779513082320877d0

	    div = dsin (ds) * dsin (dcr) +
	          dcos (ds) * dcos(dcr) * dcos (rs - rcr)
	    xir = dcos (ds) * dsin (rs - rcr) / div
	    xi[i] = RADTOSEC(xir)

	    etar = (dsin (ds) * dcos (dcr) -
		    dcos (ds) * dsin (dcr) * dcos (rs - rcr)) / div
	    eta[i] = RADTOSEC(etar)

	    call treqst (rcr, dcr, rs, ds, xi[i], eta[i])
	}
end


procedure inormeq (x, y, xpm, ypm, npts, ncoeff)

# Build the normal equations (equations of condition)

double	x[ARB], y[ARB]				# Pixel coords (mm)
double	xpm[npts,ncoeff], ypm[npts,ncoeff]	# Plate model
int	npts					# Number of data points
int	ncoeff					# Number of coefficients

int	i
double	xref, yref

begin
	do i = 1, npts {
	    xref = x[i]
	    yref = y[i]

	    xpm[i,1] = xref
	    xpm[i,2] = yref
	    xpm[i,3] = 1.0d0
	    xpm[i,4] = xref * xref
	    xpm[i,5] = xref * yref
	    xpm[i,6] = yref * yref
	    xpm[i,7] = xref * xref + yref * yref

	    ypm[i,1] = yref
	    ypm[i,2] = xref
	    ypm[i,3] = 1.0d0
	    ypm[i,4] = yref * yref
	    ypm[i,5] = xref * yref
	    ypm[i,6] = xref * xref
	    ypm[i,7] = xref * xref + yref * yref
	}
end


procedure pixeqv (px, py, valid, nstars,
	racen, deccen, xcrpix, ycrpix, xpxsiz, ypxsiz,
	xcoeff, ycoeff, ncoeff)

real	px[ARB], py[ARB]		# Pixel coordinates
int	valid[ARB]			# Valid?
int	nstars				# Number of stars
double	racen, deccen			# Reference coordinates (degrees)
double	xcrpix, ycrpix			# Reference pixel
double	xpxsiz, ypxsiz			# Pixel size (microns)
double	xcoeff[ARB], ycoeff[ARB]	# Plate solution coefficients
int	ncoeff				# Number of coefficients

double	ra, dec				# Equatorial coordinates in degrees
int	star

begin
	do star = 1, nstars {
	    # For each star

	    if (valid[star] == NO)
		next

	    # Find the equatorial coordinates
	    call rpixtoeq (px[star], py[star], racen, deccen, 
		xcrpix, ycrpix, xpxsiz, ypxsiz, xcoeff, ycoeff, ncoeff,
		ra, dec)

	    call printf ("%6d %10.6g %10.6g  %011.2h %011.1h\n")
		call pargi (star)
		call pargr (px[star])
		call pargr (py[star])
		call pargd (ra/15.0)
		call pargd (dec)
	}
end

int procedure acount (value, vec, nvec)

int	value				# Value to be looked for
int	vec[ARB]			# Vector to look for it in
int	nvec				# Length of vector

int	count, ivec

begin
	# Get a count on the number of values in a vector

	count = 0
	do ivec = 1, nvec {
	    if (vec[ivec] == value)
		count = count + 1
	}

	return (count)
end
