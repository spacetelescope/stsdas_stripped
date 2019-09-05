include <imhdr.h>
include <math.h>		# for DEGTORAD
include <tbset.h>

# values for unit_id:
define	DEGREES		1
define	ARCSEC		2
define	MS		3
define	MILLIARCSEC	4

# jimage -- create a jitter image
# This task reads jitter information from a table and creates an image.
# The input jitter info consists of U2 and U3 positions at different times.
#
# Phil Hodge, 13-Feb-1992  Task created.
# Phil Hodge, 24-Dec-1992  Use median instead of mean for center position.
# Phil Hodge,  1-Feb-1993  Use amedr instead of gqsort to get median;
#			change name of jmedian subroutine to jgetu2u3.
# Phil Hodge, 10-May-1995  Add info to message about points missing image.
# Phil Hodge, 15-May-1995  Change error message in jgetu2u3; normalize by
#			number of non-indef values; add header parameters
#			for statistics; convert to double precision.

procedure t_jimage()

pointer intable			# scratch for table name
pointer output			# scratch for output image name
char	u2col[SZ_COLNAME]	# name of column for U2
char	u3col[SZ_COLNAME]	# name of column for U3
double	pixsize			# pixel size (e.g. arcseconds/pixel)
double	angle			# angle in degrees from X clockwise to U2
int	nx, ny			# size of output image
bool	flip_u3			# change sign of U3?
#--
pointer sp
pointer message		# scratch for possible error message
pointer unit2, unit3	# scratch for units for U2 and U3 columns
pointer tp		# pointer to table descriptor
pointer cpu2, cpu3	# pointers to column descriptors for X & Y
pointer pu2, pu3	# U2, U3 values
pointer im		# pointer to image header descriptor
pointer v		# pointer to image data
double	cosa, sina	# cos & sin of angle
double	u2, u3		# position from table
double	x, y		# x,y pixel coordinates in image
double	u2_0, u3_0	# medians of U2, U3 (i.e. center of jitter)
double	smaj		# standard deviation of (u2,u3) along major axis
double	smin		# std dev along minor axis of jitter ellipse
double	pa		# position angle of major axis
double	scale		# to convert units to degrees
int	unit_id		# identifier for units
int	ix0, iy0	# center of image
int	nrows		# number of rows in table
int	npts		# number of rows that are not indef
int	n		# number of U2,U3 points within image
int	i		# loop index
pointer tbtopn()
pointer immap(), imps2r()
double	clgetd()
int	clgeti()
int	strdic()
int	tbpsta()
bool	clgetb(), strne()

begin
	call smark (sp)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (message, SZ_FNAME, TY_CHAR)
	call salloc (unit2, SZ_FNAME, TY_CHAR)
	call salloc (unit3, SZ_FNAME, TY_CHAR)

	call clgstr ("intable", Memc[intable], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	nx = clgeti ("nx")
	ny = clgeti ("ny")
	pixsize = clgetd ("pixsize")
	angle = clgetd ("angle")
	if (pixsize == 0.d0)
	    call error (1, "pixsize can't be zero")
	call clgstr ("u2col", u2col, SZ_COLNAME)
	call clgstr ("u3col", u3col, SZ_COLNAME)
	flip_u3 = clgetb ("flip_u3")

	cosa = cos (DEGTORAD(angle))
	sina = sin (DEGTORAD(angle))

	tp = tbtopn (Memc[intable], READ_ONLY, NULL)
	call tbtnam (tp, Memc[intable], SZ_FNAME)	# get full name
	nrows = tbpsta (tp, TBL_NROWS)

	# Find U2 and U3 columns.
	call tbcfnd (tp, u2col, cpu2, 1)
	call tbcfnd (tp, u3col, cpu3, 1)
	if (cpu2 == NULL || cpu3 == NULL) {
	    call sprintf (Memc[message], SZ_FNAME,
		"column `%s' and/or `%s' not found in table")
		call pargstr (u2col)
		call pargstr (u3col)
	    call error (1, Memc[message])
	}

	# Get units for U2 & U3.
	call tbcigt (cpu2, TBL_COL_UNITS, Memc[unit2], SZ_FNAME)
	call tbcigt (cpu3, TBL_COL_UNITS, Memc[unit3], SZ_FNAME)
	if (strne (Memc[unit2], Memc[unit3])) {
	    call eprintf (
		"warning:  units for `%s' and `%s' are not the same\n")
		call pargstr (u2col)
		call pargstr (u3col)
	    scale = 3600.d0			# assume arcseconds
	}
	unit_id = strdic (Memc[unit2], Memc[unit2], SZ_FNAME,
		"|degrees|arcseconds|ms|milliarcseconds|")
	switch (unit_id) {
	case DEGREES:
	    scale = 1.d0
	case ARCSEC:
	    scale = 3600.d0
	case MS, MILLIARCSEC:
	    scale = 3600000.d0
	default:
	    if (Memc[unit2] != EOS)
		call eprintf ("warning:  unrecognized units for U2 & U3\n")
	    scale = 3600.d0
	}

	# Get U2 & U3 data, and compute median values.
	call jgetu2u3 (tp, cpu2, cpu3, pu2, pu3, npts)

	# Compute statistics.  If flip_u3 = true, we'll also flip the
	# U3 coordinates around the median value u3_0.
	call jim_stat (Memd[pu2], Memd[pu3], npts, flip_u3,
		u2_0, u3_0, smaj, smin, pa)

	# Add angle to position angle.
	pa = pa + angle
	pa = mod (pa, 360.d0)
	if (pa < 0.d0)
	    pa = pa + 360.d0

	# Create output image.
	im = immap (Memc[output], NEW_IMAGE, NULL)
	IM_LEN(im,1) = nx
	IM_LEN(im,2) = ny
	IM_NDIM(im) = 2
	IM_PIXTYPE(im) = TY_REAL

	# Center of image.
	ix0 = (nx + 1) / 2
	iy0 = (ny + 1) / 2

	# Get pointer to output image data, and initialize it to zero.
	v = imps2r (im, 1, nx, 1, ny)
	call aclrr (Memr[v], nx * ny)

	# Increment the value of a pixel whenever the (X,Y) position
	# from the table falls within the pixel.
	n = 0
	do i = 0, npts-1 {			# zero indexed

	    # Subtract median position and scale according to the pixel size.
	    u2 = (Memd[pu2+i] - u2_0) / pixsize
	    u3 = (Memd[pu3+i] - u3_0) / pixsize

	    # Rotate.  If angle = 0, x is aligned with U2 and y with -U3.
	    x = u2 * cosa - u3 * sina + ix0
	    y = u2 * sina + u3 * cosa + iy0

	    # If the pixel is within the image, increment the value.
	    call jimfill (Memr[v], nx, ny, x, y, n)
	}

	# Normalize the image values to a volume of one.
	call adivkr (Memr[v], real(npts), Memr[v], nx*ny)

	if (n < npts) {
	    if (n == npts - 1) {
		call printf ("%s:  one point out of %d missed the image\n")
		    call pargstr (Memc[intable])
		    call pargi (npts)
	    } else {
		call printf ("%s:  %d points out of %d missed the image\n")
		    call pargstr (Memc[intable])
		    call pargi (npts - n)
		    call pargi (npts)
	    }
	}

	# Add header parameters.
	call imastr (im, "intable", Memc[intable])
	call imastr (im, "u2column", u2col)
	call imastr (im, "u3column", u3col)
	call imastr (im, "colunits", Memc[unit2])
	call imaddr (im, "angle", real (angle))
	call imaddb (im, "flip_u3", flip_u3)
	call imastr (im, "ctype1", "PIXEL")
	call imastr (im, "ctype2", "PIXEL")
	call imaddi (im, "crpix1", ix0)
	call imaddi (im, "crpix2", iy0)
	call imaddd (im, "cd1_1", pixsize/scale * cosa) 
	call imaddd (im, "cd1_2", pixsize/scale * sina)
	call imaddd (im, "cd2_1", pixsize/scale * sina)
	call imaddd (im, "cd2_2", -pixsize/scale * cosa)
	call imaddd (im, "crval1", u2_0/scale)
	call imaddd (im, "crval2", u3_0/scale)
	call imaddi (im, "ntotal", npts)
	call imaddi (im, "nmissed", npts-n)
	call imaddr (im, "sigma_a", real (smaj))
	call imaddr (im, "sigma_b", real (smin))
	call imaddr (im, "posangle", real (pa))

	call imunmap (im)
	call tbtclo (tp)

	call mfree (pu3, TY_DOUBLE)		# allocated by jgetu2u3
	call mfree (pu2, TY_DOUBLE)
	call sfree (sp)
end

# jgetu2u3 -- get U2,U3 values and their medians
# This routine reads U2,U3 values from two rows of a table and returns
# the medians of U2 and U3.  If either U2 or U3 is INDEF for a row,
# that row will be ignored.
# Pointers pu2 and pu3 are allocated by this routine, and they should
# be deallocated by the calling routine.

procedure jgetu2u3 (tp, cpu2, cpu3, pu2, pu3, npts)

pointer tp		# i: pointer to table struct
pointer cpu2, cpu3	# i: pointers to U2 & U3 column descriptors
pointer pu2		# o: pointer to U2 values
pointer pu3		# o: pointer to U3 values
int	npts		# o: size of U2, U3 arrays
#--
double	u2, u3		# temporary values of U2 & U3
int	row		# loop index for row number
int	nrows		# number of rows in table
int	tbpsta()

begin
	nrows = tbpsta (tp, TBL_NROWS)
	if (nrows <= 0)
	    call error (1, "There are no rows in the input table.")

	# Allocate memory for U2, U3 data.
	call malloc (pu2, nrows, TY_DOUBLE)
	call malloc (pu3, nrows, TY_DOUBLE)

	# Get values of U2 and U3.
	npts = 0
	do row = 1, nrows {
	    call tbegtd (tp, cpu2, row, u2)
	    call tbegtd (tp, cpu3, row, u3)
	    if (!IS_INDEFD(u2) && !IS_INDEFD(u3)) {
		Memd[pu2+npts] = u2
		Memd[pu3+npts] = u3
		npts = npts + 1
	    }
	}
	if (npts <= 0)
	    call error (1, "All data in table are INDEF.")
end

# jimfill -- assign one pixel value in image
# This routine adds one to one pixel in the output image and increments
# the value of n if the specified pixel location is within the image.

procedure jimfill (image, nx, ny, x, y, n)

real	image[nx, ny]		# io:  image to be written to
int	nx, ny			# i: size of image
double	x, y			# i: location of point to be added to image
int	n			# io: number of points added to image
#--
int	ix, iy			# x, y rounded to integer

begin
	# Round to integer.
	ix = nint (x)
	iy = nint (y)

	# Assign the value and increment counter.
	if (ix >= 1 && ix <= nx && iy >= 1 && iy <= ny) {
	    image[ix,iy] = image[ix,iy] + 1.
	    n = n + 1
	}
end

# jim_stat -- compute statistics
# This routine first computes the medians of U2 and U3.  If flip_u3 = true,
# the U3 coordinates are "flipped" around the median u3_0 of U3.  Then
# the RMS of the jitter is computed.  Two values for the RMS are computed,
# one parallel to the major axis of the jitter and one parallel to the
# minor axis.  The position angle of the major axis (counterclockwise
# from the +X axis) is also returned.  These computations were based on
# source code written by Kavan Ratnatunga.

procedure jim_stat (u2, u3, npts, flip_u3, u2_0, u3_0, smaj, smin, pa)

double	u2[npts]	# i: array of U2 values
double	u3[npts]	# io: array of U3 values
int	npts		# i: size of arrays
bool	flip_u3		# i: change sign of U3?
double	u2_0, u3_0	# o: medians of U2 & U3
double	smaj		# o: standard deviation of (u2,u3) along major axis
double	smin		# o: std dev along minor axis of jitter ellipse
double	pa		# o: position angle of major axis
#--
double	x, y		# one u2,u3 pair
double	sx, sy, sx2, sy2, sxy
double	cpa, spa	# cosine & sine of position angle
double	temp
int	i
double	amedd()

begin
	if (npts < 2) {
	    smaj = 0.d0
	    smin = 0.d0
	    pa = 0.d0
	    return
	}

	# Compute medians.
	u2_0 = amedd (u2, npts)
	u3_0 = amedd (u3, npts)

	# If we are to change the parity, do it by flipping around U3 = u3_0.
	if (flip_u3) {
	    do i = 1, npts
		u3[i] = u3_0 - u3[i]
	}

	sx = 0.d0
	sy = 0.d0
	sx2 = 0.d0
	sy2 = 0.d0
	sxy = 0.d0

	# Increment sums.
	do i = 1, npts {
	    x = u2[i] - u2_0
	    y = u3[i] - u3_0
	    sx = sx + x
	    sy = sy + y
	    sx2 = sx2 + x**2
	    sy2 = sy2 + y**2
	    sxy = sxy + x * y
	}

	# Compute standard deviations.
	sx = sx / double(npts)
	sy = sy / double(npts)
	sx2 = sx2 / double(npts) - sx * sx
	sy2 = sy2 / double(npts) - sy * sy
	sxy = sxy / double(npts) - sx * sy

	# Compute position angle.
	if (sx2 == sy2 && sxy == 0.d0)
	    pa = 0.d0
	else
	    pa = 0.5d0 * atan2 (2.d0 * sxy, sx2 - sy2) + PI
	cpa = cos (pa)
	spa = sin (pa)
	smaj = sqrt (sx2*cpa**2 + sy2*spa**2 + 2.d0*sxy*cpa*spa)
	smin = sqrt (sx2*spa**2 + sy2*cpa**2 - 2.d0*sxy*cpa*spa)

	# Interchange major and minor axes?
	if (smin > smaj) {
	    temp = smaj
	    smaj = smin
	    smin = temp
	    pa = pa + HALFPI
	}

	pa = RADTODEG (pa)
	pa = mod (pa, 360.d0)
	if (pa < 0.d0)
	    pa = pa + 360.d0
end
