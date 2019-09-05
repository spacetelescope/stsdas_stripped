include <imhdr.h>
include <imio.h>		# for IM_VOFF, IM_VSTEP
include <math.h>		# for DEGTORAD
include <mwset.h>		# for MW_NPHYSDIM

define	FLAT		1.e-5	# smaller slope than this is flat
define	ARROW_SIZE	0.25	# size of arrow head as fraction of arrow length
define	ARROW_ANGLE	160.	# angle of arrow head

# compass -- draw north and east arrows
# This task draws north and east arrows on an image or on an image display.
# The north arrow has an arrow head, and the east arrow does not, in order
# to clearly distinguish the two directions.  The image must be 2-D.
#
# The arrows will be drawn on the image display if the output parameter
# is null.  If an image name is specified by output, pixel values in the
# image with that name will be assigned (permanently modifying the image)
# in order to draw the north and east arrows.
#
# Phil Hodge, 29-Sep-1992  Task created.
# Phil Hodge, 29-Jun-2000  Change the data type of 'angle' to a string,
#		so that its value can be either a specific number or the
#		name of a keyword to use for getting the value from a header.
#		Add subroutine ne_angle to do the conversion.

procedure compass()

pointer ilist, olist		# imt pointers for input & output
real	xpos, ypos		# position for center of compass (pixels)
pointer angle			# degrees from north eastward toward Y axis
real	length			# length of line segments to draw (pixels)
real	lineval			# value to put in image for center of line
real	clearval		# value for image at edge of line
real	width			# line width in pixels
bool	verbose
pointer device
int	frame			# frame number from display task
#--
pointer sp
pointer input			# name of an input image
pointer output			# name of an output image
pointer iim, oim		# for input, output images
pointer iv, ov			# pointers to image data for copying
pointer gp
real	north[2]		# north vector
real	east[2]			# east vector
real	ahead[2]		# line segment for arrow head
real	left, right		# viewport
real	bottom, top		# viewport
real	x1, x2, y1, y2		# window
real	lval, cval		# lineval & clearval for a particular image
real	x, y			# vector elements
real	x_off, y_off		# ignored
int	line			# line number in image for copying
int	junk
bool	inplace			# true if we're writing into input image
bool	fill
pointer imtopenp()
int	imtlen(), imtgetim()
pointer immap(), imgl2r(), impl2r()
pointer gopen()
real	clgetr()
int	clgeti()
bool	clgetb()
int	strncmp()
bool	streq()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (angle, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)

	ilist = imtopenp ("input")
	olist = imtopenp ("output")

	xpos = clgetr ("xpos")
	ypos = clgetr ("ypos")
	call clgstr ("angle", Memc[angle], SZ_FNAME)
	length = clgetr ("length")
	verbose = clgetb ("verbose")

	# If output images were specified, draw on the images.
	if (imtlen (olist) > 0) {

	    # Get the data values and the line width.
	    width = clgetr ("width")
	    lineval = clgetr ("lineval")
	    clearval = clgetr ("clearval")

	    if (imtlen (ilist) != imtlen (olist))
		call error (1, "input and output lists must be the same length")

	    while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
		junk = imtgetim (olist, Memc[output], SZ_FNAME)

		inplace = streq (Memc[input], Memc[output])

		if (inplace) {
		    # We will be modifying the image in-place.
		    if (verbose) {
			call printf ("%s (in-place)\n")
			    call pargstr (Memc[output])
			call flush (STDOUT)
		    }
		    oim = immap (Memc[output], READ_WRITE, NULL)
		    call ne_section (oim, xpos, ypos, x, y)

		} else {
		    # Copy to an output image.
		    if (verbose) {
			call printf ("%s --> %s\n")
			    call pargstr (Memc[input])
			    call pargstr (Memc[output])
			call flush (STDOUT)
		    }
		    iim = immap (Memc[input], READ_ONLY, NULL)
		    oim = immap (Memc[output], NEW_COPY, iim)
		    do line = 1, IM_LEN(iim,2) {
			iv = imgl2r (iim, line)
			ov = impl2r (oim, line)
			call amovr (Memr[iv], Memr[ov], IM_LEN(iim,1))
		    }
		    # Correct for image section, if one was specified.
		    call ne_section (iim, xpos, ypos, x, y)
		    call imunmap (iim)
		}

		# If lineval and/or clearval is INDEF, replace with the
		# maximum and/or minimum image data value respectively.
		lval = lineval			# values for current image
		cval = clearval
		if (IS_INDEF(lval) || IS_INDEF(cval))
		    call ne_minmax (oim, lval, cval)

		# Draw the compass in the output image at (x,y).
		call ne_draw (oim, x, y, Memc[angle], length,
			    lval, cval, width)
		call imunmap (oim)
	    }

	    call imtclose (olist)

	} else {

	    # Write to a graphics device, e.g. imd.

	    if (imtlen (ilist) > 1)
		call error (1,
	"can't have more than one input image when writing to graphics device")

	    if (imtgetim (ilist, Memc[input], SZ_FNAME) == EOF)
		call error (1, "input must be specified")

	    call clgstr ("device", Memc[device], SZ_FNAME)
	    frame = clgeti ("frame")
	    if (verbose) {
		call printf ("Drawing on %s device, frame %d.\n")
		    call pargstr (Memc[device])
		    call pargi (frame)
		call flush (STDOUT)
	    }

	    iim = immap (Memc[input], READ_ONLY, NULL)

	    # Modify xpos,ypos for image section, if one was specified.
	    call ne_section (iim, xpos, ypos, xpos, ypos)

	    gp = gopen (Memc[device], APPEND, STDGRAPH)

	    if (strncmp (Memc[device], "imd", 3) == 0) {
		# We're drawing on the image display device.  Calculate
		# what values are appropriate for the viewport, and set
		# the viewport and window.  (Copied from wcslab.)
		x1 = 1.
		x2 = IM_LEN(iim,1)
		y1 = 1.
		y2 = IM_LEN(iim,2)
		left = INDEF; right = INDEF; bottom = INDEF; top = INDEF
		call wl_imd_viewport (frame, iim, x1, x2, y1, y2,
			    left, right, bottom, top, x_off, y_off)
		fill = true
		call wl_map_viewport (gp, x1, x2, y1, y2,
			    left, right, bottom, top, fill)
	    }

	    # Get the directions to north and east.
	    call ne_angle (Memc[angle], iim, length, north, east)

	    # Draw north arrow.
	    call gamove (gp, xpos, ypos)
	    call gadraw (gp, xpos+north[1], ypos+north[2])

	    # Draw arrow head.
	    x = xpos + north[1]
	    y = ypos + north[2]
	    call gamove (gp, x, y)
	    # Reduce size of north vector to size of arrow head.
	    north[1] = north[1] * ARROW_SIZE
	    north[2] = north[2] * ARROW_SIZE
	    call ne_rotate (north, ARROW_ANGLE, ahead)
	    call gadraw (gp, x+ahead[1], y+ahead[2])
	    call gamove (gp, x, y)
	    call ne_rotate (north, -ARROW_ANGLE, ahead)
	    call gadraw (gp, x+ahead[1], y+ahead[2])

	    # Draw east arrow.
	    call gamove (gp, xpos, ypos)
	    call gadraw (gp, xpos+east[1], ypos+east[2])

	    call gclose (gp)

	    call imunmap (iim)
	}
	call imtclose (ilist)
	call sfree (sp)
end

# ne_draw -- get north and east vectors
# This routine computes the north and east vectors, in image pixels.
# If angle is null or indef (the default), the image CD matrix is used
# to compute the north and east directions.  If angle contains a numeric
# value, that is taken as the orientation of the Y axis (second axis)
# eastward from north.  If angle contains a character string, that is
# assumed to be an image header keyword containing the angle.

procedure ne_angle (angle, im, length, north, east)

char	angle[ARB]	# i: null; explicit value; or keyword name
pointer im		# i: imio pointer
real	length		# i: length of line segments to draw (pixels)
real	north[2]	# o: north vector
real	east[2]		# o: east vector
#--
double	cd[2,2]			# CD matrix
real	xangle			# value of angle read from param or keyword
int	ip, nchar, ctor()
int	strncmp(), strlen()
int	imaccf()
real	imgetr()

begin
	if (angle[1] == EOS || strncmp (angle, "INDEF", 5) == 0) {
	    call ne_cd_get (im, cd)
	    call ne_vectors (cd, length, north, east)
	} else {
	    ip = 1
	    nchar = ctor (angle, ip, xangle)
	    if (nchar <= 0 || ip < strlen (angle)) {
		# angle is character type, not numeric
		if (imaccf (im, angle) != YES) {
		    call eprintf ("keyword = %s not found in header\n")
			call pargstr (angle)
		    call error (1, "")
		}
		xangle = imgetr (im, angle)
	    }
	    north[1] = length * sin (DEGTORAD(xangle))
	    north[2] = length * cos (DEGTORAD(xangle))
	    east[1] = length * sin (DEGTORAD(xangle-90.))
	    east[2] = length * cos (DEGTORAD(xangle-90.))
	}
end

# ne_draw -- draw lines in-place in an image
# This routine calls ne_line to draw each of the line segments
# that make up the north & east arrows and the arrow head on north.

procedure ne_draw (im, xpos, ypos, angle, length,
		lineval, clearval, width)

pointer im		# i: for image to be written to
real	xpos, ypos	# i: pixel location of compass in image
char	angle[ARB]	# i: orientation of Y axis, eastward from north
real	length		# i: length in pixels of each line to draw
real	lineval		# i: value to put in image at center of line
real	clearval	# i: value to put in image at edge of line
real	width		# i: line width in pixels
#--
pointer iv, ov		# pointers to input, output data in section
real	north[2]	# north vector
real	east[2]		# east vector
real	vtemp[2]	# scratch vector
real	ahead[2]	# vector for line segment of arrow head
real	nx, ny		# end of north vector
real	ex, ey		# end of east vector
real	ah1x, ah1y	# end of one line segment for arrow head
real	ah2x, ah2y	# end of the other line segment for arrow head
real	xr1, xr2, yr1, yr2	# coordinates relative to image section
int	ix1, ix2, iy1, iy2	# limits of image section containing compass
int	npix1, npix2		# size of image section
pointer imgs2r(), imps2r()

begin
	# Get the north and east vectors.
	call ne_angle (angle, im, length, north, east)

	# Add center position to get image pixel coordinates
	# of endpoints of line segments.
	nx = xpos + north[1]
	ny = ypos + north[2]
	ex = xpos + east[1]
	ey = ypos + east[2]

	#Get the endpoints of the lines for the arrow head.
	vtemp[1] = north[1] * ARROW_SIZE
	vtemp[2] = north[2] * ARROW_SIZE
	call ne_rotate (vtemp, ARROW_ANGLE, ahead)
	ah1x = nx + ahead[1]
	ah1y = ny + ahead[2]
	call ne_rotate (vtemp, -ARROW_ANGLE, ahead)
	ah2x = nx + ahead[1]
	ah2y = ny + ahead[2]

	# These are the limits of the image section to contain the compass.
	ix1 = min (nx, ex, ah1x, ah2x, xpos) - width		# truncate
	iy1 = min (ny, ey, ah1y, ah2y, ypos) - width
	ix2 = max (nx, ex, ah1x, ah2x, xpos) + width + 1	# round up
	iy2 = max (ny, ey, ah1y, ah2y, ypos) + width + 1
	ix1 = max (ix1, 1)
	iy1 = max (iy1, 1)
	ix2 = min (ix2, IM_LEN(im,1))
	iy2 = min (iy2, IM_LEN(im,2))

	npix1 = ix2 - ix1 + 1
	npix2 = iy2 - iy1 + 1

	# Get the image section containing the arrow.
	iv = imgs2r (im, ix1, ix2, iy1, iy2)
	ov = imps2r (im, ix1, ix2, iy1, iy2)
	call amovr (Memr[iv], Memr[ov], npix1*npix2)

	# These are the endpoints of a line to be drawn, but the
	# coordinates are relative to the image section.
	xr1 = xpos - ix1 + 1.
	yr1 = ypos - iy1 + 1.
	xr2 = nx - ix1 + 1.
	yr2 = ny - iy1 + 1.

	# Draw the line pointing north.
	call ne_line (Memr[ov], npix1, npix2, xr1, yr1, xr2, yr2,
		lineval, clearval, width)

	xr2 = ex - ix1 + 1.
	yr2 = ey - iy1 + 1.

	# Draw the line pointing east.
	call ne_line (Memr[ov], npix1, npix2, xr1, yr1, xr2, yr2,
		lineval, clearval, width)

	# Draw the arrow head on the north line.
	xr1 = nx - ix1 + 1.
	yr1 = ny - iy1 + 1.
	xr2 = ah1x - ix1 + 1.
	yr2 = ah1y - iy1 + 1.
	call ne_line (Memr[ov], npix1, npix2, xr1, yr1, xr2, yr2,
		lineval, clearval, width)
	xr2 = ah2x - ix1 + 1.
	yr2 = ah2y - iy1 + 1.
	call ne_line (Memr[ov], npix1, npix2, xr1, yr1, xr2, yr2,
		lineval, clearval, width)
end

# ne_line -- draw one line

procedure ne_line (v, npix1, npix2, xr1, yr1, xr2, yr2,
		lval, cval, width)

real	v[npix1,npix2]		# io: image section to be overwritten
int	npix1, npix2		# i: size of image section
real	xr1, yr1, xr2, yr2	# i: line endpoints relative to image section
real	lval			# i: value to put in image at center of line
real	cval			# i: value to put in image at edge of line
real	width			# i: line width in pixels
#--
real	x1, y1, x2, y2	# like xr1, etc, but perhaps endpoints swapped
real	miny, maxy	# min & max of yr1 & yr2
real	slope		# slope of line to be drawn
real	pslope		# slope of line perpendicular to line to be drawn
real	xc, yc		# closest point on line to a pixel
real	dx, dy		# offset of current pixel from closest point on line
real	d		# perpendicular distance (pixels) from pixel to line
real	hwidth		# half of width
real	val		# a value to be put into image section
int	xlow, xhigh	# x limits of do loop
int	ylow, yhigh	# y limits of do loop
int	i, j

begin
	# Swap the endpoints if necessary to ensure that the first point
	# is to the left of the second point.
	if (xr1 > xr2) {
	    x1 = xr2
	    y1 = yr2
	    x2 = xr1
	    y2 = yr1
	} else {
	    x1 = xr1
	    y1 = yr1
	    x2 = xr2
	    y2 = yr2
	}
	miny = min (yr1, yr2)
	maxy = max (yr1, yr2)

	ylow = miny - width / 2.		# truncate
	yhigh = maxy + width / 2. + 1.		# round up
	ylow = max (ylow, 1)
	yhigh = min (yhigh, npix2)

	if (abs (y2 - y1) < FLAT * abs (x2 - x1)) {

	    # Horizontal line.

	    yc = (y1 + y2) / 2.

	    # Fill the lines.
	    do j = ylow, yhigh {
		do i = 1, npix1 {		# fill the current line
		    if (i < x1) {
			dx = i - x1
			dy = j - y1
			d = sqrt (dx**2 + dy**2) / width
		    } else if (i > x2) {
			dx = i - x2
			dy = j - y2
			d = sqrt (dx**2 + dy**2) / width
		    } else {
			d = (yc - j) / width
			d = abs (d)
		    }
		    if (d >= 0.5)
			next
		    else if (d < 0.25)
			# We're close to the center, so use center value.
			val = lval
		    else
			# Interpolate from lval near center to cval at edge.
			val = (0.5 - d) * 4. * (lval - cval) + cval

		    if (lval > cval)
			v[i,j] = max (v[i,j], val)
		    else
			v[i,j] = min (v[i,j], val)
		}
	    }

	} else if (abs (x2 - x1) < FLAT * abs (y2 - y1)) {

	    # Vertical line.

	    xc = (x1 + x2) / 2.

	    xlow = xc - width / 2.
	    xhigh = xc + width / 2. + 1.
	    xlow = max (1, xlow)
	    xhigh = min (npix1, xhigh)

	    do j = ylow, yhigh {

		do i = xlow, xhigh {

		    if (j < miny) {
			dx = i - xc
			dy = j - miny
			d = sqrt (dx**2 + dy**2) / width
		    } else if (j > maxy) {
			dx = i - xc
			dy = j - maxy
			d = sqrt (dx**2 + dy**2) / width
		    } else {
			d = (i - xc) / width
			d = abs (d)
		    }

		    if (d >= 0.5)
			next
		    else if (d < 0.25)
			val = lval
		    else
			val = (0.5 - d) * 4. * (lval - cval) + cval

		    if (lval > cval)
			v[i,j] = max (v[i,j], val)
		    else
			v[i,j] = min (v[i,j], val)
		}
	    }

	} else {

	    # Get the tangent (slope) that the
	    # line to be drawn makes with the +X direction.
	    slope = (y2 - y1) / (x2 - x1)
	    pslope = - (x2 - x1) / (y2 - y1)

	    hwidth = width / 2.

	    do j = ylow, yhigh {

		do i = 1, npix1 {

		    # Find the point (xc,yc) on the line that is closest to
		    # the current pixel.
		    xc = (x1 * slope - i * pslope + j - y1) / (slope - pslope)
		    yc = pslope * (xc - i) + j

		    if (xc < x1) {
			# Left endpoint is closest.
			xc = x1
			yc = y1
		    } else if (xc > x2) {
			# Right endpoint is closest.
			xc = x2
			yc = y2
		    }

		    # Distance squared from current pixel to (xc,yc).
		    d = (xc - i)**2 + (yc - j)**2
		    if (d > hwidth**2)
			next

		    d = sqrt (d) / width	# distance in fraction of width

		    if (d < 0.25)
			val = lval
		    else
			val = (0.5 - d) * 4. * (lval - cval) + cval

		    if (lval > cval)
			v[i,j] = max (v[i,j], val)
		    else
			v[i,j] = min (v[i,j], val)
		}
	    }
	}
end

# ne_section -- correct for image section
# This routine is needed if the image name includes a section.  The
# input position (xpos,ypos) is given in pixels relative to the image
# itself, not relative to the section.  The output position (x,y) is
# the pixel location relative to the image section.

procedure ne_section (im, xpos, ypos, x, y)

pointer im		# i: imhdr pointer for input image
real	xpos, ypos	# i: coordinates in image without section
real	x, y		# o: position relative to section
#--

begin
	x = (xpos - IM_VOFF(im,1) + (IM_VSTEP(im,1) - 1.) / 2.) / IM_VSTEP(im,1)
	y = (ypos - IM_VOFF(im,2) + (IM_VSTEP(im,2) - 1.) / 2.) / IM_VSTEP(im,2)
end

# ne_cd_get -- get the cd matrix

procedure ne_cd_get (im, cd)

pointer im		# i: pointer to imhdr struct
double	cd[2,2]		# o: CD matrix
#--
pointer mw

# These are coordinate parameters uncorrected for the image section.
double	crval[IM_MAXDIM]
double	crpix[IM_MAXDIM]
double	o_cd[IM_MAXDIM,IM_MAXDIM]

double	n_cd[IM_MAXDIM,IM_MAXDIM]	# CD matrix corrected for section

double	ltm[IM_MAXDIM,IM_MAXDIM]	# lterm matrix
double	i_ltm[IM_MAXDIM,IM_MAXDIM]	# inverse of ltm
double	ltv[IM_MAXDIM]			# lterm vector

int	wcsdim				# dimension of mwcs coordinates
pointer mw_openim()
int	mw_stati()

begin
	mw = mw_openim (im)
	wcsdim = mw_stati (mw, MW_NPHYSDIM)		# get mwcs dimension

	# Get the wterm and the lterm.
	call mw_gwtermd (mw, crpix, crval, o_cd, wcsdim)
	call mw_gltermd (mw, ltm, ltv, wcsdim)

	# Convert the wterm to be the values relative to the current
	# image section.  (Comments & code copied from mwcs.)

	# Output CD matrix = CD' =  (CD * inv(LTM)).
	call mw_invertd (ltm, i_ltm, wcsdim)
	call mw_mmuld (o_cd, i_ltm, n_cd, wcsdim)

	# Extract the CD matrix.
	call ne_cd_extract (mw, n_cd, wcsdim, cd)

	call mw_close (mw)
end

# ne_cd_extract -- extract cd matrix
# This routine is needed to take care of the situation where the dimension
# of the input image was reduced by taking an image section.  In that case,
# the coordinate information gotten using mwcs has the dimension of the
# original image, which results in two problems.  We need to know which
# axis of the original image maps to which axis of the image that we've
# got.  We also have to dimension the CD matrix differently, because when
# mwcs puts values into a 2-D array it is dimensioned wcsdim X wcsdim,
# but we declared it IM_MAXDIM X IM_MAXDIM in the calling routine, so we
# have to declare it wcsdim X wcsdim in this routine.

procedure ne_cd_extract (mw, n_cd, wcsdim, cd)

pointer mw			# i: mwcs pointer
double	n_cd[wcsdim,wcsdim]	# i: CD matrix
int	wcsdim			# i: dimension of wcs
double	cd[2,2]			# o: CD matrix extracted from n_cd
#--
int	ndim			# actual dimension of image
int	ax[IM_MAXDIM]		# physical axis number for each logical axis
int	axno[IM_MAXDIM]		# axis numbers
int	axval[IM_MAXDIM]
int	i, j

begin
	ndim = 2

	# Get the axis mapping.
	call mw_gaxmap (mw, axno, axval, wcsdim)

	# Find the image axis numbers corresponding to the mwcs numbers.
	ax[1] = 0
	ax[2] = 0
	do i = 1, wcsdim {
	    if (axno[i] == 1)
		ax[1] = i
	    if (axno[i] == 2)
		ax[2] = i
	}
	# If we can't get the axis numbers, just assign default values.
	if (ax[1] < 1 || ax[2] < 1)
	    do i = 1, wcsdim
		ax[i] = i

	# Extract the CD matrix.
	# Note that we transpose the CD matrix because of different
	# conventions regarding how a matrix is stored.
	do i = 1, ndim {
	    do j = 1, ndim
		cd[i,j] = n_cd[ax[j],ax[i]]	# transpose
	}
end

# ne_vectors -- get north and east vectors

procedure ne_vectors (cd, length, north, east)

double	cd[2,2]		# i: CD matrix
real	length		# i: length of vectors
real	north[2]	# o: vector pointing north
real	east[2]		# o: vector pointing east
#--
double	d		# determinant of CD matrix
double	x, y		# scratch for vector components
double	l		# length of a vector

begin
	d = cd[1,1] * cd[2,2] - cd[1,2] * cd[2,1]
	if (d == 0.d0)
	    call error (1, "CD matrix is singular")

	# North.
	x = -cd[1,2] / d
	y =  cd[1,1] / d

	# Normalize by the length and copy to output.
	l = sqrt (x**2 + y**2)
	north[1] = x * length / l
	north[2] = y * length / l

	# East.
	x =  cd[2,2] / d
	y = -cd[2,1] / d
	l = sqrt (x**2 + y**2)
	east[1] = x * length / l
	east[2] = y * length / l
end

# ne_rotate -- rotates a vector
# This routine rotates a vector by the specified angle.  A positive angle
# rotates the vector clockwise with respect to the coordinate system.
# The input and output vectors may be the same.

procedure ne_rotate (invec, angle, outvec)

real	invec[2]	# i: vector to rotate
real	angle		# i: angle by which to rotate
real	outvec[2]	# o: rotated vector
#--
real	cosa, sina	# cosine & sine of angle
real	temp

begin
	cosa = cos (DEGTORAD(angle))
	sina = sin (DEGTORAD(angle))

	temp      =  invec[1] * cosa + invec[2] * sina
	outvec[2] = -invec[1] * sina + invec[2] * cosa
	outvec[1] = temp
end

# ne_minmax -- replace INDEF with max or min

procedure ne_minmax (im, lineval, clearval)

pointer im		# i: for image to be written to
real	lineval		# io: value to put in image at center of line
real	clearval	# io: value to put in image at edge of line
#--
pointer v
real	dmin, dmax	# min & max data values
int	i, j
pointer imgl2r()

begin
	# Only do the work of finding the min & max if we have to.
	if (IS_INDEF(lineval) || IS_INDEF(clearval)) {

	    v = imgl2r (im, 1)
	    dmin = Memr[v]			# initial values
	    dmax = Memr[v]

	    # Find min & max values.
	    do j = 1, IM_LEN(im,2) {
		v = imgl2r (im, j)
		do i = 0, IM_LEN(im,1)-1 {
		    dmin = min (dmin, Memr[v+i])
		    dmax = max (dmax, Memr[v+i])
		}
	    }

	    if (IS_INDEF(lineval))
		lineval = dmax

	    if (IS_INDEF(clearval))
		clearval = dmin
	}
end
