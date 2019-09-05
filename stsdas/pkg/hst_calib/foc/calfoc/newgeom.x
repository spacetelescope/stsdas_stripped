include	<imhdr.h>

define	SZ_PODPS_LINE	79	# size of message
define	DMAX		0.1	# maximum allowed percentage difference
define	NTARG		20	# max number of messages for bad pixels

# newgeom -- geometrically correct an image
# This section is from the "new" geometric correction routine by Robert
# Jedrzejewski and Bill Sparks.  This is the part that applies the distortion
# correction and has been separated from the rest of the task in order to be
# included in calfoc.
# If set_values = true, then geo_scale will be called to either copy the
# coordinate parameters from the GEO header (if SMMMODE = INBEAM) or set the
# CD matrix elements based on IMSCALE from the GEO header and ORIENTAT from
# the input header.
# If apply_dist = true, then geo_coord will be called to update the coordinate
# parameters CRPIX1, CRPIX2, and the CD matrix.  This is appropriate if the
# coordinate parameters of the input image really are a fair representation
# of the coordinate system of that image.  It is not appropriate if those
# parameters actually describe the coordinate system of the geometrically
# corrected image, as has been the case in the pipeline.
# It should not be the case that both set_values and apply_dist are true.
#
# Robert Jedrzejewski		Original.
# Phil Hodge, Jan-1991		Extracted from newgeom task.
# Phil Hodge,  5-Aug-1991	Add defval to calling sequence.
# Phil Hodge, 10-Jul-1992	Add chg_coords to calling sequence.
# Phil Hodge,  7-Apr-1994	Change chg_coords to set_values & apply_dist;
#				verbose ==> print percent done.
# Phil Hodge, 24-Feb-1995	Check for calc=0 when computing pdif.

procedure newgeom (iim, oim, geopt, defval,
		set_values, apply_dist, box_check, verbose)

pointer iim		# i: imhdr ptr for input image
pointer oim		# i: imhdr ptr for output (geo corrected) image
pointer geopt		# i: imhdr ptr for geo reference file
real	defval		# i: background (default) value for output image
bool	set_values	# i: set coord param based on GEO header?
bool	apply_dist	# i: modify coord param based on geo distortion?
bool	box_check	# i: check for errors in computing pixel overlap?
bool	verbose		# i: print line numbers as they are processed?
#--
pointer sp
pointer mess		# scratch for possible error message
pointer in, out
pointer xy1, xy2	# x & y coordinates of lower & upper edges of a row

int	tenth			# print message after this many lines
int	gnaxis1, gnaxis2		# size of geopt image
int	inaxis1, inaxis2		# size of iim image
int	naxis1, naxis2			# size of oim image
int	offset				# from x to y in xy1 and xy2 arrays
int	icount, i, j, k
int	nbx, nby
int	ixmin, ixmax, iymin, iymax, nbad
real	value, area, bad, xmin, xmax, ymin, ymax
real	xcorn[5], ycorn[5]
real	xdum[8], ydum[8]
real	a1, a2, a3, a4, summed, calc, pdif
real	datamin, datamax		# min & max pixel values
pointer imgs2r(), imgl2r(), impl2r()
int	negint(), imgeti(), imaccf()

begin
	call smark (sp)
	call salloc (mess, SZ_PODPS_LINE, TY_CHAR)

	# Size of input image.
	inaxis1 = IM_LEN(iim,1)
	inaxis2 = IM_LEN(iim,2)

	# Size of geo reference file.
	gnaxis1 = IM_LEN(geopt,1)
	gnaxis2 = IM_LEN(geopt,2)

	# Set the size of the output image.
	if (imaccf (geopt, "npix1") == YES &&
	    imaccf (geopt, "npix2") == YES) {
	    naxis1 = imgeti (geopt, "npix1")
	    naxis2 = imgeti (geopt, "npix2")
	} else if (imaccf (geopt, "columns") == YES &&
		   imaccf (geopt, "rows") == YES) {
	    naxis1 = imgeti (geopt, "columns")
	    naxis2 = imgeti (geopt, "rows")
	}
	naxis1 = min (naxis1, gnaxis1/2-1)
	naxis2 = min (naxis2, gnaxis2-1)
	IM_LEN(oim,1) = naxis1
	IM_LEN(oim,2) = naxis2

	# Set up for printing the info regarding how far we have gotten.
	if (verbose)
	    call printf ("percent done: ")
	tenth = (naxis2 + 9) / 10

	# We don't need to allocate space for xy2 also, because it's gotten
	# by a call to imgl2r.
	call salloc (xy1, gnaxis1, TY_REAL)

	# Read the entire input image into memory.
	in = imgs2r (iim, 1, inaxis1, 1, inaxis2)

	nbad = 0

	datamax = -1.e20			# initial values
	datamin = 1.e20

	# Each line of the geo file contains x and y coordinates, first x,
	# then y; this offset gets us to the y coordinates.
	offset = gnaxis1 / 2

	# Read the first line into the second buffer (will be copied to xy1
	# inside the loop).
	xy2 = imgl2r (geopt, 1)

	# Do for each row of the output image.
	do j = 1, naxis2 {

	    # Copy the previous second line into the current first line.
	    call amovr (Memr[xy2], Memr[xy1], gnaxis1)
	    # The input geo file has one more row than the output image.
	    xy2 = imgl2r (geopt, j+1)

	    # Set up output line.
	    out = impl2r (oim, j)
	    do i = 1, naxis1 {

		xdum[1] = Memr[xy1+i-1]
		xdum[2] = Memr[xy1+i]
		xdum[3] = Memr[xy2+i]
		xdum[4] = Memr[xy2+i-1]
		ydum[1] = Memr[xy1+offset+i-1]
		ydum[2] = Memr[xy1+offset+i]
		ydum[3] = Memr[xy2+offset+i]
		ydum[4] = Memr[xy2+offset+i-1]
		do k = 1, 4 {
		    xdum[k+4] = xdum[k]
		    ydum[k+4] = ydum[k]
		}
		call sortcorn (xdum, ydum, xcorn, ycorn)

		xmin = xcorn[1]
		xmax = xcorn[1]
		do icount = 2, 4 {
		    if (xcorn[icount] > xmax) xmax = xcorn[icount]
		    if (xcorn[icount] < xmin) xmin = xcorn[icount]
		}
		ixmin = negint (xmin)
		ixmax = negint (xmax)
		nbx = ixmax - ixmin + 1
		ymin = ycorn[1]
		ymax = ycorn[1]
		do icount = 2, 4 {
		    if (ycorn[icount] > ymax) ymax = ycorn[icount]
		    if (ycorn[icount] < ymin) ymin = ycorn[icount]
		}
		iymin = negint (ymin)
		iymax = negint (ymax)
		nby = iymax - iymin + 1

		if (ixmax > 0 && ixmin < inaxis1 &&
		    iymax > 0 && iymin < inaxis2) {

		    call interpb (Memr[in], inaxis1, inaxis2,
				nbx, nby, xcorn, ycorn,
				box_check, value, area, bad)

		    # This section is to verify that we correctly computed
		    # the areas of overlap of the undistorted pixel with the
		    # pixels in the distorted image.
		    if (box_check) {
			if (area == 0.0 && bad == 0.0) {
			    call sprintf (Memc[mess], SZ_PODPS_LINE,
				    "Area = 0 at i=%d, j=%d")
				call pargi (i)
				call pargi (j)
			    call logmsg (Memc[mess])
			}

			# Have to take off xmin and ymin from the corners;
			# otherwise, we get significant rounding errors if the
			# x & ycorns are big.

			a1 = (ycorn[2]-ymin) * (xcorn[3]-xmin)
			a1 = a1 - (xcorn[2]-xmin) * (ycorn[3]-ymin)
			a2 = (ycorn[3]-ymin) * (xcorn[4]-xmin)
			a2 = a2 - (xcorn[3]-xmin) * (ycorn[4]-ymin)
			a3 = (ycorn[4]-ymin) * (xcorn[1]-xmin)
			a3 = a3 - (xcorn[4]-xmin) * (ycorn[1]-ymin)
			a4 = (ycorn[1]-ymin) * (xcorn[2]-xmin)
			a4 = a4 - (xcorn[1]-xmin) * (ycorn[2]-ymin)
			calc = 0.5 * (a1+a2+a3+a4)
			summed = area + bad
			if (calc == 0.)
			    pdif = 100.0 * summed
			else
			    pdif = 100.0 * (summed - calc) / calc

			if (abs (pdif) >= DMAX) {
			    nbad = nbad + 1
			    if (nbad <= NTARG) {
				call sprintf (Memc[mess], SZ_PODPS_LINE,
					"Bad sum at i=%d, j=%d")
				    call pargi (i)
				    call pargi (j)
				call logmsg (Memc[mess])
				do k = 1, 4 {
				    call sprintf (Memc[mess], SZ_PODPS_LINE,
					"   %0.9g   %0.9g")
					call pargr (xcorn[k])
					call pargr (ycorn[k])
				    call logmsg (Memc[mess])
				}
				call sprintf (Memc[mess], SZ_PODPS_LINE,
					"Calc=%0.9g, sum=%0.9g, diff=%0.9g")
				    call pargr (calc)
				    call pargr (summed)
				    call pargr (pdif)
				call logmsg (Memc[mess])
			    }
			}
		    }		# end if (box_check)
		} else {
		    # There's no point in the input image that corresponds
		    # to the current pixel in the output image, so assign
		    # the default value.
		    value = defval
		}
		Memr[out+i-1] = value		# assign current pixel value
		if (value > datamax)		# update datamin & datamax
		    datamax = value
		else if (value < datamin)
		    datamin = value
	    }

	    if (verbose) {
		# Print a message every ten percent of the way through.
		if (mod (j, tenth) == 0 && j < naxis2) {
		    call printf (" %d")
			call pargi (j / tenth * 10)
		    call flush (STDOUT)
		}
	    }
	}
	if (verbose)
	    call printf (" 100\n")

	if (nbad > 0) {
	    call sprintf (Memc[mess], SZ_PODPS_LINE, "%d bad pixels")
		call pargi (nbad)
	    call logmsg (Memc[mess])
	}

	# Put datamin & datamax into header.
	call imputr (oim, "i_maxpixval", datamax)
	call imputr (oim, "i_minpixval", datamin)
	IM_LIMTIME(oim) = IM_MTIME(oim) + 1

	if (set_values) {

	    # Assign values for the output CD matrix elements based on
	    # the image scale and orientation.  Or for spectrographic mode,
	    # simply replace all the coordinate parameters.
	    call geo_scale (oim, geopt)

	} else if (apply_dist) {

	    # Update the coordinate parameters based on the distortion.
	    iferr {
		call geo_coord (oim, geopt)
	    } then {
		call logmsg ("newgeom:  can't update coordinate parameters")
	    }
	}

	call sfree (sp)
end
