include	<tbset.h>
include	<imhdr.h>
include <mach.h>
include "wcs.h"

define	MARK_SIZE	9	# Half-width of marker in pixels
define	CLEAR_SIZE	3	# Half-width of clear space in marker
define	NUM_COLS	5	# Number of table columns

procedure t_pxcoord ()

#  PXCOORD -- Find the pixel coordinates corresponding to the celestial
#  coordinates in a Guide Star Catalog table.

pointer	sp, iname
pointer	inim			# Input image descriptor
pointer	tp			# GSC table descriptor
pointer	cd[NUM_COLS]		# Table column descriptors
int	nrows			# Number of rows in table
int	row
double	coord[2]		# WC coordinates
pointer	wcs			# WCS structure descriptor
bool	null[2]			# Null row?
real	pix[2]			# Pixel coordinates
int	x, y
bool	tabcoord		# Print existing pixel coordinates in table?
bool	update			# Update pixel coords in table?

pointer	immap(), wcsinit()
bool	clgetb()

begin
	call smark (sp)
	call salloc (iname, SZ_FNAME, TY_CHAR)
	call clgstr ("inimage", Memc[iname], SZ_FNAME)

	# Map input image
	inim = immap (Memc[iname], READ_WRITE, 0)

	# Find the coordinate transformation parameters and
	# initialize the coordinate transformation
	wcs = wcsinit (inim)

	# Open the table and map the columns
	call gsctab (tp, cd, nrows)

	# Mark using existing pixel coordinates?
	tabcoord = clgetb ("tabcoord")

	# Update pixel coordinate columns?
	update = clgetb ("update")

	do row = 1, nrows {
	    # For each row of the table

	    if (tabcoord) {
		# Print existing pixel coordinates in table?
		# Use existing pixel coordinates
		call tbrgtr (tp, cd[3], pix, null, 2, row)
		if (null[1] || null[2])
		    next

	    } else {
		# Fetch the coordinates
		call tbrgtd (tp, cd, coord, null, 2, row)
		if (null[1] || null[2])
		    next

		# Find the pixel coordinates
		call xt_wc_pix (wcs, coord, pix, 2)

		if (update)
		    # Update the pixel coordinates in the table
		    call tbrptr (tp, cd[3], pix, 2, row)
	    }
	
	    # Nearest integer pixel
	    x = int (pix[1] + 0.5)
	    y = int (pix[2] + 0.5)

	    if (x >= 1 && x <= IM_LEN(inim,1) &&
		y >= 1 && y <= IM_LEN(inim,2)) {
		# The star is on the image
		call printf ("%8.1f %8.1f\n")
		    call pargr (pix[1])
		    call pargr (pix[2])

		if (!tabcoord && update)
		    # Set the row to valid
		    call tbrpti (tp, cd[5], YES, 1, row)
	    }
	}

	call tbtclo (tp)
	call xt_wcs_free (wcs)
	call imunmap (inim)
end


pointer procedure wcsinit (inim)

# WCSINIT -- Find the coordinate transformation parameters and
# initialize the coordinate transformation

pointer	inim			# Input image descriptor

pointer	wcs			# WCS structure descriptor
bool	imtrans			# Transformation parameters from image?
real	crpix[2]		# Reference pixel
real	cdelt[2]		# Increment per pixel
real	crota			# Rotation angle
double	crval[2]		# Coordinate at reference pixel
char	ctype[SZ_CTYPE,2]	# Coordinate type

bool	clgetb()
real	clgetr()
double	clgetd()

begin
	imtrans = clgetb ("imtrans")

	if (imtrans) {
	    # Initialize the WCS with the transformation parameters from
	    # the image
	    call xt_wcs_init (inim, wcs)
	    if (W_VALID(wcs) == NO)
		call error (0, "No valid coordinate transformation in image")

	} else {
	    # Get the transformation parameters from the cl
	    crval[1] = clgetd ("crval1")
	    crval[2] = clgetd ("crval2")
	    crpix[1] = clgetr ("crpix1")
	    crpix[2] = clgetr ("crpix2")
	    cdelt[1] = clgetr ("cdelt1")
	    cdelt[2] = clgetr ("cdelt2")
	    crota = clgetr ("crota")
	    call clgstr ("ctype1", ctype[1,1], SZ_CTYPE)
	    call clgstr ("ctype2", ctype[1,2], SZ_CTYPE)

	    # Initialize the WCS with the transformation parameters
	    call xt_wcs_init_c (crval, crpix, cdelt, crota, ctype, 2, wcs)

	    if (W_VALID(wcs) == NO)
		call error (0, "Coordinate transformation invalid")
	}

	return (wcs)
end
