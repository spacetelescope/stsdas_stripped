include <tbset.h>
include <math.h>
include "pls.h"

procedure  rd_tab (tp, pn, crpix1, crpix2, crval1, crval2, 
		    nref_stars, ncolumns, x_pixel_size, y_pixel_size)

pointer	tp				# table descriptor
pointer pn				# pointer to objects structure
double	crpix1, crpix2			# pixel position of ref point
double	crval1, crval2			# equatorial coordinates of ref point
int	nref_stars			# i,Number of lines in input table
int	ncolumns			# i.
real	x_pixel_size, y_pixel_size	# pixel size (microns)

pointer sp, spx, spy
double	dra, ddec
double	ra_cen, dec_cen
int	i

begin

# get ready to fetch input data about position of objects from a table
# supplied by an extracting program.


	call smark (sp)
	call salloc (spx, nref_stars, TY_REAL)
	call salloc (spy, nref_stars, TY_REAL)
	call pls_input_table (tp, pn, nref_stars, ncolumns,
			     Memr[spx], Memr[spy])

# 	Load arrays from the input table with RA, DEC, x, y and other 
#	possible parameters.

	ra_cen = DEGTORAD(crval1)
	dec_cen = DEGTORAD(crval2)
	do i = 0, nref_stars - 1 {
	  dra  = DEGTORAD (Memd[PRA(pn)+i])
	  ddec = DEGTORAD (Memd[PDEC(pn)+i])

	  call treqst (ra_cen, dec_cen, dra, ddec, 
		       Memd[PXI(pn)+i], Memd[PETA(pn)+i])

	  Memd[X_PREF(pn)+i] = (crpix1 - Memr[spx+i])*x_pixel_size / 1000.0d0
	  Memd[Y_PREF(pn)+i] = (Memr[spy+i] - crpix2)*y_pixel_size / 1000.0d0
#	  call printf ("%g  %g  %g  %g\n")
#		call pargr (Memr[spx+i])
#		call pargr (Memr[spy+i])
#		call pargd (Memd[X_PREF(pn)+i])
#		call pargd (Memd[Y_PREF(pn)+i])
	}

	call sfree(sp)

# Set the equations of conditions from the above arrays to be used in the
# regression solution.

	call normeq (pn, nref_stars, Memd[XPA(pn)], Memd[YPA(pn)])

# Init the weights for the fit routine

	call amovkr (1.0, WEIGHT(pn), nref_stars)
end
