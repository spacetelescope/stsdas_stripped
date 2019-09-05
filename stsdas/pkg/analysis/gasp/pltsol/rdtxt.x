include <math.h>
include "pls.h"

procedure  rd_txt (tp, pn, crpix1, crpix2, crval1, crval2, 
		    nref_stars, x_pixel_size, y_pixel_size)

pointer	tp				# text file descriptor
pointer pn				# pointer to objects structure
double	crpix1, crpix2			# pixel position of ref point
double	crval1, crval2			# equatorial coordinates of ref point
int	nref_stars			# Number of lines in input table
real	x_pixel_size, y_pixel_size	# pixel size (microns)

double	ra_cen, dec_cen
int	getline(), clgeti()
int	i, icol[6], ncols
int	nskip
char	buf[SZ_LINE]
double	dra, ddec
real	x, y
errchk	rd_col
begin

# get ready to fetch input data about position of objects from a table
# supplied by an extracting program.

	ra_cen = DEGTORAD (crval1)
	dec_cen = DEGTORAD (crval2)

# 	Load arrays from the input table with RA, DEC, x, y and other 
#	possible parameters.

	# get number of lines to skip if necessary
	nskip = clgeti("nskip")
	# get cl parameters about the order of columns
	icol(1) = clgeti ("xcolnum")           # X col number
	icol(2) = clgeti ("ycolnum")	       # Y col number
	icol(3) = clgeti ("racolnum")	       # RA col number
	icol(4) = clgeti ("decolnum")	       # DEC col number
	icol(5) = clgeti ("magcolnum")	       # MAG col number
	icol(6) = clgeti ("colcolnum")	       # COLOR col number
	# count the number of columns to read (4 minimum)
	ncols = 0
	do i = 1, 6 {
	     if (icol(i) != 0 )
		ncols = ncols + 1
	}
	if (ncols < 4)
	    call error (13, "Number of columns in table is < 4")

	do i = 1, nskip {
	   if (getline(tp,buf) == EOF)
	      call error (13, "EOF reached while skipping lines")
	}
	i = 0
	while (getline(tp, buf) != EOF) {
         iferr( call rd_col (buf, icol, x, y, dra, ddec, Memr[PMAG(pn)+i],
			Memr[PCOL(pn)+i]))
	      break
	  call treqst (ra_cen, dec_cen, dra, ddec, 
		       Memd[PXI(pn)+i], Memd[PETA(pn)+i])

	  Memd[X_PREF(pn)+i] = (crpix1 - x)*x_pixel_size / 1000.0d0
	  Memd[Y_PREF(pn)+i] = (y - crpix2)*y_pixel_size / 1000.0d0
	
	  i = i + 1
	  if (mod(i,100) == 0) {
	     # boots allocated memory by 100
	     nref_stars = nref_stars + 100
	     call pls_realloc (pn, nref_stars)
	  }
	}
# Set the equations of conditions from the above arrays to be used in the
# regression solution.

	nref_stars = i
	call normeq (pn, nref_stars, Memd[XPA(pn)], Memd[YPA(pn)])

# Init the weights for the fit routine

	call amovkr (1.0, WEIGHT(pn), nref_stars)
end
