include <imhdr.h>
include "../fourier.h"

# cz_gline -- get a line from/to input/output image(s)
# This routine gets pointers to one line of the input images and pointers
# to the output images.  The images can be any dimension.  If some part
# does not exist, the corresponding pointer to data will not be modified.
# The function value will be the value returned by impnlr.

int procedure cz_gline (fti1, fti2, fto, v,
			x1r, x1i, x2r, x2i, oxr, oxi)

pointer fti1, fti2, fto		# i: pointers to ft struct for input, output
long	v[IM_MAXDIM,ARB]	# i: line index to get/put
pointer x1r, x1i		# o: pointer to data, first image
pointer x2r, x2i		# o: pointer to data, second image
pointer oxr, oxi		# o: pointer to output data
#--
int	npix			# value returned by imgnlr, impnlr
int	imgnlr(), impnlr()

begin
	if (FT_REAL(fti1) == YES) {
	    npix = imgnlr (FT_REPT(fti1), x1r, v[1,1])
	    if (npix == EOF)
		return (EOF)
	}
	if (FT_IMAG(fti1) == YES) {
	    npix = imgnlr (FT_IMPT(fti1), x1i, v[1,2])
	    if (npix == EOF)
		return (EOF)
	}
	if (FT_REAL(fti2) == YES) {
	    npix = imgnlr (FT_REPT(fti2), x2r, v[1,3])
	    if (npix == EOF)
		return (EOF)
	}
	if (FT_IMAG(fti2) == YES) {
	    npix = imgnlr (FT_IMPT(fti2), x2i, v[1,4])
	    if (npix == EOF)
		return (EOF)
	}
	if (FT_REAL(fto) == YES) {
	    npix = impnlr (FT_REPT(fto), oxr, v[1,5])
	    if (npix == EOF)
		return (EOF)
	}
	if (FT_IMAG(fto) == YES) {
	    npix = impnlr (FT_IMPT(fto), oxi, v[1,6])
	    if (npix == EOF)
		return (EOF)
	}

	return (npix)
end
