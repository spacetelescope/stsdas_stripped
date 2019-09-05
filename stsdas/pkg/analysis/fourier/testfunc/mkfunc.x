include <imhdr.h>
define	SZ_FUNCT	16

# MKFUNC -- make a function
# This task creates a 1-D image with functional form data and appropriate
# FITS coordiate parameters.  The user specifies the function name (same
# functions as for testfunc task), number of points, and origin offset
# from pixel 1.
#
# C. D. Biemesderfer, 30 Dec 87
# Phil Hodge, 28-Jan-1989  Converted from a cl script to a subroutine.

procedure mkfunc()

pointer sp
pointer im		# pointer to image descriptor
pointer value		# pointer to output values
pointer output		# scratch for name of output image
pointer func		# scratch for name of function
double	yval
double	incr
int	npts, shft
pointer immap(), impl1d()
double	clgetd()
int	clgeti()

begin
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (func, SZ_FUNCT, TY_CHAR)

	# Get user parameters.
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("function", Memc[func], SZ_FUNCT)
	npts = clgeti ("npts")
	shft = clgeti ("shift")
	yval = clgetd ("constant")
	incr = 1.d0 / clgetd ("fcw")	# reciprocal of function width

	# Create the output image.
	im = immap (Memc[output], NEW_IMAGE, NULL)
	IM_LEN(im,1) = npts
	IM_PIXTYPE(im) = TY_REAL	# not really necessary
	value = impl1d (im)		# note:  double precision

	# Make the test function.
	call mk_test_func (Memc[func], yval, incr, npts, shft, Memd[value])

	# Add coordinate parameter values.
	call imaddd (im, "crval1", 0.d0)
	call imaddr (im, "crpix1", shft+1.)

	call imunmap (im)
	call sfree (sp)
end
