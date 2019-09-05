include <imhdr.h>
include "../fourier.h"
include "ftarith.h"

# cz_init -- open images, etc
# This routine opens the input and output images, sets the eflag depending
# on which images (i.e. which real & imaginary parts) actually exist, and
# converts the cutoff fraction to an actual lower limit for division.
#
# Phil Hodge, 23-July-1990  Subroutine created.
# Phil Hodge, 21-July-1993  Use cz_open_i & cz_open_o; use fti1 instead of
#			fti2 as template if the first image exists.
# Phil Hodge, 15-Jan-1996  Save names of real & imaginary parts in ft struct.

procedure cz_init (input1, input2, output,
		fti1, fti2, fto, operation,
		num_exists, eflag, cutoff, lowlim, npix)

char	input1[ARB]		# i: name of first input
char	input2[ARB]		# i: name of second input
char	output[ARB]		# i: name of output
pointer fti1, fti2, fto		# i: pointers to ft struct
int	operation		# i: multiplication or division
bool	num_exists		# o: does the first input file exist?
int	eflag			# o: existence flag for real & imag parts
real	cutoff			# i: specifies lower limit for division
real	lowlim[2]		# o: lower limit for division
int	npix			# o: size of a line in the images
#--
bool	crereal, creimag	# create real, imaginary part of output?
bool	streq()

begin
	# Open input images.
	if (input1[1] == EOS || input1[1] == ' ' ||
		streq (input1, "1") || streq (input1, "1.")) {
	    if (operation == FT_MULTIPLY)
		call error (1, "first input image name is null")
	    num_exists = false
	    call strcpy ("", FT_NAME_R(fti1), SZ_LINE)
	    FT_REAL(fti1) = NO
	    FT_REPT(fti1) = NULL
	    call strcpy ("", FT_NAME_I(fti1), SZ_LINE)
	    FT_IMAG(fti1) = NO
	    FT_IMPT(fti1) = NULL
	    FT_IMAGE(fti1) = NULL
	} else {
	    num_exists = true
	    call cz_open_i (fti1, input1)
	}
	call cz_open_i (fti2, input2)

	# Set eflag depending on which files exist; set crereal, creimag
	# depending on which output parts can be non-zero; get the lengths
	# of the axes.
	call cz_which (fti1, fti2, fto,
		eflag, crereal, creimag, npix)

	# Get the lower limit for division.
	if (operation == FT_DIVIDE)
	    call cz_cutoff (fti2, cutoff, lowlim)

	# Create output files.  Use the first input image as a template
	# unless only the second image exists.
	if (FT_IMAGE(fti1) == NULL)
	    call cz_open_o (fti2, fto, output, crereal, creimag)
	else
	    call cz_open_o (fti1, fto, output, crereal, creimag)
end
