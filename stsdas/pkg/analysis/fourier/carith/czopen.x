include	<imhdr.h>
include	"../fourier.h"
include	"../fterr.h"

# This file contains cz_open_i and cz_open_o.
# Cz_open_i is similar to the previous version of ft_open_files_i,
# except that it does not have the getreal & getimag flags, it allows
# an image of any dimension, and the coordinate information is ignored.
# Cz_open_o is the same as the previous version of ft_open_files_o.
#
# The real and imaginary parts of the complex data are kept in separate
# images.  The file names for these parts are the user-specified names
# with "r" and "i" appended to the root portion.
#
# Phil Hodge, 21-Jul-1993  Created based on ft_open_files_i, etc.
# Phil Hodge, 15-Jan-1996  Save names of real & imaginary parts in ft struct.

# cz_open_i -- open input images for carith

procedure cz_open_i (fti, infile)

pointer	fti		# i: FFT pointer; input image
char	infile[ARB]	# i: Input file name
#--
pointer	immap(), irp, iip
int	imaccess()

begin
	# Apply naming convention for real part.
	call ft_fname (infile, "r", FT_NAME_R(fti), SZ_LINE)	# append "r"

	# Open input real data
	if (imaccess (FT_NAME_R(fti), READ_ONLY) == YES) {
	    irp = immap (FT_NAME_R(fti), READ_ONLY, 0)
	    FT_REAL(fti) = YES
	    FT_REPT(fti) = irp

	# Be charitable - allow the user to not follow the convention
	# on input for the real part.
	} else if (imaccess (infile, READ_ONLY) == YES) {
	    call strcpy (infile, FT_NAME_R(fti), SZ_LINE)
	    irp = immap (FT_NAME_R(fti), READ_ONLY, 0)
	    FT_REAL(fti) = YES
	    FT_REPT(fti) = irp
	} else {
	    call strcpy ("", FT_NAME_R(fti), SZ_LINE)
	    irp = NULL
	    FT_REAL(fti) = NO
	    FT_REPT(fti) = NULL
	}

	# Apply naming convention for imaginary part.
	call ft_fname (infile, "i", FT_NAME_I(fti), SZ_LINE)	# append "i"

	# Open input imaginary data
	if (imaccess (FT_NAME_I(fti), READ_ONLY) == YES) {
	    iip = immap (FT_NAME_I(fti), READ_ONLY, 0)
	    FT_IMAG(fti) = YES
	    FT_IMPT(fti) = iip
	} else {
	    call strcpy ("", FT_NAME_I(fti), SZ_LINE)	# no imaginary part
	    iip = NULL
	    FT_IMAG(fti) = NO
	    FT_IMPT(fti) = NULL
	}

	# Preferentially use input real file as template.  Select imaginary
	# file only if there is no real part specified.  If neither one
	# exists, that's an error.

	if (FT_REAL(fti) == YES)
	    FT_IMAGE(fti) = irp
	else if (FT_IMAG(fti) == YES)
	    FT_IMAGE(fti) = iip
	else
	    call ft_error (FT_NOINPUT, FT_FATAL)

	FT_NAXIS(fti) = IM_NDIM(FT_IMAGE(fti))

	# Coordinate information is ignored.
	FT_CRVAL(fti,1) = 0.d0
	FT_CRVAL(fti,2) = 0.d0
	FT_CRPIX(fti,1) = 1.d0
	FT_CRPIX(fti,2) = 1.d0
	FT_CD(fti,1,1) = 1.d0
	FT_CD(fti,1,2) = 0.d0
	FT_CD(fti,2,1) = 0.d0
	FT_CD(fti,2,2) = 1.d0
	FT_CTYPE(fti,1) = EOS
	FT_CTYPE(fti,2) = EOS
end

# cz_open_o -- open output images for carith

procedure cz_open_o (fti, fto, outfile, crereal, creimag)

pointer fti		# i: FFT pointer; input image as template
pointer fto		# i: FFT pointer; output image
char	outfile[ARB]	# i: Output file name
bool	crereal		# i: Create output real part?
bool	creimag		# i: Create output imaginary part?
#--
pointer	immap(), orp, oip

begin
	# Apply naming convention and open output.
	if (crereal) {
	    call ft_fname (outfile, "r", FT_NAME_R(fto), SZ_LINE) # append "r"
	    orp = immap (FT_NAME_R(fto), NEW_COPY, FT_IMAGE(fti))
	    FT_REAL(fto) = YES
	    FT_REPT(fto) = orp
	    IM_PIXTYPE(orp) = TY_REAL		# in case input is int or short
	} else {
	    call strcpy ("", FT_NAME_R(fto), SZ_LINE)
	    FT_REAL(fto) = NO
	    FT_REPT(fto) = NULL
	}

	# Open output imaginary data.
	if (creimag) {
	    call ft_fname (outfile, "i", FT_NAME_I(fto), SZ_LINE) # append "i"
	    oip = immap (FT_NAME_I(fto), NEW_COPY, FT_IMAGE(fti))
	    FT_IMAG(fto) = YES
	    FT_IMPT(fto) = oip
	    IM_PIXTYPE(oip) = TY_REAL
	} else {
	    call strcpy ("", FT_NAME_I(fto), SZ_LINE)
	    FT_IMAG(fto) = NO
	    FT_IMPT(fto) = NULL
	}

	if (FT_REAL(fto) == YES)
	    FT_IMAGE(fto) = orp
	else if (FT_IMAG(fto) == YES)
	    FT_IMAGE(fto) = oip
	else
	    call ft_error (FT_NOOUTPUT, FT_FATAL)

	FT_NAXIS(fto) = FT_NAXIS(fti)
end
