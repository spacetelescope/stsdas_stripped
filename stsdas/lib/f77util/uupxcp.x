include <imhdr.h>
include <iraf77.h>

# UUPXCP -- Copy pixel array from input file to output file.
#
# This routine copies (moves) the entire input pixel array from the input
# file to the output file.
#
# C.D.Biemesderfer, STScI, 27 May 1987

procedure uupxcp (ifl, ofl, istat)

pointer	ifl		# i: Input file descriptor
pointer	ofl		# i: Output file descriptor
int	istat		# o: Return status
#--
int	imgnls, imgnli, imgnll, imgnlr, imgnld
long	v[IM_MAXDIM]

begin
	istat = ER_OK

	call amovkl (long(1), v, IM_MAXDIM)

	switch (IM_PIXTYPE(ifl)) {
	case TY_SHORT:
	    while (imgnls(ifl, ofl, v) != EOF)
		;

	case TY_INT:
	    while (imgnli(ifl, ofl, v) != EOF)
		;

	case TY_LONG:
	    while (imgnll(ifl, ofl, v) != EOF)
		;

	case TY_REAL:
	    while (imgnlr(ifl, ofl, v) != EOF)
		;

	case TY_DOUBLE:
	    while (imgnld(ifl, ofl, v) != EOF)
		;

	default:
	    call eprintf ("uupxcp - unsupported datatype %d\n")
		call pargi (IM_PIXTYPE(ifl))
	    istat = ER_IMBADTYPE
	}
end
