# This file defines flags to specify which input real & imaginary parts exist.

define	FT1r1i2r2i	1	# all parts exist
define	FT1r__2r2i	2	# imaginary part of first input does not exist
define	FT__1i2r2i	3	# real part of first input does not exist
define	FT1r1i2r__	4
define	FT1r1i__2i	5
define	FT1r__2r__	6
define	FT1r____2i	7
define	FT__1i2r__	8
define	FT__1i__2i	9
define	FT____2r2i	10	# no numerator (irrelevant for multiplication)
define	FT____2r__	11	# no numerator
define	FT______2i	12	# no numerator

# These are used to distinguish between the two options in code that is
# common to both operations.

define	FT_MULTIPLY	1
define	FT_DIVIDE	2
