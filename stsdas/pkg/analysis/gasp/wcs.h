# WCS.H -- Include file for routines to convert between pixel and WCS
# coordinates.

define	LEN_WCS		136	# size of wcs struct for naxis <= 7

define	W_VALID		Memi[$1]	# coordinates valid, YES or NO?
define	W_NAXIS		Memi[$1+1]	# number of axes
define	W_RA_AX		Memi[$1+2]	# which axis is RA?  zero if none
define	W_DEC_AX	Memi[$1+3]	# which axis is Dec?  zero if none
define	W_PROJECTION	Memi[$1+4]	# projection type

#   6 is currently not used

#   7 -  55:  full CD matrix (7x7); units = e.g. degrees
#  56 - 104:  LU decomposition of CD matrix
# 105 - 111:  index returned by ludcmp for use by lubksb
# 112 - 118:  reference pixel location
# 119 - 122:  cosine & sine of declination at the reference pixel
# 123 - 136:  coordinates at crpix; units = e.g. degrees

define	W_CD		Memr[$1+6 +($2-1)+($3-1)*7]
define	W_CDLU		Memr[$1+55 +($2-1)+($3-1)*7]
define	W_CDINDX	Memr[$1+104]		# this is an array of 7
define	W_CRPIX		Memr[$1+110+$2]
define	W_COSDEC	Memd[P2D($1+118)]
define	W_SINDEC	Memd[P2D($1+120)]
define	W_CRVAL		Memd[P2D($1+120)+$2]

# Projection types.

define	W_LINEAR	0
define	W_GNOMONIC	1	# TAN
define	W_SINE		2	# SIN
define	W_ARC		3	# ARC
define	W_NORTH_POLAR	4	# NCP, north celestial pole (Westerbork)
define	W_STEREOGRAPHIC	5	# STG (conformal)
define	W_AITOFF	6	# AIT (equal-area)
define	W_GLOBAL_SINE	7	# GLS (equal-area)
define	W_MERCATOR	8	# MER (conformal)
