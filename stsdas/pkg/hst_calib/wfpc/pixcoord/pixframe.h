# PIXFRAME.H -- Coordinate frame transformation structure

define	LEN_FRAME	3

define	FRM_MWCS	Memi[$1]	# MWCS Structure
define	FRM_PX2LM	Memi[$1+1]	# Pixel to (l,m) transformation
define	FRM_LM2PX	Memi[$1+2]	# (l,m) to pixel transformation

