include "pls.h"

# PLS_ALLOC -- Allocate space for objects information.

procedure pls_alloc (sp, nobjs)

pointer	sp		# Structure pointer to objects info
int	nobjs		# Number of objects

begin

	call calloc (sp, LEN_STRUCT, TY_STRUCT)
	call calloc (X_PREF(sp), nobjs, TY_DOUBLE)	# X coordinates
	call calloc (Y_PREF(sp), nobjs, TY_DOUBLE)	# Y coordinates
	call calloc (PRA(sp), nobjs, TY_DOUBLE)		# RA values
	call calloc (PDEC(sp), nobjs, TY_DOUBLE)	# DEC values
	call calloc (PMAG(sp), nobjs, TY_REAL)		# magnitude values
	call calloc (PCOL(sp), nobjs, TY_REAL)		# color values
	call calloc (PXI(sp), nobjs, TY_DOUBLE)		# XI coordinates
	call calloc (PETA(sp), nobjs, TY_DOUBLE)	# ETA coordinates
	call calloc (PXIC(sp), nobjs, TY_DOUBLE)	# xi regression val.
	call calloc (PETAC(sp), nobjs, TY_DOUBLE)	# eta regression val.
	call calloc (PW(sp), nobjs, TY_REAL)		# coord weights
	call calloc (POFLAG(sp), nobjs, TY_INT)		# coord flag
	call calloc (XPA(sp), nobjs*NTERMS_MODEL, TY_DOUBLE)
	call calloc (YPA(sp), nobjs*NTERMS_MODEL, TY_DOUBLE)

end
