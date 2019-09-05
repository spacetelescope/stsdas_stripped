include "pls.h"

# PLS_REALLOC -- procedure to reallocate mem.

procedure pls_realloc (sp, nobjs)

pointer	sp		# Structure pointer to objects info
int	nobjs		# Number of objects

begin

	call realloc (X_PREF(sp), nobjs, TY_DOUBLE)	# X coordinates
	call realloc (Y_PREF(sp), nobjs, TY_DOUBLE)	# Y coordinates
	call realloc (PRA(sp), nobjs, TY_DOUBLE)		# RA values
	call realloc (PDEC(sp), nobjs, TY_DOUBLE)	# DEC values
	call realloc (PMAG(sp), nobjs, TY_REAL)		# magnitude values
	call realloc (PCOL(sp), nobjs, TY_REAL)		# color values
	call realloc (PXI(sp), nobjs, TY_DOUBLE)		# XI coordinates
	call realloc (PETA(sp), nobjs, TY_DOUBLE)	# ETA coordinates
	call realloc (PXIC(sp), nobjs, TY_DOUBLE)	# xi regression val.
	call realloc (PETAC(sp), nobjs, TY_DOUBLE)	# eta regression val.
	call realloc (PW(sp), nobjs, TY_REAL)		# coord weights
	call realloc (POFLAG(sp), nobjs, TY_INT)		# coord flag
	call realloc (XPA(sp), nobjs*NTERMS_MODEL, TY_DOUBLE)
	call realloc (YPA(sp), nobjs*NTERMS_MODEL, TY_DOUBLE)

end
