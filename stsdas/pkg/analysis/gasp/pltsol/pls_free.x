include "pls.h"

# PLS_FREE -- Free space for objects information.

procedure pls_free (sp)

pointer	sp		# Structure pointer to objects info

begin

	call mfree (X_PREF(sp), TY_DOUBLE)	# X coordinates
	call mfree (Y_PREF(sp), TY_DOUBLE)	# Y coordinates
	call mfree (PRA(sp), TY_DOUBLE)		# RA values
	call mfree (PDEC(sp), TY_DOUBLE)	# DEC values
	call mfree (PMAG(sp), TY_REAL)		# magnitude values
	call mfree (PCOL(sp), TY_REAL)		# color values
	call mfree (PXI(sp), TY_DOUBLE)		# XI coordinates
	call mfree (PETA(sp), TY_DOUBLE)	# ETA coordinates
	call mfree (PXIC(sp), TY_DOUBLE)	# xi regression val.
	call mfree (PETAC(sp), TY_DOUBLE)	# eta regression val.
	call mfree (PW(sp), TY_REAL)		# coord weights
	call mfree (POFLAG(sp), TY_INT)		# coord flag
	call mfree (XPA(sp), TY_DOUBLE)
	call mfree (YPA(sp), TY_DOUBLE)
	call mfree (sp, TY_STRUCT)

end
