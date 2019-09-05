procedure sky_close (gp, pl_cnst, star_str)

#  Free structures and memory allocated by sky_open().

include	"skymap.h"

pointer	gp			# Graphics descriptor pointer
pointer	pl_cnst			# Plate constants structure pointer
pointer	star_str		# Stars markers structure pointer

begin
	call gclose (gp)

	call mfree (pl_cnst, TY_STRUCT)

	if (star_str != NULL) {
	    call mfree (CAT_ROW_P(star_str), TY_INT)
	    call mfree (CAT_RA_P(star_str),  TY_DOUBLE)
	    call mfree (CAT_DEC_P(star_str), TY_DOUBLE)
	    call mfree (CAT_MAG_P(star_str), TY_REAL)
	    call mfree (CAT_NAM_P(star_str), TY_CHAR)
	}

	call mfree (star_str, TY_STRUCT)
end
