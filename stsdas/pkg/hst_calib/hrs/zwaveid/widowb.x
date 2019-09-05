include	"wid.h"
include	"ot.h"
include	"line.h"

# Memory management.
define	D		Memr[d]
define	ID		Memi[d]
define	S1		Memr[s1]
define	S2		Memr[s2]

#---------------------------------------------------------------------------
.help wid_o_wtab 11Apr95 source
.ih
NAME
wid_o_wtab -- Write locations to output table.
.endhelp
#---------------------------------------------------------------------------
procedure wid_o_wtab (wid, ll, ot)

pointer	wid			# I:  WAVEID object.
pointer	ll			# I:  Line List object.
pointer	ot			# I:  OT object.

# Declarations.
pointer	d			# Difference between observed and predicted.
pointer	s1, s2			# Sample positions.

errchk	malloc, mfree, tbcptd, tbcptr

begin
	# Write the lines.
	call tbcptd (OT_T(ot), OT_COL(ot,C_LINE), LL_WAVE(ll,1),
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	call tbcptd (OT_T(ot), OT_COL(ot,C_WAVEO), LL_WAVEO(ll,1),
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	call tbcptr (OT_T(ot), OT_COL(ot,C_PPOSO), LL_POSO(ll,1),
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	call tbcptr (OT_T(ot), OT_COL(ot,C_PPOSP), LL_POSP(ll,1),
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	call tbcptr (OT_T(ot), OT_COL(ot,C_INTP), LL_INTP(ll,1),
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))

	# Get difference in wavelengths.
	call malloc (d, LL_N(ll), TY_DOUBLE)
	call asubd (LL_WAVEO(ll,1), LL_WAVE(ll,1), Memd[d], LL_N(ll))
	call tbcptd (OT_T(ot), OT_COL(ot,C_DIFFW), Memd[d],
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	call mfree (d, TY_DOUBLE)

	# Get difference in pixels
	call malloc (d, LL_N(ll), TY_REAL)
	call asubr (LL_POSO(ll,1), LL_POSP(ll,1), D, LL_N(ll))
	call tbcptr (OT_T(ot), OT_COL(ot,C_DIFFP), D,
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))
	
	# Convert pixel positions to sample positions for the predicted
	# position.
	call malloc (s1, LL_N(ll), TY_REAL)
	call malloc (s2, LL_N(ll), TY_REAL)
	call altmr (LL_POSP(ll,1), S1, LL_N(ll), WID_SD(wid),
		    WID_S0(wid)-WID_SD(wid))
	call tbcptr (OT_T(ot), OT_COL(ot,C_SPOSP), S1,
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))

	call altmr (LL_POSO(ll,1), S2, LL_N(ll), WID_SD(wid),
		    WID_S0(wid)-WID_SD(wid))
	call tbcptr (OT_T(ot), OT_COL(ot,C_SPOSO), S2,
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))

	call asubr (S2, S1, D, LL_N(ll))
	call tbcptr (OT_T(ot), OT_COL(ot,C_DIFFS), D,
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))

	# Write out the spectral order.
	call malloc (d, LL_N(ll), TY_INT)
	call amovki (WID_M(wid), ID, LL_N(ll))
	call tbcpti (OT_T(ot), OT_COL(ot,C_SPORDER), ID,
		     OT_NROWS(ot)+1, OT_NROWS(ot)+LL_N(ll))

	OT_NROWS(ot) = OT_NROWS(ot) + LL_N(ll)

	# That's all folks.
	call mfree (s1, TY_REAL)
	call mfree (s2, TY_REAL)
	call mfree (d, TY_INT)
end
#---------------------------------------------------------------------------
# end of wid_o_wtab
#---------------------------------------------------------------------------
