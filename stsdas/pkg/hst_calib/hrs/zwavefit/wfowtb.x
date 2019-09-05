include	"wf.h"

#---------------------------------------------------------------------------
.help wf_o_wtab 24Mar95 source
.ih
NAME
wf_o_wtab -- Write coefficients to output table.
.endhelp
#---------------------------------------------------------------------------
procedure wf_o_wtab (wf)

pointer	wf			# I:  Wavefit object.

# Declarations.
char	grating[WF_OT_GRATING_SZ]	# Grating name.
int	i				# Generic.
int	word_find()			# Get word from string.

errchk	tbrptd

begin
	i = word_find (WF_GRATING(wf), GRATING_DICT, grating, WF_OT_GRATING_SZ)
	
	WF_OT_NROWS(wf) = WF_OT_NROWS(wf) + 1
        call tbrpti (WF_OT(wf), WF_OT_CP(wf,WF_OT_CARPOS), WF_CARPOS(wf),
                     1, WF_OT_NROWS(wf))
	call tbrptt (WF_OT(wf), WF_OT_CP(wf,WF_OT_GRATING), grating,
		     WF_OT_GRATING_SZ, 1, WF_OT_NROWS(wf))
	call tbrptd (WF_OT(wf), WF_OT_CP(wf,WF_OT_COEF_COL),
                     WF_A(wf,1), WF_NCOEF(wf), WF_OT_NROWS(wf))
end
#---------------------------------------------------------------------------
# end of wf_o_wtab
#---------------------------------------------------------------------------
