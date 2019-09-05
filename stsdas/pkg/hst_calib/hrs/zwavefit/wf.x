include	"wf.h"

#---------------------------------------------------------------------------
.help wf.x 23Mar95 source
.ih
NAME
wf.x -- routines for handling the wavefit object.
.endhelp
#---------------------------------------------------------------------------
pointer procedure wf_alloc (ncoef)

int	ncoef			# I:  Number of coefficients that will be fit.

# Declarations
pointer	wf			# Wavefit object.

errchk	calloc

begin
	call calloc (wf, WF_SZ, TY_STRUCT)

	call malloc (WF_A_PTR(wf), ncoef, TY_DOUBLE)
        call malloc (WF_AFIT_PTR(wf), ncoef, TY_INT)
        
        call amovki (YES, WF_AFIT(wf,1), ncoef)
	WF_NCOEF(wf) = ncoef

	call malloc (WF_CS_PTR(wf), WF_N_CS, TY_DOUBLE)

	return (wf)
end
#---------------------------------------------------------------------------
# End of wf_alloc
#---------------------------------------------------------------------------
procedure wf_free (wf)

pointer	wf			# IO:  Wavefit object; NULL on return

# Declarations
errchk	mfree, tbtclo

begin
	# If output table is open, close it.
	if (WF_OT(wf) != NULL) {
	    call tbtclo (WF_OT(wf))
            call mfree (WF_OT_CP_PTR(wf), TY_POINTER)
        }

	# Free arrays.
	call mfree (WF_X1_PTR(wf), TY_DOUBLE)
	call mfree (WF_X2_PTR(wf), TY_DOUBLE)
	call mfree (WF_Y_PTR(wf), TY_DOUBLE)
	call mfree (WF_SIG_PTR(wf), TY_DOUBLE)
	call mfree (WF_A_PTR(wf), TY_DOUBLE)
        call mfree (WF_AFIT_PTR(wf), TY_INT)
        call mfree (WF_CS_PTR(wf), TY_DOUBLE)
	
	call mfree (wf, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wf_free
#---------------------------------------------------------------------------
