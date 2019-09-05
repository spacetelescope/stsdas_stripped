include "gsky.h"

#  T_GSKY  --  Compute sky level.
#
#  This task uses the mode algorithm used in xcrrej to compute a sky
#  level and output it into a CL parameter and/or a group header keyword.
#
#
#
#  Revision history:
#  ----------------
#
#  Date		Author		Description
#  ----		------		-----------
#  12-Jun-1996	I. Busko	Naive adaptation from xcrrej
#  25-Mar-1997  I. Busko	Add SUBSKY keyword.
#
# ------------------------------------------------------------------------------
# task gsky = t_gsky

procedure t_gsky ()

pointer	tpin 			
pointer	tpmask 			
pointer	par
#==============================================================================
begin
	# define the parameter structure
	call malloc (par, LEN_PAR, TY_STRUCT)

	# get CL parameters and related quantities
	call gsky_in (tpin, tpmask, par)
 
	# put STDOUT header
	if (VERBOSE(par)) {
	    call printf ("#\n")
	    call printf ("# *** GSKY - Version %s ***\n")
	        call pargstr (VERSION)
	    call printf ("# Image name       Group     Sky (DN)\n")
	    call printf ("#\n")
	    call flush (STDOUT)
	}

	# perform the calculation
	call gsky_do (tpin, tpmask, par)

	# close file template
	call imtclose (tpin)
	call imtclose (tpmask)

	call mfree (par, TY_STRUCT)
end
