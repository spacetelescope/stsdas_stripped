include "crrej.h"

#  t_crrej -- cosmic ray rejection
#
#  Description:
#  ------------
#
#  Date		Author	verison	Description
#  ----		------	-------	-----------
#  08-27-1993	JC Hsu		initial design and coding
#  12-07-1995	JC Hsu		enhanced to include sky, deal with different 
#				exposure times, and individual output mask
#  04-30-1998	JC Hsu	2.1	include DQ in initial image calculation, also
#				compute initial variances for the median case
#  04-25-2003	JC Hsu	2.2	To make it to work with FITS files.
# ------------------------------------------------------------------------------
procedure t_crrej ()

pointer	tpin 			
char	fout[SZ_FNAME]
pointer	tpmask 			
pointer	par
int	niter
int	sigma[MAX_ITER]
#==============================================================================
begin
	# define the parameter structure
	call malloc (par, LEN_PAR, TY_STRUCT)

	# get CL parameters and related quantities
	call crrej_in (tpin, fout, tpmask, par, niter, sigma)
 
	# announce start of the task
	call printf ("*** CRREJ - Version %s ***\n")
	    call pargstr (VERSION)

	# perform the calculation
	call crrej_do (tpin, fout, tpmask, par, niter, sigma)

	# close file template
	call imtclose (tpin)
	call imtclose (tpmask)

	call mfree (par, TY_STRUCT)
end
