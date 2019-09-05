include "mkdark.h"

#  t_mkdark -- make dark calibration file
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  08-27-1993	J.-C. Hsu		initial design and coding
#  04-17-2003	J.-C. Hsu		Make it to work on extension FITS files
# ------------------------------------------------------------------------------

procedure t_mkdark ()

pointer	fin 			
char	fout[SZ_FNAME]
char	fout2[SZ_FNAME]
int	niter
int	sigma[MAX_ITER]
char	sigmas[SZ_LINE]
real	psigma
real	rej		
char	initial[SZ_LINE]
real	readout, gain, scale	
real	hotthresh
real	minval
real	fillval
bool	verbose
#==============================================================================
begin
	# announce start of the task
	call printf ("*** MKDARK - Version %s ***\n")
	    call pargstr (VERSION)

	# get CL parameters and related quantities
	call mkdark_in (fin, fout, fout2, niter, sigma, sigmas, psigma, rej, 
			initial, readout, gain, scale, hotthresh, minval, 
			fillval, verbose)
 
	# perform the calculation
	call mkdark_do (fin, fout, fout2, niter, sigma, sigmas, psigma, rej, 
			initial, readout, gain, scale, hotthresh, minval, 
			fillval, verbose)

	# close file template
	call imtclose (fin)

	# announce completion of task
	call printf ("Task MKDARK completed with no error\n")
end
