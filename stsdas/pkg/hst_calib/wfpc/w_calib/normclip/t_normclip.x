#  t_normclip -- Normalize WFPC flat field between chips
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  08-19-92	J.-C. Hsu		adapt from Jeff Hester's C code
# ------------------------------------------------------------------------------

procedure t_normclip ()

pointer	tpin, tpinmask, tpout, tpoutmask
real	flatmin, flatmax
bool	verbose
#==============================================================================
begin

	# get CL parameters and related quantities
	call norm_in (tpin, tpinmask, tpout, tpoutmask, flatmin, flatmax, 
			verbose)
 
	# announce start of the task
	call printf ("*** NORMCLIP - Version 1.0 ***\n")

	# perform the calculation
	call norm_do (tpin, tpinmask, tpout, tpoutmask, flatmin, flatmax, 
			verbose)

	# close file template
	call imtclose (tpin)
	call imtclose (tpinmask)
	call imtclose (tpout)
	call imtclose (tpoutmask)
end
