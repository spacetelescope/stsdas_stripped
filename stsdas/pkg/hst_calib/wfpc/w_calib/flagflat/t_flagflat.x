#  t_flagflat -- Flag WFPC pixels with extreme flat field values
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  12-29-92	J.-C. Hsu		design and coding
# ------------------------------------------------------------------------------

procedure t_flagflat ()

pointer	tpin, tpinmask, tpoutmask
int	boxsize
real	sigma
#==============================================================================
begin

	# get CL parameters and related quantities
	call flagflat_in (tpin, tpinmask, tpoutmask, boxsize, sigma)
 
	# announce start of the task
	call printf ("*** FLAGFLAT - Version 1.0 ***\n")

	# perform the calculation
	call flagflat_do (tpin, tpinmask, tpoutmask, boxsize, sigma)

	# close file template
	call imtclose (tpin)
	call imtclose (tpinmask)
	call imtclose (tpoutmask)
end
