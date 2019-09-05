#  t_wfixup -- Interpolate over bad columns in an image
#
#  Description:
#  ------------
#
#  Date		Author      Version	Description
#  ----		------	    -------	-----------
#  01-18-93	J.-C. Hsu		design and coding
#  07-05-95	J.-C. Hsu   1.2		Use selective DQF value 
#  04-18-03	J.-C. Hsu   1.3		Make it to work on extension FITS files
# ------------------------------------------------------------------------------
procedure t_wfixup ()

pointer	tpin, tpinmask, tpout
int	maxgap
real	fillval
short	dqval
#==============================================================================
begin

	# get CL parameters and related quantities
	call wfixup_in (tpin, tpinmask, tpout, maxgap, fillval, dqval)
 
	call printf ("*** WFIXUP - Version 1.3 ***\n")

	# perform the calculation
	call wfixup_do (tpin, tpinmask, tpout, maxgap, fillval, dqval)

	# close file template
	call imtclose (tpin)
	call imtclose (tpinmask)
	call imtclose (tpout)
end
