#  cr_scaling -- Determine the scaling factors according to exposure times or
#		 other user specified scheme.
#
#  Description:
#  ------------
#  If using the exposure time, the scaling factors are normalized to ratios 
#  relative to the max exposure. 
#
#  Date		Author			Description
#  ----		------			-----------
#  22-Feb-1995  J.-C. Hsu		convert from Rick White's IDL code
#  06-Dec-1995  J.-C. Hsu		Add scaling input parameter
#------------------------------------------------------------------------------
procedure cr_scaling (expname, ipin, fdata, nfiles,   efac)

# inputs:
char	expname[ARB]
pointer	ipin[ARB]
char	fdata[SZ_FNAME, ARB]
int	nfiles

# outputs:
real	efac[ARB]

int	k
char	text[SZ_LINE]
	
real	imgetr()
#==============================================================================
begin
	# if the parameter scaling is null, all images have equal weight.
	if (expname[1] == EOS) {
	    do k = 1, nfiles
		efac[k] = 1.
	    return
	}

	# Use exposure time as scaling factor
	do k = 1, nfiles {
	    iferr (efac[k] = imgetr (ipin[k], expname)) {
		call sprintf (text, SZ_LINE, 
			"cannot read the keyword '%s' from file '%s'")
		    call pargstr (expname)
		    call pargstr (fdata[1,k])
	 	call error (1, text)
	    }
	    if (efac[k] <= 0.) {
		call sprintf (text, SZ_LINE, 
			"exposure time of file '%s' is zero or negative")
		    call pargstr (fdata[1,k])
	 	call error (1, text)
	    }
	}
end
