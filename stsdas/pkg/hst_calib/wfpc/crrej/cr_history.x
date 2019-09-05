include	"crrej.h"

#  cr_history -- write the history to the output files
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  11-Dec-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure cr_history(ipout, fdata, nfiles, par)

# inputs:
pointer	ipout 			# output image pointer
char	fdata[SZ_FNAME, ARB] 	# input images 
int	nfiles			# number of input images
pointer	par			# the par structure pointer

# locals:
int	n
char	text[SZ_LINE], tstring[SZ_LINE]
	
long	clktime()
#==============================================================================
begin
	call cnvtime(clktime(0), tstring, SZ_LINE)
	call sprintf(text, SZ_LINE, 
		"Task CRREJ, version %s, starts at %s")
	    call pargstr(VERSION)
	    call pargstr(tstring)
	call gf_iputh(ipout, "HISTORY", text)

	# write input file names to output file as history
	call sprintf(text, SZ_LINE, "input files used are:")
	call gf_iputh(ipout, "HISTORY", text)
	do n = 1, nfiles {
	    call sprintf(text, SZ_LINE, "  %s")
	        call pargstr(fdata[1,n])
	    call gf_iputh(ipout, "HISTORY", text)
    	}

	# record parameters in the header
	call sprintf(text, SZ_LINE, "Rejection levels are : %s sigmas")
	    call pargstr(SIGMAS(par))
	call gf_iputh(ipout, "HISTORY", text)
	call sprintf(text, SZ_LINE, "Expansion radius is : %0.2f pixels")
	    call pargr(REJ(par))
	call gf_iputh(ipout, "HISTORY", text)
	call sprintf(text, SZ_LINE, 
			"Expansion discriminant reduction factor is : %0.3f")
	    call pargr(PSIGMA(par))
	call gf_iputh(ipout, "HISTORY", text)

	call sprintf(text, SZ_LINE, "Readout noise(s) = %s(DN)")
	    call pargstr(READNOISE(par))
	call gf_iputh(ipout, "HISTORY", text)
	call sprintf(text, SZ_LINE, "Gain(s) = %s(electron/DN)")
	    call pargstr(ATODGAIN(par))
	call gf_iputh(ipout, "HISTORY", text)
	call sprintf(text, SZ_LINE, "Scale noise(s) = %s(percent)")
	    call pargstr(SCALENOISE(par))
	call gf_iputh(ipout, "HISTORY", text)

	call sprintf(text, SZ_LINE, "Data range: > %0.2f DN and < %0.2f DN")
	    call pargr(LOWER(par))
	    call pargr(UPPER(par))
	call gf_iputh(ipout, "HISTORY", text)

	call sprintf(text, SZ_LINE, "Initial average image used : %s")
	    call pargstr(INITIAL(par))
	call gf_iputh(ipout, "HISTORY", text)
	call sprintf(text, SZ_LINE, "Sky used : %s")
	    call pargstr(SKY(par))
	call gf_iputh(ipout, "HISTORY", text)
end
