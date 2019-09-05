#  qmosaic_in -- Read parameters for the task qmosaic.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input science data file template name
#  "outfile"		Output science data file template name
#  "interp"		How to scale the PC in WFPC2?
#
#  Date		Author			Description
#  ----		------			-----------
#  31-Mar-1994  J.-C. Hsu		modified from wmosaic
#------------------------------------------------------------------------------

procedure qmosaic_in (tpin, fout, nfin, interp)

pointer	tpin		# output: file template pointers
int	nfin		# output: number of input files
char	interp[SZ_LINE]

char	fout[SZ_FNAME]
int	dum

pointer	imtopenp() 
int	imtlen()
int	clgwrd()
#==============================================================================
begin

	# open the input file template 
	tpin = imtopenp ("infile")

	call clgstr ("outfile", fout, SZ_FNAME)
	if (fout[1] == EOS) 
	    call error (1, "blank output file name")

	nfin = imtlen (tpin)
	if (nfin < 1)
	    call error (1, 
		"blank input file template or input files do not exist")
	
	dum = clgwrd ("interp", interp, SZ_LINE, 
			"|none|nearest|linear|poly3|poly5|spline3|")
end
