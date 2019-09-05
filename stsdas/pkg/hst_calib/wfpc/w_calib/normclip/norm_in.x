#  norm_in -- Read CL parameters for the task normclip.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input data file template name
#  "inmask"		Input data quality file template name
#  "outfile"		Output data file template name
#  "outmask"		Output data quality file template name
#  "flatmin"		Minimum value of the flat field value allowed
#  "flatmax"		Maximum value of the flat field value allowed
#
#  Date		Author			Description
#  ----		------			-----------
#  23-Aug-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure norm_in (tpin, tpinmask, tpout, tpoutmask, flatmin, flatmax, verbose)

pointer	tpin, tpinmask, tpout, tpoutmask
real	flatmin, flatmax
bool	verbose

real	clgetr()
bool	clgetb()
int	imtlen()
pointer	imtopenp()
#==============================================================================
begin

	# open input file template and find out how many files are in there 
	tpin = imtopenp ("infile")
	tpinmask = imtopenp ("inmask")
	tpout = imtopenp ("outfile")
	tpoutmask = imtopenp ("outmask")

	# check that the input template can not be empty
	if (imtlen(tpin) == 0)
	    call error (1, "empty input template")
	if (imtlen(tpout) != 0 && imtlen(tpout) < imtlen(tpin))
	    call error (1, "Not enough output files in the template")
	if (imtlen(tpinmask) != 0 && imtlen(tpinmask) < imtlen(tpin))
	    call error (1, "Not enough input masks in the template")
	if (imtlen(tpoutmask) != 0 && imtlen(tpoutmask) < imtlen(tpin))
	    call error (1, "Not enough output masks in the template")

	# read other CL parameters
	flatmin = clgetr ("flatmin")
	flatmax = clgetr ("flatmax")
	verbose = clgetb ("verbose")
end
