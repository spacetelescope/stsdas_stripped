#  wfixup_in -- Read CL parameters for the task wfixup.
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
#  "maxgap"		Largest gap allowed for interpolation
#  "fillval"		Fill value for pixels in a gap larger than maxgap
#
#  Date		Author			Description
#  ----		------			-----------
#  18-Jan-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure wfixup_in (tpin, tpinmask, tpout, maxgap, fillval, dqval)

pointer	tpin, tpinmask, tpout
int	maxgap
real	fillval
short	dqval

int	badbits

int	clgeti()
real	clgetr()
int	imtlen()
pointer	imtopenp()
#==============================================================================
begin

	# open input file template and find out how many files are in there 
	tpin = imtopenp ("infile")
	tpinmask = imtopenp ("inmask")
	tpout = imtopenp ("outfile")

	# check that the input template can not be empty
	if (imtlen(tpin) == 0)
	    call error (1, "empty input template")
	if (imtlen(tpinmask) != 0 && imtlen(tpinmask) < imtlen(tpin))
	    call error (1, "Not enough input masks in the template")
	if (imtlen(tpout) != 0 && imtlen(tpout) < imtlen(tpin))
	    call error (1, "Not enough output files in the template")

	# read other CL parameters
	maxgap = clgeti ("maxgap")
	fillval = clgetr ("fillval")

	# get the data quality value to be used
	call dqinit (badbits)
	dqval = short (badbits)
end
