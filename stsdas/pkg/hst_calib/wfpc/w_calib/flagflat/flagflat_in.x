#  flagflat_in -- Read CL parameters for the task flagflat.
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
#  "outmask"		Output data quality file template name
#  "boxsize"		Size of the averaging box
#  "sigma"		discriminating factor from the mean flat field
#
#  Date		Author			Description
#  ----		------			-----------
#  23-Dec-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure flagflat_in (tpin, tpinmask, tpoutmask, boxsize, sigma)

pointer	tpin, tpinmask, tpoutmask
int	boxsize
real	sigma

int	clgeti()
real	clgetr()
int	imtlen()
pointer	imtopenp()
#==============================================================================
begin

	# open input file template and find out how many files are in there 
	tpin = imtopenp ("infile")
	tpinmask = imtopenp ("inmask")
	tpoutmask = imtopenp ("outmask")

	# check that the input template can not be empty
	if (imtlen(tpin) == 0)
	    call error (1, "empty input template")
	if (imtlen(tpinmask) != 0 && imtlen(tpinmask) < imtlen(tpin))
	    call error (1, "Not enough input masks in the template")
	if (imtlen(tpoutmask) != 0 && imtlen(tpoutmask) < imtlen(tpin))
	    call error (1, "Not enough output masks in the template")

	# read other CL parameters
	boxsize = clgeti ("boxsize")
	sigma = clgetr ("sigma")
end
