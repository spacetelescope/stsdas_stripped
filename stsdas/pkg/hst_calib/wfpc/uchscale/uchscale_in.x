#  uchscale_in -- Read parameters for the task uchscale.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input science data file template name
#  "newscale"		New plate scale
#
#  Date		Author			Description
#  ----		------			-----------
#  23-Sep-1994  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure uchscale_in (tpin, nfin, strscale) 

pointer	tpin		# output: file template pointer
int	nfin		# output: number of input files

char	strscale[SZ_LINE]

pointer	imtopenp() 
int	imtlen()
#==============================================================================
begin

	# open file templates and find out how many files are in the templates
	tpin = imtopenp ("infile")

	nfin = imtlen (tpin)

	# check input parameters, the following files can not be empty
	if (nfin < 1)
	    call error (1, 
		"blank input file template or input files do not exist")

	call clgstr ("newscale", strscale, SZ_LINE)
end
