#  uchcoord_in -- Read parameters for the task uchcoord.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#  "input"		Input science data file template name
#  "update"		Update the group parameters?
#
#  Date		Author			Description
#  ----		------			-----------
#  17-Nov-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure uchcoord_in (tpin, nfin, update) 

pointer	tpin		# output: file template pointer
int	nfin		# output: number of input files
bool	update		# output: Update the group parameters?

pointer	imtopenp() 
int	imtlen()
bool	clgetb()
#==============================================================================
begin

	# open file templates and find out how many files are in the templates
	tpin = imtopenp ("input")

	nfin = imtlen (tpin)

	# check input parameters, the following files can not be empty
	if (nfin < 1)
	    call error (1, 
		"blank input file template or input files do not exist")

	update = clgetb ("update")
end
