#  mosaic_in -- Read parameters for the task wmosaic.
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
#
#  Date		Author			Description
#  ----		------			-----------
#  22-Feb-1993  J.-C. Hsu		rewrite in SPP
#------------------------------------------------------------------------------

procedure mosaic_in (tpin, tpout, nfin)

pointer	tpin, tpout	# output: file template pointers
int	nfin		# output: number of input and output files

int	nfout

pointer	imtopenp() 
int	imtlen()
#==============================================================================
begin

	# open file templates and find out how many files are in the templates
	tpin = imtopenp ("infile")
	tpout = imtopenp ("outfile")

	nfin = imtlen (tpin)
	nfout = imtlen (tpout)

	# check input parameters, the following files can not be empty
	if (nfin < 1)
	    call error (1, 
		"blank input file template or input files do not exist")

	# check the numbers of files in each template are consistent/reasonable
	if (nfin != nfout)
	    call error (1, "mismatch no. of input and output files")
end
