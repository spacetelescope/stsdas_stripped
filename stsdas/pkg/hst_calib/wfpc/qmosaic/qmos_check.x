include <imhdr.h>

#  qmosaic_check -- check input files' attributes for the qmosaic task
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  31-Mar-1994  J.-C. Hsu		modified from wmosaic
#  24-Apr-2003  J.-C. Hsu		modify to work for FITS files
#------------------------------------------------------------------------------

procedure qmosaic_check (tpin, ipin, fin, ngrp)

pointer	tpin			# input: input file template pointer
pointer	ipin			# output: file pointers
char	fin[SZ_FNAME]
int	ngrp			# output: number of groups

char	text[SZ_LINE]
int	nchar
	
pointer	gf_map()
int	imtgetim()
int	gf_gcount()
#==============================================================================
begin

	# read the next file name in the template list
	nchar = imtgetim(tpin, fin, SZ_FNAME) 

	# open the input image
	iferr (ipin = gf_map(fin, READ_ONLY, 0)) {
	    call sprintf(text, SZ_LINE, "input data file %s does not exist")
	    	call pargstr(fin)
	    call error(1, text)
	}
	
	# input file must be 2-D 
	if (IM_NDIM(ipin) != 2) {
	    call sprintf(text, SZ_LINE, "input data file %s is not 2-D")
		call pargstr(fin)
	    call error(1, text)
	}

	# if GCOUNT doe not exist, assume it is single group file
	iferr (ngrp = gf_gcount(ipin))
	    ngrp = 1

	call flush(STDOUT)
end
