include <imhdr.h>
include	"wmosaic.h"

#  mosaic_check -- check input files' attributes for the wmosaic task
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  24-Feb-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mosaic_check (tpin, tpout, ipin, fout, ngrp)

pointer	tpin, tpout		# input: file template pointers
pointer	ipin			# output: file pointers
char	fout[SZ_FNAME]		# output: output file name
int	ngrp			# output: number of groups

char	fin[SZ_FNAME]
char	flat[SZ_LINE]
char	text[SZ_LINE]
int	nchar
	
pointer	gf_map()
int	imtgetim()
int	gf_gcount()
bool	strne()
int	imaccf()
#==============================================================================
begin

	# read the next file name in the template list
	nchar = imtgetim (tpin, fin, SZ_FNAME) 
	nchar = imtgetim (tpout, fout, SZ_FNAME) 

	# open the input image
	iferr (ipin = gf_map (fin, READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, "input image %s does not exist")
	    	call pargstr (fin)
	    call error (1, text)
	}
	
	# input file must be 2-D 
	if (IM_NDIM(ipin) != 2) {
	    call sprintf (text, SZ_LINE, "input image %s is not 2-D")
		call pargstr (fin)
	    call error (1, text)
	}

	# check the input file size
	if (IM_LEN(ipin, 1) != DIM_X || IM_LEN(ipin, 2) != DIM_Y) {
	    call sprintf (text, SZ_LINE, 
		    "input image %s does not have correct size(s)")
		call pargstr (fin)
	    call error (1, text)
	}

	# make sure the keyword INSTRUME exists
	if (imaccf (ipin, "INSTRUME") == NO) {
	    call sprintf (text, SZ_LINE, 
		    "input image %s does not have the keyword INSTRUME")
		call pargstr (fin)
	    call error (1, text)
	}
    
	###ngrp = imgeti(ipin, "GCOUNT")
	ngrp = gf_gcount(ipin)

	# check the file is corrected for flat field
	call imgstr(ipin, "FLATCORR", flat, SZ_LINE)
	if (strne (flat, "DONE") && strne (flat, "COMPLETE")) {
	    call printf (
		"CAUTION: input file %s is not corrected for flat field \n")
		call pargstr (fin)
	}

	call flush (STDOUT)
end
