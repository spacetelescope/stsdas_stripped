include <imhdr.h>

#  fflat_check -- check input files of flagflat
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  29-Dec-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure fflat_check (tpin, tpinmask, tpoutmask, ipin, ipinmask, 
			foutmask, ngrp, dim_x, dim_y, samemask)

pointer	tpin, tpinmask, tpoutmask
pointer	ipin, ipinmask
char	foutmask[SZ_FNAME]
int	ngrp
int	dim_x, dim_y
bool	samemask

char	fin[SZ_FNAME], finmask[SZ_FNAME]
char	text[SZ_LINE]
#char	extname[SZ_FNAME]
#int	rootlen, extlen
int	nchar
	
pointer	immap()
int	imtlen()
int	imtgetim()
int	imgeti()
#int	fnextn(), strlen()
bool	streq()
#==============================================================================
begin

	# if output template is empty, use the input file as output file
	samemask = (imtlen(tpoutmask) == 0)

	# read the next file name in the template list
	nchar = imtgetim (tpin, fin, SZ_FNAME) 
	nchar = imtgetim (tpinmask, finmask, SZ_FNAME) 

	# open input file
	iferr (ipin = immap (fin, READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, "input data file %s does not exist")
	    	call pargstr (fin)
	    call error (1, text)
	}
	if (!samemask) {
	    nchar = imtgetim (tpoutmask, foutmask, SZ_FNAME) 
	    samemask = streq (foutmask, finmask)
	}
	if (samemask) {
	    iferr (ipinmask = immap (finmask, READ_WRITE, 0)) {
	        call sprintf (text, SZ_LINE, 
		"input data quality file %s does not exist")
	    	    call pargstr (finmask)
	        call error (1, text)
	    }
	    call strcpy (finmask, foutmask, SZ_FNAME)
	} else {
	    iferr (ipinmask = immap (finmask, READ_ONLY, 0)) {
	        call sprintf (text, SZ_LINE, 
		"input data quality file %s does not exist")
	    	    call pargstr (finmask)
	        call error (1, text)
	    }
	}

	    # get the root name and extension name
	    #extlen = fnextn (fin, extname, SZ_FNAME)
	    #if (extlen == 0) 
		#call strcat (".", fin, SZ_FNAME)
	    #rootlen = strlen(fin)-extlen
	    #call strcpy (fin, finmask, rootlen)

	    # if input mask is not specified, use the default (.b6h) extension
	    #if (extlen == 0)
	        #call strcat ("b6h", finmask, SZ_FNAME)

	# input file must be 2-D data and real data type
	if (IM_NDIM(ipin) != 2 || IM_PIXTYPE(ipin) != TY_REAL) {
	    call sprintf (text, SZ_LINE,
		    "input data file %s is not 2-D or is not data type REAL")
		call pargstr (fin)
	    call error (1, text)
	}

	# check the file size
	dim_x = IM_LEN(ipin, 1) 
	dim_y = IM_LEN(ipin, 2)
    
	# input data mask file must be 2-D data and short integer data type
	if (IM_NDIM(ipinmask) != 2 || IM_PIXTYPE(ipinmask) != TY_SHORT) {
	    call sprintf (text, SZ_LINE, 
	    "input data quality file %s is not 2-D or is not data type SHORT")
		call pargstr (finmask)
	    call error (1, text)
	}

	# check the file size
	if (IM_LEN(ipinmask, 1) != dim_x || IM_LEN(ipinmask, 2) != dim_y) {
	    call sprintf (text, SZ_LINE,
	    "%s and its data quality file %s differ in sizes")
		call pargstr (fin)
		call pargstr (finmask)
	    call error (1, text)
	}

	# number of groups must be equal between data file and mask file
	ngrp = imgeti (ipin, "GCOUNT")
	if (imgeti(ipinmask, "GCOUNT") != ngrp) { 
	    call sprintf (text, SZ_LINE, 
	 "%s and its data quality file %s differ in group counts")
		call pargstr (fin)
		call pargstr (finmask)
	    call error (1, text)
	}
end
