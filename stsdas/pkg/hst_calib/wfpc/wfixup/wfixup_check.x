include <imhdr.h>

#  wfixup_check -- check input files of wfixup
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  18-Jan-1993  J.-C. Hsu		design and coding
#  04-Feb-1994  J.-C. Hsu		input file can be just one group
#------------------------------------------------------------------------------

procedure wfixup_check (tpin, tpinmask, tpout, ipin, ipinmask, 
			fout, ngrp, dim_x, dim_y, same)

pointer	tpin, tpinmask, tpout
pointer	ipin, ipinmask
char	fout[SZ_FNAME]
int	ngrp
int	dim_x, dim_y
bool	same

char	fin[SZ_FNAME], finmask[SZ_FNAME]
char	text[SZ_LINE]
char	cluster[SZ_PATHNAME]
char	sect[SZ_FNAME]
char 	ksect[SZ_FNAME]
int	mgrp, cl_index, cl_size
int	nchar
	
pointer	gf_map()
int	imtlen()
int	imtgetim()
int	gf_gcount()
bool	streq()
#==============================================================================
begin

	# if output template is empty, use the input file as output file
	same = (imtlen(tpout) == 0)

	# read the next file name in the template list
	nchar = imtgetim (tpin, fin, SZ_FNAME) 
	nchar = imtgetim (tpinmask, finmask, SZ_FNAME) 

	# open input data quality file
	call imparse(finmask, cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
			sect, SZ_FNAME, cl_index, cl_size)
	iferr(ipinmask = gf_map(finmask, READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, 
			"input data quality file %s does not exist")
	    	call pargstr (finmask)
	    call error (1, text)
	}
	mgrp = 1
	if (cl_index <= 0)
	    mgrp = gf_gcount(ipinmask)
	
	call imparse (fin, cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
			sect, SZ_FNAME, cl_index, cl_size)
	if (!same) {
	    nchar = imtgetim (tpout, fout, SZ_FNAME) 
	    same = streq (fout, fin)
	}
	if (same) {
	    iferr (ipin = gf_map(fin, READ_WRITE, 0)) {
	        call sprintf (text, SZ_LINE, 
		"input data file %s does not exist")
	    	    call pargstr (fin)
	        call error (1, text)
	    }
	    call strcpy (fin, fout, SZ_FNAME)
	} else {
	    iferr (ipin = gf_map(fin, READ_ONLY, 0)) {
	        call sprintf (text, SZ_LINE, 
		"input data file %s does not exist")
	    	    call pargstr (fin)
	        call error (1, text)
	    }
	}
	ngrp = 1
	if (cl_index <= 0)
	    ngrp = gf_gcount(ipin)

	# input file must be 2-D data 
	if (IM_NDIM(ipin) != 2) {
	    call sprintf (text, SZ_LINE, "input data file %s is not 2-D")
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
	if (mgrp != ngrp) { 
	    call sprintf (text, SZ_LINE, 
	 "%s and its data quality file %s differ in group counts")
		call pargstr (fin)
		call pargstr (finmask)
	    call error (1, text)
	}
end
