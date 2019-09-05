include <imhdr.h>
include	"normclip.h"

#  norm_check -- check input files of normclip
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  24-Aug-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure norm_check (tpin, tpinmask, tpout, tpoutmask, ipin, ipinmask, fout,
			foutmask, ngrp, samefile, samemask)

pointer	tpin, tpinmask, tpout, tpoutmask
pointer	ipin, ipinmask
char	fout[SZ_FNAME], foutmask[SZ_FNAME]
int	ngrp
bool	samefile, samemask

char	fin[SZ_FNAME], finmask[SZ_FNAME]
#char	cam1[SZ_LINE], cam2[SZ_LINE]
#char	extname[SZ_FNAME]
#int	rootlen, extlen
int	nchar
	
pointer	immap()
int	imtlen()
int	imtgetim()
int	imgeti()
#int	fnextn(), strlen()
#bool	strne()
bool	streq()
#==============================================================================
begin

	# if output template is empty, use the input file as output file
	samefile = (imtlen(tpout) == 0)
	samemask = (imtlen(tpoutmask) == 0)

	# read the next file name in the template list
	nchar = imtgetim (tpin, fin, SZ_FNAME) 
	nchar = imtgetim (tpinmask, finmask, SZ_FNAME) 

	# open input file
	if (!samefile) {
	    nchar = imtgetim (tpout, fout, SZ_FNAME) 
	    samefile = streq (fout, fin)
	}
	if (samefile) {
	    iferr (ipin = immap (fin, READ_WRITE, 0)) {
	        call printf ("input file %s does not exist\n")
	    	    call pargstr (fin)
	    }
	    call strcpy (fin, fout, SZ_FNAME)
	} else {
	    iferr (ipin = immap (fin, READ_ONLY, 0)) {
	        call printf ("input file %s does not exist\n")
	    	    call pargstr (fin)
	    }
	}
	if (!samemask) {
	    nchar = imtgetim (tpoutmask, foutmask, SZ_FNAME) 
	    samemask = streq (foutmask, finmask)
	}
	if (samemask) {
	    iferr (ipinmask = immap (finmask, READ_WRITE, 0)) {
	        call printf ("input mask %s does not exist\n")
	    	    call pargstr (finmask)
	    }
	    call strcpy (finmask, foutmask, SZ_FNAME)
	} else {
	    iferr (ipinmask = immap (finmask, READ_ONLY, 0)) {
	        call printf ("input mask %s does not exist\n")
	    	    call pargstr (finmask)
	    }
	}

	    # get the root name and extension name
	    #extlen = fnextn (fin, extname, SZ_FNAME)
	    #if (extlen == 0) 
		#call strcat (".", fin, SZ_FNAME)
	    #rootlen = strlen(fin)-extlen
	    #call strcpy (fin, finmask, rootlen)

	    # if there is no extension, use the default (.c0h) extension
	    #if (extlen == 0)
		#call strcat ("c0h", fin, SZ_FNAME)

	    # always use the data quality file produced by PODPS, i.e. .c1h
	    #call strcat ("c1h", finmask, SZ_FNAME)

	# input file must be 2-D data and real data type
	if (IM_NDIM(ipin) != 2 || IM_PIXTYPE(ipin) != TY_REAL) {
	    call printf ("data file %s is not 2-D or is not data type REAL\n")
		call pargstr (fin)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}

	# check the file size
	if (IM_LEN(ipin, 1) != DIM_X || IM_LEN(ipin, 2) != DIM_Y) {
	    call printf ("data file %s does not have correct size(s)\n")
		call pargstr (fin)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}
    
	# input data mask file must be 2-D data and short integer data type
	if (IM_NDIM(ipinmask) != 2 || IM_PIXTYPE(ipinmask) != TY_SHORT) {
	    call printf ("mask file %s is not 2-D or is not data type SHORT\n")
		call pargstr (finmask)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}

	# check the file size
	if (IM_LEN(ipinmask, 1) != DIM_X || IM_LEN(ipinmask, 2) != DIM_Y) {
	    call printf ("mask file %s does not have correct size(s)\n")
		call pargstr (finmask)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}

	# number of groups must be equal between data file and mask file
	ngrp = imgeti (ipin, "GCOUNT")
	if (imgeti(ipinmask, "GCOUNT") != ngrp) { 
	    call printf ("input file/mask %s have different group count\n")
		call pargstr (fin)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}

	# check the camera ID
	# comment out the following lines so WFPC2 images can run(JC Hsu 5/2/95)
	#call imgstr(ipin, "CAMERA", cam1, SZ_LINE)
	#call imgstr(ipinmask, "CAMERA", cam2, SZ_LINE)
	#if (strne (cam1, cam2)) {
	    #call printf ("input file/mask %s have different camera\n")
		#call pargstr (fin)
	    #call imunmap (ipin)
	    #call imunmap (ipinmask)
	#}

	# check the filters
	if (imgeti(ipin, "FILTER1") != imgeti(ipinmask, "FILTER1") || 
	    imgeti(ipin, "FILTER2") != imgeti(ipinmask, "FILTER2")) { 
	    call printf ("input file/mask %s have different filter(s)\n")
		call pargstr (fin)
	    call imunmap (ipin)
	    call imunmap (ipinmask)
	}

	call flush (STDOUT)
end
