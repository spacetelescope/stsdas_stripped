include <imhdr.h>
include	"mkdark.h"

#  mkdark_check -- check input files of mkdark 
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mkdark_check (fin, fdata, ipin, nf, initial, init, ipest, ngrp, 
			dim_x, dim_y)

pointer	fin 			# input: file template pointer
char	fdata[SZ_FNAME, MAX_FILES]
pointer	ipin[MAX_FILES]
int	nf			# output: number of existing files
char	initial[SZ_FNAME]
int	init
pointer	ipest
int	ngrp
int	dim_x, dim_y

int	nchar
int	n
int	grp, cl_index, cl_size
char	text[SZ_LINE]
char	cluster[SZ_PATHNAME]
char	sect[SZ_FNAME]
char 	ksect[SZ_FNAME]
	
pointer	gf_map()
int	imtlen()
int	imtgetim()
int	gf_gcount()
bool	streq()
#==============================================================================
begin

	# loop all input files 
	nf = 1
	do n = 1, imtlen(fin) {

	    # read the next file name in the template list
	    nchar = imtgetim (fin, fdata[1,n], SZ_FNAME) 

	    # find out the group spec
	    call imparse (fdata[1,n], cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	    # open the input image
	    iferr (ipin[nf] = gf_map (fdata[1,n], READ_ONLY, 0)) {
		call printf ("data file %s does not exist\n")
		    call pargstr (fdata[1,n])
		next
	    }
	    if (nf == 1) {
		dim_x = IM_LEN (ipin[nf], 1)
		dim_y = IM_LEN (ipin[nf], 2)
		ngrp = 1
		if (cl_index <= 0)
		    ngrp = gf_gcount(ipin[1])
	    }

	    # input file must be 2-D 
	    if (IM_NDIM(ipin[nf]) != 2) {
		call printf ("data file %s is not 2-D\n")
		    call pargstr (fdata[1,n])
		call gf_unmap (ipin[nf])
		next
	    }

	    # check the file size
	    if (IM_LEN(ipin[nf], 1) != dim_x || IM_LEN(ipin[nf], 2) != dim_y) {
		call printf (
		"file %s does not have the same size as the first image\n")
		    call pargstr (fdata[1,n])
		call gf_unmap (ipin[nf])
		next
	    }
    
	    # check the group number
	    grp = 1
	    if (cl_index <= 0) 
		grp = gf_gcount(ipin[nf])
	    if (grp < ngrp) { 
		call printf (
		    "file %s does not have as many groups as the first image\n")
		    call pargstr (fdata[1,n])
		call gf_unmap (ipin[nf])
		next
	    }

	    nf = nf + 1
	}
	call flush (STDOUT)

	nf = nf - 1
	if (nf == 0) 
	    call error (1, "No valid input file")
	if (nf > MAX_FILES) 
	    call error (1, "Too many input images")

	if (streq(initial, "min"))
	    init = MINIMUM
	else if (streq(initial, "med"))
	    init = MEDIAN
	else {
	    init = IMAGE

	    # find out the group spec
	    call imparse (initial, cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	    # open the initial estimate image
	    iferr (ipest = gf_map (initial, READ_ONLY, 0)) {
		call sprintf (text, SZ_LINE, "file %s does not exist")
		    call pargstr (initial)
		call error (1, text)
	    }

	    # input file must be 2-D 
	    if (IM_NDIM(ipest) != 2) {
		call sprintf (text, SZ_LINE, "file %s is not 2-D")
		    call pargstr (initial)
		call error (1, text)
	    }

	    # check the file size
	    if (IM_LEN(ipest, 1) != dim_x || IM_LEN(ipest, 2) != dim_y) {
		call sprintf (text, SZ_LINE,
		"file %s does not have the same size as the first image\n")
		    call pargstr (initial)
		call error (1, text)
	    }
    
	    # check the group number
	    grp = 1
	    if (cl_index <= 0) 
		grp = gf_gcount(ipest)
	    if (grp < ngrp) { 
		call sprintf (text, SZ_LINE,
		    "file %s does not have as many groups as the first image\n")
		    call pargstr (initial)
		call error (1, text)
	    }
	}
end
