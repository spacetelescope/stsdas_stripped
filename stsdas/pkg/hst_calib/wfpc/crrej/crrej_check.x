include <imhdr.h>
include	"crrej.h"

#  crrej_check -- check input files of crrej
#
#  Description:
#  ------------
#  Open and check input images/masks to have consistent dimensions and number
#  of groups.
#  
#  Date		Author			Description
#  ----		------			-----------
#  22-Feb-1995  J.-C. Hsu		convert from Rick White's IDL code
#  08-Dec-1995  J.-C. Hsu		modify from cr2_check, add the initial
#					file option
#------------------------------------------------------------------------------
procedure crrej_check (tpin, tpmask, par,   fdata, ipin, ipmask, ngrp, 
			dim_x, dim_y, init, ipest)

# inputs:
pointer	tpin 			# input template pointer
pointer	tpmask			# mask template pointer
pointer	par			# par structure pointer

# outputs:
char	fdata[SZ_FNAME, ARB]
pointer	ipin[ARB]
pointer	ipmask[ARB]
int	ngrp
int	dim_x, dim_y
int	init			# initial image choice flag
pointer	ipest			# initial image pointer

# local:
char	fmask[SZ_FNAME, MAX_FILES]
int	nchar
int	k, n
int	acmode
int	grp, cl_index, cl_size
real	buf
char	append[SZ_FNAME]
char	dumch[1]
char	text[SZ_LINE]
char	cluster[SZ_PATHNAME]
char	sect[SZ_FNAME]
char 	ksect[SZ_FNAME]
	
pointer	gf_map()
int	imtlen()
int	imtgetim()
real	imgetr()
int	itoc()
bool	streq()
int     gf_gcount()
#==============================================================================
begin

	# decide the access mode of input images
	if (SKYNAME(par) == EOS) acmode = READ_ONLY
	else acmode = READ_WRITE

	# loop all input files 
	do n = 1, imtlen(tpin) {

	    # read the next input image name in the template list
	    nchar = imtgetim (tpin, fdata[1,n], SZ_FNAME) 

	    # find out the group spec
	    call imparse (fdata[1,n], cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	    # open the input image
	    iferr (ipin[n] = gf_map (fdata[1,n], acmode, 0)) {
		call sprintf (text, SZ_LINE, "Cannot open data file '%s'")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }

	    # use the first image's attributes to compare with the rest of 
	    # the files
	    if (n == 1) {
		dim_x = IM_LEN (ipin[n], 1)
		dim_y = IM_LEN (ipin[n], 2)
		
		# check if the group parameter specified by SKYNAME exists
		if (SKYNAME(par) != EOS) {
		    iferr (buf = imgetr (ipin[n], SKYNAME(par))) {
		        call printf ("Group parameter '%s' does not exist.\n")
			    call pargstr (SKYNAME(par))
		        SKYNAME(par) = EOS
		    }
		}

		# if there is group specification, only one group
		if (cl_index <= 0) {
		    ngrp = gf_gcount(ipin[n])
		} else ngrp = 1
	    }

	    # input image must be 2-D 
	    if (IM_NDIM(ipin[n]) != 2) {
		call sprintf (text, SZ_LINE, "data file '%s' is not 2-D")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }

	    # verify the image size to be the same as the first image
	    if (IM_LEN(ipin[n], 1) != dim_x || IM_LEN(ipin[n], 2) != dim_y) {
		call sprintf (text, SZ_LINE,
		    "file '%s' does not have the same size as the first image")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }
    
	    # verify the image has as many groups as the first image
	    if (cl_index <= 0) {
		grp = gf_gcount(ipin[n])
	    } else grp = 1
	    if (grp < ngrp) { 
		call sprintf (text, SZ_LINE,
		    "file '%s' does not have as many groups as the first image")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }
	
	    # open masks, if any.  If masks do not exist, create masks using 
	    # the input image as a template
	    if (imtlen(tpmask) > 0) {

	        # read the next output mask name in the template list
	        nchar = imtgetim (tpmask, fmask[1,n], SZ_FNAME) 
	     
	        iferr (ipmask[n] = gf_map (fmask[1,n], READ_WRITE, 0)) {
		    call strcpy ("[1/", append, SZ_FNAME)
		    k = itoc (ngrp, dumch, 1)
		    call strcat (dumch, append, SZ_FNAME)
		    call strcat ("]", append, SZ_FNAME)
	            call strcat (append, fmask[1,n], SZ_FNAME)

	            ipmask[n] = gf_map (fmask[1,n], NEW_COPY, ipin[n])
		    IM_PIXTYPE(ipmask[n]) = TY_SHORT
	        } else {

	            # find out the group spec
	            call imparse (fmask[1,n], cluster, SZ_PATHNAME, ksect, 
				SZ_FNAME, sect, SZ_FNAME, cl_index, cl_size)

	            # mask must be 2-D 
	            if (IM_NDIM(ipmask[n]) != 2) {
		        call sprintf (text, SZ_LINE, 
					"mask file '%s' is not 2-D")
		            call pargstr (fmask[1,n])
		        call error (1, text)
	            }

	            # verify the mask size to be the same as the first image
	            if (IM_LEN(ipmask[n], 1) != dim_x || 
		        IM_LEN(ipmask[n], 2) != dim_y) {
		        call sprintf (text, SZ_LINE,
		     "file '%s' does not have the same size as the first image")
		            call pargstr (fmask[1,n])
		        call error (1, text)
	            }
    
	            # verify the mask has as many groups as the first image
	            if (cl_index <= 0) {
			grp = gf_gcount(ipmask[n])
	            } else grp = 1
	            if (grp < ngrp) { 
		        call sprintf (text, SZ_LINE,
		    "file '%s' does not have as many groups as the first image")
		            call pargstr (fmask[1,n])
		        call error (1, text)
	            }
	        }
	    }
	}

	if (streq(INITIAL(par), "min"))
	    init = MINIMUM
	else if (streq(INITIAL(par), "med"))
	    init = MEDIAN
	else {
	    init = IMAGE

	    # find out the group spec
	    call imparse (INITIAL(par), cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	    # open the initial estimate image
	    iferr (ipest = gf_map (INITIAL(par), READ_ONLY, 0)) {
		call sprintf (text, SZ_LINE, "file '%s' does not exist")
		    call pargstr (INITIAL(par))
		call error (1, text)
	    }

	    # input file must be 2-D 
	    if (IM_NDIM(ipest) != 2) {
		call sprintf (text, SZ_LINE, "file '%s' is not 2-D")
		    call pargstr (INITIAL(par))
		call error (1, text)
	    }

	    # check the file size
	    if (IM_LEN(ipest, 1) != dim_x || IM_LEN(ipest, 2) != dim_y) {
		call sprintf (text, SZ_LINE,
		 "file '%s' does not have the same size as the first image\n")
		    call pargstr (INITIAL(par))
		call error (1, text)
	    }
    
	    # check the group number
	    if (cl_index <= 0) {
		grp = gf_gcount(ipest)
	    } else grp = 1
	    if (grp < ngrp) { 
		call sprintf (text, SZ_LINE,
		  "file '%s' does not have as many groups as the first image\n")
		    call pargstr (INITIAL(par))
		call error (1, text)
	    }
	}
end
