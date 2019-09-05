include <imhdr.h>
include	"gsky.h"

#  crrej_check -- check input files of crrej
#
#  Description:
#  ------------
#  Open and check input images to have consistent dimensions and number
#  of groups.
#  
#  Date		Author		Description
#  ----		------		-----------
#  12-Jun-1996  I. Busko	Naive adaptation from xcrrej.
#  09-Feb-1998  I. Busko	Test ngroups before setting skyname=EOS
#------------------------------------------------------------------------------
procedure gsky_check (tpin, tpmask, par, fdata, ipin, ipmask, ngrp, 
                      dim_x, dim_y)

# inputs:
pointer	tpin 			# input template pointer
pointer tpmask			# mask template pointer
pointer	par			# par structure pointer

# outputs:
char	fdata[SZ_FNAME, ARB]
pointer	ipin[ARB]
pointer ipmask[ARB]
int	ngrp
int	dim_x, dim_y

# local:


char	fmask[SZ_FNAME, MAX_FILES]
int	nchar, n, acmode
int	grp, cl_index, cl_size
real	buf
#char	append[SZ_FNAME]
char	text[SZ_LINE]
char	cluster[SZ_PATHNAME]
char	sect[SZ_FNAME]
char 	ksect[SZ_FNAME]
	
pointer	immap()
int	imtlen()
int	imtgetim()
int	imgeti()
real	imgetr()
#int	itoc()
#==============================================================================
begin

	# decide the access mode of input images
	if (SKYNAME(par) == EOS && !SUBSKY(par)) acmode = READ_ONLY
	else acmode = READ_WRITE

	# loop all input files 
	do n = 1, imtlen(tpin) {

	    # read the next input image name in the template list
	    nchar = imtgetim (tpin, fdata[1,n], SZ_FNAME) 

	    # find out the group spec
	    call imparse (fdata[1,n], cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	    # open the input image
	    iferr (ipin[n] = immap (fdata[1,n], acmode, 0)) {
		call sprintf (text, SZ_LINE, "Cannot open data file '%s'")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }

	    # use the first image's attributes to compare with the rest of 
	    # the files
	    if (n == 1) {
		dim_x = IM_LEN (ipin[n], 1)
		dim_y = IM_LEN (ipin[n], 2)
		
		# if no group specification, read the keyword GCOUNT, if 
		# no such keyword, there is only one group
		if (cl_index <= 0) {
		    iferr (ngrp = imgeti (ipin[1], "GCOUNT")) ngrp = 1
		} else ngrp = 1

		# check if the group parameter specified by SKYNAME exists
		if (SKYNAME(par) != EOS) {
		    iferr (buf = imgetr (ipin[n], SKYNAME(par))) {
		        call printf ("Group parameter '%s' does not exist.\n")
			    call pargstr (SKYNAME(par))
	                if (ngrp > 1)
	                    SKYNAME(par) = EOS
		    }
		}
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
		iferr (grp = imgeti(ipin[n], "GCOUNT")) grp = 1
	    } else grp = 1
	    if (grp < ngrp) { 
		call sprintf (text, SZ_LINE,
		    "file '%s' does not have as many groups as the first image")
		    call pargstr (fdata[1,n])
		call error (1, text)
	    }
	
	    # open masks, if any.
	    if (imtlen(tpmask) > 0) {

	        # read the next mask name in the template list
	        nchar = imtgetim (tpmask, fmask[1,n], SZ_FNAME) 
	     
	        iferr (ipmask[n] = immap (fmask[1,n], READ_ONLY, 0))
	            ipmask[n] = NULL
	        else {

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
		        iferr (grp = imgeti(ipmask[n], "GCOUNT")) grp = 1
	            } else grp = 1
	            if (grp < ngrp) { 
		        call sprintf (text, SZ_LINE,
		    "file '%s' does not have as many groups as the first image")
		            call pargstr (fmask[1,n])
		        call error (1, text)
	            }
	        }
	    } else
	        ipmask[n] = NULL
	}
end
