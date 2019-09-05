include <imhdr.h>
include	"warmpix.h"

#  warm_check -- check input files of warmpix
#
#  Description:
#  ------------
#  Open and check input images/masks to have consistent dimensions and number
#  of groups.
#  
#  Date		Author			Description
#  ----		------			-----------
#  22-Aug-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure warm_check (tpin, tpmask, fdata, ipin, ipmask, ipdark, ipflat, ngrp)

# inputs:
pointer	tpin 			# input template pointer
pointer	tpmask

# outputs:
char	fdata[SZ_FNAME]
pointer	ipin
pointer	ipmask
pointer	ipdark, ipflat
int	ngrp

# local:
char	fmask[SZ_FNAME]
int	nchar
int	grp, cl_index, cl_size
char	strval[SZ_LINE]
char	text[SZ_LINE]
char	cluster[SZ_PATHNAME]
char	sect[SZ_FNAME]
char 	ksect[SZ_FNAME]
	
pointer	gf_map()
int	imtgetim()
int	gf_gcount()
bool	strne()
bool	streq()
#==============================================================================
begin
	# read the next input image name in the template list
	nchar = imtgetim (tpin, fdata, SZ_FNAME) 

	# find out the group spec
	call imparse (fdata, cluster, SZ_PATHNAME, ksect, SZ_FNAME, 
				sect, SZ_FNAME, cl_index, cl_size)

	# open the input image
	iferr (ipin = gf_map(fdata, READ_WRITE, 0)) {
	    call sprintf (text, SZ_LINE, "cannot open image %s for READ/WRITE")
	        call pargstr (fdata)
	    call error (1, text)
	}

	# input image must be WFPC2 image
	call imgstr (ipin, "INSTRUME", strval, SZ_LINE)
	if (strne (strval, "WFPC2")) {
	    call sprintf (text, SZ_LINE, "data file %s is not WFPC2 image")
	        call pargstr (fdata)
	    call error (1, text)
	}

	# verify the image size is correct
	if (IM_LEN(ipin, 1) != DIM_X || IM_LEN(ipin, 2) != DIM_Y) {
	    call sprintf (text, SZ_LINE,
		    "file %s does not have the correct size (DIM_X x DIM_Y)")
		call pargstr (fdata)
	    call error (1, text)
	}
    
	# how many groups?
	ngrp = 1
	if (cl_index <= 0) 
	iferr (ngrp = gf_gcount(ipin)) ngrp = 1

        # read the next input mask name in the template list
        nchar = imtgetim (tpmask, fmask, SZ_FNAME) 

        iferr (ipmask = gf_map(fmask, READ_WRITE, 0)) {
	    call sprintf (text, SZ_LINE, "cannot open mask %s for READ/WRITE")
	        call pargstr (fmask)
	    call error (1, text)
        }

        # find out the group spec
        call imparse (fmask, cluster, SZ_PATHNAME, ksect, 
			SZ_FNAME, sect, SZ_FNAME, cl_index, cl_size)

        # input mask must be 2-D 
        if (IM_NDIM(ipmask) != 2) {
	    call sprintf (text, SZ_LINE, "data file %s is not 2-D")
	        call pargstr (fmask)
	    call error (1, text)
        }

        # verify the input mask size to be the same as the first image
        if (IM_LEN(ipmask, 1) != DIM_X || IM_LEN(ipmask, 2) != DIM_Y) {
	    call sprintf (text, SZ_LINE,
	      "file %s does not have the correct size (DIM_X x DIM_Y)")
	        call pargstr (fmask)
	    call error (1, text)
        }
    
	# verify the input mask has as many groups as the first image
        grp = 1
        if (cl_index <= 0) 
	    iferr (grp = gf_gcount(ipmask)) grp = 1
        if (grp != ngrp) { 
	    call sprintf (text, SZ_LINE, "data quality file %s does not have as many groups as the data file")
	        call pargstr (fmask)
	    call error (1, text)
        }
	
	# open the reference files
	call imgstr (ipin, "DARKCORR", strval, SZ_LINE)
	if (streq(strval, "COMPLETE")) {
	    call imgstr (ipin, "DARKFILE", strval, SZ_LINE)
	    iferr (ipdark = gf_map(strval, READ_ONLY, 0)) {
	        call printf ("No dark reference file available for %s\n")
		    call pargstr (fdata)
		ipdark = EMPTY
	    }
	} else 
	    ipdark = EMPTY

	call imgstr (ipin, "FLATCORR", strval, SZ_LINE)
	if (streq(strval, "COMPLETE")) {
	    call imgstr (ipin, "FLATFILE", strval, SZ_LINE)
	    iferr (ipflat = gf_map(strval, READ_ONLY, 0)) {
	        call printf ("No flatfield reference file available for %s\n")
		    call pargstr (fdata)
		ipflat = EMPTY
	    }
	} else 
	    ipflat = EMPTY
end
