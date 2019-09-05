include <imhdr.h>
include	"streakflat.h"

#  flat_check -- check input files of streakflat 
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Mar-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure flat_check (fin, fdata, fmask, ftmp, ipin, ipmask, 
			nf, camera, ngrp, strpa)

pointer	fin 			# input: file template pointer
char	fdata[SZ_FNAME, MAX_FILES], fmask[SZ_FNAME, MAX_FILES]
char	ftmp[SZ_FNAME, MAX_FILES]
pointer	ipin[MAX_FILES], ipmask[MAX_FILES]
char	camera[SZ_LINE]
char	instr[SZ_LINE]
int	ngrp
real	strpa[MAX_FILES]
int	nf			# output: number of existing files

char	fshp[SZ_FNAME]
char	cam1[SZ_LINE], cam2[SZ_LINE]
char	extname[SZ_FNAME]
int	filter1, filter2
int	rootlen, extlen, nchar
int	n
	
pointer	immap()
int	imtlen()
int	imtgetim()
int	imgeti()
int	fnextn(), strlen()
bool	streq()
bool	strne()
#==============================================================================
begin

	# loop all input files 
	nf = 1
	do n = 1, imtlen(fin) {

	    # read the next file name in the template list
	    nchar = imtgetim (fin, fdata[1,n], SZ_FNAME) 

	    # get the root name and extension name
	    extlen = fnextn (fdata[1,n], extname, SZ_FNAME)
	    if (extlen == 0) 
		call strcat (".", fdata[1,n], SZ_FNAME)
	    rootlen = strlen(fdata[1,n])-extlen
	    call strcpy (fdata[1,n], fmask[1,n], rootlen)
	    call strcpy (fdata[1,n], ftmp[1,n], rootlen)
	    call strcpy (fdata[1,n], fshp, rootlen)

	    # if there is no extension, use the default (.c0h) extension
	    if (extlen == 0)
		call strcat ("c0h", fdata[1,n], SZ_FNAME)

	    # always use the data quality file produced by PODPS, i.e. .c1h
	    call strcat ("c1h", fmask[1,n], SZ_FNAME)
	    call strcat ("sth", ftmp[1,n], SZ_FNAME)
	    call strcat ("shh", fshp, SZ_FNAME)

	    # open and read SHP file header keywords to calculate the streak
	    # angle
	    iferr (call streak_angle (fshp, strpa[nf])) {
		call printf (
		  "SHP file %s doesn't exist or doesn't have needed keywords\n")
		    call pargstr (fshp)
		next
	    }

	    # open input file/mask
	    iferr (ipin[nf] = immap (fdata[1,n], READ_ONLY, 0)) {
		call printf ("data file %s does not exist\n")
		    call pargstr (fdata[1,n])
		next
	    }
	    iferr (ipmask[nf] = immap (fmask[1,n], READ_ONLY, 0)) {
		call printf ("mask file %s does not exist\n")
		    call pargstr (fmask[1,n])
		call imunmap (ipin[nf])
		next
	    }

	    # input file must be 2-D data and real data type
	    if (IM_NDIM(ipin[nf]) != 2 ||
	    	IM_PIXTYPE(ipin[nf]) != TY_REAL) {
		call printf (
		        "data file %s is not 2-D or is not data type REAL\n")
		    call pargstr (fdata[1,n])
		call imunmap (ipin[nf])
		call imunmap (ipmask[nf])
		next
	    }

	    # check the file size
	    if (IM_LEN(ipin[nf], 1) != DIM_X || IM_LEN(ipin[nf], 2) != DIM_Y) {
		call printf ("data file %s does not have correct size(s)\n")
		    call pargstr (fdata[1,n])
		call imunmap (ipin[nf])
		call imunmap (ipmask[nf])
		next
	    }
    
	    # input data mask file must be 2-D data and short integer data type
	    if (IM_NDIM(ipmask[nf]) != 2 ||
	    	IM_PIXTYPE(ipmask[nf]) != TY_SHORT) {
		call printf (
			"mask file %s is not 2-D or is not data type SHORT\n")
		    call pargstr (fmask[1,n])
		call imunmap (ipin[nf])
		call imunmap (ipmask[nf])
		next
	    }

	    # check the file size
	    if (IM_LEN(ipmask[nf], 1) != DIM_X || 
		IM_LEN(ipmask[nf], 2) != DIM_Y) {
		call printf ("mask file %s does not have correct size(s)\n")
		    call pargstr (fmask[1,n])
		call imunmap (ipin[nf])
		call imunmap (ipmask[nf])
		next
	    }

	    if (nf == 1) {
		ngrp = imgeti (ipin[1], "GCOUNT")
		call imgstr (ipin[1], "INSTRUME", instr, SZ_LINE)
		if (streq(instr, "WFPC"))
		    call imgstr (ipin[1], "CAMERA", camera, SZ_LINE)
		else if (streq(instr, "WFPC2"))
		    call strcpy (instr, camera, SZ_LINE)
		else
		    camera[1] = EOS
		filter1 = imgeti (ipin[1], "FILTER1")
		filter2 = imgeti (ipin[1], "FILTER2")

	        # number of groups must be less than NGROUPS
	    	if (ngrp > NGROUPS) {
		    call printf ("first file %s has too many groups\n")
		        call pargstr (fdata[1,n])
		    call imunmap (ipin[nf])
		    call imunmap (ipmask[nf])
		    next
		}
	    }
		
	    # check the group number
	    if (imgeti(ipin[nf], "GCOUNT") != ngrp || 
	    	imgeti(ipmask[nf], "GCOUNT") != ngrp) { 
		call printf ("input file/mask %s has incorrect group count\n")
		    call pargstr (fdata[1,n])
		call imunmap (ipin[nf])
		call imunmap (ipmask[nf])
		next
	    }

	    # check the camera ID
	    if (streq(instr, "WFPC")) {
	        call imgstr(ipin[nf], "CAMERA", cam1, SZ_LINE)
	        call imgstr(ipmask[nf], "CAMERA", cam2, SZ_LINE)
	        if (strne (camera, cam1) || strne (camera, cam2)) {
		    call printf ("input file/mask %s uses a different camera\n")
		        call pargstr (fdata[1,n])
		    call imunmap (ipin[nf])
		    call imunmap (ipmask[nf])
		    next
		}
	    }

	    # check the filters
	    if (imgeti(ipin[nf], "FILTER1") != filter1 || 
	    	imgeti(ipin[nf], "FILTER2") != filter2 || 
	    	imgeti(ipmask[nf], "FILTER1") != filter1 || 
	    	imgeti(ipmask[nf], "FILTER2") != filter2) { 
		call printf ("input file/mask %s uses different filter(s)\n")
		    call pargstr (fdata[1,n])

		# do not abandon, just warn, if filters are different
		#call imunmap (ipin[nf])
		#call imunmap (ipmask[nf])
		#next
	    }

	    nf = nf + 1
	}
	call flush (STDOUT)

	nf = nf - 1
	if (nf == 0) 
	    call error (1, "No valid input file/mask")
end
