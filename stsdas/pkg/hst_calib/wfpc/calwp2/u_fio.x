#			File:	u_fio.x

include <imhdr.h>
#include <imio.h>
include <fio.h>
include	<tbset.h>
include <mach.h>
include <error.h>
include	"u_incl.h"
include "u_data.h"
#include "u_context.h"

#################################################################################
#										#
#  U_INPUT --	Open selected element of input image for reading and return	#
#		image descriptor.  						#
#										#
#  Last modified:								#
#	22 Jul 92 by RAShaw	Initial implementation				#

pointer procedure u_input (element, name)

#  Calling arguments:
int	element			# element number
char	name[SZ_FNAME]		# name of input image

#  Local variables:
char	fullname[SZ_FNAME]	# full filename 
pointer	im			# input image descriptor (returned)
char	text[SZ_LINE]		# text of error message

#  Functions used:
pointer	gf_map()

errchk	u_addgrp, u_error, gf_map

begin

# add appropriate group element to file name


	call u_addgrp (name, fullname, element)
	
	iferr (im = gf_map (fullname, READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, "Error opening input %s" )
	    call pargstr (fullname)
	    call u_error (text)
	}

	return (im)
end


#################################################################################
#										#
#  U_IN_GSKIP -- Skip to specified group of open input image for reading.  	#
#		 This approach is much faster than closing/reopening images.  	#
#										#
#  Last modified:								#
#	20 Aug 92 by RAShaw	Initial implementation				#

procedure u_in_gskip (element, imp)

#  Calling arguments:
int	element			# element number
pointer	imp			# input image descriptor

#  Local variables:
real	dmin, dmax		# input image min/max (dummy)
char	text[SZ_LINE]		# text of error message

errchk	u_error, gf_opengr

begin

	iferr (call gf_opengr (imp, element, dmin, dmax, NULL)) {
	    call sprintf (text, SZ_LINE, 
		"Error skipping groups in input file, group: %1d")
	        call pargi (element)
	        call u_error (text)
	}

end


#################################################################################
#										#
#  U_OUTPUT --	Open selected element of output image for writing.  		#
#										#
#  Last modified:								#
#	11 Aug 92 by RAShaw	Initial implementation				#

pointer procedure u_output (element, name, template)

#  Calling arguments:
int	element			# element number
char	name[SZ_FNAME]		# name of input image
pointer	template		# image template (usually the input image)

#  Local variables:
char	fullname[SZ_FNAME]	# full filename 
pointer	im			# output image descriptor (returned)
char	text[SZ_LINE]		# text of error message

#  Functions used:
int	imgeti()		# get header keyword TY_INT
pointer	gf_map()		# map image descriptor

errchk	gf_map, sprintf, u_error

begin

#  Add appropriate group element to file name
	call strcpy (name, fullname, SZ_FNAME)
	call sprintf (text, SZ_FNAME, "[%1d/%1d]")
#call strcat ("[inherit]", fullname, SZ_FNAME)
	    
	    call pargi (element)
	    call pargi (imgeti (template, "GCOUNT"))

	call strcat (text, fullname, SZ_FNAME)
#call strcat ("[INHERIT]", fullname, SZ_FNAME)

#  Create the output image descriptor
	iferr (im = gf_map (fullname, NEW_COPY, template) ) {
	    call sprintf (text, SZ_LINE, "Error opening output file : %s")
	    call pargstr (fullname)
	    call u_error (text)
	}

	return (im)
end


#################################################################################
#										#
#  U_OUT_GSKIP -- Skip to specified group of open output image for writing.  	#
#		  This approach is much faster than closing/reopening images.  	#
#										#
#  Last modified:								#
#	20 Aug 92 by RAShaw	Initial implementation				#

procedure u_out_gskip (imp, element, dmin, dmax, tmpl)

#  Calling arguments:
int	element			# element number
pointer	imp			# output image descriptor
real	dmin, dmax
pointer	tmpl

#  Local variables:
char	text[SZ_LINE]		# text of error message

errchk	u_error, gf_opengr

begin

	iferr (call gf_opengr (imp, element, dmin, dmax, tmpl)) {
	    call sprintf (text, SZ_LINE, 
		"Error skipping groups in output file, group: %1d" )
	        call pargi (element)
	        call u_error (text)
	}

end


#################################################################################
#										#
#  U_GRPFILE --	Open output group parameter text file.  			#
#										#
#  Last modified:								#
#	24 Jul 92 by RAShaw	Initial implementation				#

int procedure u_grpfile (element, name)

#  Calling arguments:
int	element			# element number (a.k.a. group number)
char	name[SZ_FNAME]		# name of output file

#  Local variables:
int	fd			# file descriptor
char	text[SZ_LINE]		# text of error message

#  Function used:
int	open()

errchk	open, u_error

begin

# New file for first group...
	if (element == 1) {	
	    iferr (fd = open (name, NEW_FILE, TEXT_FILE) ) {
		call sprintf (text, SZ_LINE,
		    "Error creating output file: %s")
		call pargstr (name)
		call u_error (text)
	    }

# ...or append to same text file
	} else {
	    iferr (fd = open (name, APPEND, TEXT_FILE)) {
		call sprintf (text, SZ_LINE,
		    "Error opening output file: %s")
		call pargstr (name)
		call u_error (text)
	    }
	}

	return (fd)
end

#################################################################################
#										#
#  U_DGRFILE --	Open input group parameter text file (.DGR) as READ_WRITE.	#
#										#
#  Last modified:								#
#	 4 Sep 93 by CYZhang	Initial implementation				#

int procedure u_dgrfile (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of output file

#  Local variables:
int	fd			# file descriptor
char	text[SZ_LINE]		# text of error message

#  Function used:
int	open()

errchk	open, u_error

begin

	iferr (fd = open (name, READ_WRITE, TEXT_FILE) ) {
	    call sprintf (text, SZ_LINE,
			  "Error opening .DGR file: %s")
	    call pargstr (name)
	    call u_error (text)
	}
	return (fd)
	
end

#################################################################################
#										#
#  U_CGRFILE --	Open CGR group parameter text file (READ_ONLY). 		#
#										#
#  Last modified:								#
#	 6 Dec 93 by CYZhang	Initial implementation				#

int procedure u_cgrfile (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of output file

#  Local variables:
int	fd			# file descriptor
char	text[SZ_LINE]		# text of error message

#  Function used:
int	open()

errchk	open, u_error

begin

	iferr (fd = open (name, READ_ONLY, TEXT_FILE) ) {
	    call sprintf (text, SZ_LINE,
			  "Error opening CGR file: %s")
	    call pargstr (name)
	    call u_error (text)
	}
	return (fd)
end

#################################################################################
#										#
#  U_TMPFILE --	Open TEMP group parameter text file as NEW_FILE.		#
#										#
#  Last modified:								#
#	 4 Sep 93 by CYZhang	Initial implementation				#

int procedure u_tmpfile (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of output file

#  Local variables:
int	fd			# file descriptor
char	text[SZ_LINE]		# text of error message

#  Function used:
int	open()

errchk	open, u_error

begin

	iferr (fd = open (name, NEW_FILE, TEXT_FILE) ) {
	    call sprintf (text, SZ_LINE,
			  "Error opening temporary file: %s")
	    call pargstr (name)
	    call u_error (text)
	}
	return (fd)
end





################################################################################
#										
#  U_REFFILE -- Open element of Reference File, selected by DETECTOR number, 	
#		for reading and validate WFPC format.  				
#										
#  Last modified:								
#	20 Aug 92 by RAShaw	Initial implementation				
#	10 Sep 93 by CYZhang	Add reference file name to the warning messages	
#	29 Sep 93 by CYZhang	Error checks on keywords for reference files	
#	06 Dec 94 by JC Hsu	get rid of adding group number
#	04 Mar 03 by JC Hsu	modify so it will work for (extension) FITS 
#				reference files


pointer procedure u_reffile (cam, name, flag)

#  Calling arguments:
pointer	cam			# pointer to CAMera data structure
char	name[SZ_FNAME]		# name of input image (fully qualified 
				# except for group element extension)
int	flag			# flag of reference file type

#  Local variables:
int	chip			# number of chip from DETECTOR keyword
char	fullname[SZ_FNAME]	# full filename 
pointer	imp			# image pointer (returned)
int	ngroup			# number of groups in images (must be 4)
char	text[SZ_LINE]		# string for error message
char	cdbs[4]			# cdbs file stamp
real	val			# temp value
char    dir[SZ_FNAME]
char    root[SZ_FNAME]
char	ext[SZ_FNAME]
char	tmpstr[SZ_FNAME]
int	len
int     isfits
int	tmp

#  Functions used:
int	imgeti()		# fetch INT keyword from image descriptor
real	imgetr()		# fetch REAL keyword from image descriptor
bool	streq()			# string comparison
bool	strne()			# string comparison
pointer	gf_map()		# map image
int 	gf_gcount()
int 	fnroot(), fnldir(), fnextn()
int	strlen(), access()

errchk	u_addgrp, u_error, u_kwerr, u_warn, imgeti, imgetr, imgstr, gf_map,
streq, strne, strupr, strcpy, strcat

begin

    #	get rid of this unnecessary adding group business which can cause
    #	problem in u_proc.x. This routine is always called for the first group
    #	anyway.  Copy name to fullname. JCHsu 12/6/94
    #	element = DETECTOR(cam)
    #	call u_addgrp (name, fullname, element)

    tmp = fnldir (name, dir, SZ_FNAME)
    tmp = fnroot (name, root, SZ_FNAME)
    tmp = fnextn (name, ext, SZ_FNAME)

    # If the specified reference file is a GEIS file name and if it does not 
    # exist, look for the FITS file with the corresponding name. 3/4/03 JCHsu
    if (strlen(ext) == 3 && ext[3] == 'h') {
        if (access(name, 0, 0) == YES) {
	    call strcpy (name, fullname, SZ_FNAME)
	    isfits = NO
        } else {
	    call strcpy(dir, fullname, SZ_FNAME)
	    call strcat(root, fullname, SZ_FNAME)
	    call strcat("_", fullname, SZ_FNAME)
	    call strcpy(ext, tmpstr, 2)
	    call strcat(tmpstr, fullname, SZ_FNAME)
	    call strcat("f.fits", fullname, SZ_FNAME)
	    isfits = YES
	}

    # vice versa if the specified reference name is a FITS file name.  
    } else {
        if (access(name, 0, 0) == YES) {
	    call strcpy (name, fullname, SZ_FNAME)
	    isfits = YES
        } else {
	    call strcpy(dir, fullname, SZ_FNAME)
	    len = strlen(root)
	    call strcpy(root, tmpstr, len-4)
	    call strcat(tmpstr, fullname, SZ_FNAME)
	    call strcat(".", fullname, SZ_FNAME)
	    call strcpy(root[len-2], tmpstr, 2)
	    call strcat(tmpstr, fullname, SZ_FNAME)
	    call strcat("h", fullname, SZ_FNAME)
	    isfits = NO
	}
    }
    
    iferr (imp = gf_map (fullname, READ_ONLY, 0) ) {	   
	call sprintf (text, SZ_LINE, "Error Opening Reference File!!: %s")
	    call pargstr (fullname)
	call u_error (text) 
    }

    # Insist that reference files have 4 groups
    if (isfits == YES) {
        iferr (ngroup = gf_gcount (imp))
            call u_kwerr ( "NEXTEND" )
    } else {
	iferr (ngroup = imgeti(imp, "GCOUNT"))
	    call u_kwerr ("GCOUNT in Reference File")
    }
	
    if (ngroup != 4) {
	call sprintf (text, SZ_LINE, "Not Four Groups in Reference File: %s")
	    call pargstr (fullname)
	call u_error (text)
    }

    # Ensure that input image is from WFPC2
    iferr(call imgstr(imp, "INSTRUME", text, SZ_LINE))
	call u_kwerr ("INSTRUME")
    call strupr (text)

    if(!streq(text, "WFPC2"))
	call u_error ("Keyword INSTRUME not equal to WFPC2")

    # Match selected element in reference file with DETECTOR number 
    iferr(chip = imgeti(imp, "DETECTOR"))
	call u_kwerr ("DETECTOR in Reference File")

    # get rid of this unnecessary step too.  JC Hsu 12/6/94.
#   if (chip != element) {
#	call sprintf (text, SZ_LINE, 
#		"DETECTOR value mismatch in Reference File: %s")
#	    call pargstr (fullname)
#	call u_warn (text)
#   }

    # Look for CDBS stamp
    iferr(call imgstr(imp, "CDBSFILE", cdbs, SZ_LINE))
	call u_kwerr ("CDBSFILE in Reference File")
    call strupr (cdbs)

    switch (flag) {
	case REF_MASK:
	    if (strne (cdbs, "MASK")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for MASK in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_ATOD:
	    if (strne (cdbs, "ATOD")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for ATOD in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_BIAS:
	    if (strne (cdbs, "BIAS")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for BIAS in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_DARK:
	    if (strne (cdbs, "DARK")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for DARK in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_FLAT:
	    if (strne (cdbs, "FLAT")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for FLAT in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_SHAD:
	    if (strne (cdbs, "SHAD")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for SHAD in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	case REF_WF4T:
	    if (strne (cdbs, "W4T")) {
		call sprintf (text, SZ_LINE,
		    "CDBSFILE flag mismatch for WF4T in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
    }

    # Check Flags and Indicators of reference files. For different reference
    # files the different flags are set up. -- CYZhang  29/9/93
	
    if (flag == REF_MASK || flag == REF_BIAS || flag == REF_DARK ||
	flag == REF_FLAT || flag == REF_SHAD) {
	iferr(call imgstr(imp, "MODE", text, SZ_LINE))
	    call u_kwerr ("MODE in Reference File")
	call strupr (text)

	if ((streq (text, "FULL") && !IS_FULL(cam)) ||
	    (streq (text, "AREA") && IS_FULL(cam))) {
	    call sprintf (text, SZ_LINE, 
		     "MODE value mismatch in Reference File: %s")
		call pargstr (fullname)
	    call u_warn (text)
	}
    }
	    
	if (flag == REF_ATOD || flag == REF_BIAS || flag == REF_DARK || 
    flag == REF_WF4T) {
	    iferr ( val = imgetr (imp, "ATODGAIN") ) {
		call sprintf (text, SZ_LINE,
		    "ATODGAIN not found in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }

	    # The ABS is added to the difference in the following expression
            # for checking matching of the ATODGAINs -- CYZ 28/1/94
	    if (abs(val - A2DGAIN(cam)) > EPSILONR) {
		call sprintf (text, SZ_LINE, 
		    "A-to-D GAIN mismatch in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	}

	if (flag == REF_DARK) {
	    iferr (call imgstr (imp, "SERIALS", text, SZ_LINE) )
		call u_kwerr ("SERIALS in Reference File")
	    call strupr (text)

	    if ((streq (text, "ON") && !IS_CLKON(cam)) ||
		(streq (text, "OFF") && IS_CLKON(cam))) {
		call sprintf (text, SZ_LINE, 
		     "SERIALS mismatch in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	}

	if (flag == REF_FLAT) {
	    iferr (call imgstr (imp, "FILTNAM1", text, SZ_LINE)) 
		call u_kwerr ("FILTNAM1 in Reference File")
	    call strupr (text)

	    if ( strne (text, FILTER1(cam))) {
		call sprintf (text, SZ_LINE, 
		     "FILTNAM1 mismatch in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }

	    iferr (call imgstr (imp, "FILTNAM2", text, SZ_LINE))
		call u_kwerr ("FILTNAM2 in Reference File")
	    call strupr (text)

	    if ( strne (text, FILTER2(cam))) {
		call sprintf (text, SZ_LINE, 
		     "FILTNAM2 mismatch in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	}

	if (flag == REF_SHAD) {
	    iferr (call imgstr (imp, "SHUTTER", text, SZ_LINE))
		call u_kwerr ("SHUTTER in Reference File")
	    call strupr (text)

	    if ( strne (text, SHTBLD(cam))) {
		call sprintf (text, SZ_LINE, 
		     "SHUTTER mismatch in Reference File: %s")
		call pargstr (fullname)
		call u_warn (text)
	    }
	}
	    
#  Match A-to-D gain setting
#  Note: may not be necessary--check with instrument team
#	iferr ( val = imgetr (imp, "ATODGAIN") ) {
#		call u_kwerr ("ATODGAIN in Reference File")
#  It's not sufficient a warning message without mentioning the actual
#  reference file name in full -- CYZ  9/9/93
#  		call u_warn ("ATODGAIN not found in Reference File")
#	    call sprintf (text, SZ_LINE,
#		"ATODGAIN not found in Reference File: %s")
#	    call pargstr (fullname)
#	    call u_warn (text)
#	}
	
#
#	if ((val - A2DGAIN(cam)) > EPSILONR) {
#	    call sprintf (text, SZ_LINE, 
#		"A-to-D GAIN mismatch in Reference File: %s")
#		call pargstr (fullname)
#		call u_warn (text)
#	}

	return (imp)
end


#################################################################################
#										#
# U_REF_GSKIP -- Skip to specified group of open reference file, selected by 	#
#		 DETECTOR number, for reading and validate WFPC2 format.  	#
#		 This approach is much faster than closing/reopening images.  	#
#										#
#  Last modified:								#
#	20 Aug 92 by RAShaw	Initial implementation				#

procedure u_ref_gskip (cam, imp)

#  Calling arguments:
pointer	cam			# pointer to CAMera data structure
pointer	imp			# reference image descriptor

#  Local variables:
int	element			# element number
real	dmin, dmax		# input image min/max (dummy)
char	text[SZ_LINE]		# string for error message
int	chip			# number of chip from DETECTOR keyword

#  Functions used:
int	imgeti()		# fetch INT keyword from image descriptor

errchk	u_addgrp, u_error, u_kwerr, u_warn, gf_opengr, imgeti

begin
	element = DETECTOR(cam)

	iferr (call gf_opengr (imp, element, dmin, dmax, NULL)) {
	    call sprintf (text, SZ_LINE, 
		"Error skipping groups in reference file, group: %1d")
		call pargi (element)
		call u_error (text)
	}

#  Match selected element in reference file with DETECTOR number 
	iferr (chip = imgeti (imp, "DETECTOR" ) )
	    call u_kwerr ("DETECTOR in Reference File")
	if (chip != element) {
	    call sprintf (text, SZ_LINE, 
		"DETECTOR value mismatch in Reference File, group: %1d")
		call pargi (element)
		call u_warn (text)
	}

end


#################################################################################
#										#
#  U_OUTTAB -- Open Reference Table for writing.  				#
#										#
#  Last modified:								#
#	7 Feb 94 by CYZhang	Initial implementation				#

pointer procedure u_outtab (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of input table

#  Local variables:
char	text[SZ_LINE]		# buffer for error message
pointer	tp

#  Function used:
pointer	tbtopn ()

errchk	tbtopn, u_error

begin

	iferr ( tp = tbtopn (name, NEW_FILE, NULL) ) {
	    call sprintf (text, SZ_LINE, 
		"Error Opening Reference Table: %s")
		call pargstr (name)
		call u_error (text)
	}

	return (tp)
end


#################################################################################
#										#
#  U_REFTAB -- Open Reference Table for reading.  				#
#										#
#  Last modified:								#
#	24 Jul 92 by RAShaw	Initial implementation				#

pointer procedure u_reftab (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of input table

#  Local variables:
char	text[SZ_LINE]		# buffer for error message
pointer	tp

#  Function used:
pointer	tbtopn ()

errchk	tbtopn, u_error

begin

	iferr ( tp = tbtopn (name, READ_ONLY, NULL) ) {
	    call sprintf (text, SZ_LINE, 
		"Error Opening Reference Table: %s")
		call pargstr (name)
		call u_error (text)
	}

	return (tp)
end


#################################################################################
#										#
# U_FINISHUP -- Closes any open files.  					#
#										#
#  Last modified:								#
#	20 Jul 92 by RAShaw	Initial implementation				#
#	10 Sep 93 by CYZhang	Add things for ATOD, SHAD, DOHISTOS, PHOT_TBL	#

procedure u_finishup (ptr)

#  Calling argument:
pointer	ptr			# pointer to image descriptors data structure

#  Local variables:
#char	obuf[SZ_FNAME]
#int	envgets()

errchk	u_close, u_gf_unmap, u_tbunmap

begin

        if (IN_IMG_P(ptr) != NULL)			# Input image
	    call u_gf_unmap (IN_IMG_P(ptr))

        if (IN_DQF_P(ptr) != NULL)			# Input image DQF
	    call u_gf_unmap (IN_DQF_P(ptr))

        if (MASK_P(ptr) != NULL)			# Static mask
	    call u_gf_unmap (MASK_P(ptr))

        if (ATOD_IMG_P(ptr) != NULL)			# AtoD image -- CYZ
	    call u_gf_unmap (ATOD_IMG_P(ptr))

        if (BLVL_IMG_P(ptr) != NULL)			# EED file
	    call u_gf_unmap (BLVL_IMG_P(ptr))

        if (BLVL_DQF_P(ptr) != NULL)			# EED file DQF
	    call u_gf_unmap (BLVL_DQF_P(ptr))

        if (BIAS_IMG_P(ptr) != NULL)			# BIAS image
	    call u_gf_unmap (BIAS_IMG_P(ptr))

        if (WF4T_IMG_P(ptr) != NULL)			# WF4T image -- WJH
	    call u_gf_unmap (WF4T_IMG_P(ptr))

	if (BIAS_DQF_P(ptr) != NULL)			# BIAS image DQF
	    call u_gf_unmap (BIAS_DQF_P(ptr))

	if (DARK_IMG_P(ptr) != NULL)			# DARK image
	    call u_gf_unmap (DARK_IMG_P(ptr))

	if (DARK_DQF_P(ptr) != NULL)			# DARK image DQF
	    call u_gf_unmap (DARK_DQF_P(ptr))

	if (FLAT_IMG_P(ptr) != NULL)			# FLAT image
	    call u_gf_unmap (FLAT_IMG_P(ptr))

	if (FLAT_DQF_P(ptr) != NULL)			# FLAT DQF
	    call u_gf_unmap (FLAT_DQF_P(ptr))

	if (SHAD_IMG_P(ptr) != NULL)			# SHAD image -- CYZ
	    call u_gf_unmap (SHAD_IMG_P(ptr))

	if (THRU_TBL_P(ptr) != NULL)			# THRUPUT table -- CYZ
	    call u_tbunmap (THRU_TBL_P(ptr))

	if (OUT_IMG_P(ptr) != NULL)			# OUT image
	    call u_gf_unmap (OUT_IMG_P(ptr))

	if (OUT_DQF_P(ptr) != NULL)			# OUT DQF
	    call u_gf_unmap (OUT_DQF_P(ptr))

	if (HIST_IMG_P(ptr) != NULL)			# HIST image
	    call u_gf_unmap (HIST_IMG_P(ptr))

end


#################################################################################
#										#
# U_gf_unmap -- Un-map an image.  						#
#										#
#  Last modified:								#
#	20 Jul 92 by RAShaw	Initial implementation				#

procedure u_gf_unmap (im)

#  Calling argument
pointer	im			# image pointer

errchk	gf_unmap, u_error

begin

	iferr ( call gf_unmap (im) )
	    call u_error ( "gf_unmap call failed" )
	im = NULL

end


#################################################################################
#										#
# U_TBUNMAP -- Un-map a table.  						#
#										#
#  Last modified:								#
#	20 Jul 92 by RAShaw	Initial implementation				#

procedure u_tbunmap (tp)

pointer	tp			# table pointer

errchk	tbtclo, u_error

begin

	iferr ( call tbtclo (tp) )
	    call u_error ( "tbunmap call failed" )
	tp = NULL

end


#################################################################################
#										#
# U_CLOSE -- Close a text file.  						#
#										#
#  Last modified:								#
#	20 Jul 92 by RAShaw	Initial implementation				#

procedure u_close (fd)

#  Calling argument
int	fd			# file descriptor

errchk	close, u_error

begin

	iferr ( call close (fd) )
		call u_error ( "u_close call failed" )
	
end

