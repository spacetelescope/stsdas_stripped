include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include "wpdef.h"

##############################################################################
# T_COMBINE --	This task combines a list of images into an output image and 
#		an optional sigma image while optionally excluding bad pixels	#
#		as identified by the Data Quality Files.  There are many 
#		combining alternatives from which to choose, which are 	
#		described in the `help' files.  This procedure is based upon 	
#		the `images.imcombine' package.  			
#									
#	 1/91	RAShaw	Development version.				
#	12/91	RAShaw	Implement enumerated parameter lists		
#	 1/92	RAShaw	Treat blank characters for "sigma" param as "NULL"	
#	 5/92	RAShaw	Trap error: less that 3 images for av/sigclip options
#	 2/94	JCHsu	Add TY_USHORT

procedure t_combine()

# Required parameters:
int	list			# List of input image rootnames
pointer	output			# Output image filename

# Optional parameters:
char	datextn[SZ_FNAME]	# Datafile extension;	default = "c0h"
char	dqfextn[SZ_FNAME]	# DQF extension;	default = "c1h"
int	log			# Log file
pointer	logfile			# Log filename
int	intype			# Datatype of Input images
int	option			# Combine option
int	outtype			# Output datatype
char	sigma[SZ_FNAME]		# Optional sigma image filename

# Local variables:
pointer	dqf			# Input DQF images
bool	dqfflag			# Use DQFs for pixel rejection?
pointer	first			# Pointer to input data values (first image)
char	fullname[SZ_FNAME]	# Filename w/type and group extensions
bool	gflag			# Flag for group format image
int	group			# Image group number
int	i, j			# Dummy indexes
pointer	in			# Input images
pointer	input			# Pointer to input image filenames
int	ngroup			# Number of groups per image
int	nimages			# Number of input images
pointer	nxt			# Pointer to input data values (subsequent images)
int	nextgrp			# Number of groups in subsequent input images
pointer	out			# Pointer to output image
pointer	sig			# Pointer to sigma image
pointer	sp			# Pointer to begining of stack memory
###bool	virgin			# First time? (i.e. thru imcombine loop)
real	xmin, xmax
char	temp[SZ_FNAME]

# List of functions used:
bool	clgetb()		# Get boolean keyword value from the CL
char	clgetc()		# Get character keyword value from the CL
int	clgwrd()		# Get kywd parameter from CL & match to dictionary
int	gf_gcount()		# Get number of groups/extensions
pointer	gf_map()		# map image into memory
pointer	imtopenp()		# Open CL parameter list 
int	imtlen()		# Determine the number of images in list
int	imtgetim()		# Get next image filename from image template
#bool	isblank()		# Is input string blank?
int	open()			# Open a new file
int	ty_max()		# Determine highest precision datatype

errchk	clgetc, clgwrd, gf_gcount, gf_map, imtgetim, open, ty_max, w_filnam

begin

# Set pointer to Stack memory and allocate space for variables.
	call smark(sp)
	call salloc(input,   SZ_FNAME, TY_CHAR)
	call salloc(output,  SZ_FNAME, TY_CHAR)
	call salloc(option,  SZ_FNAME, TY_CHAR)
	call salloc(logfile, SZ_FNAME, TY_CHAR)

	# Get string parameters from the CL; other parameters are obtained in 
	# lower-level procedures.
	list = imtopenp("input")
	call clgstr("output",     Memc[output],  SZ_FNAME)
	call clgstr("sigma",      sigma,   SZ_FNAME)
	call clgstr("logfile",    Memc[logfile], SZ_FNAME)
	option = clgwrd("option", Memc[input],   SZ_FNAME, COMBINE)
	outtype = clgetc("outtype")
	call clgstr("datextn", datextn,       SZ_FNAME)
	dqfflag = clgetb("usedqf")
	call clgstr("dqfextn", dqfextn,       SZ_FNAME)

	# Abort if there are fewer than 3 images for AV/SIGCLIP options, or if 
	# fewer than 2 images for all other options
	nimages = imtlen(list)
	if (nimages < 2) {
	    call imtclose(list)
	    call sfree(sp)
	    call error(0, "Too few input images to combine")
	}
	if (option == SIGCLIP || option == AVSIGCLIP) {
	    if (nimages < 3) {
		call imtclose(list)
		call sfree(sp)
	 	call error(0, "Too few input images for this option")
	    }
	}
	call salloc(in,  nimages, TY_POINTER)
	call salloc(dqf, nimages, TY_POINTER)

	# Construct input filenames from image name template and map them  
	# (first group only).
	group  = 1
	ngroup = INDEFI
	i      = 0
	while (imtgetim(list, Memc[input], SZ_FNAME) != EOF) {
	    call imgcluster(Memc[input], fullname, SZ_FNAME)
	    #call w_filnam(fullname, group, ngroup)
	    Memi[in+i] = gf_map(fullname, READ_ONLY, 0)

	    # Map the corresponding DQF images, if appropriate.
	    if (dqfflag) {
		call splicstr(fullname, fullname, datextn, dqfextn)
		Memi[dqf+i] = gf_map(fullname, READ_ONLY, 0) 
	    } else 
		Memi[dqf] = NULL

	    i = i + 1
	}

	# Check that all images have the same group count.
	first = Memi[in]
	intype = IM_PIXTYPE(first)
	iferr (ngroup = gf_gcount(first)) {
	    gflag  = false
	    ngroup = 1
	}
	do i = 2, nimages {
	    nxt = Memi[in+i-1]
	    if (gflag) {
		iferr (nextgrp = gf_gcount(nxt))
		    call error(0, "Subsequent input image not in group format")
		if (nextgrp != ngroup)
		    call error(0, "No. of image groups not the same")
	    }

	    # Check that all images are of the same dimension and size. 
	    if (IM_NDIM(nxt) != IM_NDIM(first))
		call error(0, "Image dimensions are not the same")
	    do j = 1, IM_NDIM(first) 
		if (IM_LEN(nxt,j) != IM_LEN(first,j))
		    call error(0, "Image sizes are not the same")

	    # Select the highest precedence datatype.
	    intype = ty_max(intype, IM_PIXTYPE(nxt))
	}

	# Open the output image and set its pixel datatype.  
        call sprintf(temp, SZ_FNAME, "[1/%d]")
            call pargi(ngroup)

	call imgcluster(Memc[output], fullname, SZ_FNAME)
	#call w_filnam(fullname, group, ngroup)
	call strcat(temp, fullname, SZ_FNAME)

	out = gf_map(fullname, NEW_COPY, Memi[in])

	switch (outtype) {
	case 's':
	    IM_PIXTYPE(out) = TY_SHORT
	case 'i':
	    IM_PIXTYPE(out) = TY_INT
	case 'l':
	    IM_PIXTYPE(out) = TY_INT
	case 'r':
	    IM_PIXTYPE(out) = TY_REAL
	case 'd':
	    IM_PIXTYPE(out) = TY_DOUBLE
#	case 'c':
#	    IM_PIXTYPE(out) = TY_COMPLEX
	default:
	    IM_PIXTYPE(out) = intype
	}

	# Open the sigma image if given.
	switch (option) {
	case SUM:
	    sigma[1] = EOS
	case MINREJECT, MAXREJECT:
	    if (nimages < 3)
		sigma[1] = EOS
	case MINMAXREJECT:
	    if (nimages < 4)
		sigma[1] = EOS
	}

	if (sigma[1] != EOS){
	    call imgcluster(sigma, fullname, SZ_FNAME)
	    #call w_filnam(fullname, group, ngroup)
	    call strcat(temp, fullname, SZ_FNAME)
	    sig = gf_map(fullname, NEW_COPY, out)
	    IM_PIXTYPE(sig) = ty_max(TY_REAL, IM_PIXTYPE(out))
	    call sprintf(IM_TITLE(sig), SZ_IMTITLE,
		"Combine sigma images for %s")
		call pargstr(output)
	} else
	    sig = NULL

	# Open the log file.
	log = NULL
	if (Memc[logfile] != EOS) {
	    iferr (log = open(Memc[logfile], APPEND, TEXT_FILE)) {
	        log = NULL
	        call erract(EA_WARN)
	    }
	}
	###virgin = true

	# Combine the images; loop over groups.  
	do group = 1, ngroup {

	    # Construct input filenames from image rootnames and map the 
	    # input/output images (groups beyond first).
	    ###if (!virgin) {

		# Added 3/20/2003 to use gf_ calls to go through the groups
	        do i = 1, nimages {
		    call gf_opengr(Memi[in+i-1], group, xmin, xmax, 0)
		    if (dqfflag) {
		        call gf_opengr(Memi[dqf+i-1], group, xmin, xmax, 0)
		    }
		}
		###call imtrew(list)		# Rewind list of image rootnames
		###i = 0
		###while (imtgetim(list, Memc[input], SZ_FNAME) != EOF) {
		    ###call imgcluster(Memc[input], fullname, SZ_FNAME)
		    #call w_filnam(fullname, group, ngroup)
		    ###Memi[in+i] = gf_map(fullname, READ_ONLY, 0)

		    # Map the corresponding DQF images, if appropriate.
		    ###if (dqfflag) {
			###call splicstr(fullname, fullname, datextn, dqfextn)
			###Memi[dqf+i] = gf_map(fullname, READ_ONLY, 0) 
		    ###} else 
			###Memi[dqf+i] = NULL

		    ###i = i + 1
		###}
		###i = INDEFI
		###call imgcluster(Memc[output], fullname, SZ_FNAME)
		#call w_filnam(fullname, group, i)
		###out = gf_map(fullname, NEW_COPY, Memi[in])
		call gf_opengr(out, group, xmin, xmax, Memi[in])
		if (sigma[1] != EOS){
		    call gf_opengr(sig, group, xmin, xmax, out)
		}

		# Map the sigma image if given.
#		if (!ISBLANK(sigma)) {
		###if (sigma[1] != EOS) {
		    ###i = INDEFI
		    ###call imgcluster(sigma, fullname, SZ_FNAME)
		    #call w_filnam(fullname, group, i)
		    ###sig = gf_map(fullname, NEW_COPY, out)
		    ###IM_PIXTYPE(sig) = ty_max(TY_REAL, IM_PIXTYPE(out))
		###}
###	    }
###	    virgin = false

	    # Call appropriate combine procedure.
	    iferr {
		switch (intype) {
		case TY_USHORT:
		    call combinei(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
		case TY_SHORT:
		    call combines(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
		case TY_INT:
	    	    call combinei(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
		case TY_LONG:
	    	    call combinei(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
		case TY_REAL:
		    call combiner(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
		case TY_DOUBLE:
		    call combined(Memi[in], Memi[dqf], nimages, out, sig, 
					option, log)
#		case TY_COMPLEX:
#		    if ((option == SIGCLIP) || (option == AVSIGCLIP))
#			call error(0, 
#			    "Option not supported for complex images")
#		    else
#			call combinex(Memi[in], Memi[dqf], nimages, out, 
#					sig, option, log)
		}
	    } then 
		call erract(EA_ERROR)

	}
	    # Unmap all the images. 
	    call gf_unmap(out)
	    do i = 1, nimages
		call gf_unmap(Memi[in+i-1])
	    if (sig != NULL)
		call gf_unmap(sig)
	    if (dqfflag) {
		do i = 1, nimages
		    call gf_unmap(Memi[dqf+i-1]) 
	    }

	# Close the log file and free Stack memory.
	if (log != NULL)
	    call close(log)
	call imtclose(list)
	call sfree(sp)
end


################################################################################
# TY_MAX --	Return the datatype of highest precedence.  This procedure is 	
#		based upon the `images.imcombine' package.  			
#										
#		Development version:	11/90	RAShaw				
#		Add TY_USHORT:		02/94	JCHsu				

int procedure ty_max(type1, type2)

int	type1, type2		# Datatypes
int	i, j			# Dummy counters
int	order[7]		# Permitted datatypes
#int	order[8]		# Permitted datatypes

#data	order/TY_USHORT,TY_SHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/
data	order/TY_USHORT,TY_SHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_REAL/

begin
	for (i=1; (i<=7) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=7) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end


################################################################################
# W_FILNAM --	Add group suffix to the file name.  Add the full group 		
#		specification (e.g. [1/4]) if "ngroup" is not set to INDEFI.  	
#										
#	8/91	Development version:	RAShaw					

procedure w_filnam(image, group, ngroup)

# Calling arguments: 
char	image[SZ_FNAME]			# filename 
int	group				# group number
int	ngroup				# Max. number of groups per image

# Local variables:
char	temp[SZ_FNAME]			# rootname plus extensions

errchk	strcpy, strcat

begin
	
	# Concatenate strings onto filename
	call strcpy(EOS, temp, SZ_FNAME)
	if (IS_INDEFI(ngroup)) {
	    call sprintf(temp, SZ_FNAME, "[%d]")
	    call pargi(group)
	} else {
	    call sprintf(temp, SZ_FNAME, "[%d/%d]")
	    call pargi(group)
	    call pargi(ngroup)
	}
	call strcat(temp, image, SZ_FNAME)
end
