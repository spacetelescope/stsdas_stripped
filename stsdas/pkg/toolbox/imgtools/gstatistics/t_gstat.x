#Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
include	<ctype.h>
include	<mach.h>
include <imio.h>
include	<imhdr.h>
include <pmset.h>
include "gstat.h"

#---------------------------------------------------------------------4 Dec 95--
.help t_gstat.x Dec95 imgtools$gstatistics
.ih
NAME
t_gstatistics - gstatistics driver task.
.ih
DESCRIPTION
Compute and print the statistics of images. This task is based on 
images.imstatistics. The new features allow: 
.nf
    (1) pixels flagged as bad in the DQF file can be excluded
    (2) statistics may be accumulated over a range(s) of groups. 
.fi
.endhelp
#-------------------------------------------------------------------------------
# T_GSTATISTICS -- Compute and print the statistics of images. 
#
#  Revision History:
# 	20-AUG-93 C.Y. Zhang	Task GSTATISTICS created.
# 	 4-Dec-95 R.A. Shaw	Bug fixes: changed order of test for .pl and 
#				pix mask files; allowed for leading whitespace 
#				in the group list range; closed mask list 
#				descriptor. Fixed comment & source indenting to 
#				show logical program flow, and moved the 
#				calculation switch setting to a separate 
#				routine.  

procedure t_gstatistics ()

# Task parameters
pointer list				# List of input images
pointer msklist				# List of input masks
char	glist[SZ_FNAME]			# List of group ranges
bool	g_accum				# Accumulate over groups?
pointer	fieldstr			# Pointer to fields string
real	lower				# Lower limit of data value window
real	upper				# Upper limit of data value window

# Local variables
char 	cluster[SZ_FNAME]		# Cluster of the image
int	cl_index			# Cluster index
int	cl_size				# Size of cluster
real  	datamin, datamax		# min and max in an image group
int	group_range[3*MAX_RANGES]	# Decoded group range list
int	gl_first_char			# 1st non-white char in glist string
int	gn				# Group No.
pointer	gst				# Pointer to stat structure
pointer	gsw				# Pointer to switch structure
pointer	fields				# Pointer to selected fields
int	i				# Loop variable
pointer	im				# Pointer to input image descriptor
pointer	image				# Current image name
int	imgnum				# Number of images in list
pointer msk				# Pointer to mask image descriptor
pointer	mskfile				# Current mask file name
int	ndim				# Number of dimensions of image
int	msknum				# Number of mask files in list
int	mskndim				# Number of dimensions of mask
bool	mskflag				# Mask operation?
int	msktype				# Pixel lists or image type masks
char	mskroot[SZ_FNAME]		# Root name of mask image
char	mskclst[SZ_FNAME]		# Cluster of mask image
char	msksect[SZ_FNAME]		# Section of mask image
int	mskclind			# Cluster index of mask image
int	mskclsize			# Size of cluster of mask image
int	m_flags				# Flag for mask type of a pl
int	nflds				# Number of fields selected
int	ngrmsk				# Number of groups in mask image
int	ngroups				# Number of groups
int	nitem				# Total number of items in group ranges
pointer	pp				# Pointer to output pset
char 	root[SZ_FNAME]	 		# Root name of the image
char	section[SZ_FNAME]		# Section of the image
pointer	sp				# Pointer to stack

# Functions called
bool 	clgetb()			# Get BOOL cl value
real	clgetr()			# Get REAL cl value
pointer	clopset()			# Open output pset file
int	decode_ranges()			# Parse group_list string
int	get_next_number()		# Get next group No
int	gst_fields()			# Decode fields string
int	gst_ihist()			# Do histograms?
int  	imaccf()			# Test if a header param exists
int	imgeti()			# Get value of keyword
pointer	immap()				# Map input image into memory
int	imtlen()			# Get number of files in input list
int	imtgetim()			# Fetch next file name from input list
pointer	imtopenp()			# Open an input file list
pointer	mio_open()			# Map a mask pixel list
int	strmatch()			# Match strings
bool	streq()				# String identical?


begin

	# Allocate stack memory
	call smark  (sp)
	call salloc (fieldstr, SZ_FNAME,  TY_CHAR)
	call salloc (fields,   NFIELDS,  TY_INT)
	call salloc (image,    SZ_FNAME, TY_CHAR)
	call salloc (mskfile,  SZ_FNAME, TY_CHAR)

	call calloc (gsw, LEN_SW,    TY_STRUCT)
	call calloc (gst, LEN_GSTAT, TY_STRUCT)
	call malloc (GS_HGM(gst), 1, TY_INT)

	# Get group lists and other parameters
	call clgstr ("groups", glist,          SZ_FNAME)
	call clgstr ("fields", Memc[fieldstr], SZ_FNAME)

	g_accum  = clgetb ("g_accum")
	lower    = clgetr ("lower")
	upper    = clgetr ("upper")

	# Get image list and corresponding mask list and open them
	# imtopenp fetches file name from cl and then do imtopen
	list    = imtopenp ("images")
	msklist = imtopenp ("masks")

	# Get number of mask files and set flag for masking operation
	msknum = imtlen (msklist) 
	if (msknum > 0)
	    mskflag = true
	else
	    mskflag = false

	# Check if number of images is equal to number of masks
	if (mskflag) {
	    imgnum  = imtlen (list)
	    if (imgnum != msknum)
	        call error (1, "Number of images must equal number of masks")
	}

	# Get the selected fields.
	# Memi[fields] -- field index; nflds -- number of fields selected

	nflds = gst_fields (Memc[fieldstr], Memi[fields], NFIELDS)
	if (nflds <= 0) {
	    call imtclose (list)
	    call imtclose (msklist)
	    call sfree (sp)
	    call error (1, "No statistical quantities selected for calculation")
	}

	# Set the computation switches.
	call gst_setswitch (gsw, Memi[fields], nflds)

	# Loop through the input image lists.

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {

	    # Parse the image root name into cluster, cl_index and cl_size

	    call imgimage (Memc[image], root, SZ_FNAME)
	    call rtparse (root, cluster, SZ_FNAME, cl_index, cl_size)
	    call imgsection (Memc[image], section, SZ_FNAME)

	    im = immap (Memc[image], READ_ONLY, 0)

	    # Get number of image dimensions.
	    ndim = IM_NDIM(im)

	    # Get number of groups from header, or alternatively default to 1 
	    # for the IRAF (OIF) images. 
	    if (imaccf (im, "GCOUNT") == YES)
		ngroups = imgeti (im, "GCOUNT")
	    else
		ngroups = 1

	    # Get mask files right: They are either pixel lists files with an 
	    # extension of ".pl", or mask images with an extension of ".??h", 
	    # following the convention of IRAF. STSDAS DQF files can be used 
	    # as mask images for corresponding HST data images and they usually 
	    # have extensions of the pattern, ".??h", which can often be 
	    # matched with, e.g., ".c1h" for calibrated HST images.

	    if (mskflag) {

		# Get mask file names from the input mask file list
		msknum = imtgetim (msklist, Memc[mskfile], SZ_FNAME)

		if (Memc[mskfile] == EOS || msknum == EOF) 
		    call error (2, "Number of masks & images must match!")

		## Branching for mask types: pixel lists or image type

		#  The following is to make sure that mask has the same section,
		#  cl_size, and cl_index as those of the parent image.

		if (strmatch (Memc[mskfile], ".??h") != 0) {

		    call imgimage (Memc[mskfile], mskroot, SZ_FNAME)
		    call rtparse (mskroot, mskclst, SZ_FNAME, 
			mskclind, mskclsize)
		    call imgsection (Memc[mskfile], msksect, SZ_FNAME)
		    if ( !(streq(msksect, section)) )
			call error (6, "Mask & image section must match!")
		    if (mskclind != cl_index)
			call error (7, "Mask & image cl_index must match!")
		    if (mskclsize != cl_size)
			call error (8, "Mask & image cl_size must match!")

		    # Mask file is an image so map it into memory using immap

		    msk = immap (Memc[mskfile], READ_ONLY, 0)
		    mskndim = IM_NDIM(msk)
		    if (ndim != mskndim)
			call error (9, "Image & mask dimensions must match!")
		    do i = 1, ndim 
			if (IM_LEN(im, i) != IM_LEN(msk, i))
			    call error (10, "Image & mask lengths must match!")

		    # Double check to see if the mask and corresponding image 
		    # have the same number of groups

		    if (imaccf (msk, "GCOUNT") == YES)
			ngrmsk = imgeti (msk, "GCOUNT")
		    else
			ngrmsk = 1

		    if (ngrmsk != ngroups)
			call error (11,"Image & mask group numbers must match!")
		    msktype = MSK_IMG

		# Pixel list test must follow the pixel mask test to allow for 
		# the possibility of an STF image type ".plh". 
		} else if (strmatch (Memc[mskfile], ".pl") != 0) {
		    if (ngroups >= 2)
			call error (3, "Pixel lists not for multigroup image")
		    msktype = MSK_LST
		    m_flags = BOOLEAN_MASK
		    msk = mio_open (Memc[mskfile], m_flags, im)

		    # The following approach cannot get the correct 
		    # information on the pixel list files:
		    # mskndim = IM_NDIM(msk)
		    # if (ndim != mskndim)
		    # call error (4, "Image & mask dimensions must match!")
		    # do i = 1, ndim 
		    #	if (IM_LEN(im, i) != IM_LEN(msk, i))
		    #	    call error (5, "Image & mask lengths must match!")

		} else 
		    call error (12, "Unrecognized extension of mask files")

	    # No mask operation
	    } else {

	   	msktype = MSK_NONE
		msk = NULL
	    }

	    # Print a header banner for the selected fields.

	    call gst_pheader (Memc[image], mskflag, Memc[mskfile], 
		g_accum, ngroups, glist, Memi[fields], nflds)

	    # Skip whitespace in glist
	    gl_first_char = 1
	    for (i=2; glist[i] != EOS || IS_WHITE(glist[i]); i=i+1)
		gl_first_char = gl_first_char + 1
	
	    if (ngroups == 1) {

		# Do statistics for IRAF (OIF) format images
 
		call gst_initialize (gst, lower, upper)
		call mntaccum (gst, im, msk, msktype, lower, upper, gsw)
		call do_stats (gst, gsw)
		if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
		    gst_ihist (gst) == YES ) {
		    call hgmaccum (gst, im, msk, msktype, lower, upper)
		    call do_midmod (gst, gsw)
		}
		call gst_print (g_accum, 0, gst, Memi[fields], nflds)
		pp = clopset ("gstpar")
		call gst_ppset (Memc[image], pp, gst)
		call clcpset (pp)

	    } else if (cl_index > 0 && ngroups > 1) {

		# Do statistics for the specified group of STSDAS (GEIS) image

		gn = cl_index
		if (gn > ngroups)
			call error (13, "Group out of range!")
		call gf_opengr (im, gn, datamin, datamax, 0)
		if (msk != NULL)
			call gf_opengr (msk, gn, datamin, datamax, 0)
		call gst_initialize (gst, lower, upper)
		call mntaccum (gst, im, msk, msktype, lower, upper, gsw)
		call do_stats (gst, gsw)

		if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
		    gst_ihist (gst) == YES ) {
		    call hgmaccum (gst, im, msk, msktype, lower, upper)
		    call do_midmod (gst, gsw)
		}
		call gst_print (g_accum, gn, gst, Memi[fields], nflds)
		pp = clopset ("gstpar")
		call gst_ppset (Memc[image], pp, gst)
		call clcpset (pp)

	    } else if (cl_index == 0 && glist[gl_first_char] == '*' && ngroups > 1) {

		# Loop through all the groups of STSDAS (GEIS) images
		if ( g_accum ) {

		    # Initialize gst structure only once
		    call gst_initialize (gst, lower, upper)

		    # Loop over groups according to gn in the images already 
		    # opened
		    do gn = 1, ngroups {

			call gf_opengr (im, gn, datamin, datamax, 0)
			if (msk != NULL)
			    call gf_opengr (msk, gn, datamin, datamax, 0)

		        # Accumulate moments over all the groups
			call mntaccum (gst, im, msk, msktype, lower, 
			    upper, gsw)
		    }

		    # Compute the moment statistics from the accumulations
		    call do_stats (gst, gsw)
		    if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
			gst_ihist (gst) == YES ) {

			# Back to the first group and loop over the groups again
			do gn = 1, ngroups {

			    call gf_opengr (im, gn, datamin, datamax, 0)
			    if (msk != NULL)
			        call gf_opengr (msk, gn, datamin, datamax, 0)

			    # Accumulate histograms over all the groups in the 
			    # image
			    call hgmaccum (gst, im, msk, msktype, lower, upper)
		        }

			# Compute the median and mode from the accumulated 
			# histogram
		        call do_midmod (gst, gsw)
		    }

		    # Print the results
		    call gst_print (g_accum, gn, gst, Memi[fields], nflds)

		# Do it separately for individual groups without accumulation
		} else {

		    # Initialize the gst structure whenever going to a new group
		    do gn = 1, ngroups {

			call gst_initialize (gst, lower,upper)
			call gf_opengr (im, gn, datamin, datamax, 0)
			if (msk != NULL)
			    call gf_opengr (msk, gn, datamin, datamax, 0)
			call mntaccum (gst, im, msk, msktype, lower, 
			    upper, gsw)
			call do_stats (gst, gsw)
			if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
			    gst_ihist (gst) == YES ) {
			    call hgmaccum (gst, im, msk, msktype, lower, upper)
		    	    call do_midmod (gst, gsw)
			}

			# Print out the results whenever is done for each image 
			# group.  Ready for the next group

			call gst_print (g_accum, gn, gst, Memi[fields], nflds)
		    }

	        }

		# Save results into a pset, gstpar.par
		pp = clopset ("gstpar")
		call gst_ppset (Memc[image], pp, gst)
		call clcpset (pp)

	    } else if (cl_index == 0 && glist[gl_first_char] != '*' && ngroups > 1) {

		# Decode the range character string into a list of group_ranges 
		# (int array) in order of repetitions of "first, last, step". 
		# White space is translated into "1, MAX_INT, 1" so that ALL 
		# groups will be looped around (equivalent to "*")

	 	if ( decode_ranges (glist, group_range, MAX_RANGES, 
		    nitem) == ERR )
		    call error (14, "Illegal group range.")
		if ( g_accum ) {

		    call gst_initialize (gst, lower, upper)

		    # Accumulating over all the groups in the range list
		    gn = 0

		    # Get the next gn in the list of group ranges. Note: gn is 
		    # incremented by one every time you call get_next_number, so
		    # gn is set up as 0 at the beginning

		    while (get_next_number (group_range, gn) != EOF) {
			if (gn > ngroups) 
			    break
			call gf_opengr (im, gn, datamin, datamax, 0)
			if (msk != NULL)
			    call gf_opengr (msk, gn, datamin, datamax, 0)
			call mntaccum (gst, im, msk, msktype, lower, 
			    upper, gsw)
		    }
		    call do_stats (gst, gsw)
		    if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
			gst_ihist (gst) == YES ) {
		        if ( decode_ranges (glist, group_range, 
		            MAX_RANGES, nitem) == ERR )
		            call error (15, "Illegal group range.")
		    	gn = 0
		        while (get_next_number (group_range, gn) != EOF) {
			    if (gn > ngroups) 
			    	break
			    call gf_opengr (im, gn, datamin, datamax, 0)
			    if (msk != NULL)
			        call gf_opengr (msk, gn, datamin, datamax, 0)
			    call hgmaccum (gst, im, msk, msktype, lower, 
			        upper)
		        }
		        call do_midmod (gst, gsw)
		    }
		    call gst_print (g_accum, gn, gst, Memi[fields], nflds)

		# Do it separately for individual groups without accumulation
		} else {
			
		    gn = 0
		    while (get_next_number (group_range, gn) != EOF) {
			if (gn > ngroups) 
			    break
			call gst_initialize (gst, lower, upper)
			call gf_opengr (im, gn, datamin, datamax, 0)
			if (msk != NULL)
			    call gf_opengr (msk, gn, datamin, datamax, 0)
			call mntaccum (gst, im, msk, msktype, lower, 
			    upper, gsw)
			call do_stats (gst, gsw)
			if ( (SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES) &&
			    gst_ihist (gst) == YES ) {
			    call hgmaccum (gst, im, msk, msktype, lower, upper)
		    	    call do_midmod (gst, gsw)
			}
		        call gst_print (g_accum, gn, gst, Memi[fields], nflds)
		    }
		}

		# Save results into a pset, gstpar.par
		pp = clopset ("gstpar")
		call gst_ppset (Memc[image], pp, gst)
		call clcpset (pp)
	    }

	    if (msktype != MSK_NONE || msk != NULL) {
		if (msktype == MSK_IMG) 
		    call imunmap (msk)
		else if (msktype == MSK_LST ) 
		    call mio_close (msk)
	    }
	    call imunmap (im)
	}

	call imtclose (list)
	call imtclose (msklist)

	# Free allocated memory

	if (GS_HGM(gst) != NULL)
	    call mfree (GS_HGM(gst), TY_INT)
	call mfree (gst, TY_STRUCT)
	call mfree (gsw, TY_STRUCT)
	call sfree (sp)
end


