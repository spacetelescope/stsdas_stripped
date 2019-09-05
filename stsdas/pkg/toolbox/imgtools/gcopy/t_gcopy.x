# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# GCOPY -- Task to copy Geis images to another non_existant geis file or
#           if more than one input images, then the output is a directory.
#          
#    Possible copy more than one input geis file into an existant one
#    (append) or create a multigroup geis image.


# P. Greenfield    12-feb-97  changed gcopy to allow copying to arbitrary
#			      places in the output file.
# P. Greenfield     7-mar-97  previous change failed to check for invalid
#                             output group numbers in gr_copy. 
# B. Simon	   27-jun-97  replaced IM_KERNEL check with check of image
#			      extension
# P. Greenfield     4-feb-98  Fixed bug where 'gcopy test.c0h test2.hhh'
#                             was producing test2.c0h instead
# P. Greenfield     2-jun-98  Previous fix introduced new bug, removed whole
#                             "fast copy" section as it seemed unnecessary.
# B. Simon	    9-dec-98  changes to support new gf library
#
# B.Simon	   12-jul-00  rewrote gcopy to remove double copy of first
#			      group

include <imio.h>
include <imhdr.h>
define MAX_RANGES  100
define  MAX_LENEXTN     4               # max length header filename extension

procedure t_gcopy()


char	input_image[SZ_PATHNAME]
char	output_image[SZ_PATHNAME]
char	group_list[SZ_LINE]
bool	i2toi4
bool    verbose                                 # Print operations?

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list

char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name

char	cluster[SZ_PATHNAME]
int	list1, list2, root_len

bool    clgetb()
int	imtopen(), imtgetim(), imtlen()
int	strcmp(), fnldir(), isdirectory()

errchk  gr_copy

begin
	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
        call clgstr ("groups", group_list, SZ_LINE)
	i2toi4 = clgetb ("i2toi4")

	call xt_stripwhite (group_list)
	if (i2toi4 || strcmp ("ALL",group_list) == 0)
	   group_list[1] = EOS

	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    while (imtgetim (list1, input_image, SZ_PATHNAME) != EOF) {

		# Strip the image section first because fnldir recognizes it
		# as part of a directory.  Place the input image name
		# without a directory or image section in string dirname1.

	        call imgcluster(input_image, cluster, SZ_PATHNAME)
		call get_root (cluster, output_image, SZ_PATHNAME)

		root_len = fnldir (output_image, dirname1, SZ_PATHNAME)
		call strcpy (output_image[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, output_image, SZ_PATHNAME)
		call strcat (dirname1, output_image, SZ_PATHNAME)

		call gr_copy (input_image, output_image, group_list, 
			      i2toi4, verbose)
	    }

	    call imtclose (list1)

	} else {
	    # Expand the input and output image lists.

	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.

	    while ((imtgetim (list1, input_image, SZ_PATHNAME) != EOF) &&
		(imtgetim (list2, output_image, SZ_PATHNAME) != EOF)) {

		call gr_copy (input_image, output_image, group_list, 
			      i2toi4, verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end

# GR_COPY -- Copy an image file to another given a range of groups 

procedure gr_copy (input_image, output_image, group_list, i2toi4, verbose)

char	input_image[ARB]	# i: input image name
char	output_image[ARB]	# i: output image name
char	group_list[ARB]		# i: the list of groups to copy
bool	i2toi4			# i: flag for conversion of gpb values
bool	verbose			# i: flag for diagnostic message
#--
int	ifast, ofast, indexi, indexo, icount, ocount, status, ign, ogn	
pointer	sp, inroot, outroot, insect, outsect, inext, outext
pointer	input, output, igroup, ogroup, irange, orange
pointer	imi, imo
real 	datamin, datamax

string	nocopy    "Cannot open output pixel file"
string	badilist  "Illegal input file number list"
string	badolist  "Illegal output file number list (internal error)"
string	toofew_1  "Warning: output file has too few groups to contain all of\n"
string	toofew_2  "the selected input groups. The last %d groups not copied.\n"

bool	streq()
int	gf_gcount(), get_next_number(), decode_ranges()
pointer	gf_map()

errchk gf_map

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (inroot, SZ_PATHNAME, TY_CHAR)
	call salloc (outroot, SZ_PATHNAME, TY_CHAR)
	call salloc (insect, SZ_PATHNAME, TY_CHAR)
	call salloc (outsect, SZ_PATHNAME, TY_CHAR)
	call salloc (inext, SZ_FNAME, TY_CHAR)
	call salloc (outext, SZ_FNAME, TY_CHAR)
	call salloc (input, SZ_PATHNAME, TY_CHAR)
	call salloc (output, SZ_PATHNAME, TY_CHAR)
	call salloc (igroup, SZ_LINE, TY_CHAR)
	call salloc (ogroup, SZ_LINE, TY_CHAR)
	call salloc (irange, 2*MAX_RANGES+1, TY_INT)
	call salloc (orange, 2*MAX_RANGES+1, TY_INT)

	# If verbose print the operation

	if (verbose) {
	    call printf ("%s -> %s\n")
	    call pargstr (input_image)
	    call pargstr (output_image)
	}

	# Extract root name and extension from image name

	call extract_name (input_image, ifast, indexi, Memc[inroot], 
			   Memc[insect], Memc[inext], SZ_PATHNAME)

	call extract_name (output_image, ofast, indexo, Memc[outroot], 
			   Memc[outsect], Memc[outext], SZ_PATHNAME)

	# Do a fast copy in the case where the image 
	# requires no special processing

	if (! i2toi4 && streq (Memc[inext], Memc[outext]) &&
	    ifast == YES && ofast == YES && group_list[1] == EOS) {

	    call imcopy (input_image, output_image)
	    call sfree (sp)
	    return
	}

	# Construct input group range if one was not passed to the program

	imi = NULL
	imo = NULL

	if (group_list[1] != EOS) {
	    call strcpy (group_list, Memc[igroup], SZ_LINE)

	} else {
	    if (indexi >= 0) {
		call sprintf (Memc[igroup], SZ_LINE, "%d")
		call pargi (indexi)

	    } else {
		imi = gf_map (input_image, READ_ONLY, NULL)
		icount = gf_gcount (imi)

		if (icount == 1) {
		    call strcpy ("1", Memc[igroup], SZ_LINE)
		} else {
		    call sprintf (Memc[igroup], SZ_LINE, "1-%d")
		    call pargi (icount)
		}
	    }
	}

        if (decode_ranges (Memc[igroup], Memi[irange], 
			   MAX_RANGES, icount) == ERR)
	   call error (1, badilist)

	# Construct output group range

	if (indexo >= 0) {
	    call sprintf (Memc[ogroup], SZ_LINE, "%d")
	    call pargi (indexo)

	} else {
	    if (icount == 1) {
		call strcpy ("1", Memc[ogroup], SZ_LINE)
	    } else {
		call sprintf (Memc[ogroup], SZ_LINE, "1-%d")
		call pargi (icount)
	    }
	}

        if (decode_ranges (Memc[ogroup], Memi[orange], 
			   MAX_RANGES, ocount) == ERR)
	   call error (1, badolist)

	# Convert short integers to long

	if (i2toi4) {
	    if (imi == NULL)
		imi = gf_map (input_image, READ_ONLY, NULL)

	    imo = gf_map (output_image, NEW_COPY, imi)

	    call gi_fixpsize (imo)
	    call gi_opix (imo, status)

	    if (status != 0) 
		call error (1, nocopy)
	}

	# Compare lengths of range lists 

	if (icount > ocount) {
	    call printf (toofew_1)
	    call printf (toofew_2)
	    call pargi (icount - ocount)
	}

	# Loop over input and output ranges, copying each group

	ign = 0
	ogn = 0
        while (get_next_number (Memi[irange], ign) != EOF &&
	       get_next_number (Memi[orange], ogn) != EOF) {

	    # Open next group in input image

	    if (imi != NULL) {
		call gf_opengr (imi, ign, datamin, datamax, NULL)

	    } else {
		if (indexi >= 0) {
		    call strcpy (input_image, Memc[input], SZ_PATHNAME)

		} else {
		    call sprintf (Memc[input], SZ_PATHNAME, "%s[%d]%s")
		    call pargstr (Memc[inroot])
		    call pargi (ign)
		    call pargstr (Memc[insect])
		}

		imi = gf_map (Memc[input], READ_ONLY, NULL)
	    }

	    # Open next group in output image

	    if (imo != NULL) {
		call gf_opengr (imo, ogn, datamin, datamax, imi)

	    } else {
		if (indexo >= 0) {
		    call strcpy (output_image, Memc[output], SZ_PATHNAME)

		} else {
		    call sprintf (Memc[output], SZ_PATHNAME, "%s[%d/%d]%s")
		    call pargstr (Memc[outroot])
		    call pargi (ogn)
		    call pargi (ocount)
		    call pargstr (Memc[outsect])
		}

		imo = gf_map (Memc[output], NEW_COPY, imi)
	    }

	    # Copy group from input to output image

	    call newcopy (imi, imo, datamin, datamax)
	}

	call gf_unmap (imi)
	call gf_unmap (imo)

	call sfree (sp)
end
	
# EXTRACT_NAME -- Extract root, section, and extension names from image name

procedure extract_name (image, fast, index, root, sect, ext, maxch)

char	image[ARB]	# i: image name
int	fast		# o: can we do a fast copy?
int	index		# o: group index in image
char	root[ARB]	# o: root name
char	sect[ARB]	# o: section name
char 	ext[ARB]	# o: extension name
int	maxch		# i: declared length of root name
#--
int	nc, size
pointer	sp, section, ksection

int	btoi(), fnextn()

begin
	call smark (sp)
	call salloc (ksection, maxch, TY_CHAR)
	call salloc (section, maxch, TY_CHAR)

	call imparse (image, root, maxch, Memc[ksection], maxch,
		      Memc[section], maxch, index, size)

	call strcpy (Memc[ksection], sect, maxch)
	call strcat (Memc[section], sect, maxch)
	nc = fnextn (root, ext, maxch)

	fast = btoi (index < 0 && sect[1] == EOS)
	call sfree (sp)
end
