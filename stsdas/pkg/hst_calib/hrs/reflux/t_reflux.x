include	<imhdr.h>

# T_REFLUX -- Correct flux-calibrated HRS spectra for new sensitivies
#
# Remove the previous absolute sensitivity correction from an HRS spectrum
# and apply the new absolute sensitivity correction. Task uses absolute
# sensitivity and wavelength net reference files, CZABSR and CZNETR, as
# described in ICD-47. Task assumes that observation to be corrected
# hras a corresponding wavelength (c0h) image with the same rootname.
# Task creates a new flux image with a new rootname. Task processes all groups.
#
# Template Support:
# The input images are given by an image template list.  The output
# is either a matching list of images or a directory.
# The number of input images may be either one or match the number of output
# images.  Image sections are allowed in the input images and are ignored
# in the output images.  If the input and output image names are the same
# then the copy is performed to a temporary file which then replaces the
# input image.
#
# S. Hulbert    Jul91   Original

procedure t_reflux()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
char	newabs[SZ_PATHNAME]	 		# new absolute sensitivity image name
char	newnet[SZ_PATHNAME]			# new wavelength net image name
bool	verbose					# Print operations?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name


int	list1, list2, root_len

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb()

begin
	# Get input and output image template lists.

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	call clgstr ("new_abs", newabs, SZ_LINE)
	call clgstr ("new_net", newnet, SZ_LINE)
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    while (imtgetim (list1, image1, SZ_PATHNAME) != EOF) {

		# Strip the image section first because fnldir recognizes it
		# as part of a directory.  Place the input image name
		# without a directory or image section in string dirname1.

		call get_root (image1, image2, SZ_PATHNAME)
		root_len = fnldir (image2, dirname1, SZ_PATHNAME)
		call strcpy (image2[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)
		call reflux_prc (image1, image2, newabs, newnet, verbose)
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

	    while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		(imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {

		call reflux_prc (image1, image2, newabs, newnet, verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end
