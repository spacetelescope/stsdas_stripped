include <imhdr.h>
include <error.h>
include "north.h"

# north -- print the image orientation
# This task prints information about orientation for a list of images.
# The values for the last image in the list is saved in the task parameters.
# Either or both of the science file and standard header packet header are
# used to obtain keyword values.
# This task is based on information determined by Warren Hack regarding
# what the header keywords actually mean, which depends on when the images
# were calibrated.
#
# Phil Hodge,  4-May-1992  Task created.
# Phil Hodge,  2-Aug-1993  Include option to leave off extension from input.

procedure t_north()

pointer list			# input list pointer
int	verbosity		# print a lot or a little?

pointer sp
pointer input			# scratch for an image name from list
pointer science			# scratch for name of .d0h image
pointer image			# scratch for name possibly without extension
pointer shp			# scratch for name of .shh image
pointer comment			# scratch for comment for verbosity > 3
pointer im1, im2		# imhdr pointers for science & shp, or NULL
pointer im			# =im1 if not NULL, else im2
char	files[SZ_KEYVAL]	# which files were used for info (sci, shp)
char	parity[SZ_KEYVAL]	# normal or reversed
double	angle			# one number representing orientation angle
double	range			# range of orientation values
double	orientat		# value of keyword from .d0h
double	cd_one, cd_two		# two orientations obtained from CD matrix
double	pa_aper			# orientation based on pa_aper from .shh
double	pa_v3			# orientation based on pa_v3 from .shh
pointer immap(), imtopenp()
int	imtgetim(), clgeti()
int	imaccess()

begin
	list = imtopenp ("input")
	verbosity = clgeti ("verbosity")

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (science, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (shp, SZ_FNAME, TY_CHAR)
	call salloc (comment, 2*SZ_LINE, TY_CHAR)

	# Print header.
	if (verbosity == 0) {
	    ;					# print nothing
	} else if (verbosity == 1) {
	    call printf ("#   image     position_angle\n")
	} else if (verbosity == 2) {
	    call printf (
		"#   image           orientat cd(one) cd(two) parity\n")
	} else if (verbosity >= 3) {
	    call printf (
"#   image           orientat cd(one) cd(two) pa_aper   pa_v3  files  parity\n")
#                   > <.....> <.....> <.....> <.....> <.....> <.....> <.....> 
	}
	if (verbosity > 0)
	    call flush (STDOUT)

	# Do for each image in the input list.
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {

	    # Construct the names of the science file header and the shp
	    # header.  Skip this image if it contains an image section.
	    iferr {
		call nor_fname (Memc[input],
			Memc[science], Memc[image], Memc[shp], SZ_FNAME)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    # Open the science file header and the shp header if they exist.
	    if (imaccess (Memc[science], READ_ONLY) == YES) {
		iferr {
		    im1 = immap (Memc[science], READ_ONLY, NULL)
		} then {
		    call erract (EA_WARN)
		    im1 = NULL
		}
	    } else {
		# Try opening the image with default (or absent) extension.
		iferr {
		    im1 = immap (Memc[image], READ_ONLY, NULL)
		} then {
		    im1 = NULL
		}
	    }

	    if (imaccess (Memc[shp], READ_ONLY) == YES) {
		iferr {
		    im2 = immap (Memc[shp], READ_ONLY, NULL)
		} then {
		    call erract (EA_WARN)
		    im2 = NULL
		}
	    } else {
		im2 = NULL
	    }

	    if (im1 == NULL && im2 == NULL) {
		call eprintf ("can't access either image:\n")
		call eprintf ("    science file:  %s\n")
		    call pargstr (Memc[science])
		call eprintf ("      shp header:  %s\n")
		    call pargstr (Memc[shp])
		next
	    }

	    # for printing image name
	    if (im1 != NULL)
		im = im1
	    else
		im = im2

	    # Get the orientation(s).
	    call nor_orient (im1, im2, angle, range,
			orientat, pa_aper, cd_one, cd_two, pa_v3,
			files, parity, SZ_KEYVAL, Memc[comment], 2*SZ_LINE)

	    # If we don't have any science file, print the name of the SHP.
	    if (im1 == NULL)
		call strcpy (Memc[shp], Memc[science], SZ_FNAME)

	    # Print the values.
	    if (verbosity == 0) {
		;
	    } else if (verbosity == 1) {
		if (!IS_INDEF(range) && range > MAX_ERROR) {
		    call printf ("%-20s %7.2f    *** range = %.2f ***\n")
			call pargstr (IM_HDRFILE(im))
			call pargd (angle)
			call pargd (range)
		} else {
		    call printf ("%-20s %7.2f\n")
			call pargstr (IM_HDRFILE(im))
			call pargd (angle)
		}
	    } else if (verbosity == 2) {	# appropriate for non-FOC
		if (!IS_INDEF(range) && range > MAX_ERROR) {
		    call printf (
		"%-20s %7.2f %7.2f %7.2f %s    *** range = %.2f ***\n")
			call pargstr (IM_HDRFILE(im))
			call pargd (orientat)
			call pargd (cd_one)
			call pargd (cd_two)
			call pargstr (parity)
			call pargd (range)
		} else {
		    call printf ("%-20s %7.2f %7.2f %7.2f %s\n")
			call pargstr (IM_HDRFILE(im))
			call pargd (orientat)
			call pargd (cd_one)
			call pargd (cd_two)
			call pargstr (parity)
		}
	    } else if (verbosity >= 3) {
		call printf ("%-20s %7.2f %7.2f %7.2f %7.2f %7.2f %-7s %s\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargd (orientat)
		    call pargd (cd_one)
		    call pargd (cd_two)
		    call pargd (pa_aper)
		    call pargd (pa_v3)
		    call pargstr (files)
		    call pargstr (parity)
		if (verbosity > 3) {
		    call printf ("%s\n")
			call pargstr (Memc[comment])
		}
	    }
	    if (verbosity > 0)
		call flush (STDOUT)

	    if (im2 != NULL)
		call imunmap (im2)
	    if (im1 != NULL)
		call imunmap (im1)
	}

	call imtclose (list)

	# Save the last set of values in the par file.
	call clpstr ("science", Memc[science])
	call clputd ("angle", angle)
	call clputd ("range", range)
	call clputd ("orientat", orientat)
	call clputd ("cd_one", cd_one)
	call clputd ("cd_two", cd_two)
	call clputd ("pa_aper", pa_aper)
	call clputd ("pa_v3", pa_v3)
	call clpstr ("files", files)
	call clpstr ("parity", parity)

	call sfree (sp)
end
