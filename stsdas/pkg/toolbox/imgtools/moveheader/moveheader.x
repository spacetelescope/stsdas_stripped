# MOVEHEADER -- Move header keywords into another image

# The user specifies an input image containing the header keywords and an
# input image containing the image pixels. Both are written to the output
# image. If the pixel image contains keywords that match those in the header
# image, the values in the pixel image overwrite those in the header image.
#
# B.Simon	01-Jun-87	Original
# Phil Hodge	13-Feb-90	Rename from movhdr to moveheader.
# B.Simon	26-Mar-90	Delete check on number of output groups
# B.Simon	21-Aug-91	Allow multiple pixel images

procedure t_moveheader()

#--
pointer	hdrimage	# Image containing header keywords
pointer	piximage	# Image(s) containing pixels
pointer	outimage	# Output image

bool	first
int	hdr_num, pix_num, out_num, junk, index
pointer	sp, pixlist, hdrgroup, pixgroup, outgroup
pointer	hdr_ptr, pix_ptr, out_ptr, list

string	notsame_hp "Number of header and pixel images not the same"

int	imtgetim(), countgrp()
pointer	imtopen(), tp_open(), tp_fetch()

begin
	call smark (sp)
	call salloc (pixlist, SZ_FNAME, TY_CHAR)
	call salloc (hdrimage, SZ_FNAME, TY_CHAR)
	call salloc (piximage, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (hdrgroup, SZ_FNAME, TY_CHAR)
	call salloc (pixgroup, SZ_FNAME, TY_CHAR)
	call salloc (outgroup, SZ_FNAME, TY_CHAR)

	# Read the task parameters

	call clgstr ("hdrimage", Memc[hdrimage], SZ_FNAME)
	call clgstr ("piximage", Memc[piximage], SZ_FNAME)
	call clgstr ("outimage", Memc[outimage], SZ_FNAME)

	# Open the pixel image template and count the number of groups

	call strcpy (Memc[piximage], Memc[pixlist], SZ_FNAME)

	list = imtopen (Memc[pixlist])
	pix_num = countgrp (list)

	# Open the header and output group template lists

	hdr_ptr = tp_open (Memc[hdrimage], 0, hdr_num)
	out_ptr = tp_open (Memc[outimage], pix_num, out_num)

	# Check the number of groups

	if (hdr_num == 1) {
	    junk = tp_fetch (hdr_ptr, Memc[hdrgroup])

	} else if (hdr_num != pix_num) {
	    call tp_close (hdr_ptr)
	    call tp_close (out_ptr)
	    call error (1, notsame_hp)
	}

	# Loop over all pixel images

	first = true
	while (imtgetim (list, Memc[piximage], SZ_FNAME) != EOF) {

	    pix_ptr = tp_open (Memc[piximage], 0, pix_num)

	    # Copy the header keywords from the header image and the pixel data
	    # from the pixel image to the output image

	    do index = 1, pix_num {
		if (hdr_num > 1)
		    junk = tp_fetch (hdr_ptr, Memc[hdrgroup])

		junk = tp_fetch (pix_ptr, Memc[pixgroup])
		junk = tp_fetch (out_ptr, Memc[outgroup])
		call cpygrp (Memc[hdrgroup], Memc[pixgroup], 
			     Memc[outgroup], first)
	    }

	    call tp_close (pix_ptr)
	}

	# Close image lists and exit

	call imtclose (list)
	call tp_close (hdr_ptr)
	call tp_close (out_ptr)

	call sfree (sp)
end
