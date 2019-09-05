include <imhdr.h>
include <imio.h>
include	"moveheader.h"

# CPYGRP -- Copy a single group

# Copy a single group, including group parameters and header parameters, 
# to the output image.
#
# B.Simon	01-Jun-87	Original
# B.Simon	15-Aug-91	Replaced getkeyidx with idbcard
# B.Simon	21-Aug-91	Check for group parameters
# P.Greenfield  10-Feb-97       Fix problem with copying history records.

procedure cpygrp (hdr_image, pixel_image, out_image, first)
           
char	hdr_image[ARB]		# i: Header image name
char	pixel_image[ARB]	# i: Pixel image name
char	out_image[ARB]		# i: Output image name
bool	first			# u: First group in output image?
#--
char	key[SZ_KEY], keyval[SZ_KEYVAL], comment[SZ_COMMENT]
int	dim, line_len, junk, ic
long	pixel_vec[IM_MAXDIM], out_vec[IM_MAXDIM]
pointer	imh, imp, imo, idb, rp, pixel_buf, out_buf

bool	streq(), cancpy()
int	immap(), idb_nextcard(), gf_gfind(), imaccf(), imgnld(), impnld()
pointer	idb_open()
long	clktime()

begin
	# Open images. 

	imh = immap (hdr_image, READ_ONLY, NULL)
	imp = immap (pixel_image, READ_ONLY, NULL)
	imo = immap (out_image, NEW_COPY, imh)

	# Copy the dimensions from the pixel image to the output image

	IM_PIXTYPE(imo) = IM_PIXTYPE(imp)
	IM_NDIM(imo) = IM_NDIM(imp)
	do dim = 1, IM_MAXDIM
	    IM_LEN(imo,dim) = IM_LEN(imp,dim)

	# Copy the pixel image to the output image 
	# This routine copies data in double precision

	call amovkl (long(1), pixel_vec, IM_MAXDIM)
	call amovkl (long(1), out_vec, IM_MAXDIM)
	line_len = IM_LEN(imp, 1)

	while (imgnld (imp, pixel_buf, pixel_vec) != EOF) {
	    junk = impnld (imo, out_buf, out_vec)
	    call amovd (Memd[pixel_buf], Memd[out_buf], line_len)
	}

	# Set the output image data min and max

	IM_MIN(imo) = IM_MIN(imp)
	IM_MAX(imo) = IM_MAX(imp)
	IM_LIMTIME(imo) = clktime (long(0))

	# Copy keywords from pixel image to output image
	# Check for group keywords

	idb = idb_open (imp, junk)
	while (idb_nextcard (idb, rp) != EOF) {
	    call strcpy (Memc[rp], key, SZ_KEY)
	    do ic = SZ_KEY, 1, -1 {
		if (key[ic] > ' ') {
		    key[ic+1] = EOS
		    break
		}
	    }

	    if (imaccf (imo, key) == YES && cancpy (key)) {
		if (first || gf_gfind (imo, key) > 0) {
		    call imgstr (imp, key, keyval, SZ_KEYVAL)

		    if (streq (key, "HISTORY") || streq (key, "COMMENT")) {
			call strcpy (Memc[rp+SZ_KEY+2], comment, SZ_COMMENT)
			call imputh (imo, key, comment)
		    } else {
			call impstr (imo, key, keyval)
		    }
		}
	    }
	}
	call idb_close (idb)

	# Close the images.

	call imunmap (imh)
	call imunmap (imp)
	call imunmap (imo)

	# Update value of first

	first = false
end
