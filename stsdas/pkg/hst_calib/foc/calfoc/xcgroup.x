include <imhdr.h>
include "calfoc.h"		# used for PODPS_ENVIRON definition

define	SZ_KEYWORD	11	# size of buffer for keyword value

# xc_group -- write group parameters to file
# This routine extracts the RA, Dec, and image orientation from the
# group parameter block and writes them to an ascii file.
# The purpose of this ascii file is to allow the archiving system to get
# these group parameters without opening the binary data file.  If this
# routine is called outside of the PODPS environment or for a non-FOC image,
# it doesn't make sense to write this file.  At the slightest excuse,
# therefore, we will return without creating the file.  First we check
# see if a particular environment variable is defined; if not, we're not
# running within PODPS, and we just return.  Next we check that all three
# parameters CRVAL1, CRVAL2, ORIENTAT are present; if not, we just return.
#
# Phil Hodge, 13-May-1991  Subroutine created.
# Phil Hodge, 19-Jul-1991  Change extension from .grp to .cgr.
# Phil Hodge,  5-Aug-1991  Input is image name rather than imhdr pointer;
#			don't write file if PODPS_CLD is not defined.
# Phil Hodge, 10-Jul-1992  Two input image names, one for coordinates and
#			the other for datamin & datamax.
# Phil Hodge,  6-Feb-1998  Two calls to imgetr had too many arguments.

procedure xc_group (coords_image, minmax_image, output)

char	coords_image[ARB]	# i: name of image for coordinates
char	minmax_image[ARB]	# i: name of image for datamin & datamax
char	output[ARB]		# i: root name of output
#--
pointer im			# pointer to imhdr struct for image
char	fname[SZ_FNAME]		# name of output file to contain parameters
char	obuf[SZ_FNAME]		# buffer for a line of output
double	crval1, crval2		# RA & Dec
real	orientat		# orientation of image (from CD matrix)
real	datamin, datamax	# minimum & maximum data values
int	fd			# fd for output file
double	imgetd()
real	imgetr()
pointer immap()
int	imaccf(), open(), envgets()
bool	strne()

# for finding datamin & datamax from data values
pointer x, imgl2r()
real	dmin, dmax
int	j

begin
	# If this environment variable is not defined, just return.
	if (envgets (PODPS_ENVIRON, obuf, SZ_FNAME) < 1)
	    return

	# First open the image for coordinates.
	im = immap (coords_image, READ_ONLY, NULL)

	# If any of these keywords is absent, just return.
	if (imaccf (im, "crval1") == NO ||
	    imaccf (im, "crval2") == NO ||
	    imaccf (im, "orientat") == NO) {
	    call imunmap (im)
	    return
	}

	# Get coordinate info from image.
	crval1 = imgetd (im, "crval1")
	crval2 = imgetd (im, "crval2")
	orientat = imgetr (im, "orientat")
	if (orientat < 0.)
	    orientat = orientat + 360.

	# If we have a different image for datamin & datamax, close the
	# coordinates image and open the minmax image.
	if (strne (coords_image, minmax_image)) {
	    call imunmap (im)
	    im = immap (minmax_image, READ_ONLY, NULL)
	}
	datamin = imgetr (im, "i_minpixval")
	datamax = imgetr (im, "i_maxpixval")

	if (datamax <= 0.) {
	    # Find the min & max.
	    x = imgl2r (im, 1)
	    call alimr (Memr[x], IM_LEN(im,1), datamin, datamax)
	    do j = 2, IM_LEN(im,2) {
		x = imgl2r (im, j)
		call alimr (Memr[x], IM_LEN(im,1), dmin, dmax)
		datamin = min (datamin, dmin)
		datamax = max (datamax, dmax)
	    }
	}

	# Done with the image(s).
	call imunmap (im)

	# Form output file name.
	call strcpy (output, fname, SZ_FNAME)
	call strlwr (fname)
	call strcat (".cgr", fname, SZ_FNAME)

	iferr {
	    fd = open (fname, NEW_FILE, TEXT_FILE)
	} then {
	    call logmsg ("info:  can't create .cgr file")
	    return
	}

	call sprintf (obuf, SZ_FNAME, "DATAMIN  = %15.7g\n")
	    call pargr (datamin)
	call putline (fd, obuf)
	call sprintf (obuf, SZ_FNAME, "DATAMAX  = %15.7g\n")
	    call pargr (datamax)
	call putline (fd, obuf)
	call sprintf (obuf, SZ_FNAME, "CRVAL1   = %15.10f\n")
	    call pargd (crval1)
	call putline (fd, obuf)
	call sprintf (obuf, SZ_FNAME, "CRVAL2   = %15.10f\n")
	    call pargd (crval2)
	call putline (fd, obuf)
	call sprintf (obuf, SZ_FNAME, "ORIENTAT = %10.5f\n")
	    call pargr (orientat)
	call putline (fd, obuf)

	call close (fd)
end
