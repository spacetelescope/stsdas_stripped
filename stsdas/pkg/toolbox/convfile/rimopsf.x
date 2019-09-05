# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<imhdr.h>
include	<imio.h>
define  COMPRESS NO
define  LEN_PIXHDR 181

# RIMOPSF -- Open (or create) the pixel storage file.  If the image header file
# is `image.imh' the associated pixel storage file will be `imdir$image.pix',
# or some variation thereon should a collision occur.  The environment variable
# IMDIR controls where the pixfile will be placed.  The following classes of
# values are provided:
#
#	path		Put pixfile in named absolute directory regardless of
#			    where the header file is.
#	./		Put pixfile in the current directory at image creation
#			    time (special case of previous case).
#	HDR$		Put pixfile in the same directory as the header file.
#	HDR$subdir/	Put pixfiles in the subdirectory `subdir' of the
#			    directory containing the header file.  IMIO will
#			    create the subdirectory if necessary.

procedure rimopsf (im)

pointer	im				# image descriptor

long	pixoff
int	blklen, pfd
int	open(), fdevblk()
errchk	open, read, falloc, fdevblk
errchk	imioff

begin

	# Compute the offset to the pixels in the pixfile.  Allow space
	# for the pixhdr pixel storage file header and start the pixels
	# on the next device block boundary.

	blklen = fdevblk (IM_PIXFILE(im))
	pixoff = LEN_PIXHDR * SZ_STRUCT
	call imalign (pixoff, blklen)

	# Call IMIO to initialize the physical dimensions of the image
	# and the absolute file offsets of the major components of the
	# pixel storage file.

	call imioff (im, pixoff, COMPRESS, blklen)

	# Open the new pixel storage file (preallocate space if
	# enabled on local system).  Save the physical pathname of
	# the pixfile in the image header, in case "imdir$" changes.
	if (IM_FALLOC == YES) {
	   call falloc (IM_PIXFILE(im), IM_HGMOFF(im) - 1)
	   pfd = open (IM_PIXFILE(im), READ_WRITE, STATIC_FILE)
	} else
	   pfd = open (IM_PIXFILE(im), NEW_FILE, BINARY_FILE)

	IM_PFD(im) = pfd
end
