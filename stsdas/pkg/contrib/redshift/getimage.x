###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure getimage (image, pix, im, npts, name, w0, wpc, vel)
#		char	image[SZ_FNAME], name[ARB]
#		int	npts
#		real	w0, wpc, vel
#		pointer	pix
#		pointer	im

#		procedure getval(line, rval)
#		char	line[ARB]
#		real	rval

#  Description:	GETIMAGE -- Opens IRAF images for gettemps and getspec
#		and returns the image file descriptors and extracted
#		header information.
#		Adapted from the onedspec package in IRAF.
#
#		GETVAL -- returns a real value from an ASCII header line
#		Adapted from the onedspec package in IRAF.

#  Arguments:	

#  Returns:	

#  Notes:

#  History:	June	1987	Gerard Kriss
#		August	2003	Robert Jedrzejewski
#				Call to stropen() wasn't reading all the
#                               header.  Modified second argument from
#                               (LEN_USER_AREA-1)*SZ_STRUCT to
                                

###########################################################################

include <imio.h>		#Added rij 8/31/03 for 
include	<imhdr.h>
include "fquot.h"

define	COL_VALUE	11	#Start column for FITS data
define	LEN_KEY		8	#Length of FITS keyword
define	USER_AREA	Memc[($1+IMU-1)*SZ_STRUCT + 1]
define	LEN_USER_AREA	2880

# GETIMAGE -- Read new image pixels

procedure getimage (image, pix, im, npts, name, w0, wpc, vel)

char	image[SZ_FNAME], name[ARB]
int	npts
real	w0, wpc, vel
pointer	pix
pointer	im

char	line[SZ_LINE]
int	fd, nline, hdrsize

int	stropen(), getline(), strncmp()
int	clgeti()
pointer	immap(), imgl1r(), imgl2r()

begin
	# Map the image
	iferr (im = immap (image, READ_ONLY, LEN_USER_AREA)) {
	    call eprintf ("Cannot open image: %s\n")
	    call pargstr (image)
	    im = ERR
	    return
	}

	npts = IM_LEN (im, 1)

	# Get image line to plot
	if (IM_NDIM(im) != 1) {
	    nline = clgeti ("line")
	    pix = imgl2r (im, nline)
	} else {
	    nline = 1
	    pix = imgl1r (im, 1)
	}

# Get header info

# Added 8/31/03 rij - coped from code in imheader task
	hdrsize = (LEN_IMDES + IM_LENHDRMEM(im) - IMU)*SZ_STRUCT

	fd = stropen (USER_AREA(im), hdrsize, READ_ONLY)

	#Initialize to defaults
	w0 = 0.
	wpc = 1.
	vel = 0.
	call strcpy (IM_TITLE(im), name, SZ_FNAME)

	# Get keywords
	while (getline (fd, line) != EOF) {

	    if (strncmp (line, "W0      ", LEN_KEY) == 0)
		call getval (line, w0)

	    else
	    if (strncmp (line, "WPC     ", LEN_KEY) == 0)
		call getval (line, wpc)

	    else
	    if (strncmp (line, "CRVAL1  ", LEN_KEY) == 0)
		call getval (line, w0)

	    else
	    if (strncmp (line, "CDELT1  ", LEN_KEY) == 0)
		call getval (line, wpc)

	    else
	    if (strncmp (line, "VELOCITY", LEN_KEY) == 0)
		call getval (line, vel)
	}

end

procedure getval(line, rval)

char	line[ARB]
real	rval

int	ip
double	dval

int	ctod()

begin
	ip = COL_VALUE
	if ( ctod(line, ip, dval) != 0 )
		rval = dval
	else
		rval = INDEFR
end
