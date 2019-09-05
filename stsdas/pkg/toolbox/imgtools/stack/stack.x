include	<imhdr.h>
include	<error.h>

define	SZ_PNAME	8

# T_STACK -- stack images producing NAXIS+1 dimension image
# so that, for example, pixel[i,j,k] comes from pixel[i,j] of image k
#
# Giaretta,		   Task created.
# Phil Hodge,  1-Jun-1994  Modify coordinate handling in interlabel.

procedure t_stack()

char	image1[SZ_FNAME], image2[SZ_FNAME]
char	list[SZ_LINE]
pointer	inlist, imtopen()		# Input image list
pointer	in, out
int	imtgetim(), imtlen(), axis
int	pixtype, image_number, imgnlr()
long	vsout[IM_MAXDIM], veout[IM_MAXDIM], vin[IM_MAXDIM], npix
long	one
char	dtype[SZ_LINE]
double	crval, crpix, cdelt	# coordinate parameters for output image
char	ctype[SZ_CTYPE]
pointer	immap(), impgsr(), inbuf, outbuf
double	clgetd()
int	clgwrd()
errchk	immap, clgwrd, imgnlr, impgsr

begin
	one = 1

	# Get task parameters.
	call clgstr ("input", list, SZ_LINE)
	inlist = imtopen (list)
	call clgstr ("output", image2, SZ_LINE)

	switch ( clgwrd( "pixtype", dtype, SZ_LINE,
			",real,long,short,int,double") ) {
	case 1:
	    pixtype = TY_REAL
	case 2:
	    pixtype = TY_LONG
	case 3:
	    pixtype = TY_SHORT
	case 4:
	    pixtype = TY_INT
	case 5:
	    pixtype = TY_DOUBLE
	}

	call clgstr ("ctype", ctype, SZ_CTYPE)
	call strupr (ctype)
	crval = clgetd ("crval")
	crpix = clgetd ("crpix")
	cdelt = clgetd ("cdelt")

	# check all images to see that they are the same size
	call int_check_images( inlist)

	# Loop through input images.
	image_number = 0
	while ( imtgetim (inlist, image1, SZ_FNAME) != EOF ) {
	    image_number = image_number + 1
	    in = immap (image1, READ_ONLY, 0)

	    # set output size if this is the first image
	    if (image_number == 1) {
		out = immap (image2, NEW_COPY, in)
		IM_PIXTYPE (out) = pixtype
		IM_NDIM(out) = IM_NDIM(in) + 1
		IM_LEN(out, IM_NDIM(out)) = imtlen( inlist )
		do axis = 1, IM_NDIM(in)
		    IM_LEN( out, axis) = IM_LEN( in, axis)
		npix = IM_LEN( in, 1)
		# add coordinate info to the output image
		iferr (call interlabel (in, out, crval, crpix, cdelt, ctype)) {
		    call erract( EA_WARN)
		}
	    }

	    # read sequential lines and insert into the appropriate section
	    # of the output image
	    # note that vin[] is incremented by imgnlr
	    call amovkl( one, vin,   IM_MAXDIM)
	    call amovkl( one, vsout, IM_MAXDIM)
	    vsout[IM_NDIM(out)] = image_number
	    veout[1] = IM_LEN(out, 1)
	    while ( imgnlr( in, inbuf, vin) != EOF ) {
		call amovl( vsout[2], veout[2], IM_MAXDIM-1)
		outbuf = impgsr( out, vsout, veout, IM_NDIM(out) )
		call amovr( Memr[inbuf], Memr[outbuf], npix)
		vsout[2] = vin[2]
	    }

	    # close files
	    call imunmap (in)
	}

	call imunmap (out)
	call imtclose (inlist)
end

# INT_CHECK_IMAGES -- check that all the images have matching sizes

procedure int_check_images( inlist)

pointer	inlist		# i: input image list structure
#--

char	image[SZ_FNAME], image1[SZ_FNAME], mess[SZ_LINE]
bool	first
int	imtgetim(), ndim1, axis, axlen[IM_MAXDIM]
pointer	in, immap()

begin

	first = true
	while ( imtgetim (inlist,  image, SZ_FNAME) != EOF ) {
	    in = immap( image, READ_ONLY, 0)
	    if (first) {
		if (IM_NDIM(in) == IM_MAXDIM)
		    call error(0,
		      "first image has maximum no. of axes allowed already")

		call strcpy( image, image1, SZ_FNAME)
		ndim1 = IM_NDIM( in)
		do axis = 1, IM_NDIM( in)
		    axlen[axis] = IM_LEN( in, axis)
		first = false
	    } else {
		do axis = 1, IM_NDIM( in)
		    if ( axlen[axis] != IM_LEN( in, axis) ||
						ndim1 != IM_NDIM(in) ) {
			call sprintf(mess,
			   " %s does not match first image %s in size",
					SZ_LINE)
			call pargstr( image)
			call pargstr( image1)
			call error( 0, mess)
		    }
	    }
	    call imunmap(in)
	}

	call imtrew(inlist)
end

# INTERLABEL -- insert the correct parameters for the output axis

procedure interlabel (in, out, crval, crpix, cdelt, ctype)

pointer	in		# i: input image
pointer	out		# i: output image
double	crval, crpix, cdelt	# i: coordinate parameters for output image
char	ctype[SZ_CTYPE]
#--
int	iax
char	pname[SZ_PNAME]

begin
	iax = IM_NDIM( out)

	# add keywords for last axis
	call sprintf (pname, SZ_PNAME, "ctype%d")
	    call pargi (iax)
	call imastr (out, pname, ctype)
	call sprintf (pname, SZ_PNAME, "crval%d")
	    call pargi (iax)
	call imaddd (out, pname, crval)
	call sprintf (pname, SZ_PNAME, "crpix%d")
	    call pargi (iax)
	call imaddd (out, pname, crpix)

	# add the CDn_m keywords - insert dummy values
	do iax = 1, IM_NDIM(out) - 1 {
	    call sprintf (pname, SZ_PNAME, "CD%d_%d")
		call pargi (iax)
		call pargi (IM_NDIM(out))
	    call imaddd (out, pname, 0.0d0)
	    call sprintf (pname, SZ_PNAME, "CD%d_%d")
		call pargi (IM_NDIM(out))
		call pargi (iax)
	    call imaddd (out, pname, 0.0d0)
	}
	call sprintf (pname, SZ_PNAME, "CD%d_%d")
	    call pargi( IM_NDIM(out)) ; call pargi( IM_NDIM(out))
	call imaddd (out, pname, cdelt)
end
