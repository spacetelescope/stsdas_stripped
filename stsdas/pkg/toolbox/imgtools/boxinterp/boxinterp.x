include	<tbset.h>
include	<imhdr.h>
include	<foc.h>

# T_BOXINTERP -- fill in rectangular areas with interpolation from surroundings
#
# Giaretta,		   Task created.
# Phil Hodge, 13-Feb-1990  Rename from rsmooth to boxinterp.
# Phil Hodge, 29-Oct-1999  The computation of innerx,innery and outerx,outery
#			were not correct; fix this.

procedure t_boxinterp()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
char	coordfile[SZ_FNAME]			# Coordinate file
int	innerx, outerx, innery, outery		# inner and outer boundaries
						# to use in fill around each pt
bool	verbose					# Print operations?

pointer	immap()
char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
pointer	in, out, xcoord, ycoord
bool	inplace
long	ncoords, xcoord_type()
int	list1, list2

int	imtopen(), imtgetim(), imtlen(), clgeti()
bool	clgetb(), streq()

begin
	# Get input and output image template lists.

	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	call clgstr ("coords", coordfile, SZ_FNAME)
	# figure out what file type and read coords into buffers xcoord
	# and ycoord (these are pointers to the data)
	ncoords = xcoord_type( coordfile, "entry", "xcol", "ycol", 
					xcoord, ycoord)
	if ( ncoords <= 0)
	    call error( 0, "must have at least one coordinate pair")

	# define annulus area to use in interpolation - convert to
	# half so we use odd sized box
	innerx = clgeti ("innerx") / 2
	outerx = clgeti ("outerx") / 2
	innery = clgeti ("innery") / 2
	outery = clgeti ("outery") / 2

	# ensure outer > inner
	outerx = max (innerx + 1, outerx)
	outery = max (innery + 1, outery)

	verbose = clgetb ("verbose")


	# Expand the input and output image lists.

	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)

	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same")
	}

	# do each set of input/output images.

	while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
	       (imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {

	    if (verbose) {
	    	call printf ("%s -> %s\n")
		call pargstr (image1)
		call pargstr (image2)
		call flush ( STDOUT )
	    }

	    # if we are writing back to itself than open READ_WRITE and
	    # operate in place
	    if (streq ( image1, image2 ) ) {
	        in      = immap (image1, READ_WRITE, 0)
	        out     = in
		inplace = true
	    } else {
	        in      = immap (image1, READ_ONLY, 0)
	        out     = immap (image2, NEW_COPY, in)
		inplace = false
	    }

	    call rsmooth (in, out, inplace, 
				Meml[xcoord], Meml[ycoord], ncoords, 
				innerx, outerx, innery, outery )

	    call imunmap (in)
	    call imunmap (out)

	}

	call imtclose (list1)
	call imtclose (list2)
	
end

# RSMOOTH -- fill in rectangular area with interpolation from surroundings
# This subroutine examines each mark and fills it in with data
# derived from the surrounding area. The fill values are taken as the weighted
# average of the bounding pixels in the line and sample directions.
#  
# A local fit in the form z=A*x + B*y + D is made using the values in
# the outer annulus


procedure rsmooth( in, out, inplace, xcoords, ycoords, ncoords, 
			innerx, outerx, innery, outery )

pointer	in				# i: Input image
pointer	out				# i: Output image
bool	inplace				# i: is operation inplace?
long	xcoords[ARB]			# i: x coordinates of blemishes
long	ycoords[ARB]			# i: y coordinates of blemishes
long	ncoords				# i: number of coords
int	innerx, outerx			# i: inner and outer x values
int	innery, outery			# i: inner and outer y values
#--

pointer	imgs2r(), imps2r(), bufin, bufout
long	vin[IM_MAXDIM], vout[IM_MAXDIM]
long	npix, ipt
int	imgnlr(), impnlr()
int	i, j, n, m, nn, mm
double	a, b, c, d
double	t1, t2
double	sum[3, 3]
double	z[2, 2]
bool	in_imagex()

begin

	npix = IM_LEN( in, 1)

	# copy the image if required
	if ( !inplace) {
	    call amovkl( long(1), vin , IM_MAXDIM)
	    call amovkl( long(1), vout, IM_MAXDIM)
	    while ( imgnlr( in, bufin , vin ) != EOF &&
		    impnlr(out, bufout, vout) != EOF     )
		call amovr( Memr[bufin], Memr[bufout], npix)
	}

	# now process each area requested
	# First initialise counters:
        do n = 1, 3
           do m = 1, 3
              sum[n, m] = 0.0

	# Sum over pixels - excluding inner box
        do n = -outerx, outerx {
            do m = -outery, outery {
                do nn = 1, 3 {
		    ####Note that 0**0 gives problems
		    if ( nn == 1)
			t2 = 1.0
		    else
                        t2 = n**(nn-1)
                    do mm =1, 3 {
			if ( mm == 1)
			    t1 = 1.0
			else
                            t1 = m**(mm-1)

			if ( abs( n ) > innerx || abs( m ) > innery )
                            sum[mm, nn] = sum[mm, nn] + t1*t2
                    }
	        }
	    }
	}

	# Loop over all the coordinates

        do ipt = 1, ncoords {

	    # Check if the point is not too close to the edge of the image
	    if ( in_imagex( out, xcoords[ipt], ycoords[ipt], 
				outerx, outerx, outery, outery ) ) {

	    # Try to fill the mark -
	    # read in appropriate section of the image
	    	bufin = imgs2r( in, xcoords[ipt]-outerx, xcoords[ipt]+outerx,
				ycoords[ipt]-outery, ycoords[ipt]+outery )
	    	bufout= imps2r(out, xcoords[ipt]-innerx, xcoords[ipt]+innerx,
				ycoords[ipt]-innery, ycoords[ipt]+innery )

	        # Calc the appropriate surface fit

		do n = 1, 3
            	    do m = 1, 3
                    	z[m, n] = 0.0

		# Sum over pixels - excluding inner box
		do n = -outerx, outerx {
                    do m = -outery, outery {
                    	do nn = 1, 2 {
			    if ( nn == 1)
				t2 = 1.0
			    else
                                t2 = n**(nn-1)
                       	    do mm =1, 2 {
				if ( mm == 1)
				    t1 = 1.0
				else
                                    t1 = m**(mm-1)
				
				if ( abs( n ) > innerx || abs( m ) > innery )
                                    z[mm, nn] = z[mm, nn] +
				       t1*t2*
				   Memr[bufin+m+outery+(n+outerx)*(2*outery+1)]
                            }
                        }
                    }
                }

		a = z[2, 1]/sum[3, 1]
		b = z[1, 2]/sum[1, 3]
		c = z[2, 2]/sum[3, 3]
		d = z[1, 1]/sum[1, 1]

		# Fill the inside area

		do i = -innerx, innerx
		    do j = -innery, innery
			Memr[bufout+j+innery+(i+innerx)*(2*innery+1)] = 
			                            a*j + b*i + c*i*j + d

	    }
	}     


end
