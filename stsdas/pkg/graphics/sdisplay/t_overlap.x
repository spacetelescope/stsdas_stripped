include	<imhdr.h>

define	W_MODES		"|linear|cos"
define	W_LINEAR	1
define	W_COS		2

define	LINE	Memr[$1 + ($2)-1]	# Line buffer element.

# OVERLAP  --  Overlaps two images, either in x or y. 
# The overlapping region is built up by a weighted average of both images.
# The weight function can be either linear or cos.
#
#  7 Jan 1991  -  I.Busko  -  Task created

procedure t_overlap ()

char	input1[SZ_FNAME]		# input image 1
char	input2[SZ_FNAME]		# input image 2
char	output[SZ_FNAME]		# output image
int	dx				# x overlap in pixels
int	dy				# y overlap in pixels

char	str[SZ_LINE]
int	wmode				# weighting mode

int	strdic(), clgeti()

begin
	# Input parameters.
	call clgstr ("input1", input1, SZ_FNAME)
	call clgstr ("input2", input2, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	dx = clgeti ("dx")
	dy = clgeti ("dy")
	call clgstr ("weight", str, SZ_LINE)
	wmode = strdic (str, str, SZ_LINE, W_MODES)
	if (wmode == 0)
	    wmode = W_LINEAR

	# Generate overlapped image.
	if ((dx > 0) && (dy == 0))
	    call overlap_x (input1, input2, output, dx, wmode)
	else if ((dy > 0) && (dx == 0))
	    call overlap_y (input1, input2, output, dy, wmode)
	else if (((dx == 0) && (dy == 0)) ||
		 ((dx >  0) && (dy >  0)))
	    call error (0,"Incompatible dx and dy.")
end


# OVERLAP_X  --  Generates output composite image with overlap in X.

procedure overlap_x (input1, input2, output, dx, wmode)

char	input1[ARB]		# input image names
char	input2[ARB]
char	output[ARB]		# output image name
int	dx			# amount of x overlap (pixels)

pointer	im1,im2			# IMIO input pointers
pointer	imout			# IMIO output pointer
pointer	l1,l2,lout		# buffer pointers
int	size1x, size1y		# image sizes
int	size2x, size2y
int	sizeox, sizeoy
int	wmode			# weighting mode
int	i,j,i1,i2
real	w1, w2			# weigths

pointer	immap(), imgl2r(),impl2r()
real	ov_weigth()

begin
	# Open input images.
	im1    = immap (input1, READ_ONLY, 0)
	im2    = immap (input2, READ_ONLY, 0)

	# Get sizes.
	size1x = IM_LEN(im1,1)
	size1y = IM_LEN(im1,2)
	size2x = IM_LEN(im2,1)
	size2y = IM_LEN(im2,2)
	if (size1y != size2y)
	    call error (0, "Input images have different Y sizes.")

	# Open output image.
	sizeox = size1x + size2x - dx
	sizeoy = size1y
	imout = immap (output, NEW_COPY, im1)
	IM_LEN(imout,1) = sizeox
	IM_LEN(imout,2) = sizeoy
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Overlap of %s and %s")
	    call pargstr (input1)
	    call pargstr (input2)

	# Do line by line.
	do j = 1, size1y {

	    # Read input lines and get output buffer.
	    l1   = imgl2r (im1,  j)
	    l2   = imgl2r (im2,  j)
	    lout = impl2r (imout,j)

	    # Copy non-overlapping sections to output buffer.
	    call amovr (LINE(l1,1),    LINE(lout,1),       size1x-dx)
	    call amovr (LINE(l2,dx+1), LINE(lout,size1x+1),size2x-dx)

	    # Generate overlapping section.
	    i1 = size1x -dx -1
	    i2 = 0
	    do i = size1x-dx+1, size1x {
	        i1 = i1 + 1
	        i2 = i2 + 1
	        # Compute weigth function.
	        w1 = ov_weigth (i2, dx, wmode)
	        w2 = 1. - w1
	        # Weigthed mean
	        LINE(lout,i) = w1 * LINE(l1,i1) + w2 * LINE(l2,i2)
	    }
	}

	# Close images.
	call imunmap (imout)
	call imunmap (im2)
	call imunmap (im1)
end


# OVERLAP_Y  --  Generates output composite image with overlap in Y.

procedure overlap_y (input1, input2, output, dy, wmode)

char	input1[ARB]		# input image names
char	input2[ARB]
char	output[ARB]		# output image name
int	dy			# amount of y overlap (pixels)

pointer	im1,im2			# IMIO input pointers
pointer	imout			# IMIO output pointer
pointer	l1,l2,lout		# buffer pointers
int	size1x, size1y		# image sizes
int	size2x, size2y
int	sizeox, sizeoy
int	wmode			# weighting mode
int	j,j2
real	w1, w2			# weigths

pointer	immap(), imgl2r(),impl2r()
real	ov_weigth()

begin
	# Open input images.
	im1    = immap (input1, READ_ONLY, 0)
	im2    = immap (input2, READ_ONLY, 0)

	size1x = IM_LEN(im1,1)
	size1y = IM_LEN(im1,2)
	size2x = IM_LEN(im2,1)
	size2y = IM_LEN(im2,2)
	if (size1x != size2x)
	    call error (0, "Input images have different X sizes.")

	# Open output image.
	sizeox = size1x
	sizeoy = size1y + size2y - dy
	imout = immap (output, NEW_COPY, im1)
	IM_LEN(imout,1) = sizeox
	IM_LEN(imout,2) = sizeoy
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Overlap of %s and %s")
	    call pargstr (input1)
	    call pargstr (input2)

	# Do line by line.
	j2 = 0
	do j = 1, sizeoy {

	    # First, copy non-overlapping section of first image.
	    if (j <= (size1y-dy)) {
	        l1   = imgl2r (im1,  j)
	        lout = impl2r (imout,j)
	        call amovr (LINE(l1,1), LINE(lout,1), size1x)
	    }

	    # Next, generate overlapping section.
	    else if ((j > (size1y-dy)) && (j <= size1y)) {
	        j2 = j2 + 1
	        l1   = imgl2r (im1,  j)
	        l2   = imgl2r (im2,  j2)
	        lout = impl2r (imout, j)
	        # Compute weigths.
	        w1 = ov_weigth (j2, dy, wmode)
	        w2 = 1. - w1
	        # Do weigthed sum.
	        call amulkr (LINE(l1,1), w1, LINE(l1,1), size1x)
	        call amulkr (LINE(l2,1), w2, LINE(l2,1), size1x)
	        call aaddr  (LINE(l1,1), LINE(l2,1), LINE(lout,1), size1x)
	    }

	    # Lastly, copy non-overlapping section of second image.
	    else if (j2 >= dy) {
	        j2 = j2 + 1
	        l2   = imgl2r (im2,  j2)
	        lout = impl2r (imout,j)
	        call amovr (LINE(l2,1), LINE(lout,1), size1x)
	    }
	}

	# Close images.
	call imunmap (imout)
	call imunmap (im2)
	call imunmap (im1)
end


# OV_WEIGTH  --  Generate weigth.

real procedure ov_weigth (index, size, mode)

int	index	
int	size
int	mode

real	weigth

begin
	switch (mode) {

	case W_LINEAR:
	    weigth = 1. - real (index) / real (size)

	case W_COS:
	    weigth = (cos (3.1416 * real(index) / real(size))) / 2. + 0.5

	default:

	}
	return (weigth)
end
