include	<imhdr.h>
include	<imset.h>
include	<math/curfit.h>
include	<math/surfit.h>

define	EPS		1.e-10
define	PROC_TYPES	"|window|extension"		# type of edge processing
define	PROC_WIN	1
define	PROC_EXT	2
define	WIN_TYPES	"|Welch|Parzen|Hanning"		# type of spectral window
define	WIN_WELCH	1
define	WIN_PARZEN	2
define	WIN_HANNING	3

define	L_OVER		5	# sample size for linear spline3 fit.
define	C_OVER		3	# sample width for corner fill-up procedure.
define	C_DEG		2	# bi-cubic spline degree.
define	MIN_FILT	0.1	# minimum gaussian filter sigma.
define	MAX_FILT	7.	# maximum gaussian filter sigma.
define	S_LINES		5	# sample lines for image background estimation.

define	LINE	Memr[$1+($2)-1]	                     # line buffer elements
define	LINEI	Memi[$1+($2)-1]
define	AR	Memr[$1+($3-1)*sizex+$2-1]           # 2-D array elements
define	AR2	Memr[$1+($3-1)*(sizex+2*edge)+$2-1]
define	ARC	Memr[$1+($3-1)*2*(edge+C_OVER)+$2-1]
define	ARO	Memr[$1+($3-1)*(edge+C_OVER)+$2-1]
define	ARE	Memr[$1+($3-1)*edge+$2-1]

# EDGE  --  Image edge massaging.
#
# This task modifies image edges by applying either of two techniques:
# (i) Windowing: spectral windows as Parzen, Welch and Hanning types may
# be applied to the input image. The windows may act on the full image, as
# usual, or only on a fraction near the edge, leaving the central regions
# untouched. The amount of area affected is controlled by a task parameter.
# (ii) Overlapped edge extension: useful in the context of image restoration
# techniques that assume circular convolution. The image edges are extended 
# beyond the data limits, generating a larger pixel array than the input one. 
# The extended region is filled up with interpolated values from the image
# itself, in such a way as to make the resulting image approximately
# circulant for the specific convolution at hand. The details are explained 
# in Bates & McDonnell ("Image Restoration and Reconstruction", 1986, Claredon 
# Press, p.~63).
#
# Images of different sizes can be input simultaneously to the task.
#
#
# 02 Apr 1993	-  I. Busko  -  Task created.
# 03 Sep 1993	-     "      -  Optimized (disk access time) version;
#                               optional user input for background value.


procedure t_edge()

char	imlisti[SZ_LINE]		# input image list
char	imlisto[SZ_LINE]		# output image list/directory
int	type				# window/edge/extension
int	func				# window function type
int	edge				# edge to be massaged
real	backg				# background value
bool	verb

char	input[SZ_PATHNAME]		# input image name
char	output[SZ_PATHNAME]		# output image name
char	dirnamei[SZ_PATHNAME]		# directory name
char	dirnameo[SZ_PATHNAME]		# directory name
char	str[SZ_LINE]
int	listi, listo, root_len
int	strdic()
int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
int	clgeti()
real	clgetr()
bool	clgetb()

begin
	# Input parameters.
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("output", imlisto, SZ_LINE)
	edge  = clgeti ("esize")
	backg = clgetr ("background")
	verb  = clgetb ("verbose")
	call clgstr ("type", str, SZ_LINE)
	type = strdic (str, str, SZ_LINE, PROC_TYPES)
	if (type == 0)
	    type = PROC_WIN
	call clgstr ("function", str, SZ_LINE)
	func = strdic (str, str, SZ_LINE, WIN_TYPES)
	if (func == 0)
	    func = WIN_WELCH

	# If the output string is a directory, generate names for
	# the new images accordingly.

	if (isdirectory (imlisto, dirnameo, SZ_PATHNAME) > 0) {
	    listi = imtopen (imlisti)
	    while (imtgetim (listi, input, SZ_PATHNAME) != EOF) {

		# Place the input image name, without a directory prefix, 
		# in string dirnamei.
		root_len = fnldir (input, dirnamei, SZ_PATHNAME)
		call strcpy (input[root_len + 1], dirnamei, SZ_PATHNAME)

		# Assemble output image name.
		call strcpy (dirnameo, output, SZ_PATHNAME)
		call strcat (dirnamei, output, SZ_PATHNAME)

		# Do it.
	        switch (type) {
	        case PROC_WIN:
	            call ed_taper (input, output, func, edge, verb)
	        case PROC_EXT:
	            call ed_extend (input, output, edge, verb)
	        }
	    }
	    call imtclose (listi)

	} else {

	    # Expand the input and output image lists.
	    listi = imtopen (imlisti)
	    listo = imtopen (imlisto)
	    if (imtlen (listi) != imtlen (listo)) {
	        call imtclose (listi)
	        call imtclose (listo)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (listi, input,  SZ_PATHNAME) != EOF) &&
		   (imtgetim (listo, output, SZ_PATHNAME) != EOF)) {
	        switch (type) {
	        case PROC_WIN:
	            call ed_taper (input, output, func, edge, verb)
	        case PROC_EXT:
	            call ed_extend (input, output, edge, backg, verb)
	        }
	    }

	    call imtclose (listi)
	    call imtclose (listo)
	}


end



# ED_TAPER  --  Tapering by window function.

procedure ed_taper (input, output, func, edge, verb)

char	input[ARB]			# input image
char	output[ARB]			# output image
int	func				# window function type
int	edge				# edge to be massaged
bool	verb

pointer	imin, imout			# IMIO pointers
pointer	li, lo				# line buffer pointers
int	sizex, sizey			# image size
int	i, j
real	aux

pointer	immap(), imgl2r(), impl2r()
real	ed_function()

begin
	if (verb) {
	    call printf ("%s -> %s      ")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}

	# Open input image and get sizes.
	imin = immap (input, READ_ONLY, 0)
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)

	# Open output image.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = sizex
	IM_LEN(imout,2) = sizey
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Windowing of `%s'")
	    call pargstr (input)

	# Do line by line.
	do j = 1, sizey {

	    # Read input line and get output buffer.
	    li = imgl2r (imin, j)
	    lo = impl2r (imout,j)

	    # Process extreme lines.
	    if (j <= edge) {
	        i   = edge - j + 1
	        aux = ed_function (i, edge, func)
	        call amulkr (LINE(li,1), aux, LINE(li,1), sizex)
	    }
	    if (j > (sizey - edge)) {
	        i   =  j - (sizey - edge)
	        aux = ed_function (i, edge, func)
	        call amulkr (LINE(li,1), aux, LINE(li,1), sizex)
	    }

	    # Process extremes of each line.
	    do i = 1, edge {
	        aux = ed_function ((edge-i+1), edge, func)
	        LINE(li,i)         = aux * LINE(li,i)
	        LINE(li,sizex-i+1) = aux * LINE(li,sizex-i+1)
	    }

	    # Move to output.
	    call amovr (LINE(li,1), LINE(lo,1), sizex)

	}

	# Close images.
	call imunmap (imout)
	call imunmap (imin)
	if (verb)
	    call printf ("\n")
end



# ED_FUNCTION  --  Generate window function.

real procedure ed_function (x, size, func)

int	x	
int	size
int	func

real	output

begin
	switch (func) {

	case WIN_PARZEN:
	    output = 1. - real (x) / real (size)

	case WIN_HANNING:
	    output = (cos (3.1416 * real(x) / real(size))) / 2. + 0.5

	case WIN_WELCH:
	    output = 1. - (real (x) / real (size)) ** 2

	default:
	    output = 0.

	}
	return (output)
end



# ED_EXTEND  --  Overlapped edge extension.

procedure ed_extend (input, output, edge, background, verb)

char	input[ARB]			# input image
char	output[ARB]			# output image
int	edge				# edge to be massaged
real	background			# background value.
bool	verb

pointer	imin, imout			# IMIO pointers
pointer	in, out				# buffer pointers
pointer	column				# column vector storage
int	sizex, sizey			# image size
int	i, j
real	sigma, smean, back

pointer	immap()
pointer imgs2r(), imps2r()
pointer imgl2r(), impl2r()

begin
	back = background

	if (verb) {
	    call printf ("%s -> %s      ")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}

	# Open input image and get size.
	imin = immap (input, READ_ONLY, 0)
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)

	# Estimate image background at edges, to be used as 
	# stabilizing value in spline fitting. Do it only if
	# not supplied by user. This procedure assumes that a 
	# representative background can be measured at the first 
	# S_LINES at the bottom of the image.
	if (IS_INDEFR(back)) {
	    smean = 0.
	    do i = 2, S_LINES {
	        in = imgl2r (imin, i)
	        call aavgr (LINE(in,1), sizex, back, sigma)
	        smean = smean + back
	    }
	    back = smean / S_LINES
	}

	# Open output image with larger size.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = sizex + 2 * edge
	IM_LEN(imout,2) = sizey + 2 * edge
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Edge extension of `%s'")
	    call pargstr (input)

	# Copy input image to center of output array.
	in  = imgs2r (imin, 1, sizex, 1, sizey)
	out = imps2r (imout, 1, IM_LEN(imout,1), 1, IM_LEN(imout,2))
	call amovkr (back, AR2(out,1,1), IM_LEN(imout,1)*IM_LEN(imout,2))
	do i = 1, sizey
	    call amovr (AR(in,1,i), AR2(out,1+edge,i+edge), sizex)

	# Close images.
	call imunmap (imout)
	call imunmap (imin)

	# Re-open output image, reset size variables.
	imin = immap (output, READ_WRITE, 0)
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)

	# Processing of line borders, flush when finished.
	do i = 1+edge, sizey-edge {
	    in  = imgl2r (imin, i)
	    out = impl2r (imin, i)
	    call ed_fit1 (in, edge, sizex, back)
	    call amovr (LINE(in,1), LINE(out,1), sizex)
	}
	call imflush (imin)

	# Processing of column borders. This must be done by reading
	# the full pixel array in memory, otherwise it becomes EXTREMELY
	# slow, in particular when image resides in a remote disk.
	in  = imgs2r (imin, 1, sizex, 1, sizey)
	out = imps2r (imin, 1, sizex, 1, sizey)
	call malloc (column, sizey, TY_REAL)
	do i = 1+edge, sizex-edge {
	    do j = 1, sizey 
	        LINE(column,j) = AR(in,i,j)
	    call ed_fit1 (column, edge, sizey, back)
	    do j = 1, sizey 
	        AR(in,i,j) = LINE(column,j)
	}
	call mfree (column, TY_REAL)
	call amovr (AR(in,1,1), AR(out,1,1), sizex*sizey)

	# Flush image and go process corners.
	call imflush (imin)
	call ed_corner (imin, sizex, sizey, edge, back)

	# Go filter edges.
	call imflush (imin)
	call ed_filter (imin, sizex, sizey, edge)

	# The following code tries to remove an artifact generated by the 
	# spline fitting: the first column (y=1) contains somewhat wrong
	# data, which impair the power spectrum smoothness. This column 
	# is filled out with the average of the background value and the 
	# pixel value taken from the neighbor column y=2.
	call imflush (imin)
	in  = imgs2r (imin, 2, 2, 1, sizey)
	out = imps2r (imin, 1, 1, 1, sizey)
	do i = 1, sizey
	    LINE(out,i) = (LINE(in,i) + back) / 2.

	# Done.
	call imunmap (imin)
	if (verb)
	    call printf ("\n")
end



# ED_FIT1  --  Fit each line/column edge with a suitable cubic spline
#              function, and fills up empty space with interpolated values.
#
# This is a slight different implementation from the one described by 
# Eq. 15.35 of Bates & MacDonnell 1986, since here the background value is 
# included at the midpoint of the data sample, with a large weight, and 
# 2 spline pieces are fitted. This presumabily helps to damp oscillations.

procedure ed_fit1 (line, edge, lsize, backgr)

pointer	line				# i/o data buffer pointer
int	lsize				# i/o data buffer size
int	edge				# size of edge region
real	backgr				# image background estimate

pointer	sp
pointer	x, y, w				# fit vectors
pointer	cv				# curfit pointer
int	vsize				# fit vectors size
int	i
real	cveval()

begin
	# Size of fit vectors
	vsize = 2 * L_OVER + 1

	# Alloc space for fit vectors.
	call smark (sp)
	call salloc (x, vsize, TY_REAL)
	call salloc (y, vsize, TY_REAL)
	call salloc (w, vsize, TY_REAL)

	# Assemble data for fit. Notice that the image's estimated background
	# is included with larger weight at the midpoint between samples.
	call amovr (LINE(line,lsize-edge-L_OVER+1), LINE(y,1), L_OVER) # Y data
	call amovr (LINE(line,edge+1), LINE(y,L_OVER+1), L_OVER)
	LINE(y,vsize) = backgr
	do i = 1, L_OVER {                                             # X data
	    LINE(x,i)        = real(i)
	    LINE(x,i+L_OVER) = real(L_OVER+2*edge+i)
	}
	LINE(x,vsize) = real(L_OVER+edge)
	call amovkr (1., LINE(w,1), vsize-1)                           # weights
	LINE(w, vsize) = real(vsize)

	# Fit 2 pieces of cubic spline.
	call cvinit (cv, SPLINE3, 2, 1., real(2*edge+2*L_OVER))
	call cvfit (cv, LINE(x,1), LINE(y,1), LINE(w,1), vsize, WTS_USER, i)
	if (i != OK)
	    call error (0, "Error in spline fitting.")

	# Fill up segments.
	do i = 1, edge {
	    LINE(line,lsize-edge+i) = cveval (cv, real(L_OVER+i))
	    LINE(line,i)            = cveval (cv, real(L_OVER+edge+i))
	}

	call cvfree (cv)
	call sfree (sp)
end



# ED_CORNER  --  Treatment of image corners. The edges of the four corners
#                are put togheter as edges of one single wrapped-around block.
#                A bi-cubic spline is fitted to these corner border pixels 
#                and the internal pixels are then filled up with
#                interpolated values. This procedure is not discussed in the
#                reference, and came out after several experiments.

procedure ed_corner (image, sizex, sizey, edge, backgr)

pointer	image
int	sizex, sizey
int	edge
real	backgr				# image background estimate

pointer	in, out				# buffer pointers
pointer	lin, col, weight		# surface fit vectors
pointer	block, wblock			# surface storage
pointer	sp
pointer	sf				# surfit pointer
int	bsize				# surface block size
int	i,j,k

pointer imgs2r(), imps2r()
real	iseval()

begin
	bsize = 2 * (C_OVER + edge)

	# Alloc space for line and column arrays, as well as data block.
	call smark (sp)
	call salloc (lin,     bsize,    TY_INT)
	call salloc (col,     bsize,    TY_INT)
	call salloc (weight,  bsize,    TY_REAL)
	call salloc (block,   bsize**2, TY_REAL)
	call salloc (wblock,  bsize**2, TY_REAL)

	# Initialize lines and columns.
	do i = 1, bsize {
	    LINEI(lin,i) = i
	    LINEI(col,i) = i
	}

	# Build data block from 4 image corners.

	# Lower left corner.
	in  = imgs2r (image, 1, edge+C_OVER, 1, edge+C_OVER)
	do j = 1, edge+C_OVER {
	    do i = 1, edge+C_OVER {
	        ARC(block,i+edge+C_OVER,j+edge+C_OVER) = ARO(in,i,j)
	    }
	}
	# Upper left corner.
	in  = imgs2r (image, 1, edge+C_OVER, sizey-edge-C_OVER+1, sizey)
	do j = 1, edge+C_OVER {
	    do i = 1, edge+C_OVER {
	        ARC(block,i+edge+C_OVER,j) = ARO(in,i,j)
	    }
	}
	# Lower right corner.
	in  = imgs2r (image, sizex-edge-C_OVER+1, sizex, 1, edge+C_OVER)
	do j = 1, edge+C_OVER {
	    do i = 1, edge+C_OVER {
	        ARC(block,i,j+edge+C_OVER) = ARO(in,i,j)
	    }
	}
	# Upper right corner.
	in  = imgs2r (image, sizex-edge-C_OVER+1, sizex,
	                     sizey-edge-C_OVER+1, sizey)
	do j = 1, edge+C_OVER {
	    do i = 1, edge+C_OVER {
	        ARC(block,i,j) = ARO(in,i,j)
	    }
	}

	# Center pixels contain background estimate.
	do j = 1, bsize {
	    ARC(block,j,edge+C_OVER) = backgr
	    ARC(block,edge+C_OVER,j) = backgr
	}

	# Now fill up weight block.
	do j = 1, C_OVER {
	    call amovkr (1., ARC(wblock,C_OVER+1,j), bsize-2*C_OVER)
	    call amovkr (0., ARC(wblock,1,j), C_OVER)
	    call amovkr (0., ARC(wblock,bsize-C_OVER+1,j), C_OVER)
	    k = bsize-C_OVER+j
	    call amovkr (1., ARC(wblock,C_OVER+1,k), bsize-2*C_OVER)
	    call amovkr (0., ARC(wblock,1,k), C_OVER)
	    call amovkr (0., ARC(wblock,bsize-C_OVER+1,k), C_OVER)
	}
	do j = C_OVER+1, bsize-C_OVER {
	    call amovkr (0., ARC(wblock,C_OVER+1,j), bsize-2*C_OVER)
	    call amovkr (1., ARC(wblock,1,j), C_OVER)
	    call amovkr (1., ARC(wblock,bsize-C_OVER+1,j), C_OVER)
	    ARC(wblock,bsize/2,j) = 1.
	}
	call amovkr (1., ARC(wblock,1, bsize/2), bsize)

	# Initialize surfit.
	call isinit (sf, SF_SPLINE3, C_DEG, C_DEG, YES, bsize, bsize)

	# Fit line by line. 
	do j = 1, bsize
	    call islfit (sf, LINEI(col,1), j, ARC(block,1,j), ARC(wblock,1,j),
	                 bsize, SF_USER, i)

	# Solve for surface.
	call issolve (sf, LINEI(lin,1), bsize, i)

	# Fill up block with fitted values.
	do j = 1, bsize {
	    do i = 1, bsize
	        ARC(block,i,j) = iseval (sf, real(i), real(j))
	}

	call isfree (sf)

	# Write back lower left corner.
	out = imps2r (image, 1, edge, 1, edge)
	do j = 1, edge {
	   do i = 1, edge 
	       ARE(out,i,j) = ARC(block,i+edge+C_OVER,j+edge+C_OVER)
	}
	call imflush (image)

	# Write back upper left corner.
	out = imps2r (image, 1, edge, sizey-edge+1, sizey)
	do j = 1, edge {
	   do i = 1, edge 
	       ARE(out,i,j) = ARC(block,i+edge+C_OVER,j+C_OVER)
	}
	call imflush (image)

	# Lower right corner.
	out = imps2r (image, sizex-edge+1, sizex, 1, edge)
	do j = 1, edge {
	   do i = 1, edge 
	       ARE(out,i,j) = ARC(block,i+C_OVER,j+edge+C_OVER)
	}
	call imflush (image)

	# Upper right corner.
	out = imps2r (image, sizex-edge+1, sizex, sizey-edge+1, sizey)
	do j = 1, edge {
	   do i = 1, edge 
	       ARE(out,i,j) = ARC(block,i+C_OVER,j+C_OVER)
	}
	call imflush (image)

	call sfree (sp)
end




# ED_FILTER  --  Apply low-pass filter to edges. This removes the high frequency
#                power between lines (columns) left by the 1-D spline smoothing.
#                Filter kernel is Gaussian with variable sigma. Sigma increases 
#                linearly with distance of line/column from image edge, from 
#                MIN_FILT up to MAX_FILT. 

procedure ed_filter (image, sizex, sizey, edge)

pointer	image
int	sizex, sizey
int	edge

pointer	in, out				# buffer pointers
pointer	linei, lineo			# auxiliary storage
pointer	sp
int	i,j
real	sigma				# filter sigma

pointer imgs2r(), imps2r()

begin
	# Lower edge
	in  = imgs2r (image, 1, sizex, 1, edge)
	out = imps2r (image, 1, sizex, 1, edge)
	do j = 1, edge {
	    sigma = MAX_FILT - real(j-1)/real(edge-1) * (MAX_FILT-MIN_FILT)
	    call ed_convolve (AR(in,1,j), AR(out,1,j), sizex, edge, sigma)
	}

	# Upper edge
	in  = imgs2r (image, 1, sizex, sizey-edge+1, sizey)
	out = imps2r (image, 1, sizex, sizey-edge+1, sizey)
	do j = 1, edge {
	    sigma = real(j-1)/real(edge-1) * (MAX_FILT-MIN_FILT) + MIN_FILT
	    call ed_convolve (AR(in,1,j), AR(out,1,j), sizex, edge, sigma)
	}

	# Alloc areas for column storage
	call smark (sp)
	call salloc (linei, sizey, TY_REAL)
	call salloc (lineo, sizey, TY_REAL)

	# Left edge
	in  = imgs2r (image, 1, edge, 1, sizey)
	out = imps2r (image, 1, edge, 1, sizey)
	do i = 1, edge {
	    do j = 1, sizey
	        LINE(linei,j) = ARE(in,i,j)
	    sigma = MAX_FILT - real(i-1)/real(edge-1) * (MAX_FILT-MIN_FILT)
	    call ed_convolve (LINE(linei,1), LINE(lineo,1), sizey, edge, sigma)
	    do j = 1, sizey
	        ARE(out,i,j) = LINE(lineo,j) 
	}

	# Right edge
	in  = imgs2r (image, sizex-edge+1, sizex, 1, sizey)
	out = imps2r (image, sizex-edge+1, sizex, 1, sizey)
	do i = 1, edge {
	    do j = 1, sizey
	        LINE(linei,j) = ARE(in,i,j)
	    sigma = real(i-1)/real(edge-1) * (MAX_FILT-MIN_FILT) + MIN_FILT
	    call ed_convolve (LINE(linei,1), LINE(lineo,1), sizey, edge, sigma)
	    do j = 1, sizey
	        ARE(out,i,j) = LINE(lineo,j) 
	}

	call sfree (sp)
end




# ED_CONVOLVE  --  Low-pass filtering by a 1-D convolution with Gaussian kernel.

procedure ed_convolve (in, out, size, edge, sigma)

real	in[ARB]					# input vector
real	out[ARB]				# output vector
int	size					# vector size
int	edge					# edge size
real	sigma					# Gaussian sigma

pointer	sp, kernel
int	knpix, bx, i
real	norm

real	asumr()

begin
	# Compute kernel size; enforce odd size.
	knpix = int (2.5 * MAX_FILT)
	if (mod(knpix,2) == 0)
	    knpix = knpix + 1

	# Alloc and fill up kernel vector.
	call smark (sp)
	call salloc (kernel, knpix, TY_REAL)
	bx = knpix / 2
	do i = 1, knpix
	    LINE(kernel,i) = exp(-0.5/sigma*(real(i-bx))**2)
	norm = asumr (LINE(kernel,1), knpix)
	call adivkr (LINE(kernel,1), norm, LINE(kernel,1), knpix)
	
	# Filter.
	call amovkr (0., out[edge+1], size-2*edge)
	call acnvr  (in[edge+1-bx], out[edge], size-2*edge+1, LINE(kernel,1), knpix)
	call amovr  (in[1], out[1], edge)
	call amovr  (in[size-edge+1], out[size-edge+1], edge)

	call sfree (sp)
end



