include <fset.h>
include <gset.h>
include <imhdr.h>

# T_MARKDQ -- Mark dots on the image in the image display

procedure t_markdq ()

pointer	image1, image2	               	# pointer to name of the image

pointer	sp, im1, im2, iw
int	frame, color[15], dotsize, wcs_status

int	clgeti()
pointer	immap (), imd_mapframe(), iw_open()

begin
	# Set standard output to flush on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)

	# Get cl parameters
	call clgstr ("image", Memc[image1], SZ_FNAME)
	im1 = immap (Memc[image1], READ_ONLY, 0)
	frame = clgeti ("frame")
	dotsize = clgeti ("pointsize")
	color[1] = clgeti ("col1")
	color[2] = clgeti ("col2")
	color[3] = clgeti ("col3")
	color[4] = clgeti ("col4")
	color[5] = clgeti ("col5")
	color[6] = clgeti ("col6")
	color[7] = clgeti ("col7")
	color[8] = clgeti ("col8")
	color[9] = clgeti ("col9")
	color[10] = clgeti ("col10")
	color[11] = clgeti ("col11")
	color[12] = clgeti ("col12")
	color[13] = clgeti ("col13")
	color[14] = clgeti ("col14")
	color[15] = clgeti ("col15")

	# Make sure dotsize is an odd number
	if (mod (dotsize, 2) == 0)
	    dotsize = dotsize + 1
	dotsize = dotsize / 2

	# Open the display frame as an image.
	im2 = imd_mapframe (frame, READ_WRITE, YES)
	iw = iw_open (im2, frame, Memc[image2], SZ_FNAME, wcs_status)

	# Mark the image frame.
	call mk_bmark (im1, im2, iw, color, dotsize)

	# Close up the file lists and free memory.
	call iw_close (iw)
	call imunmap (im1)
	call imunmap (im2)

	call sfree (sp)
end

# MK_BMARK -- Procedure to mark symbols in the frame buffer given a coordinate
# list and a mark type.

procedure mk_bmark (im1, im2, iw, color, dotsize)

pointer	im1	# image descriptor
pointer	im2	# frame image descriptor
pointer iw	# pointer to the wcs structure
int	color[11]
int	dotsize

int	ncols, nlines, x1, x2, y1, y2
int	i, j, k
int	nx, ny
real	fx, fy
pointer	line
int	mk_plimits()
pointer	imgl2s()
bool	ands()
short	val

begin

	nx = IM_LEN(im1,1)
	ny = IM_LEN(im1,2)

	ncols  = IM_LEN(im2,1)
	nlines = IM_LEN(im2,2)

	call malloc (line, nx, TY_SHORT)

	# Loop over the image lines.
	do j = 1, ny {

	    # Load the line of the DQ image
	    call amovs (Mems[imgl2s(im1,j)], Mems[line], nx)

	    # Loop through the pixels in this line
	    do i = 1, nx {

		# Is the DQ non-zero for this pixel?
		if (Mems[line+i-1] > 0) {

		    # Check to see which DQ flags are set
		    do k = 1, 11 {
		       val = 2**(k-1)
		       if (ands(Mems[line+i-1],val) && color[k] != 0) {
		      
			   # Get the display x and y coords.
			   call iw_im2fb (iw, real(i), real(j), fx, fy)
			   if (mk_plimits (fx, fy, dotsize, ncols, nlines,
					   x1, x2, y1, y2) == YES)

			   # Draw a flag
			   call mk_drawpt (im2, x1, x2, y1, y2, color[k])
		       }
		    }
		}
	    }
	}

	call imflush (im2)
	call mfree (line, TY_SHORT)

end


# MK_DRAWPT -- Procedure to draw a  point into the frame buffer.

procedure mk_drawpt (im, x1, x2, y1, y2, graylevel)

pointer	im		# pointer to the frame image
int	x1, x2		# column limits
int	y1, y2		# line limits
int	graylevel	# color of dot to be marked

int	i, npix
pointer	vp
pointer	imps2s()

begin
	npix = (x2 - x1 + 1) * (y2 - y1 + 1)
	vp = imps2s (im, x1, x2, y1, y2)
	do i = 1, npix
	    Mems[vp+i-1] = graylevel
end


# MK_PLIMITS -- Compute the extent of a dot.

int procedure mk_plimits (fx, fy, szdot, ncols, nlines, x1, x2, y1, y2)

real	fx, fy		# frame buffer coordinates of point
int	szdot		# size of a dot
int	ncols, nlines	# dimensions of the frame buffer
int	x1, x2		# column limits
int	y1, y2		# line limits

begin
	x1 = nint (fx) - szdot
	x2 = x1 + 2 * szdot
	if (x1 > ncols || x2 < 1)
	    return (NO)
	x1 = max (1, min (ncols, x1))
	x2 = min (ncols, max (1, x2))

	y1 = nint (fy) - szdot
	y2 = y1 + 2 * szdot 
	if (y1 > nlines || y2 < 1)
	    return (NO)
	y1 = max (1, min (nlines, y1))
	y2 = min (nlines, max (1, y2))

	return (YES)
end

