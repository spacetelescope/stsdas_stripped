include <imhdr.h>

# IMEXTREME  --  Finds the extreme (maximum and minimum) pixels
#                in an image.
#
#
#  06/05/96 - Created (I.Busko)
#  03/14/97 - Added box center coordinates (IB)

procedure t_imextreme ()

pointer	im, line
char	name[SZ_FNAME]
int	i, j
int	imin, imax, jmin, jmax
int	xmin, ymin, xmax, ymax
real	box, xbox, ybox, max, min
bool	verbose

pointer	immap(), imgl2r()
real	clgetr()
bool	clgetb()

begin
	max = -1.0E38
	min = -max

	call clgstr ("image", name, SZ_FNAME)
	box     = clgetr ("boxsize")
	xbox    = clgetr ("x")
	ybox    = clgetr ("y")
	verbose = clgetb ("verbose")

	im = immap (name, READ_ONLY, 0)

	# Box size is INDEF, search in full image.
	if (IS_INDEF(box)) {
	    imin = 1
	    jmin = 1
	    imax = IM_LEN(im, 1)
	    jmax = IM_LEN(im, 2)

	# Box size is specified but center isn't.
	} else if (IS_INDEF(xbox) || IS_INDEF(ybox)) {
	    imin = (IM_LEN(im, 1) - int(box)) / 2
	    imax = (IM_LEN(im, 1) + int(box)) / 2
	    jmin = (IM_LEN(im, 2) - int(box)) / 2
	    jmax = (IM_LEN(im, 2) + int(box)) / 2

	# Box size AND center are specified.
	} else {
	    imin = xbox - int(box) / 2
	    imax = xbox + int(box) / 2
	    jmin = ybox - int(box) / 2
	    jmax = ybox + int(box) / 2
        }

	# Make sure box fits into image.
	if (imin < 1)             imin = 1
	if (jmin < 1)             jmin = 1
	if (imax > IM_LEN(im, 1)) imax = IM_LEN(im, 1)
	if (jmax > IM_LEN(im, 2)) jmax = IM_LEN(im, 2)

	# Do the search.
	do j = jmin, jmax {
	    line = imgl2r (im, j)
	    do i = imin, imax {
	        if (Memr[line+i-1] > max) {
	            max = Memr[line+i-1]
	            xmax = i
	            ymax = j
	        }
	        if (Memr[line+i-1] < min) {
	            min = Memr[line+i-1]
	            xmin = i
	            ymin = j
	        }
	    }
	}
	call imunmap (im)

	# Output.
	call clputr ("max",  max)
	call clputi ("xmax", xmax)
	call clputi ("ymax", ymax)
	call clputr ("min",  min)
	call clputi ("xmin", xmin)
	call clputi ("ymin", ymin)

	if (verbose) {
	    call printf ("Maximum = %g  at (%d,%d)\n")
	        call pargr (max)
	        call pargi (xmax)
	        call pargi (ymax)
	    call printf ("Minimum = %g  at (%d,%d)\n")
	        call pargr (min)
	        call pargi (xmin)
	        call pargi (ymin)
	}
end
