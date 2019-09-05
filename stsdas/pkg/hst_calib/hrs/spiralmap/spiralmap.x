include <imio.h>
include <imhdr.h>

define 	PIX2	$1[$2 + ($4 - 1) + ($5 - 1) * $3]

# SPIRALMAP -- Construct HRS spiral-search map 
#
# Steve Hulbert, Aug90

procedure t_spiralmap ()

char	input[SZ_FNAME]			# input image
char	output[SZ_FNAME]                # output image
int	col1				# number of columns to trim on left
int	col2				# number of columns to trim on right
int	row1				# number of rows to trim on bottom
int	row2				# number of rows to trim on top  
int	overlap				# number of pixels to overlap adjacent
					# individual frames

pointer	im_in, im_out, im_temp
pointer	temp
char	tempfile[SZ_FNAME]
real	datamax, datamin
int	groups, istat, i, j, k, l
int	vlen, hlen, naxis
int	h0, h1, v0, v1
int	vlo, hlo
int	voff[25], hoff[25]
int	vtrim, htrim, vside, hside
int	grside

int 	clgeti(), imgeti(), immap()
int	imps2r(), imgl2r(), impl2r()

# vertical & horizontal incremental steps of individual frames in spiral search
data	hoff /0, 1, 0,-1,-1, 0, 0, 1, 1, 1, 0, 0, 0,-1,-1,-1,-1, 
	        0, 0, 0, 0, 1, 1, 1, 1/
data	voff /0, 0,-1, 0, 0, 1, 1, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 
	        1, 1, 1, 1, 0, 0, 0, 0/

begin

	# get input from the cl
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	col1 = clgeti ("left")
	col2 = clgeti ("right")
	row1 = clgeti ("bottom")
	row2 = clgeti ("top")
	overlap = clgeti ("overlap")

	# map images
	im_in  = immap (input, READ_ONLY, 0)
	im_out  = immap (output, NEW_COPY, im_in)
	call mktemp ("tmp$ssmap", tempfile, SZ_FNAME)
	im_temp = immap (tempfile, NEW_COPY, im_in)

	# get image info for input
	groups = imgeti (im_in, "GCOUNT")
	naxis = IM_NDIM (im_in)
	hlen = IM_LEN (im_in, 1)
	vlen = IM_LEN (im_in, 2)

	# number of pixels in single trimmed frame
	vtrim = vlen - (row1 + row2)
	htrim = hlen - (col1 + col2)
	if (vtrim < 1 || vtrim > vlen || htrim < 1 || htrim > hlen)
	    call error (0, "Trimmed individual frame has inproper size.")

	# number of individual frames on one side of full image
	if (groups == 1) {
	    grside = 1
	} else if (groups <= 9) {
	    grside = 3
	} else if (groups <= 25) {
	    grside = 5
	} else {
	    call error (0, "Too many groups in input image")
	}

	# number of pixels in full image
	vside = grside * vtrim + row1 + row2 - (grside - 1) * overlap
	hside = grside * htrim + col1 + col2 - (grside - 1) * overlap

	# set dimensions of output image
	IM_NDIM (im_temp) = 2 
	IM_LEN (im_temp, 1) = grside * hlen 
	IM_LEN (im_temp, 2) = grside * vlen
	IM_PIXTYPE (im_temp) = TY_REAL
	IM_NDIM (im_out) = 2 
	IM_LEN (im_out, 1) = hside 
	IM_LEN (im_out, 2) = vside 
	IM_PIXTYPE (im_out) = TY_REAL

	# bottom left-hand corner of individual frame in full frame
	vlo = grside / 2 * vlen + 1
	hlo = grside / 2 * hlen + 1

	# allocate working buffer
	call malloc (temp, grside * grside * vlen * hlen, TY_REAL)

	# read observation
	do i = 1, groups {

	    # point to a group
	    call gf_opengr (im_in, i, datamin, datamax, istat)

	    # increment coordinates of lower left-hand corner
	    vlo = vlo + voff[i] * vlen 
	    hlo = hlo + hoff[i] * hlen 
	    # read data 
	    do k = 1, vlen {
	        call amovr [Memr[imgl2r(im_in, k)], 
		PIX2[Memr, temp, grside * hlen, hlo, vlo + k - 1], 
		hlen] 
	    }
	}

	# dump intermediate image
	call amovr [Memr[temp], Memr[imps2r(im_temp, 1, grside * hlen, 
		1, grside * vlen)], grside * grside * vlen * hlen] 
	call imunmap (im_temp)

	# for each row
	do j = 1, grside * vlen {

	    # for each overlapping column region
	    do k = 1, grside - 1 {

	    	# skip trimmed regions and average overlapping region
	        h0 = col1 + (htrim - overlap) * k
	        do l = 1, overlap { 
	            h0 = h0 + l
	            h1 = h0 + overlap + col2 + col1 
		    PIX2[Memr, temp, grside * hlen, h0, j] =	
			    (PIX2[Memr, temp, grside * hlen, h0, j] +	
			    PIX2[Memr, temp, grside * hlen, h1, j]) / 2.0
		}

		# shift the rest of the image 
		do l = h0 + 1, grside * hlen {
	            h1 = l + overlap + col2 + col1 
		    if (h1 <= grside * hlen) {
		        PIX2[Memr, temp, grside * hlen, l, j] =	
		    	    PIX2[Memr, temp, grside * hlen, h1, j] 
		    }
		}
	    }
	}

	# for each column 
	do i = 1, grside * hlen {

	    # for each overlapping row region
	    do k = 1, grside - 1 {

	    	# skip trimmed regions and average overlapping region
	        v0 = row1 + (vtrim - overlap) * k 
	        do l = 1, overlap { 
	            v0 = v0 + l
	            v1 = v0 + row2 + row1 + overlap
		    PIX2[Memr, temp, grside * hlen, i, v0] =	
			    (PIX2[Memr, temp, grside * hlen, i, v0] +	
			    PIX2[Memr, temp, grside * hlen, i, v1]) / 2.0
		}

		do l = v0 + 1, grside * vlen {
	            v1 = l + row2 + row1 + overlap 
		    if (v1 <= grside * vlen) {
		        PIX2[Memr, temp, grside * hlen, i, l] =	
		    	    PIX2[Memr, temp, grside * hlen, i, v1] 
		    }
		}
	    }
	}

	do j = 1, vside {
	    call amovr [PIX2[Memr, temp, grside * hlen, 1, j], 
		Memr[impl2r(im_out, j)], hside] 
	}

	call mfree (temp, TY_REAL)

	call imunmap (im_in)
	call imunmap (im_out)
	
end
