include	<imhdr.h>
define	TOLERANCE	0.5

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# STARCENTER -- Compute centroid of star in box
#
# B.Simon	07-Jun-90	Original

procedure starcenter (im, xlog, ylog, box, maxtrip, ntrip, xstar, ystar)

pointer	im		# i: Image descriptor
real	xlog		# i: Approximate x position
real	ylog		# i: Approximate y position
int	box		# i: Search box size
int	maxtrip		# i: Maximum number of iterations
int	ntrip		# i: Number of iterations performed
real	xstar		# o: Centroided x position
real	ystar		# o: Centroided y position
#--
int	boxsize, ncol, nrow, halfbox
int	x1, x2, y1, y2, nx, ny, ix, iy
pointer	sp, xvec, yvec, buffer, bufptr
real	xmid, ymid, sum1, sum2, backgnd, weight

pointer	imgs2r()
real	amedr()

begin

	# Force box size to be odd

	if (mod (box, 2) == 0) {
	    boxsize = box + 1
	} else {
	    boxsize = box
	}

	ncol = IM_LEN(im,1)
	nrow = IM_LEN(im,2)

	xstar = xlog
	ystar = ylog
	halfbox = (boxsize - 1)/ 2

	for (ntrip = 1; ntrip <= maxtrip; ntrip = ntrip + 1) {

	    # Exit loop if star position located within tolerance

	    if (abs (xmid - xstar) < TOLERANCE &&
		abs (ymid - ystar) < TOLERANCE &&
		abs (xmid - xlog) < real(halfbox) &&
		abs (ymid - ylog) < real(halfbox)    )
		break

	    # Center box and compute mean position

	    xmid = xstar
	    ymid = ystar

	    # Get region containg the box from the image

	    x1 =  max (xmid - halfbox, 1.0) + 0.5
	    x2 =  min (xmid + halfbox, real(ncol)) + 0.5
	    y1 =  max (ymid - halfbox, 1.0) + 0.5
	    y2 =  min (ymid + halfbox, real(nrow)) + 0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    buffer = imgs2r (im, x1, x2, y1, y2)
	    backgnd = amedr (Memr[buffer], nx*ny)

	    call smark (sp)
	    call salloc (xvec, nx, TY_REAL)
	    call salloc (yvec, ny, TY_REAL)
	    bufptr = buffer

	    call aclrr (Memr[xvec], nx)
	    call aclrr (Memr[yvec], ny)

	    # Sum over rows and columns

	    do ix = 1, nx {
		do iy = 1, ny {
		    Memr[xvec+ix-1] = Memr[xvec+ix-1] + Memr[bufptr]
		    Memr[yvec+iy-1] = Memr[yvec+iy-1] + Memr[bufptr]
		    bufptr = bufptr + 1
		}
	    }

	    # Compute the star's mean position in x

	    sum1 = 0.0
	    sum2 = 0.0
	    do ix = 1, nx {
		weight = Memr[xvec+ix-1] - backgnd * ny
		if (weight > 0.0) {
		    sum1 = sum1 + (ix - 1) * weight
		    sum2 = sum2 + weight
		}
	    }
	    xstar = x1 + sum1 / sum2
	    
	    # Compute the star's mean position in y

	    sum1 = 0.0
	    sum2 = 0.0
	    do iy = 1, ny {
		weight = Memr[yvec+iy-1] - backgnd * nx
		if (weight > 0.0) {
		    sum1 = sum1 + (iy - 1) * weight
		    sum2 = sum2 + weight
		}
	    }
	    ystar = y1 + sum1 / sum2
	    
	    call sfree (sp)

	}

end
