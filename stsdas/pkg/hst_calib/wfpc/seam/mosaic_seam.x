include	"wmosaic.h"

#  mosaic_seam -- smooth the seams between WFPC's 4 chips
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Mar-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mosaic_seam (camera, outbuf, length, width, delta, npbg)

int	camera
real	outbuf[SZ_OUT, SZ_OUT]
int	length
int	width
real	delta		# discrimination level for bad pixels
int	npbg		# number of pixels used for background in each "stitch"

int	x0, y0
int	i, j, k, grp
int	j1, j2, jbad1, jbad2
int	hlen, hw
pointer	med, med1, med2
int	ipx, ipx1, ipx2
real	ave1, ave2		# median background values
real	ave
real	output[SZ_OUT]
#==============================================================================
begin
	hlen = length/2
	hw = width/2

	call malloc (med,  npbg*(2*hw+1), TY_REAL)
	call malloc (med1, npbg*(2*hw+1), TY_REAL)
	call malloc (med2, npbg*(2*hw+1), TY_REAL)

	# go through points which were along X axis of group 1 and 3
	do grp = 1, 3, 2 {

	    do i = 1, DIM_X {
	   	
		# determine the starting point of each 'stitch" which intersects
		# with the boundary
		call xborder (camera, i, grp, x0, y0)
		if (y0 > SZ_OUT || y0 < 1) next
		j1 = x0 - hlen
		j2 = x0 + hlen

		ipx = 0
		do k = y0-hw, y0+hw {
		    if (k > SZ_OUT || k < 1) next
  	            do j = j1, j1+npbg-1 {
		        if (j > SZ_OUT || j < 1) next
		    	ipx = ipx + 1
		    	Memr[med+ipx-1] = outbuf[j, k]
		    }
		}
		call median (Memr[med], ipx, ave)

		# compare count level of the rest of the row to that of the 
		# median of the first NPBG points.  When an abnormally high 
		# or low count is detected, mark it as the beginning of the 
		# section to be interpolated.
  	        do j = j1+npbg, j2 {
		    if (abs(outbuf[j, y0]-ave) > delta*ave) {
			break
		    }
		}
		jbad1 = j 

		# do the same from the other end of the "stitch"
		ipx = 0
		do k = y0-hw, y0+hw {
		    if (k > SZ_OUT || k < 1) next
  	            do j = j2-npbg+1, j2 {
		        if (j > SZ_OUT || j < 1) next
		    	ipx = ipx + 1
		    	Memr[med+ipx-1] = outbuf[j, k]
		    }
		}
		call median (Memr[med], ipx, ave)

		# search for the ending of the section to be interpolated
  	        do j = j2-npbg, j1, -1 {
		    if (abs(outbuf[j, y0]-ave) > delta*ave) {
			break
		    }
		}
		jbad2 = j 
		
		if (jbad1 <= jbad2) {
		    ipx1 = 0
		    ipx2 = 0
		    do k = y0-hw, y0+hw {
	    		if (k > SZ_OUT || k < 1) next
  	    		do j = jbad1-npbg, jbad1-1 {
	    		    if (j > SZ_OUT || j < 1) next
	    		    ipx1 = ipx1 + 1
	    		    Memr[med1+ipx1-1] = outbuf[j, k]
	    		}
  	    		do j = jbad2+1, jbad2+npbg {
	    		    if (j > SZ_OUT || j < 1) next
	    		    ipx2 = ipx2 + 1
	    		    Memr[med2+ipx2-1] = outbuf[j, k]
	    		}
		    }
		    call median (Memr[med1], ipx1, ave1)
		    call median (Memr[med2], ipx2, ave2)

		    # linearly interpolate the masked section from median 
		    # values of both ends
		    call mosaic_intrp (jbad1, jbad2, ave1, ave2, output)
		    do j = jbad1, jbad2
			outbuf[j, y0] = output[j]
		}
	    }
	}

	# go through points which were along X axis of group 2 and 4
	do grp = 2, 4, 2 {

	    do i = 1, DIM_X {
	   	
		# determine the starting point of each "stitch" which intersects
		# with the boundary
		call xborder (camera, i, grp, x0, y0)
		if (x0 > SZ_OUT || x0 < 1) next
		j1 = y0 - hlen
		j2 = y0 + hlen

		ipx = 0
		do k = x0-hw, x0+hw {
		    if (k > SZ_OUT || k < 1) next
  	            do j = j1, j1+npbg-1 {
		        if (j > SZ_OUT || j < 1) next
		    	ipx = ipx + 1
		    	Memr[med+ipx-1] = outbuf[k, j]
		    }
		}
		call median (Memr[med], ipx, ave)

		# compare count level of the rest of the row to the first 
		# NPBG points.  When an abnormally high or low count is detected,
 		# mark it as the beginning of the section to be interpolated.
  	        do j = j1+npbg, j2 {
		    if (abs(outbuf[x0, j]-ave) > delta*ave) {
			break
		    }
		}
		jbad1 = j 

		# do the same from the other end of the "stitch"
		ipx = 0
		do k = x0-hw, x0+hw {
		    if (k > SZ_OUT || k < 1) next
  	            do j = j2+npbg+1, j2 {
		        if (j > SZ_OUT || j < 1) next
		    	ipx = ipx + 1
		    	Memr[med+ipx-1] = outbuf[k, j]
		    }
		}
		call median (Memr[med], ipx, ave)

		# search for the ending of the section to be interpolated
  	        do j = j2-npbg, j1, -1 {
		    if (abs(outbuf[x0, j]-ave) > delta*ave) {
			break
		    }
		}
		jbad2 = j 
		
		if (jbad1 <= jbad2) {
		    ipx1 = 0
		    ipx2 = 0
		    do k = x0-hw, x0+hw {
	    		if (k > SZ_OUT || k < 1) next
  	    		do j = jbad1-npbg, jbad1-1 {
	    		    if (j > SZ_OUT || j < 1) next
	    		    ipx1 = ipx1 + 1
	    		    Memr[med1+ipx1-1] = outbuf[k, j]
	    		}
  	    		do j = jbad2+1, jbad2+npbg {
	    		    if (j > SZ_OUT || j < 1) next
	    		    ipx2 = ipx2 + 1
	    		    Memr[med2+ipx2-1] = outbuf[k, j]
	    		}
		    }
		    call median (Memr[med1], ipx1, ave1)
		    call median (Memr[med2], ipx2, ave2)

		    # linearly interpolate the masked section from median 
		    # values of both ends
		    call mosaic_intrp (jbad1, jbad2, ave1, ave2, output)
		    do j = jbad1, jbad2
			outbuf[x0, j] = output[j]
		}
	    }
	}

	call mfree (med, TY_REAL)
	call mfree (med1, TY_REAL)
	call mfree (med2, TY_REAL)
end

procedure xborder (camera, i, grp, x0, y0)

# Determine the coordinates of the border by using a straight line to represent
# the border.  These lines are determined from the K spots measurements.

int	camera
int	i, grp, x0, y0
		
begin
	# from the file ft01.fit, JC Hsu 7/2/93.
	if (camera == PC) {
	    if (grp == 1) {
	        y0 = 781 + i
	        x0 = nint(792.1239 - 0.01655933 * real(y0))
	    } else if (grp == 3) {
	        y0 = 782 - i
	        x0 = nint(792.7953 - 0.01544182 * real(y0))
	    } else if (grp == 2) {
	        x0 = 780 - i
	        y0 = nint(770.4383 + 0.01502905 * real(x0))
	    } else if (grp == 4) {
	        x0 = 779 + i
	        y0 = nint(770.1249 + 0.01372053 * real(x0))
	    }
	# from the file fu01.fit, JC Hsu 7/2/93.
	} else if (camera == WF) {
	    if (grp == 1) {
	        y0 = 782 + i
	        x0 = nint(795.0882 - 0.002353003 * real(y0))
	    } else if (grp == 3) {
	        y0 = 783 - i
	        x0 = nint(795.4633 - 0.001298388 * real(y0))
	    } else if (grp == 2) {
	        x0 = 794 - i
	        y0 = nint(781.092  + 0.002515194 * real(x0))
	    } else if (grp == 4) {
	        x0 = 793 + i
	        y0 = nint(780.4837 + 0.001614297 * real(x0))
	    }
	}
end

procedure mosaic_intrp (jbad1, jbad2, ave1, ave2, output)

int	jbad1, jbad2
real	ave1, ave2		# median background values
real	output[ARB]

int	i
real	step

begin
	step = (ave2 - ave1) / real(jbad2-jbad1+2)

	do i = jbad1, jbad2 {
	    output[i] = ave1 + real(i-jbad1+1) * step
	}
end

procedure median (arr, n, med)

real	arr[ARB]		# input/output: sample array, sorted into 
				# ascending order on output 
int	n			# number of samples
real	med			# median of the array arr

int	halfn

begin
	call piksrt (arr, n)
	halfn = n/2
	
	# even case
	if (halfn*2 == n)
	    med = (arr[halfn] + arr[halfn+1]) / 2.

	# odd case
	else
	    med = arr[halfn+1]
end
