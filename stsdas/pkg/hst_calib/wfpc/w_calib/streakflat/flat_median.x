include "streakflat.h"

define	ITER	2
define	NSIGMA	2.

#  flat_median -- Calculate the median flat field of the streak flats
#  
#  Date		Author			Description
#  ----		------			-----------
#  20-Apr-1992  J.-C. Hsu		adapted from Jeff Hester's C code
#------------------------------------------------------------------------------

procedure flat_median (fdpic, fdmask, nfin, ngood, median)

pointer	fdpic[MAX_FILES], fdmask[MAX_FILES]	# input: input file descripters
int	nfin					# input: number of input files
int	ngood					# input: min good points needed
real	median[DIM_X, DIM_Y]			# output: median pixel value

double	sum, sumsq
real	ave, sigma, high, low
real	accum[DIM_X]
int	npts[DIM_X]
pointer	img[MAX_FILES], mask[MAX_FILES], cent
real	med, x
real	mean_ratio[MAX_FILES]
real	temp[MAX_FILES]
int	npix				# size of the input data
int	indx, ipx		
int	ncent, np
int	i, j, k, m, n, nf

pointer	imgs2r(), imgs2s()
#==============================================================================
begin
	npix = DIM_X*DIM_Y 

	do k = 1, nfin {
	    img[k] = imgs2r(fdpic[k], 1, DIM_X, 1, DIM_Y)
	    mask[k] = imgs2s(fdmask[k], 1, DIM_X, 1, DIM_Y)
	}

	# Step 1: calculate the average image
	#====================================
	# first, reset the average/sum array
	# note: the average array shares the same space as the median array
	do j = 1, DIM_Y {
	    do i = 1, DIM_X {
		accum[i] = 0.
		npts[i] = 0
	    }

	    do k = 1, nfin {
		do i = 1, DIM_X {
		    ipx = (j-1)*DIM_X+i-1
		    if (Mems[mask[k]+ipx] == OKVAL && 
				Memr[img[k]+ipx] != BADVAL) {
			accum[i] = accum[i] + Memr[img[k]+ipx]
			npts[i] = npts[i] + 1
		    }
		}
	    }
	    do i = 1, DIM_X {
		if (npts[i] == 0)
		    median[i,j] = BADVAL
		else
		    median[i,j] = accum[i] / real(npts[i])
	    }
	}

	# in only one input file, no need to go to steps 2 and 3
	if (nfin == 1) 
	    return

	# Step 2: calculate the mean ratio for each image
	#================================================

	np = (CENT_X2-CENT_X1+1)*(CENT_Y2-CENT_Y1+1)
	call malloc (cent, np, TY_REAL)
	do k = 1, nfin {
	    sum = 0.d0
	    sumsq = 0.d0
	    ncent = 0

	    do j = CENT_Y1, CENT_Y2 {
	    	do i = CENT_X1, CENT_X2 {
		    ipx = (j-1)*DIM_X+i-1
		    indx = (j-CENT_Y1)*(CENT_X2-CENT_X1+1)+(i-CENT_X1)
		    if (Mems[mask[k]+ipx] == OKVAL && 
			    Memr[img[k]+ipx] != BADVAL && 
			    median[i,j] != BADVAL) {
		        ncent = ncent + 1
		        x = Memr[img[k]+ipx] / median[i,j]
		        Memr[cent+indx] = x
		        sum = sum + x
		        sumsq = sumsq + x**2
		    } else
		        Memr[cent+indx] = BADVAL
	        }
	    }
	    do n = 1, ITER {
		ave = sum / double(ncent)
		sigma = sumsq/double(ncent)-ave**2
		sigma = sqrt(sigma)
		high = ave+NSIGMA*sigma
		low = ave-NSIGMA*sigma
		sum = 0.d0
		sumsq = 0.d0
		ncent = 0
	
	        do m = 1, np {
		    x = Memr[cent+m-1]
		    if (x != BADVAL && x < high && x > low) {
		        sum = sum + x
		    	sumsq = sumsq + x**2
		    	ncent = ncent + 1
		    } else
			Memr[cent+m-1] = BADVAL
		}
	    }
	    mean_ratio[k] = sum / double(ncent)
	}
	call mfree (cent, TY_REAL)

	# Step 3: determine the median value for each pixel
	#==================================================
	# since median determination needs all data in the memory, we only
	# read one line for all files and determin medians for each pixel
	# in this line.  We then do the same for successive lines till 
	# the whole image is done.
	do j = 1, DIM_Y {
	    do i = 1, DIM_X {
		nf = 0
		ipx = (j-1)*DIM_X+i-1
		do k = 1, nfin {
		    if (Mems[mask[k]+ipx] == OKVAL && 
				Memr[img[k]+ipx] != BADVAL) {
			nf = nf + 1
			temp[nf] = Memr[img[k]+ipx] / mean_ratio[k]
		    }
		}
		if (nf == 0 || nf < ngood)
		    med = BADVAL
		else if (nf == 1)
		    med = temp[1]
		else {
		    call piksrt (temp, nf)
		    med = temp[(nf+1)/2]
		}
		median[i,j] = med
	    }
	}
end
