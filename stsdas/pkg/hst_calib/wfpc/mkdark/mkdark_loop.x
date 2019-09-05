include <imhdr.h>
include	"mkdark.h"

define	OK	0
define	HIT	8
define	SPILL	2

#  mkdark_loop -- Perform cosmic ray rejection
#
#  Description:
#  ------------
#  Iterate the rejection using different sigma sizes.  Also reject pixels 
#  next to the cosmic ray hit.
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mkdark_loop (ipin, nfiles, rej, width, niter, dim_x, dim_y, 
			sigma, psigma, readout, gain, scale, hotthresh,
			ave, npts, minval, fillval, verbose)

pointer	ipin[MAX_FILES]	# input: input image header pointers
int	nfiles		# input: number of input images
real	rej		# input: the radius to which pixels adjacent to a
			#   (directly) rejected pixel should also be discarded
int	width
int	niter		# input: number of rejection iterations
int	dim_x, dim_y
real	sigma[*]
real	psigma
real	readout, gain, scale		# input: noise parameters
real	hotthresh
real	ave[dim_x, dim_y]		# input: average value
short	npts[dim_x, dim_y]		# output: number of good points 
real	minval
real	fillval
bool	verbose

pointer	pic, mask, thresh
pointer	sum, buf
pointer	sp, v
int	i, j, k, n, indx, jndx
int	iter
int	ii, jj, j2
real	rog2, sig2, psig2, rej2
real	val

int	imgnlr()
#==============================================================================
begin
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call malloc (pic, dim_x*dim_y, TY_REAL)
	call malloc (thresh, dim_x*dim_y, TY_REAL)
	call malloc (sum, dim_x*dim_y, TY_REAL)
	call malloc (mask, dim_x*dim_y, TY_SHORT)
	
	# readout in DN
	rog2 = readout**2
	rej2 = rej**2
	psig2 = psigma**2
	if (psigma == 0.)
	    psig2 = -1.

	# start the rejection iteration
	do iter = 1, niter {
	    if (verbose) {
		call printf ("iteration %d\n")
		    call pargi (iter)
		call flush (STDOUT)
	    }

	    sig2 = sigma[iter]**2

	    # reset the sum/file count arrays
	    do indx = 0, dim_x*dim_y-1 {
		Memr[sum+indx] = 0.
	    }
	    do j = 1, dim_y {
		do i = 1, dim_x
		    npts[i,j] = 0
	    }

	    # calculate the threshold for each pixel
	    do j = 1, dim_y {
		indx = (j-1)*dim_x-1
		do i = 1, dim_x {
		    val = max(ave[i,j], 0.)
		    Memr[thresh+indx+i] = sig2 * 
				    (rog2 + val/gain + (scale * val)**2)
		}
	    }

	    do n = 1, nfiles {

	        # reset the mask
	        do indx = 0, dim_x*dim_y-1
		    Mems[mask+indx] = OK
		call amovkl (long(1), Meml[v], IM_MAXDIM)

		# read in data 
		do j = 1, dim_y {
		    k = imgnlr (ipin[n], buf, Meml[v])
		    indx = (j-1)*dim_x-1
		    do i = 1, dim_x 
			Memr[pic+indx+i] = Memr[buf+i-1]
		}

		do j = 1, dim_y {
		    indx = (j-1)*dim_x-1
		    do i = 1, dim_x {

			# exclude deep negative DN's
			if (Memr[pic+indx+i] <= minval)
			    Mems[mask+indx+i] = SPILL

			# find the CR
			else if ((Memr[pic+indx+i]-ave[i,j])**2 > 
					Memr[thresh+indx+i]) {
			    Mems[mask+indx+i] = HIT

			    # mark the surrounding pixels also as CR
		    	    if (ave[i,j] <= hotthresh) {
			        do jj = j-width, j+width {
				    if (jj > dim_y || jj < 1) next
				    j2 = (jj-j)**2
				    jndx = (jj-1)*dim_x-1
				    do ii = i-width, i+width {
					if (real((ii-i)**2+j2) > rej2) next
				        if (ii > dim_x || ii < 1) next

					if ((Memr[pic+jndx+ii]-ave[ii,jj])**2 <=
					    psig2*Memr[thresh+jndx+ii]) next
				        if (Mems[mask+jndx+ii] != HIT)
					    Mems[mask+jndx+ii] = SPILL
				    }
				}
			    }
			}
		    }
		}

		# accumulate the sum
		do j = 1, dim_y {
		    indx = (j-1)*dim_x - 1
		    do i = 1, dim_x {
			if (Mems[mask+indx+i] == OK) {
			    Memr[sum+indx+i] = Memr[sum+indx+i] + 
					       Memr[pic+indx+i]
			    npts[i,j] = npts[i,j] + 1
			}
		    }
		}
	    }

	    # calculate the new average after the rejection
	    do j = 1, dim_y {
		do i = 1, dim_x {
		    indx = (j-1)*dim_x + (i-1)
		    if (npts[i,j] > 0)
		        ave[i,j] = Memr[sum+indx] / real(npts[i,j])
		    else {
			if (iter == niter && fillval != INDEF)
			    ave[i,j] = fillval
		    }
		}
	    } 
	}

	call mfree (pic, TY_REAL)
	call mfree (thresh, TY_REAL)
	call mfree (sum, TY_REAL)
	call mfree (mask, TY_SHORT)
	call sfree (sp)
end
