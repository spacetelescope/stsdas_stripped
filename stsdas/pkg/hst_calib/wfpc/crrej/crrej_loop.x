include <imhdr.h>
include <imio.h>
include <mach.h>
include	"crrej.h"

define	OK	0
define	HIT	8
define	SPILL	2
define	EXCLUDE	4

#  crrej_loop -- Perform cosmic ray rejection
#
#  Description:
#  ------------
#  Iterate the rejection using different sigma sizes.  Also reject pixels 
#  next to the cosmic ray hit.
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#  11-Dec-1995  J.-C. Hsu		enhance for individual mask output
#------------------------------------------------------------------------------
procedure crrej_loop (ipin, ipmask, nfiles, nmasks, par, niter, init,
			dim_x, dim_y, sigma, readout, gain, scale, 
			efac, skyval, ave, avevar, efacsum)

# inputs:
pointer	ipin[ARB]	# input image pointers
pointer	ipmask[ARB]	# mask pointers
int	nfiles		# number of input images
int	nmasks		# number of masks
pointer	par		# the par structure pointer
int	niter		# number of rejection iterations
int	init		# initial guess scheme
int	dim_x, dim_y
real	sigma[ARB]
real	readout, gain, scale		# noise parameters
real	efac[ARB], skyval[ARB]		# exposure factors and sky levels
real	ave[dim_x, dim_y]		# average value
real	avevar[dim_x, dim_y]		# average value's variance
real	efacsum[dim_x, dim_y]		# sum of exposure factors

# locals:
pointer	pic, mask, thresh
pointer	sum, tmp, buf, buf2
pointer	sp, v, v2
int	width
int	i, j, k, n, indx, jndx
int	iter
int	ii, jj, j2
real	rog2, sig2, psig2, rej2, exp2
real	val
short	crflag, dqpat, szero
bool	readmask

int	imgnlr()
int	imgnls()
int	impnls()
short	ands()
short	ors()
#==============================================================================
begin
	crflag = short (CRVAL(par))
	dqpat = short (BADBITS(par))
	szero = short (0)

	if (IS_INDEFR(LOWER(par))) LOWER(par) = -MAX_REAL
	if (IS_INDEFR(UPPER(par))) UPPER(par) =  MAX_REAL

	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call malloc (pic, dim_x*dim_y, TY_REAL)
	call malloc (thresh, dim_x*dim_y, TY_REAL)
	call malloc (sum, dim_x*dim_y, TY_REAL)
	call malloc (mask, dim_x*dim_y, TY_SHORT)
	
	# readout in DN
	rog2 = readout**2
	rej2 = REJ(par)**2
	psig2 = PSIGMA(par)**2
	if (PSIGMA(par) <= 0.)
	    psig2 = -1.

	width = int (REJ(par)+1.e-5)

	# start the rejection iteration
	do iter = 1, niter {
	    if (VERBOSE(par)) {
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
		    efacsum[i,j] = 0.
	    }
	
	    do n = 1, nfiles {
		exp2 = efac[n] ** 2

	        # reset the mask
	        do indx = 0, dim_x*dim_y-1
		    Mems[mask+indx] = OK

		call amovkl (long(1), Meml[v], IM_MAXDIM)
		call amovkl (long(1), Meml[v2], IM_MAXDIM)

                # calculate the threshold for each pixel
	        if (init == MINIMUM && iter == 1) {
                    do j = 1, dim_y {
                        indx = (j-1)*dim_x-1
                        do i = 1, dim_x {
		            Memr[thresh+indx+i] = sig2 * avevar[i,j]
		        }
                    }
	        } else {
                    do j = 1, dim_y {
                        indx = (j-1)*dim_x-1
                        do i = 1, dim_x {
                            val = max(ave[i,j]*efac[n]+skyval[n], 0.)
		            Memr[thresh+indx+i] = sig2 * ((rog2 + val/gain + 
					          (scale * val)**2)) / exp2 
			}
                    }
                }

		# read in data, subtract sky and scale by the exposure factor 
		if (ipmask[n] == NULL) readmask = false
		else readmask = nmasks > 0 && IM_ACMODE(ipmask[n]) != NEW_COPY 
		do j = 1, dim_y {
		    k = imgnlr (ipin[n], buf, Meml[v])
		    if (readmask) k = imgnls (ipmask[n], buf2, Meml[v2])

		    indx = (j-1)*dim_x-1
		    do i = 1, dim_x {

			# exclude points too high and too low
			# pixels marked with SPILL will not propagate the 
			# flagging to its neighbors
			if (Memr[buf+i-1] < LOWER(par) || Memr[buf+i-1] > UPPER(par))
			    Mems[mask+indx+i] = EXCLUDE
			if (readmask) {
			    if (ands(Mems[buf2+i-1], dqpat) != szero)
			        Mems[mask+indx+i] = EXCLUDE
			}
			Memr[pic+indx+i] = (Memr[buf+i-1] - skyval[n]) / efac[n]
# for debug
#if(i==355 && j==388) {
#if (n==1) {
#call printf("ave=%0.3g\n")
#call pargr(ave[i,j])
#}
#call printf("file=%d raw DN=%0.2f scaled DN/sec=%0.3g sigma=%0.3g\n")
#call pargi(n)
#call pargr(Memr[buf+i-1])
#call pargr(Memr[pic+indx+i])
#call pargr(sqrt(Memr[thresh+indx+i]/sig2))
#call flush(STDOUT)
#}
		    }
		}

		do j = 1, dim_y {
		    indx = (j-1)*dim_x-1
		    do i = 1, dim_x {

			# find the CR by using statistical rejection
			if ((Memr[pic+indx+i]-ave[i,j])**2 > 
				Memr[thresh+indx+i] && 
			     	Mems[mask+indx+i] != EXCLUDE) {
			    Mems[mask+indx+i] = HIT

			    # mark the surrounding pixels also as CR
		    	    #if (ave[i,j] <= UPPER(par)) {
			        do jj = j-width, j+width {
				    if (jj > dim_y || jj < 1) next
				    j2 = (jj-j)**2
				    jndx = (jj-1)*dim_x-1
				    do ii = i-width, i+width {
					if (real((ii-i)**2+j2) > rej2) next
				        if (ii > dim_x || ii < 1) next
				        if (Mems[mask+jndx+ii] == EXCLUDE) next

					if ((Memr[pic+jndx+ii]-ave[ii,jj])**2 <=
					    psig2*Memr[thresh+jndx+ii]) next
				        if (Mems[mask+jndx+ii] != HIT)
					    Mems[mask+jndx+ii] = SPILL
				    }
				}
			    #}
			}
		    }
		}

		# accumulate the total counts in each pixel
		do j = 1, dim_y {
		    indx = (j-1)*dim_x - 1
		    do i = 1, dim_x {
			if (Mems[mask+indx+i] == OK) {

			    # add the sky-subtracted but UN-scaled counts
			    Memr[sum+indx+i] = Memr[sum+indx+i] + 
					       Memr[pic+indx+i] * efac[n]
			    efacsum[i,j] = efacsum[i,j] + efac[n]
			}
		    }
		}

		# at the last iteration, write out individual masks
		if (iter == niter && nmasks > 0) {

		    # reset the line-counter
		    call amovkl (long(1), Meml[v], IM_MAXDIM)
		    call amovkl (long(1), Meml[v2], IM_MAXDIM)
		    do j = 1, dim_y {
		        k = imgnls (ipmask[n], tmp, Meml[v])
		        indx = (j-1)*dim_x-1
		        do i = 1, dim_x {

			    # All masked pixels have the same flag value and
			    # is combined with what was in the mask, if any.
			    if (Mems[mask+indx+i] == HIT || 
				    Mems[mask+indx+i] == SPILL) 
				Mems[tmp+i-1] = ors (Mems[tmp+i-1], crflag)
		        }
		        k = impnls (ipmask[n], buf2, Meml[v2])
			call amovs (Mems[tmp], Mems[buf2], dim_x)
		    }
	        }
	    }

	    # calculate the new average after the rejection
	    do j = 1, dim_y {
		do i = 1, dim_x {
		    indx = (j-1)*dim_x + (i-1)
		    if (efacsum[i,j] > 0.)
		        ave[i,j] = Memr[sum+indx] / efacsum[i,j]
		    else {
			if (iter == niter)
			    ave[i,j] = INDEF
		    }
		}
	    } 
	}

	# release memories
	call mfree (pic, TY_REAL)
	call mfree (thresh, TY_REAL)
	call mfree (sum, TY_REAL)
	call mfree (mask, TY_SHORT)
	call sfree (sp)
end
