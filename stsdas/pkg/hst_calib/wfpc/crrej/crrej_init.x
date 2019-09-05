include <imhdr.h>
include <imio.h>
include <mach.h>
include	"crrej.h"

#  crrej_init -- get the initial average pixel values
#
#  Description:
#  ------------
#  Get the initial average according to the specified scheme
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#  29-Apr-1998  J.-C. Hsu		include DQ
#------------------------------------------------------------------------------
procedure crrej_init (ipin, ipmask, nfiles, nmasks, par, dim_x, dim_y, 
			init, readout, gain, scale, efac, skyval, 
			ave, avevar,   work, work2)

# inputs:
pointer	ipin[ARB]		# input image header pointers
pointer	ipmask[ARB]		# input mask header pointers
int	nfiles			# number of input images
int	nmasks			# number of input masks
pointer par             	# the par structure pointer
int	dim_x, dim_y		# sizes of the input images
int	init			# scheme of obtaining the initial average
real	readout, gain, scale	# noise parameters
real	skyval[ARB]		# background levels
real	efac[ARB]		# exposure scaling factors

# output:
real	ave[dim_x, dim_y]	# initial average value, this is scaled
				# to the exposure time of the longest exposure
real	avevar[dim_x, dim_y]	# initial value's variance

# input but only used locally:
real	work[nfiles, dim_x]	# working array
real	work2[nfiles, dim_x]	# working array

# local:
real	rog2, exp2[MAX_FILES], raw, raw0, r
pointer	buf, buf2
int	i, j, k, n
int	dum
pointer	sp, npts, vn[MAX_FILES], vm[MAX_FILES]
short	dqpat, szero
bool    readmask[MAX_FILES]

int	imgnlr()
int	imgnls()
short   ands()
#==============================================================================
begin
	rog2 = readout ** 2
	dqpat = short (BADBITS(par))
        szero = short (0)

        if (IS_INDEFR(LOWER(par))) LOWER(par) = -MAX_REAL
        if (IS_INDEFR(UPPER(par))) UPPER(par) =  MAX_REAL

	# set up pointers
	call smark (sp)
	call malloc (npts, dim_x, TY_INT)

	do n = 1, nfiles {
            if (ipmask[n] == NULL) readmask[n] = false
            else readmask[n] = nmasks > 0 && IM_ACMODE(ipmask[n]) != NEW_COPY

	    exp2[n] = efac[n] ** 2

	    call salloc (vn[n], IM_MAXDIM, TY_LONG)
	    call salloc (vm[n], IM_MAXDIM, TY_LONG)

	    # initialize pointers
	    call amovkl (long(1), Meml[vn[n]], IM_MAXDIM)
	    call amovkl (long(1), Meml[vm[n]], IM_MAXDIM)
	}

	if (init == MEDIAN) 
	    r = 0.5
	else if (init == MINIMUM) 
	    r = 0.

	# use the stack median or minimum to construct the initial image
	# one row at a time
	do j = 1, dim_y {
	    do i = 1, dim_x 
		Memi[npts+i-1] = 0

	    # build the stack for each row
	    do n = 1, nfiles {
	    	k = imgnlr (ipin[n], buf, Meml[vn[n]])
                if (readmask[n]) k = imgnls (ipmask[n], buf2, Meml[vm[n]])
		do i = 1, dim_x {
		    raw = Memr[buf+i-1]
                    if (raw < LOWER(par) || raw > UPPER(par))
                        next
                    if (readmask[n]) {
                        if (ands(Mems[buf2+i-1], dqpat) != szero)
                            next
                    }
		    Memi[npts+i-1] = Memi[npts+i-1]+1
		    work[Memi[npts+i-1],i] = (raw - skyval[n]) / efac[n]
		    raw0 = max(raw, 0.)
		    work2[Memi[npts+i-1],i] = (rog2 + raw0/gain + 
						(scale*raw0)**2) / exp2[n]
		}
	    }

	    # examine each pixel stack
	    do i = 1, dim_x {
		dum = Memi[npts+i-1]
		if (dum == 0) {
		    ave[i,j] = LOWER(par)
		    avevar[i,j] = 0.
		} else {
		    call piksr2 (work[1,i], work2[1,i], dum)
		    k = max (1, int(r*(dum-1)+1.))
		    ave[i,j] = work[k,i]
		    avevar[i,j] = work2[k,i]
		}
	    }
	}

	# free memories
	call mfree (npts, TY_INT)
	call sfree (sp)
end
