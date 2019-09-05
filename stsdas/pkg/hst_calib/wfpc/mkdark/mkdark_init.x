include <imhdr.h>
include	"mkdark.h"

#  mkdark_init -- get the initial average pixel values
#
#  Description:
#  ------------
#  Get the initial average according to the specified scheme
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mkdark_init (ipin, ipest, nfiles, dim_x, dim_y, init, minval, ave, 
			work)

pointer	ipin[MAX_FILES]		# input: input image header pointers
pointer	ipest			# input: initial image header pointer
int	nfiles			# input: number of input images
int	dim_x, dim_y		# input: sizes of the input images
int	init			# input: scheme of obtaining the initial average
real	minval			# input: minimum allowed DN value
real	ave[dim_x, dim_y]	# output: initial average value
real	work[nfiles, dim_x]	# input: working array


pointer	buf
int	i, j, k, n
int	dum
pointer	sp, npts, v, vn[MAX_FILES]

int	imgnlr()
#==============================================================================
begin
	# set up pointers
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	do n = 1, nfiles
	    call salloc (vn[n], IM_MAXDIM, TY_LONG)
	call malloc (npts, dim_x, TY_INT)

	# initialize pointers
	call amovkl (long(1), Meml[v], IM_MAXDIM)
	do n = 1, nfiles
	    call amovkl (long(1), Meml[vn[n]], IM_MAXDIM)
	   
	# use an input image as the initial average
	if (init == IMAGE) {

	    # read in data 
	    j = 0
	    while (imgnlr (ipest, buf, Meml[v]) != EOF) {
		j = j + 1
		do i = 1, dim_x
		    ave[i,j] = Memr[buf+i-1]
	    }

	# use the stack minimum/median to construct the initial average
	} else {
	    do j = 1, dim_y {
		do i = 1, dim_x 
		    Memi[npts+i-1] = 0

	    	do n = 1, nfiles {
	    	    k = imgnlr (ipin[n], buf, Meml[vn[n]])
		    do i = 1, dim_x {
			if (Memr[buf+i-1] >= minval) {
			    Memi[npts+i-1] = Memi[npts+i-1]+1
			    work[Memi[npts+i-1],i] = Memr[buf+i-1]
			}
		    }
		}
		do i = 1, dim_x {
		    dum = Memi[npts+i-1]
		    if (dum == 0)
			ave[i,j] = minval
		    else {
			call piksrt (work[1,i], dum)
			if (init == MINIMUM)
			    ave[i,j] = work[1,i]
			else if (init == MEDIAN) {
			    if ((dum/2)*2 == dum) 
				ave[i,j] = (work[dum/2,i] + work[dum/2+1,i])/2.
			    else
				ave[i,j] = work[dum/2+1,i]
			}
		    }
		}
	    }
	}
	call mfree (npts, TY_INT)
	call sfree (sp)
end
