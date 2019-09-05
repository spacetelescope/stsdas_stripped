include <tbset.h>
include	"mka2d.h"

#  GET_ERR -- Read A-to-D errors from an SDAS table
#
#  Description:
#  ------------
#
#  Input table column names
#  ------------------------
#  "DETECTOR"			Detector number
#  "BIT"			Bit number
#  "ERROR"			bit error
#
#  Date		Author			Description
#  ----		------			-----------
#  11-Oct-1991	J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure get_err (errtable, errors, camera)

char	errtable[SZ_FNAME]	# input: error table name
real	errors[NBITS, NGROUP]	# output: A-to-D errors
char	camera[SZ_CAMERA]	# output: camera ID

int	detector[NBITS*NGROUP]
int	bit[NBITS*NGROUP]
real	err[NBITS*NGROUP]
int	idet[2*NGROUP]

int	npts		# number of rows in the table
int	i, j, j1, j2
int	offset
pointer	tp
pointer	sp, nullflag
pointer	colptr[10]

pointer	tbtopn()
int	tbpsta()
bool	streq()
#==============================================================================
begin

	tp = tbtopn (errtable, READ_ONLY, 0)
	npts = tbpsta (tp, TBL_NROWS)

	do i = 1, 2*NGROUP
	    idet[i] = 0

	# number of rows in the table must be equal to the number of bits times
	# the number of groups
	if (npts != NBITS * NGROUP)
	    call error (1, "error table has incorrect number of rows")

	call smark (sp)
	call salloc (nullflag, npts, TY_BOOL)

	call tbcfnd (tp, "DETECTOR", colptr[1], 1)
	call tbcfnd (tp, "BIT", colptr[2], 1)
	call tbcfnd (tp, "ERROR", colptr[3], 1)

	# read the data
	call tbcgti (tp, colptr[1], detector, Memb[nullflag], 1, npts)
	call tbcgti (tp, colptr[2], bit, Memb[nullflag], 1, npts)
	call tbcgtr (tp, colptr[3], err, Memb[nullflag], 1, npts)

	call tbtclo (tp)
	call sfree (sp)

	# sort out the errors according to the detectors
	do i = 1, npts {
	    if (detector[i] > 2*NGROUP || detector[i] < 1)
		call error (1, "illegal detector number in the error table")
	    idet[detector[i]] = idet[detector[i]] + 1
	}
	call strcpy ("WF", camera, SZ_CAMERA) 
	do i = 1, NGROUP {
	    if (idet[i] != NBITS) {
		call strcpy ("PC", camera, SZ_CAMERA) 
		do j = NGROUP+1, 2*NGROUP {
	    	    if (idet[j] != NBITS)
			call error (1, "incomplete error table")
		}
		break
	    }
	}

	# sort out the errors according to the bits
	offset = 0
	if (streq(camera, "PC"))
	    offset = NGROUP
	do i = 1, npts {
	    j2 = detector[i] - offset
	    j1 = nint(log(real(bit[i]))/log(2.)) + 1
	    errors[j1, j2] = err[i]
	}

	# check the errors, issue warning if any one is overtaking the next bit
	do i = 1, NGROUP {
	    do j = 1, NBITS {
		if (errors[j, i] > 2**(j-1) || -errors[j, i] > 2**(j-2)) {
		    call printf ("Warning: Large error at bit %d of detector %d\n")
			call pargi (2**(j-1))
			call pargi (i+offset)
		    break
		}
	    }
	}
end
