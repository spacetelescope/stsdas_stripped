define	OK	0
define	HOTPIX	1024
define	FAC	0.2

include	<tbset.h>

#  T_O1DRANGE -- read the STIS 1-D spectra table (3-D FITS table) and determine
#  the data range
#
#  Description:
#  ------------
#
#  Input table column names
#  ------------------------
#  "NELEM", "FLUX", "DQ"		Column names	
#
#  Input table parameters
#  ----------------------
#
#  Date		Author			Description
#  ----		------			-----------
#  11-Mar-1998	J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure t_o1drange ()

char    input[SZ_FNAME]
real	fluxmin, fluxmax

pointer	tp		# table pointer
int	nrows		# number of rows in the table

pointer	nelem		
pointer	flux, dq, iflux, idq
pointer	colptr[10]
int	i, j, imax, npts, nel

pointer tbtopn()
int	tbpsta()
#==============================================================================
begin

        # read input file name
        call clgstr ("input", input, SZ_FNAME)

	# open the table
        tp = tbtopn (input, READ_ONLY, 0)

	# find the following columns "NELEM", "FLUX", "DQ"
	call tbcfnd (tp, "NELEM", colptr[1], 1)
	call tbcfnd (tp, "FLUX", colptr[2], 1)
	call tbcfnd (tp, "DQ", colptr[3], 1)

	# how many rows are in the table?
        nrows = tbpsta (tp, TBL_NROWS)

	fluxmax = -INDEF
	fluxmin = INDEF
	imax = 1
	npts = 0

	call calloc (nelem, nrows, TY_INT)

	# compute total number of points
	do i = 1, nrows {
	    call tbegti (tp, colptr[1], i, Memi[nelem+i-1])
	    npts = npts + Memi[nelem+i-1]
	}

	# allocate array for flux and DQ
	call calloc (flux, npts, TY_REAL)
	call calloc (dq, npts, TY_SHORT)
	iflux = flux
	idq = dq
	
	# read flux and DQ from each cell
	do i = 1, nrows {
	    nel = Memi[nelem+i-1]
	    call tbagtr (tp, colptr[2], i, Memr[iflux], 1, nel)
	    call tbagts (tp, colptr[3], i, Mems[idq], 1, nel)
	    iflux = iflux + nel
	    idq = idq + nel
	}

	do i = 1, npts {
	    if (Mems[dq+i-1] == OK) {
		if (Memr[flux+i-1] < fluxmin)
		    fluxmin = Memr[flux+i-1]
	    }
	}

	# find the maximum, the neighboring pixels must not be too small 
	# compared with the maximum, this is to avoid hot pixels being
	# used as range delimiter while preserving emission lines
	do j = 1, max(100, npts/10) {
	    do i = 1, npts {
		if (Mems[dq+i-1] == OK) {
		    if (Memr[flux+i-1] > fluxmax) {
			fluxmax = Memr[flux+i-1]
			imax = i
		    }
		}
	    }

# for debug
#call printf ("%d\n")
#call pargi(j)
#call printf (" fluxmax = %0.3g imax = %d\n")
#call pargr(fluxmax)
#call pargi(imax)
#call printf (" (left) flux = %0.3g (right) flux = %0.3g\n")
#call pargr(Memr[flux+imax-2])
#call pargr(Memr[flux+imax])
#call printf (" (left) dq = %d (right) dq = %d\n")
#call pargs(Mems[dq+imax-2])
#call pargs(Mems[dq+imax])
#call flush (STDOUT)
	    if (fluxmax < 0.) fluxmax = 0.

	    if (imax > 1 && Mems[dq+imax-2] == OK) {
		if (Memr[flux+imax-2] > FAC*fluxmax)
		    break
	    }
	    if (imax < npts && Mems[dq+imax] == OK) {
		if (Memr[flux+imax] > FAC*fluxmax)
		    break
	    }

	    # mask the maximum as hot pixel, and reset the max value/index
	    Mems[dq+imax-1] = HOTPIX
	    fluxmax = -INDEF
	    imax = 1
	}

	# write the result to CL parameters
	call clputr ("fluxmin", fluxmin)
	call clputr ("fluxmax", fluxmax)

	call mfree (flux, TY_REAL)
	call mfree (dq, TY_SHORT)
	call mfree (nelem, TY_INT)

	call tbtclo (tp)
end
