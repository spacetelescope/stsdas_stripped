#  INTRP_STATE -- state vector interpolation for a specified epoch
#
#  Description:
#  ------------
#  Using Lagrange's formula of polynomial interpolation, obtain the state
#  vector of any epoch from the input state vector table.
#  The input array of times must be monotonically increasing, but they
#  don't have to be uniformly spaced.
#
#  Date 	Author 		Description
#  ----		------		-----------
#  28-Feb-1988  J.-C. Hsu       original, vxyzin.for
#  17-Apr-1990	J.-C. Hsu	rewrite in SPP
#  10-Jun-2003  Phil Hodge	include time in calling sequence, instead of
#				computing time from first value and increment
#-------------------------------------------------------------------------------

procedure intrp_state (epoch, time, x, y, z, xyz, npts, nlag)

double	epoch		# i: desired epoch
double	time[npts]	# i: times (MJD) corresponding to x, y, z
double	x[npts], y[npts], z[npts]	# i: state vectors
double	xyz[3]		# o: state vector of the desired epoch
int	npts		# i: number of entries of the input state vectors
int	nlag		# i: number of points used in Lagrange's
			#        polynomial interpolation formula

int	icenter, istart, istop
int	i1, i2		# indexes for searching for epoch in time array
double	dxyz[3]		# error of xyz (ignored)
pointer	sp, c, d	# work space for vpolin
#------------------------------------------------------------------------------
begin

	# allocate space for the working arrays
	call smark (sp)
	call salloc (c, nlag, TY_DOUBLE)
	call salloc (d, nlag, TY_DOUBLE)

	# find the array index of the desired epoch (binary search)
	i1 = 1
	i2 = npts
	while (i2 - i1 > 1) {
	    icenter = (i1 + i2) / 2
	    if (epoch > time[icenter])
		i1 = icenter
	    else
		i2 = icenter
	}
	icenter = i1

        istart = icenter - nlag / 2
        istop = istart + nlag - 1

	# make sure the starting and stopping points are within limits
        if (istart < 1 || istop > npts) {
	    call eprintf ("epoch = %.4f; ephemeris range = %.4f to %.4f\n")
		call pargd (epoch)
		call pargd (time[1])
		call pargd (time[npts])
	    call error (1, "input epoch is out of ephemeris range")
	}

	# perform the interpolation
        call vpolin (time[istart], x[istart], nlag, Memd[c], Memd[d], epoch,
			xyz[1], dxyz[1])
        call vpolin (time[istart], y[istart], nlag, Memd[c], Memd[d], epoch,
			xyz[2], dxyz[2])
        call vpolin (time[istart], z[istart], nlag, Memd[c], Memd[d], epoch,
			xyz[3], dxyz[3])

	call sfree (sp)
end
