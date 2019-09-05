define	SMALL_RADIUS	100	# Use Schertl's algorithm within this radius.

# encirc -- profile and encircled energy
# This works out the profile and encircled energy.
#
# Robert Jedrzejewski, May-1992  Subroutine created.
# Phil Hodge, 16-Jul-1992  Converted to SPP.
# Phil Hodge, 30-Oct-1992  npix_ee = min (npix_ee, npix)
# Phil Hodge, 13-May-1994  Change SMALL_RADIUS.

procedure encirc (pix, nx, ny, xc, yc, pixsize,
		nrmax, background, target, stddev,
		radius, profile, sigma,
		profile_u, profile_l,
		energy, npix, npix_ee, prof1)

real	pix[nx,ny]	# i: pixel data
int	nx, ny		# i: size of array
real	xc, yc		# i: location of center of PSF
real	pixsize		# i: pixel size in arcseconds per pixel
int	nrmax		# i: maximum size of arrays
real	background	# i: background value to be subtracted
real	target		# i: normalize by dividing by this
bool	stddev		# i: compute standard deviations?
real	radius[ARB]	# o: array of radius values in arcsec
real	profile[ARB]	# o: azimuthally averaged profile
real	sigma[ARB]	# o: standard deviation of azimuthally averaged profile
real	profile_u[ARB]	# o: profile + 1 standard deviation
real	profile_l[ARB]	# o: profile - 1 standard deviation
real	energy[ARB]	# o: encircled energy
int	npix		# o: actual size of arrays
int	npix_ee		# o: actual size of encircled-energy array
real	prof1		# o: non-normalized value of profile at first pixel
#--
real	max_prof	# value of profile array used for normalization
int	ix1, ix2, iy1, iy2	# distances from xc,yc to borders of section
int	i

begin
	# The encircled energy curve must stop at the nearest border
	# of the image section.  Add one because the radius goes from
	# zero to the maximum radius, and npix_ee is the number of pixels,
	# not the radius.
	# After we have found a value for npix (in ee_vrradp), we'll use
	# npix as an upper limit to npix_ee.
	ix1 = xc				# truncate to integer
	ix2 = nx - xc + 1.
	iy1 = yc
	iy2 = ny - yc + 1.
	npix_ee = min (ix1, ix2, iy1, iy2)

	# Compute radial profile and standard deviations.
	call ee_vrradp (pix, nx, ny, xc, yc,
		nrmax, background, stddev,
		profile, sigma, energy, npix)

	# npix_ee can't be larger than npix.
	npix_ee = min (npix_ee, npix)

	# Assign values to radius.
	do i = 1, npix
	    radius[i] = pixsize * (i-1)

	if (stddev) {
	    do i = 1, npix {
		profile_u[i] = profile[i] + sigma[i]
		profile_l[i] = profile[i] - sigma[i]
	    }
	} else {
	    do i = 1, npix {
		profile_u[i] = profile[i]
		profile_l[i] = profile[i]
	    }
	}

	do i = npix+1, nrmax {
	    radius[i] = 0.
	    profile_u[i] = 0.
	    profile_l[i] = 0.
	}

	# Normalize the profile(s) by dividing by the value at the center
	# (i.e. at the first pixel), unless it's zero.  Note that the value
	# we return really is profile[1], even if it is zero.
	# Normalize the encircled energy by dividing by the target counts
	# and converting to percent.
	max_prof = profile[1]
	prof1 = profile[1]
	if (max_prof == 0.d0)
	    max_prof = 1.
	do i = 1, npix {
	    profile[i] = profile[i] / max_prof
	    sigma[i] = sigma[i] / max_prof
	    profile_u[i] = profile_u[i] / max_prof
	    profile_l[i] = profile_l[i] / max_prof
	    energy[i] = energy[i] / target * 100.
	}
end

# ee_vrradp -- compute radial profile
# This subroutine computes an azimuthal average
# and the encircled energy of a 2-dim image.
# The original also allowed different scales in the X and Y directions.
#
# (c) D. Schertl, MPIfR Bonn  Original Fortran version.
# Phil Hodge, 24-Jul-1992  Converted to SPP; include standard deviation.

procedure ee_vrradp (pix, nx, ny, xc, yc,
		nrmax, background, stddev,
		profile, sigma, energy, npix)

real	pix[nx,ny]	# i: pixel data
int	nx, ny		# i: size of array
real	xc, yc		# i: location of center of PSF
int	nrmax		# i: max radius
real	background	# i: background value to be subtracted
bool	stddev		# i: compute standard deviations?
real	profile[ARB]	# o: azimuthally averaged profile
real	sigma[ARB]	# o: standard deviation of azimuthally averaged profile
real	energy[ARB]	# o: encircled energy
int	npix		# o: actual maximum radius
#--
pointer sp
pointer count		# scratch for number of pixels in annulus
pointer prof		# scratch for sums for profile
pointer ssq		# scratch for sum of squares
pointer w		# scratch for weights
double	w1, w2		# weights for two adjacent radii
double	dx, dy		# difference between pixel location and xc, yc
double	radius		# radial distance from xc, yc, plus one for indexing
double	value		# pixel value minus background; profile value
double	arg		# argument to square root
double	xn		# = n
int	n		# counts in an annulus
int	n_irad		# nearest integer to radius
int	irad		# integer part of radius
int	small_rad	# use fractional weights within this radius
int	i, j

begin
	call smark (sp)
	call salloc (count, nrmax, TY_INT)
	call salloc (prof, nrmax, TY_DOUBLE)
	call salloc (ssq, nrmax, TY_DOUBLE)
	call salloc (w, nrmax, TY_DOUBLE)

	do i = 1, nrmax {
	    profile[i] = 0.
	    sigma[i] = 0.
	    energy[i] = 0.
	    Memi[count+i-1] = 0
	    Memd[prof+i-1] = 0.d0
	    Memd[ssq+i-1] = 0.d0
	    Memd[w+i-1] = 0.d0
	}

	small_rad = min (SMALL_RADIUS, nrmax-1)

	npix = 0				# initial value
	do j = 1, ny {
	    do i = 1, nx {

		dx = i - xc
		dy = j - yc
		# Add one to the radius so arrays will be one indexed.
		radius = sqrt (dx**2 + dy**2) + 1.d0
		n_irad = nint (radius)		# nearest integer

		if (n_irad <= nrmax) {
		    npix = max (n_irad, npix)

		    value = pix[i,j] - background

		    if (n_irad <= small_rad) {

			# Use Schertl's algorithm with fractional weights.
			irad = radius		# truncate
			w1 = irad + 1.d0 - radius
			w2 = 1.d0 - w1
			Memd[prof+irad-1] = Memd[prof+irad-1] + w1 * value
			Memd[prof+irad]   = Memd[prof+irad]   + w2 * value
			Memd[w+irad-1] = Memd[w+irad-1] + w1
			Memd[w+irad]   = Memd[w+irad]   + w2
			Memi[count+irad-1] = Memi[count+irad-1] + 1
			Memi[count+irad]   = Memi[count+irad]   + 1
			if (stddev) {
			    Memd[ssq+irad-1] = Memd[ssq+irad-1] + w1 * value**2
			    Memd[ssq+irad]   = Memd[ssq+irad]   + w2 * value**2
			}

		    } else {

			# Unit weight.  Note:  use n_irad instead of irad.
			Memd[prof+n_irad-1] = Memd[prof+n_irad-1] + value
			Memd[w+n_irad-1] = Memd[w+n_irad-1] + 1.d0
			Memi[count+n_irad-1] = Memi[count+n_irad-1] + 1
			if (stddev)
			    Memd[ssq+n_irad-1] = Memd[ssq+n_irad-1] + value**2
		    }
		}
	    }
	}

	# Calculate encircled energy.  This works even for small radii
	# because w1 + w2 = 1 for each pixel that was included in the average.
	energy[1] = Memd[prof]
	do i = 2, npix
	    energy[i] = energy[i-1] + Memd[prof+i-1]

	# Take the average to get the profile and standard deviation.
	do i = 1, npix {

	    n = Memi[count+i-1]
	    if (n > 0) {
		value = Memd[prof+i-1] / Memd[w+i-1]
		profile[i] = value			# convert from double

		if (stddev && n > 1) {
		    xn = n
		    arg = (xn / (xn-1.d0)) *
			(Memd[ssq+i-1] / Memd[w+i-1] - value ** 2)
		    if (arg >= 0.d0)
			sigma[i] = sqrt (arg)
		}
	    }
	}

	call sfree (sp)
end
