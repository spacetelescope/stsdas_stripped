include <math/iminterp.h>
include "sa_defines.h"

# Sigma rejection for figuring average.
define	SIGMA			2.d0

#---------------------------------------------------------------------------
.help sa_comb Jan97 source
.ih
NAME
sa_comb -- Combine spectra while removing noise.
.ih
USAGE
call sa_comb (sdata, weight, delta, shift, goff, gran_mask, gran_data, 
              g_err, out_mask, out_data, o_err, len, slen, niter, ksize, 
              korder, n_shifts, glen, olen, rsfunc)
.ih
ARGUMENTS
.ls sdata (I: double[olen,n_shifts])
The input resampled data to be combined.
.le
.ls weight (I: double[olen,n_shifts])
The weights for the resampled data array.
.le
.ls delta (I: double)
The damping factor to apply to the differences between each iteration.
.le
.ls shift (I: double[n_shifts])
The shifts for each line of the input data array.
.le
.ls goff (I: double[n_shifts])
The granularity shifts for each line of the input data array.
.le
.ls gran_mask (I: double[glen])
The granularity weighting.  If values are 0, then that particular
pixel of the granularity is not included in any calculations.
.le
.ls gran_data (IO: double[glen])
On input, contains the initial granularity.  On output, contains the
newly calculated granularity.
.le
.ls g_err (O: double[glen])
The standard deviation of the mean, where the mean is the average
granularity calculated at each iteration.
.le
.ls out_mask (I: double[olen])
The output weighting.  If values are 0, then that particular pixel of
the output is not included in any calculations.
.le
.ls out_data (IO: double[olen])
On input, contains the initial guess at the output spectrum.  On
output, contains the calculated spectrum.
.le
.ls o_err (O: double[olen])
The standard deviation of the mean, where the mean is the average
spectrum calculated at each iteration.
.le
.ls len (I: int)
Length of each line of data.
.le
.ls slen (I: int[n_shifts])
Array to hold the actual lengths of the shifted data in each row of sdata.
.le
.ls niter (I: int)
Number of iterations to perform.
.le
.ls ksize (I: int)
Savitzky-Golay smoothing kernel size (full size)
.le
.ls korder (I: int)
Savitzky-Golay order of smoothing polynomial for smoothing kernel.
.le
.ls n_shifts (I: int)
The number of shifts.
.le
.ls glen (I: int)
The length of the granularity-related arrays.
.le
.ls olen (I: int)
Length of the output spectrum-related arrays.
.le
.ls rsfunc (I: int)
Function to be used for the resampling method.
.ih
.le
.ih
DISCUSSION
This routine uses a variation of an algorithm presented in the paper

.nf
	Banuolo,W. and Gies,D.,"Tomographic Separation of Composite
	Spectra: The Components of the O-Star Spectroscopic Binary AO
	Cassiopeiae", ApJ.,376:266-271,20July1991.
.fi

Specifically, the formula is:

.nf
        so = i + Soff[s]
        go = i + Goff[s]
	dX = (Sdata[so,s] * Weight[so,s]) - (Granularity[go] * Out[so])

where	Sdata[so,s]     = Resampled data of spectrum s at pixel so
        Weight[so,s]    = Weighting for Sdata of spectrum s at pixel so
	Granularity[go] = Current value of granularity at pixel go
	Out[so]	        = Current value of composite output at pixel so
	Goff[s]		= Granularity offset for spectrum s
	Soff[s]		= Output offset for spectrum s
.fi

Subsequently, the difference is added back to the output and
granularity, with optional smoothing of the granularity.  This process
is then repeated for the indicated number of times.

The algorithm has been updated to utilize the full precision of the derived
spectral shifts, as computed by poffsets, for the shifting/resampling of 
the individual spectra.  The original version of this task only accommodated 
integral pixel shifting/resampling.
.endhelp
#---------------------------------------------------------------------------
procedure sa_comb (sdata, weight, delta, shift, goff, gran_mask, gran_data, 
                   g_err, out_mask, out_data, o_err, len, slen, niter, 
                   ksize, korder, n_shifts, glen, olen, rsfunc)

# Passed variables.
double  sdata[olen, n_shifts]   # I:  The resampled spectral data to combine.
double  weight[olen, n_shifts]  # I:  Weight array corresponding to the sdata.
double	delta			# I:  Damping factor between iterations.
double	shift[n_shifts]		# I:  The shifts of the spectral data.
double	goff[n_shifts]		# I:  Shifts in the granularity/noise.
double	gran_mask[glen]		# I:  Mask of the granularity.
double	gran_data[glen]		# IO: The predicted/calculated granularity.
double	g_err[glen]		# O:  Standard deviation of the granularity.
double	out_mask[olen]		# I:  Mask of the output spectrum.
double	out_data[olen]		# IO: Predicted/calculated final spectrum.
double	o_err[olen]		# O:  Standard deviation of the final spectrum.
int	len			# I:  Length of the spectral input.
int     slen[n_shifts]          # I:  Actual length of shifted row in sdata.
int	niter			# I:  Number of iterations.
int	ksize			# I:  Size of the smoothing kernel.
int	korder			# I:  Order to preserve in the smoothing.
int	n_shifts		# I:  The number of shifts present.
int	glen			# I:  The length of the granularity.
int	olen			# I:  Length of output spectrum.
int     rsfunc                  # I:  Resampling function.

# Summation/differences.
pointer g_diff			# Difference between data and granularity.
pointer g_n			# Number of values for each pixel in g_diff.
int     max_shift		# Maximum shift found.
pointer o_diff			# Differences between data and "ideal" data.
pointer o_n			# Number of values for each pixel in o_diff.
pointer ou_n			# Number of unique shifts in spectra.
int	overlap			# Number of pixels that overlap all spectra.
double	ratio			# Ratio between granularity and data.

# Statistics.
pointer g_sum, g_sumsq		# Sums for the granularity.
pointer o_sum, o_sumsq		# Sums for the output.

# Misc.
double	ahivd()			# Return maximum value in double array.
int     aravd()			# Determine mean/sigma with rejection.
real    xpos                    # Position to sample the granularity array.
double  gval                    # Output sampled granularity value.
double  frac                    # Fractional remainder between goff - int(goff).
double	dx, dy			# Generic.
int	i, ix, iy		# Generic.
int	iter			# Current iteration.
int	s			# Current shift.
int     ishift                  # Integer shift for particular spectrum.
int     mode                    # Interpolation mode.
pointer rgran_data              # Real version of gran_data for arieval.
pointer sp			# Stack pointer.
pointer asi                     # Interpolant descriptor.

# Functions.
real   asieval()                # Interpolant evaluator.
double sas_neg()		# Value to replace with a negative square root.
double sas_zero()		# Value to replace when divide by zero.
extern sas_neg()
extern sas_zero()

begin
	call smark (sp)
	call salloc (g_diff, glen, TY_DOUBLE)
	call salloc (g_n, glen, TY_DOUBLE)
	call salloc (g_sum, glen, TY_DOUBLE)
	call salloc (g_sumsq, glen, TY_DOUBLE)
	call salloc (o_diff, olen, TY_DOUBLE)
	call salloc (o_n, olen, TY_DOUBLE)
	call salloc (o_sum, olen, TY_DOUBLE)
	call salloc (o_sumsq, olen, TY_DOUBLE)
	call salloc (ou_n, olen, TY_DOUBLE)
        call salloc (rgran_data, glen, TY_REAL)

        # Determine the sampling mode for the granularity.
        mode = 0
        switch (rsfunc) {
        case RS_LINEAR:
            mode = II_LINEAR
        case RS_SPLINE3:
            mode = II_SPLINE3
        case RS_POLY3:
            mode = II_POLY3
        case RS_POLY5:
            mode = II_POLY5
        }

	# For the following, only normalize in the regions that overlap.
	max_shift = int(ahivd (shift, n_shifts))
	overlap   = len - max_shift
	
	# Determine ratio between spectrum and granularity.
	ix = aravd (out_data[max_shift+1], overlap, ratio, dx, SIGMA)

	# Determine how many "unique", i.e. different, spectra contribute 
        # to a point.
	call aclrd (Memd[ou_n], olen)
	do s = 1, n_shifts {
            ishift = int(shift[s])
            call aaddkd (Memd[ou_n+ishift], 1.0d0, Memd[ou_n+ishift], slen[s])
	}

        # Check the output mask.  More than one spectrum must contribute to a 
        # point in order to compute the granularity.
	do i = 1, olen
	    if (Memd[ou_n+i-1] <= 1.0d0)
		out_mask[i] = 0.0d0
	
	# Iterate.
	dx = INDEFD
	dy = INDEFD
	call aclrd (Memd[g_sum], glen)
	call aclrd (Memd[g_sumsq], glen)
	call aclrd (Memd[o_sum], olen)
	call aclrd (Memd[o_sumsq], olen)

        # Initialize the interpolant descriptor.
        if (rsfunc != RS_NONE) 
            call asiinit (asi, mode)

	do iter = 1, niter {

	    # Clear working arrays.
	    call aclrd (Memd[g_diff], glen)
	    call aclrd (Memd[o_diff], olen)
	    call aclrd (Memd[g_n], glen)
	    call aclrd (Memd[o_n], olen)
            call aclrr (Memr[rgran_data], glen)

            # Fill rgran_data with the latest gran_data.
            call achtdr (gran_data, Memr[rgran_data], glen)

            # Set up the interpolating function, if necessary.
            if (rsfunc != RS_NONE) 
                call asifit (asi, Memr[rgran_data], glen)

	    # Find the differences from each spectrum.
	    do s = 1, n_shifts {
                frac = goff[s] - int(goff[s])
		do i = 1, slen[s] {
		    ix = i + int(goff[s])
		    iy = i + int(shift[s])

                    # Sample the granularity at the appropriate location.
                    if ((frac > RS_MINDIST) && (rsfunc != RS_NONE)) {
                        xpos = i + goff[s]
                        gval = asieval (asi, xpos)
                    }
                    else
                        gval = gran_data[ix]

		    dx   = (sdata[iy,s] * weight[iy,s]) - 
                            (gval * out_data[iy]) 

		    Memd[g_diff+ix-1] = Memd[g_diff+ix-1] +
					(out_mask[iy] * dx)
		    Memd[g_n+ix-1] = Memd[g_n+ix-1] +
				     out_mask[iy]

		    Memd[o_diff+iy-1] = Memd[o_diff+iy-1] +
					(gran_mask[ix] * dx)
		    Memd[o_n+iy-1] = Memd[o_n+iy-1] +
				     gran_mask[ix]
		}
            }

	    # Average the differences.
	    call advzd (Memd[g_diff], Memd[g_n], Memd[g_diff], glen, sas_zero)
	    call advzd (Memd[o_diff], Memd[o_n], Memd[o_diff], olen, sas_zero)

	    # Damp it.
	    call amulkd (Memd[g_diff], delta, Memd[g_diff], glen)
	    call amulkd (Memd[o_diff], delta, Memd[o_diff], olen)

	    # Scale the granularity back down.
	    call adivkd (Memd[g_diff], ratio, Memd[g_diff], glen)

	    # Apply to the data.
	    call aaddd (Memd[g_diff], gran_data, gran_data, glen)
	    call aaddd (Memd[o_diff], out_data, out_data, olen)

	    # Smooth the granularity and renormalize.
	    call sg_convolve (ksize, korder, gran_data, gran_data, glen)
	    ix = aravd (gran_data, glen, dx, dy, SIGMA)
	    call adivkd (gran_data, dx, gran_data, glen)

	    # Collect statistics.
	    call aaddd (gran_data, Memd[g_sum], Memd[g_sum], glen)
	    call amuld (gran_data, gran_data, Memd[g_diff], glen)
	    call aaddd (Memd[g_diff], Memd[g_sumsq], Memd[g_sumsq], glen)

	    call aaddd (out_data, Memd[o_sum], Memd[o_sum], olen)
	    call amuld (out_data, out_data, Memd[o_diff], olen)
	    call aaddd (Memd[o_diff], Memd[o_sumsq], Memd[o_sumsq], olen)
	}

	# Compute the standard deviation of the granularity.
	dx = niter
	dy = max (niter-1,1)
	call adivkd (Memd[g_sum], dx, Memd[g_sum], glen)
	call amuld  (Memd[g_sum], Memd[g_sum], Memd[g_sum], glen)
	call amulkd (Memd[g_sum], dx, Memd[g_sum], glen)
	call asubd  (Memd[g_sumsq], Memd[g_sum], g_err, glen)
	call adivkd (g_err, dy, g_err, glen)
	call asqrd  (g_err, g_err, glen, sas_neg)
	
	# Compute the standard deviation of the output spectrum.
	call adivkd (Memd[o_sum], dx, Memd[o_sum], olen)
	call amuld  (Memd[o_sum], Memd[o_sum], Memd[o_sum], olen)
	call amulkd (Memd[o_sum], dx, Memd[o_sum], olen)
	call asubd  (Memd[o_sumsq], Memd[o_sum], o_err, olen)
	call adivkd (o_err, dy, o_err, olen)
	call asqrd  (o_err, o_err, olen, sas_neg)
	
	# That's all folks.
        if (rsfunc != RS_NONE) 
            call asifree (asi)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of sa_comb
#---------------------------------------------------------------------------
