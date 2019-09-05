include <mach.h>
#---------------------------------------------------------------------------
.help sa_resample Jan97 source
.ih
NAME
sa_resample -- Resample the input array using the specified interpolant.
.ih
USAGE
call sa_resample (data, sdata, weight, frac, n_shifts, len, olen, ishift, s, mode)
.ih
ARGUMENTS
.ls data (I: double[len,n_shifts])
The arrays to be shifted and added.
.le
.ls sdata (O: double[olen,n_shifts])
The resampled data array.
.le
.ls weight (O: double[olen,n_shifts])
The weights for the resampled data array.
.le
.ls frac (I: double)
Fractional portion of pixel shifted to a new pixel bin.
.le
.ls n_shifts (I: int)
The number of shifts present.
.le
.ls len (I: int)
The length of each line of data.
.le
.ls olen (I: int)
Length of the output arrays.
.le
.ls ishift (I: int)
The integral shift for the current row being shifted/resampled.
.le
.ls s (I: int)
The current row of data being shifted/resampled.
.le
.ls mode (I: int)
The chosen resampling mode.
.ih
DESCRIPTION
This routine shifts each row of the input array by the amount specified in the 
"shift" array using a chosen interpolant.  The interpolants are a subset of 
those available in the image interpolator package -- iminterp.
.endhelp
#---------------------------------------------------------------------------
procedure sa_resample (data, sdata, weight, frac, n_shifts, len, olen, ishift, s, mode)

# Passed parameters.
double	data[len,n_shifts]	# I:  The spectral data to be put together.
double  sdata[olen,n_shifts]    # O:  The resampled version of the data array.
double  weight[olen,n_shifts]   # O:  Weights for the shifted data array.
double  frac                    # I:  Fractional portion of pixel shift.
int	n_shifts		# I:  The number of shifts.
int	len			# I:  Length of each data element.
int	olen			# I:  Length of result.
int     ishift                  # I:  Integer shift of the data line.
int     s                       # I:  Current shift operating on.
int     mode                    # I:  Resampling mode.

# Local variables.
int     j                       # Loop index.
int     index                   # Insertion index for shift data matrix.
real    xpos                    # Evaluation position of pixel.
double  dx                      # Residual fraction of value in pixel.
pointer rdata                   # Single row of "data" in real for asieval.
pointer sp                      # Stack pointer.
pointer asi                     # Interpolant descriptor.

# Functions.
real    asieval()               # Interpolant evaluator.

begin
	call smark (sp)
        call salloc (rdata, len, TY_REAL)

        # Copy one row of the input array into a real for resampling routine.
        call aclrr (Memr[rdata],len)
        call achtdr (data[1,s], Memr[rdata], len)

        # Set up the interpolating function.
        call asiinit (asi, mode)
        call asifit  (asi, Memr[rdata], len)
 
        # Determine the evaluation fraction.
        dx = 1.0d0 - frac

        # Evaluate the function pixel-by-pixel.
        do j = 2, len {
            index = ishift + j
            xpos = (j - 1) + dx
            sdata[index,s] = asieval (asi, xpos)

            # Accumulate the weighting for the output pixels.
            weight[index,s] = 1.0d0
        }

        # Avoid extrapolation on either end of the resampled array.
        # Compute the value of the new first pixel by duplicating the
        # second value in the array, weighted appropriately.
        index = ishift + 1
        sdata[index,s]  = sdata[index+1,s] * dx
        weight[index,s] = dx

        # Compute the value of the new end pixel by duplicating the last 
        # computed value, weighted appropriately.  
        index = ishift + len + 1
        sdata[index,s]  = sdata[index-1,s] * frac
        weight[index,s] = frac

	# Clean up memory allocation.
        call asifree (asi)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of sa_resample
#---------------------------------------------------------------------------
