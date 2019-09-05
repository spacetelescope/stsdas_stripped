include "im2gki.h"

# Define what the maximum log is.
define  MAXLOG  3

#---------------------------------------------------------------------------
.help i2g_scale 1May92 develop
.ih
NAME
i2g_scale -- Scale the data to a specified range.
.endhelp
#---------------------------------------------------------------------------

procedure i2g_scale (in, n_elements, function, zscale, low, high, out_low,
                     out_high, out)

real    in[n_elements]          # I:  The input data.
int     n_elements              # I:  Number of elements in the data.
int     function                # I:  Function to do scaling with.
bool    zscale                  # I:  True to use mean+-sigma scaling.
real    low, high               # I:  Range of data to scale.
real    out_low, out_high       # I:  Range of output data.
real    out[n_elements]         # O:  The scaled data.

# Declarations.
real    in_low, in_high         # Actual data range.
real    mean, sigma             # Stats on the input image.
real    temp                    # Temporary storage.

begin
        
        # Determine range of data.
        call alimr (in, n_elements, in_low, in_high)
        
        # If zscale is specified, determine the mean and standard deviation and
        # scale to the range one sigma around the mean.
        if (zscale)  {
            call aavgr (in, n_elements, mean, sigma)
            low = min (in_high, max (in_low, mean - sigma))
            high = max (in_low, min (in_high, mean + sigma))
        }
        
        # Else, use the specified low/high.  If one is not specified, then set
        # it to the extreme values.
        else {
            if (IS_INDEFR(low))
                low = in_low
            if (IS_INDEFR(high))
                high = in_high
            if (low > high) {
                temp = low
                low = high
                high = temp
            }
        }
        
        # If the image is constant, make the image low.
        if (low == high)
            high = high + 1.
        
        # Now do the scaling according to function.
        switch (function) {
        case I2G_SCALE_LOG:
            call amapr (in, out, n_elements, low, high, 1.0, 10.0 ** MAXLOG)
            call alogr (out, out, n_elements)
            call amapr (out, out, n_elements,
                        1.0, real(MAXLOG), out_low, out_high)
        case I2G_SCALE_NONE:
            call amovr (in, out, n_elements)
        default:
            call amapr (in, out, n_elements, low, high, out_low, out_high)
        }
        
end
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
