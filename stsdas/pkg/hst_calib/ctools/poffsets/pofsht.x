# A goto
define  done_                   10

#---------------------------------------------------------------------------
.help pof_shift Jun93 source
.ih
NAME
pof_shift -- Find shift between values in two arrays of wavelengths.
.ih
USAGE
int = pof_shift (w, zw, l, sample)
.ih
RETURNS
The pixel shift between the arrays w and zw, in the sense of
shift = w - zw.
.ih
ARGUMENTS
.ls w (I: double[l])
The array of monotonicly increasing/decreasing wavelengths.
.le
.ls zs (I: double[l])
The array of monotonicly increasing/decreasing wavelengths.  The
increase/decrease must be the same as for array "w".
.le
.ls l (I: int)
The lengths of the "w" and "zw" arrays.
.le
.ls sample (I: int)
The sampling rate used for the arrays.  I.e.  if sample = 5, then
every fifth pixel is compared.  This is useful since most wavelength
lists are well-behaved and there is no need to look at every
wavelength to determine the shift.
.le
.ih
DESCRIPTION
The pixel shift between wavelengths in the "w" array and "zw" array is
calculated.  This is done by searching the "zw" array for a value
found in the "w" array.  The shift is then the pixel position of this
value in the "w" array minus the pixel position in the "zw" array.

What pixels are searched for depends on the sampling rate.  If sample
= 1, every pixel in the "w" array is used to search for its
counterpart in the "zw" array.  If sample = 5, then every fifth pixel
is used.  The shift for each value is averaged together and returned
as the shift between the two arrays.

The criterion for pixel position is as follows:

.nf
        zw[j-1] <= w[i] <= zw[j]
.fi

If the above is true, then the value w[i] is said to reside at pixel j
in the zw array.  This can introduce biases, but since a number of
values are compared, the average should smooth over any bias.

To help speed up the process, an initial guess as to where the value
is in the zw array is determined by a linear approximation to the zw
array.  Then the position is refined by searching from the initial
guess.
.endhelp
#---------------------------------------------------------------------------
int procedure pof_shift (w, zw, l, sample)

double  w[l]                    # I:  Array of data.
double  zw[l]                   # I:  Array of data to find shift relative.
int     l                       # I:  Lenght of arrays.
int     sample                  # I:  Sampling rate.

# Returns
int     shift                   # The guessed shift.

# Misc.
bool    dec                     # TRUE if decreasing monotonically.
int     i, j                    # Pixel positions of common wavelength.
int     incr                    # Direction to travel in the zero-point table.
int     n                       # Number of shifts in the sum.
int     sum                     # Sum of the average.
double  wmin, wmax              # Minimum/maximum of current wavelengths.
double  zmin,zmax, zdiff        # Minimum/maximum of zero wavelengths.

begin
        # Get min/max of each set of data.  If there is no overlap,
        # then error.
        call alimd (zw, l, zmin, zmax)
        call alimd (w, l, wmin, wmax)
        if (zmin > wmax || zmax < wmin) {
            call eprintf ("poffsets: Wavelength ranges do not overlap\n")
            shift = INDEFI
            goto done_
        }
        
        # Check that both are increasing/decreasing.
        if ((w[1] <= w[l] && zw[1] > zw[l]) ||
            (w[1] >= w[l] && zw[1] < zw[l])) {
            call eprintf ("poffsets: zero-point and input wavelengths not both increasing or decreasing\n")
            shift = INDEFI
            goto done_
        }
        
        # Find the shift.  Since the wavelengths may be non-linear, find the
        # average shift per wavelength.  Also check for monotonic.
        dec = (w[1] > w[l])
        sum = 0
        n = 0
        zdiff = zmax - zmin
        do i = 2, l, sample {

            # Check for monotonic.
            if (dec) {
                if (w[i] > w[i-1] || zw[i] > zw[i-1]) {
                    call eprintf ("poffsets: wavelengths are not monotonically decreasing\n")
                    goto done_
                }
            } else {
                if (w[i] < w[i-1] || zw[i] < zw[i-1]) {
                    call eprintf ("poffsets: wavelengths are not monotonically increasing\n")
                    goto done_
                }
            }
            
            # If not in the zero-wavelength table, skip it.
            if (w[i] < zmin || w[i] > zmax)
                next

            # Guess where the wavelength will be in the zero-point.
            j = l * abs ((w[i] - zmin) / zdiff)
            if (j <= 1 || j >= l)
                next

            # Search for the wavelength in the zero-point table.
            if (w[i] < zw[j])
                incr = -1
            else
                incr = 1
            if (dec)
                incr = -1 * incr
            
            repeat {
                if (w[i] >= min (zw[j-1], zw[j]) &&
                    w[i] <= max (zw[j-1], zw[j])) {
                    sum = sum + i - j
                    n = n + 1
                    break
                }
                j = j + incr
            } until (j == 1 || j == L-1)

        }

        # Average them all together
        if (n > 0)
            shift = sum / n

        # That's all folks
done_
        return (shift)
        
end
#---------------------------------------------------------------------------
# End of pof_shift
#---------------------------------------------------------------------------
