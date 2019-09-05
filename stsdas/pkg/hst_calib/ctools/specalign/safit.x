include <math/curfit.h>
include <od.h>

#---------------------------------------------------------------------------
.help sa_fit Jun93 source
.ih
NAME
sa_fit -- Fit a function to an array and apply to files.
.ih
USAGE
call sa_fit (func, nterms, wdata, l, wave, out, ol)
.ih
ARGUMENTS
.ls func (I: int)
The CURFIT function to fit to the data, wdata.  Valid values can be
found in the include file <math/curfit.h>.  If 0, then no fit should be done.
.le
.ls nterms (I: int)
Number of terms for the polynomial, or number of pieces for the spline
fits, to use.
.le
.ls wdata (I: double[l])
The data to be fit.
.le
.ls l (I: int)
Length of the wdata array.
.le
.ls wave (I: pointer)
OD file descriptor if the function is to be solved and written out as
a table of values.  NULL if not to do this.
.le
.ls out (I: pointer)
OD file descriptor of the file to write the fit as MULTISPEC MWCS
header keywords.  NULL if not to do this.
.le
.ls ol (I: int)
Length of the output array if writing the function to a table of
values.
.le
.ih
DISCUSSION
Since the wavelengths for the input data were just averaged together
to produce a wavelength solution for the combined output, it is
entirely possible that the average is not monotonic in nature.  Since
the data is presumed to still be monotonic, in pixel space anyways, it
is unreasonable to let the wavelengths wonder.  To constrain them back
to being monotonic, the average is replaced by a fit to the average.
This fit can then either be written out as a list of values, or
written to the output spectrum's header as a MULTISPEC MWCS system.
.endhelp
#---------------------------------------------------------------------------
procedure sa_fit (func, nterms, wdata, l, wave, out, ol)

int     func                    # I:  The function to fit.
int     nterms                  # I:  Number of terms to fit.
double  wdata[l]                # I:  The data to be fit.
int     l                       # I:  Size of data array.
pointer wave                    # I:  Output file for tables.
pointer out                     # I:  Output file for WCS
int     ol                      # I:  Output array lengths.

# Misc.
pointer cv                      # Curve fit descriptor.
double  dx                      # Generic.
pointer sa_create_muspec()      # Create a MULTISPEC MWCS.
int     i                       # Generic.
pointer mw                      # MWCS descriptor.
pointer sp                      # Stack Pointer.
pointer x                       # X values.
pointer w                       # Weights.

begin
        call smark (sp)
        call salloc (x, max (l, ol), TY_DOUBLE)
        call salloc (w, max (l, ol), TY_DOUBLE)

	# Sanity check.  If func is 0, ans wave is not defined, then
	# punt.
	if (func <= 0 && wave == NULL)
	    call error (1, "specalign: writing to wcs but no function is defined, reset parameter 'func'")

        # Fill the x, weight arrays.
        do i = 0, max (l, ol) - 1 {
            Memd[x+i] = i+1
            Memd[w+i] = 1.d0
        }

	# Do the fitting if requested.
	if (func > 0) {

	    # Create the fit descriptor.
	    dx = l
	    call dcvinit (cv, func, nterms, 1.d0, dx)
	    
	    # Fit the data.
	    call dcvfit (cv, Memd[x], wdata, Memd[w], l, WTS_UNIFORM, i)
	    if (i != OK)
		call error (1, "specalign: Could not fit function to wavelengths, try different parameters")
	}
	
        # If there is a wavelength file, recover the fit as a table and
        # write it out.
        if (wave != NULL) {
	    if (func > 0) {
		call dcvvector (cv, Memd[x], Memd[w], ol)
		call od_put (wave, Memd[w])
	    } else
		call od_put (wave, wdata)
        }

        # Else, write the fit as MULTISPEC header.
        else {
            mw = sa_create_muspec (cv, ol)
            call mw_saveim (mw, OD_FD(out))
            call mw_close (mw)
        }

        # That's all folks.
	if (func > 0)
	    call dcvfree (cv)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of sa_fit
#---------------------------------------------------------------------------
