include <imhdr.h>
include <imio.h>
include <math/curfit.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mk_create Mar93 source
.ih
NAME
mk_create -- Create the MULTISPEC WCS from the CURFIT parameters.
.ih
USAGE
mwcs =  mk_create (len, function, cv, label, units, coeff_fmt)
.ih
ARGUMENTS
.ls len (integer :input)
Length of the input data arrays.
.le
.ls function (integer :input)
The original function specified for the fits.  Used to determine whether
an actual fit is stored in the cv pointer, or an array of wavelengths.
.le
.ls cv (pointer :input)
The CURFIT descriptor containing the fit parameters to create the
MULTISPEC WCS with.
.le
.ls label (char[ARB] :input)
The label to use for the wavelength scale.
.le
.ls units (char[ARB] :input)
The units of the wavelength scale.
.le
.ls coeff_fmt (char[ARB] :input)
Format to use when writing the coefficients to the MWCS descriptor.
.le
.ih
DESCRIPTION
The routine that actually populates the MWCS keywords with MULTISPEC
information.  Much of this is pulled from the NOAO,
noao.onedspec.splot task.  For information about MULTISPEC, see the
help pages for the onedspec package, "help onedspec".
.endhelp
#---------------------------------------------------------------------------
pointer procedure mk_create (len, function, cv, label, units, coeff_fmt)

int     len                     # I:  Length of the data.
int     function                # I:  Function of fit.
pointer cv                      # I:  CURFIT descriptor.
char    label[ARB]              # I:  The label for wavelengths.
char    units[ARB]              # I:  The units for wavelenghts.
char    coeff_fmt[ARB]          # I:  Format to write the coefficients with.

int     axes[2]                 # Axes to operate on.
int     axmap[2,2]              # Mapping between axes.
int     dcvstati()              # Get status of the CURFIT descriptor.
int     i                       # Generic
pointer mw                      # MWCS descriptor.
pointer mw_open()               # Open the MWCS descriptor.
int     n_coeff                 # Number of fit coefficients.
pointer save                    # CURFIT save array.
pointer sb_open()               # Open a string buffer.
pointer sb_string()             # Return string array containing the buffer.
pointer sfd                     # String buffer pointer.
pointer sp                      # Stack pointer.
pointer tmp_str                 # Generic.

data    axes/1,2/,axmap/1,0,0,0/

begin
        call smark (sp)

        # Get number of coefficients.
        if (function == MK_TABLE)
            n_coeff = len
        else
            n_coeff = dcvstati (cv, CVNCOEFF)

        # Get the save structure.
        if (function != MK_TABLE) {
            call salloc (save, dcvstati (cv, CVNSAVE), TY_DOUBLE)
            call dcvsave (cv, Memd[save])
        }

        # Open the coefficient string.
        call salloc (tmp_str, SZ_LINE, TY_CHAR)
        sfd = sb_open()
        
        # Depending an fit, write the coordinates/coefficients.
        # For the table, the first three numbers are 1 = weight for this
        # function, 0 = zero redshift, and 5 = TABLE.  The following
        # "coefficients" are the wavelengths themselves.
        if (function == MK_TABLE) {
            call sprintf (Memc[tmp_str], SZ_LINE, "1 0 5 %d ")
            call pargi (n_coeff)
            call sb_cat (sfd, Memc[tmp_str])
            do i = 1, n_coeff {
                call sprintf (Memc[tmp_str], SZ_LINE, coeff_fmt)
                call pargd (Memd[cv+i-1])
                call sb_cat (sfd, Memc[tmp_str])
            }
        }

        # Else...
        # For the other functions, the numbers are 1 = Weight for this function,
        # 0 = zero redshift, the type of function, order of function.
        # then, in the save block, the first two number are the minimum and
        # maximum X range followed by the function coefficients.
        else {
            call sprintf (Memc[tmp_str], SZ_LINE, "1 0 %d %d ")
            i = Memd[save]
            call pargi (i)
            i = Memd[save+1]
            call pargi (i)
            call sb_cat (sfd, Memc[tmp_str])
            do i = 1, n_coeff + 2 {
                call sprintf (Memc[tmp_str], SZ_LINE, coeff_fmt)
                call pargd (Memd[save+i+1])
                call sb_cat (sfd, Memc[tmp_str])
            }
        }

        # Now its showtime, create the MWCS structure.
        tmp_str = sb_string (sfd)
        mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axes, 2, "multispec", "")
	call mw_saxmap (mw, axmap[1,1], axmap[1,2], 2)
	call mw_ssystem (mw, "multispec")
        call mw_swattrs (mw, 1, "label", label)
        call mw_swattrs (mw, 1, "units", units)
	call mk_swattrs (mw, 1, 1, 1, 2, 0.d0, 0.d0, len, 0.d0,
                           0.d0, 0.d0, Memc[tmp_str])
        
        # That's all folks.
        call mfree (tmp_str, TY_CHAR)
        call sb_close (sfd)
        call sfree (sp)
        return (mw)
end
#---------------------------------------------------------------------------
# End of mk_create
#---------------------------------------------------------------------------
# mk_swattrs -- set spectrum attribute parameters
#---------------------------------------------------------------------------
procedure mk_swattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
	coeff)

pointer	mw				# MWCS pointer
int	line				# Physical line number
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
double	aplow, aphigh			# Aperture limits
char	coeff[ARB]			# Nonlinear coeff string

int	sz_val, strlen()
pointer	sp, key, val

begin
	sz_val = strlen (coeff) + SZ_LINE

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, sz_val, TY_CHAR)

	# We can't use SPRINTF for the whole string because it can only
	# handle a limited length and trucates long coefficient strings.
	# Use STRCAT instead.

	call sprintf (Memc[key], SZ_FNAME, "spec%d")
	    call pargi (line)
	call sprintf (Memc[val], sz_val, ATTR_FORMAT)
	    call pargi (ap)
	    call pargi (beam)
	    call pargi (dtype)
	    call pargd (w1)
	    call pargd (dw)
	    call pargi (nw)
	    call pargd (z)
	    call pargd (aplow)
	    call pargd (aphigh)
	if (coeff[1] != EOS) {
	    call strcat (" ", Memc[val], sz_val)
	    call strcat (coeff, Memc[val], sz_val)
	}
	call mw_swattrs (mw, 2, Memc[key], Memc[val])

	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of mk_swattrs
#---------------------------------------------------------------------------
