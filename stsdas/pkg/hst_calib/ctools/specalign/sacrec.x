include <math/curfit.h>

# Define the generic format for the first line of attributes.
# Note in the length, 5 is added for fudge factor.
define  ATTR_FORMAT     "%10d %10d %3d %25.16g %25.16g %10d %25.16g %12.2f %12.2f"

#---------------------------------------------------------------------------
.help sa_create_muspec Jun93 source
.ih
NAME
sa_create_muspec -- Create a MULTISPEC MWCS.
.ih
USAGE
pointer = sa_create_muspec (cv, len)
.ih
RETURNS
An MWCS descriptor containing the MULTISPEC transformation.
.ih
ARGUMENTS
.ls cv (I: pointer)
The CURFIT descriptor containing the fit parameters.
.le
.ls len (I: int)
The length of the data whose header will contain the MWCS object
returned.
.le
.ih
DISCUSSION
This routine is lifted from NOAO.ONEDSPEC package for creating
MULTISPEC coordinate transformation systems.
.endhelp
#---------------------------------------------------------------------------
pointer procedure sa_create_muspec (cv, len)

pointer cv                      # I:  Curve fit descriptor.
int     len                     # I:  Length of the data array.

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
        n_coeff = dcvstati (cv, CVNCOEFF)

        # Get the save structure.
        call salloc (save, dcvstati (cv, CVNSAVE), TY_DOUBLE)
        call dcvsave (cv, Memd[save])

        # Open the coefficient string.
        call salloc (tmp_str, SZ_LINE, TY_CHAR)
        sfd = sb_open()
        
        # Depending an fit, write the coordinates/coefficients.
        # For the other functions, the numbers are 1 = Weight for this function,
        # 0 = zero redshift, the type of function, order of function.
        # then, in the save block, the first two number are the minimum and
        # maximum X range followed by the function coefficients.
        call sprintf (Memc[tmp_str], SZ_LINE, "1 0 %d %d ")
        i = Memd[save]
        call pargi (i)
        i = Memd[save+1]
        call pargi (i)
        call sb_cat (sfd, Memc[tmp_str])
        do i = 1, n_coeff + 2 {
            call sprintf (Memc[tmp_str], SZ_LINE, "%g ")
            call pargd (Memd[save+i+1])
            call sb_cat (sfd, Memc[tmp_str])
        }   

        # Now its showtime, create the MWCS structure.
        tmp_str = sb_string (sfd)
        mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axes, 2, "multispec", "")
	call mw_saxmap (mw, axmap[1,1], axmap[1,2], 2)
	call mw_ssystem (mw, "multispec")
        call mw_swattrs (mw, 1, "label", "Wavelengths")
        call mw_swattrs (mw, 1, "units", "Angstroms")
	call sa_swattrs (mw, 1, 1, 1, 2, 0.d0, 0.d0, len, 0.d0,
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
# sa_swattrs -- set spectrum attribute parameters
#---------------------------------------------------------------------------
procedure sa_swattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
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
# End of sa_swattrs
#---------------------------------------------------------------------------
