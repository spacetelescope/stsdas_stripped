include	<od.h>
include "poffsets.h"

#---------------------------------------------------------------------------
.help pof_wcs_guess May93 source
.ih
NAME
pof_table_guess -- Take a guess at the shift from WCS informatin.
.ih
USAGE
guess = pof_wcs_guess (p, in)
.ih
RETURNS
An integer representing the guess it thinks that shift should be at.  If a
shift cannot be guessed at, the returned value is INDEF.
.ih
ARGUMENTS
.ls p (pointer :input)
The poffsets descriptor.
.le
.ls in (pointer :input)
The file descriptor of the current image that offsets are being found for.
.le
.endhelp
#---------------------------------------------------------------------------
int procedure pof_wcs_guess (p, in)

pointer p                       # I:  The POFFSETS descriptor.
pointer in                      # I:  The image that shifts will be found for.

# Returns
int     guess                   # The guess, INDEF if no determination.

# Misc.
pointer adx, ady, adz           # Generic double arrays.
int     i, ix                   # Generic.
int     len                     # Length of the arrays.
int     pof_shift()             # Shift between arrays.
pointer sp                      # Stack pointer.
bool    streq()                 # Are two strings equal?
pointer w, zw                   # Wavelength arrays.

begin
        call smark (sp)
        
        # Check that the input has WCS information.
        guess = INDEFI
        if (OD_MW(in) != NULL) {

            # Allocate the arrays.
            len = OD_LEN(in) / SAMPLE
            call salloc (adx, len, TY_DOUBLE)
            call salloc (ady, len, TY_DOUBLE)
            call salloc (adz, len, TY_DOUBLE)
            call salloc (w, len, TY_DOUBLE)
            call salloc (zw, len, TY_DOUBLE)

            # Get only a sampling of the wavelengths.
            ix = 0
            do i = 1, OD_LEN(in), SAMPLE {
                Memd[adx+ix] = i
                Memd[ady+ix] = 1.d0
                ix = ix + 1
            }

            # Get the transformations.
            if (streq (OD_WSYS(ZERO(p)), "multispec"))
                call mw_v2trand (OD_LW(ZERO(p)), Memd[adx], Memd[ady],
                                 Memd[zw], Memd[adz], len)
            else
                call mw_v1trand (OD_LW(ZERO(p)), Memd[adx], Memd[zw], len)

            if (streq (OD_WSYS(in), "multispec"))
                call mw_v2trand (OD_LW(in), Memd[adx], Memd[ady],
                                 Memd[w], Memd[adz], len)
            else
                call mw_v1trand (OD_LW(in), Memd[adx], Memd[w], len)

            # Find the shift between the arrays.  Since the array has
            # already been sampled, the sampling factor must be included.
            guess = pof_shift (Memd[w], Memd[zw], len, 1)
            if (guess < 0)
                guess = SAMPLE * (guess + 1)
            else
                guess = SAMPLE * guess
        }

        # That's all folks.
        call sfree (sp)
        return (guess)
end
#---------------------------------------------------------------------------
# End of pof_wcs_guess
#---------------------------------------------------------------------------
