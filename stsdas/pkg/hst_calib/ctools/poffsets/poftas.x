include	<od.h>
include "poffsets.h"

# Define the length of the tables.
define  L                       OD_LEN(ZERO_WAVE(p))

#---------------------------------------------------------------------------
.help pof_table_guess Apr93 source
.ih
NAME
pof_table_guess -- Take a guess at the shift from wavelength tables.
.ih
USAGE
guess = pof_table_guess (p, wave)
.ih
ARGUMENTS
.ls p (pointer :input)
The poffsets descriptor.
.le
.ls wave (pointer :input)
The file descriptor of the current wavelength image.
.le
.ih
RETURNS
An integer representing the guess it thinks that shift should be at.  If a
shift cannot be guessed at, the returned value is INDEF.
.endhelp
#---------------------------------------------------------------------------
int procedure pof_table_guess (p, wave)

pointer p                       # I:  The POFFSETS descriptor.
pointer wave                    # I:  The current wavelength file, null if none.

# Returns
int     shift                   # The shift.

# Misc.
pointer pof_shift()             # Determine shift from the tables.
pointer sp                      # Stack pointer.
pointer w                       # Current wavelength table.
pointer zw                      # Zero wavelength table.

begin
        call smark (sp)

        # Get the data.
        call salloc (zw, L, TY_DOUBLE)
        call salloc (w, L, TY_DOUBLE)
        call od_get (ZERO_WAVE(p), Memd[zw])
        call od_get (wave, Memd[w])

        # Get the shift.
        shift = pof_shift (Memd[w], Memd[zw], L, SAMPLE)
        
        # That's all folks.
        call sfree (sp)
        return (shift)
end
#---------------------------------------------------------------------------
# End of pof_table_guess
#---------------------------------------------------------------------------
