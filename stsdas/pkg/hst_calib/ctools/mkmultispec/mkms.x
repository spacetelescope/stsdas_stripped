include <imhdr.h>
include <imio.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mk_ms Mar93 source
.ih
NAME
mk_ms -- Make the MULTISPEC MWCS for the specified image.
.ih
USAGE
call mk_ms (input, n_groups, fit)
.ih
ARGUMENTS
.ls input (pointer)
The input image descriptor that will have the MULTISPEC MWCS added.
.le
.ls n_groups (int)
The number of groups to deal with for this input.
.le
.ls fit (pointer :input)
The fit descriptor.
.le
.ih
DESCRIPTION
Write the MULTISPEC keywords to each group of the output image.
.endhelp
#---------------------------------------------------------------------------
procedure mk_ms (input, n_groups, fit)

pointer input                   # I:  The input image.
int     n_groups                # I:  The number of groups in the input.
pointer fit                     # I:  The fit descriptor.

int     group                   # Current group.
int     ngrps                   # Number of groups to work on
real    rx                      # Generic.

errchk  gf_opengr, mk_create

begin
        # If specified number of groups doesn't equal number of groups
        # in the image then do all groups.
        if (n_groups < IO_NGRP(input))
            ngrps = IO_NGRP(input)
        else
            ngrps = n_groups
        
        # Operate on each group
        do group = 1, ngrps {

            # Open the appropropriate data.
            if (group > 1)
                call gf_opengr (IO_FD(input), group, rx, rx, 0)

            # Save the MWCS to the image.
            IM_HDRLEN(IO_FD(input)) = IM_LENHDRMEM(IO_FD(input))
            call mw_saveim (FIT_MW(fit,min(group,FIT_N_MW(fit))),
                            IO_FD(input))
        }

end
#---------------------------------------------------------------------------
# End of mk_ms
#---------------------------------------------------------------------------
