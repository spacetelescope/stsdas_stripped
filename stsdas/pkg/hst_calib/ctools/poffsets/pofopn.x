include "poffsets.h"

#---------------------------------------------------------------------------
.help pof_open,pof_close Feb93 source
.ih
NAME
.nf
pof_open  -- Create the poffsets' basic memory.
pof_close -- Free the memory.
.fi
.endhelp
#---------------------------------------------------------------------------
pointer procedure pof_open

pointer p                       # The memory structure.

errchk  malloc

begin
        call calloc (p, POF_SZ_POF, TY_STRUCT)

        return (p)
end
#---------------------------------------------------------------------------
# End of pof_open
#---------------------------------------------------------------------------
procedure pof_close (p)

pointer p                       # IO:  The memory to free, NULL on return.

errchk  mfree

begin
        call mfree (p, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of pof_close (p)
#---------------------------------------------------------------------------
