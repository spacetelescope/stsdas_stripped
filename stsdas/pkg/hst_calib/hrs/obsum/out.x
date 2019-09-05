include "out.h"

#---------------------------------------------------------------------------
.help out Feb93 source
.ih
NAME
.nf
obs_open_out -- Open output.
obs_flush_out -- Flush the output.
obs_print_out -- Print output.
obs_close_out -- Close output.
.fi
.endhelp
#---------------------------------------------------------------------------
pointer procedure obs_open_out

pointer o                       # The output structure.

int     open()

errchk  malloc, open

begin
        # Create the memory.
        call malloc (o, OO_SZ_OO, TY_STRUCT)
        call malloc (OO_NAME_PTR(o), SZ_PATHNAME, TY_CHAR)

        # Create the file.
        call mktemp ("tmp$out", OO_NAME(o), SZ_PATHNAME)
        OO_FD(o) = open (OO_NAME(o), NEW_FILE, TEXT_FILE)
        
        return (o)
end
#---------------------------------------------------------------------------
# End of obs_open_out
#---------------------------------------------------------------------------
procedure obs_close_out (o)

pointer o                       # IO: The output structure, NULL on return.

errchk  mfree

begin
        call obs_flush_out (o)
        call close (OO_FD(o))
        call delete (OO_NAME(o))
        call mfree (OO_NAME_PTR(o), TY_CHAR)
        call mfree (o, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of obs_close_out
#---------------------------------------------------------------------------
procedure obs_print_out (o, str)

pointer o                       # I:  The output stucture.
char    str[ARB]                # I:  The string to print out.

errchk  fprintf

begin
        call fprintf (OO_FD(o), str)
end
#---------------------------------------------------------------------------
# End of obs_print_out
#---------------------------------------------------------------------------
procedure obs_flush_out (o)

pointer o                       # I:  The output structure.

int     open()

errchk  close, delete, open

begin
        call close (OO_FD(o))
        OO_FD(o) = open (OO_NAME(o), READ_ONLY, TEXT_FILE)
        call fcopyo (OO_FD(o), STDOUT)
        call close (OO_FD(o))
        call delete (OO_NAME(o))
        OO_FD(o) = open (OO_NAME(o), NEW_FILE, TEXT_FILE)
end
#---------------------------------------------------------------------------
# End of obs_flush_out
#---------------------------------------------------------------------------
