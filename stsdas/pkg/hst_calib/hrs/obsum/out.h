#---------------------------------------------------------------------------
.help out.h Feb93 source
.ih
NAME
out.h -- Memory structure for long string output.
.endhelp
#---------------------------------------------------------------------------

define  OO_FD           Memi[$1]        # Output file descriptor.
define  OO_NAME_PTR     Memi[$1+1]      # Name of temporary output.
define  OO_NAME         Memc[OO_NAME_PTR($1)]
define  OO_SZ_OO        2               # Size of memory structure.
#---------------------------------------------------------------------------
# End of out.h
#---------------------------------------------------------------------------
