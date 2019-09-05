#---------------------------------------------------------------------------
.help obsum.h Jun92 source
.ih
NAME
obsum.h -- Define for the obsum task.
.endhelp
#---------------------------------------------------------------------------

# Define the extensions.  Note that the order of definition and the order
# of the string should be the same.
define  ZEXTENSIONS     ".shh .ulh .d0h .x0h"
define  ZSHP            1
define  ZUDL            2
define  ZSCI            3
define  ZX0H            4

# Define how many of the above extensions are not included in the
# packet time ordering.
define  ZNO_PACKET      1

#---------------------------------------------------------------------------
# End of obsum.h
#---------------------------------------------------------------------------
