include "newcont.h"

# nc_get_function - Decode string into a function type
#
# Returns
#   Returns an integer indicating the function type.  Possible values:
#      FUNC_LINEAR -> Use linear function.
#      FUNC_LOG    -> Use log function.
#      FUNC_RLOG   -> Use log function but in the reverse direction.
#
#   If the string cannot be decoded, a warning message is produced and
#   this returns FUNC_LINEAR.
#
# History
#   29Jan91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

int procedure nc_get_function( input )

char input[ARB]  # String representing desired function.  Can be:
                 #  "linear" -> Use linear function.
                 #  "log"    -> Use log function.
                 #  "rlog"   -> Use log function but in reverse direction.

# Declarations.
int function     # The funcition to use.

# Function declarations.
bool streq()

begin

  # Lowercase the string.
  call strlwr( input )

  # Now determine which function is used.
  if ( streq( input, "linear" ) )
    function = FUNC_LINEAR
  else if( streq( input, "log" ) )
    function = FUNC_LOG
  else if( streq( input, "rlog" ) )
    function = FUNC_RLOG
  else {
    call eprintf( "nc_get_function: Unknown function, using linear.\n" )
    function = FUNC_LINEAR
  }

  return( function )

end
#---------------------------------------------------------------------------
# End of nc_get_function
#---------------------------------------------------------------------------
