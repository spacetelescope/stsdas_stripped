include "siaper.h"

# si_string_to_degrees - Convert from a string to a number.
#
# Description
#   Read in a string and convert to a real number representing degrees.
#
# Function Returns
#  This returns the single coordinate value converted to a base system 
#  (degrees).  If the string is empty, then INDEF is returned.
#
# History
#  14Dec90 - Created by Jonathan D. Eisenhamer, STScI.
#   7Feb91 - Made less restrictive. jde
#  13Mar91 - Made to handle just HMS/DMS to degrees.  Taken from t_wcslab.x
#            jde
#---------------------------------------------------------------------------

real procedure si_string_to_degrees( input, ra_dec )

char input[ARB]  # I: The string input.
int  ra_dec      # I: Indicator of whether Hours:Minutes:Seconds is
                 #    to be read or Degrees:Minutes:Seconds.  Possible values:
                 #      LONGITUDE -> Read in the HMS format
                 #      LATITUDE  -> Read in the DMS format

# Declarations.
real value     # The return value.

# Function declarations.
int strlen()

begin

  # It is possible that the value was not defined.
  if ( strlen( input ) <= 0 )
    value = INDEF

  # Decode based on format type.
  else {

    # Since SPP FMTIO can handle the HH:MM:SS format, just let it read
    # in the value.  However, there is no way to destinquish H:M:S from
    # D:M:S.  If the axis being read is RA, assume that it was H:M:S.
    call sscan( input )
    call gargr( value )

    # If the axis is Longitude == RA, then convert the hours to degrees.
    if ( ra_dec == LONGITUDE )
      value = HRSTODEG( value )

  }

  return( value )

end
#---------------------------------------------------------------------------
# End of si_string_to_degrees
#---------------------------------------------------------------------------
