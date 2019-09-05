include <ctotok.h>
include "newcont.h"

# nc_contours_from_list - Construct contours from a list or file.
#
# Description
#   This routines reads a list of numbers from a string or file and
#   fills the contour/color array appropriately.
#   The form of the list is a white-space seperated list with two types
#   of entries.  One is just a number.  This is taken to be the next contour
#   level.  The next is an entry of the form "color=#"  where # is some
#   integer representing the color desired.  The new color will take effect
#   for the next contour. The contour levels must be specified in a
#   monotonically increasing sequence.  As many as 100 contour levels may be 
#   specified.
#
# History
#   24Jan91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure nc_contours_from_list( list, max_levels, contours, colors, 
                                 ncontours )

char list[ARB]      # I:  The string containing the contour specifications or
                    #     containing the name of a file that contains the
                    #     the specifications.
int  max_levels     # I:  Maximum number of contours possible.
real contours[ARB]  # O:  The contour levels.
int  colors[ARB]    # O:  The color of the corresponding level.
int  ncontours      # O:  The number of contours specified.

# Declarations
pointer sp                  # Stack pointer.
pointer value_string        # The next token.

int  current_color          # The color for the next contour.
int  fd                     # File descriptor.
int  i, j                   # Generic
int  state                  # The state of parsing.
int  token                  # The token identifier of the next word.

# Function declarations.
int strmatch(), stropen(), open(), fscan(), strlen(), ctor(), ctoi()
bool strne()

begin

  # Allocate string space.
  call smark( sp )
  call salloc( value_string, SZ_LINE, TY_CHAR )

  # Initialize the contour level and color.
  current_color = SOLID_THIN
  ncontours = 0

  # See if the is a file specification.  The string must contain something
  # of the form '@<filename>'.  If the first character is not an '@', then
  # it is assumed that the string contains contour/color specifications.
  # Open the file descriptor depending on whether we should be reading from
  # the string or the file.
  i = strmatch( list, "^#@" )
  if ( i == 0 )
    fd = stropen( list, strlen( list ), READ_ONLY )
  else
    fd = open( list[i], READ_ONLY, TEXT_FILE )

  # Now read each word.  This could be a comma or whitespace seperated list
  # of either numbers or the phrase "color=" followed by a number.  The
  # plain numbers are contour levels, the color specifier changes the
  # color of the resulting plot.
  state = NEXT_INPUT
  while( fscan( fd ) != EOF && ncontours < max_levels ) {
    repeat {
      call gargtok( token, Memc[value_string], SZ_LINE )
      switch( token ) {
  
        # If the string is an identifier of some type, it better be "color"
        case TOK_IDENTIFIER:
          if ( strne( Memc[value_string], "color" ) )
            call error( 1, "nc_contours_from_list: Invalid identifier" )
          else
            state = GET_COLOR
          
        # A number is seen.  Parse depending on what state the system is in.
        case TOK_NUMBER:
          if ( state == GET_COLOR ) {
            i = 1
            j = ctoi( Memc[value_string], i, current_color )
            state = NEXT_INPUT
          } else {
            ncontours = ncontours + 1
            i = 1
            j = ctor( Memc[value_string], i, contours[ncontours] )
            colors[ncontours] = current_color
          }
  
        # If an operator, it better be '=' and we better be getting a color.
        case TOK_OPERATOR:
          if( state != GET_COLOR || strne( Memc[value_string], "=" ) )
            call error( 1, "nc_contours_from_list: Invalid operator" )

        # For end of string or line, just get out.
        case TOK_EOS:
          break
  
        # Anything else we'll let go, but reset the state back to the next
        # input.
        default:
          state = NEXT_INPUT
  
      }
    } until( ncontours == max_levels )
  }

  # Make sure one more color is defined.
  colors[ncontours+1] = current_color

  # Close up and clear out.
  call close( fd )
  call sfree( sp )

end
#---------------------------------------------------------------------------
# End of nc_contours_from_list
#---------------------------------------------------------------------------
