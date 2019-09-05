include <gset.h>
include "newcont.h"

# nc_set_graphics - Set GIO graphing parameters depending on the type index.
#
# Description
#   Depending on the type index, the GIO polyline parameters are set.
#   this is to overcome the shortcoming of fill area and color implemented
#   in GIO.
#
# History
#  21Jan91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure nc_set_graphics( gp, index )

pointer gp     # Pointer to graphics structured.
int     index  # What line style/thickness is desired.

# Function declarations
real gstatr()

begin
  switch( index ) {
    case SOLID_THIN:
      call gseti( gp, G_PLTYPE, GL_SOLID )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THIN )

    case SOLID_THICK:
      call gseti( gp, G_PLTYPE, GL_SOLID )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THICK )

    case DOTTED_THIN:
      call gseti( gp, G_PLTYPE, GL_DOTTED )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THIN )

    case DOTTED_THICK:
      call gseti( gp, G_PLTYPE, GL_DOTTED )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THICK )

    case DASHED_THIN:
      call gseti( gp, G_PLTYPE, GL_DASHED )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THIN )

    case DASHED_THICK:
      call gseti( gp, G_PLTYPE, GL_DASHED )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THICK )

    case DOTDASH_THIN:
      call gseti( gp, G_PLTYPE, GL_DOTDASH )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THIN )

    case DOTDASH_THICK:
      call gseti( gp, G_PLTYPE, GL_DOTDASH )
      call gsetr( gp, G_PLWIDTH, CONTOUR_THICK )

    # Strange case- just emphasize the current line type.
    case BOLD:
      if( gstatr( gp, G_PLWIDTH ) == CONTOUR_THIN )
        call gsetr( gp, G_PLWIDTH, CONTOUR_THICK )
      else
        call gsetr( gp, G_PLWIDTH, CONTOUR_THIN )

  }

end
#---------------------------------------------------------------------------
# End of nc_set_graphics
#---------------------------------------------------------------------------
