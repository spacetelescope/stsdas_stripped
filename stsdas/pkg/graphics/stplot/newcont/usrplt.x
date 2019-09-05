include <gset.h>
include "newcont.h"

# usrplt - The routine the performs the actual plotting for farb2d.
#
# Description
#  The routine farb2d and all its subroutine perform this one call to
#  do all their plot output requirements.  This routine is as described
#  in farb2d, except for the following.
#
#  There are two problems.  First is the mode.  IRAF's GIO fill area calls
#  ARE NOT IMPLEMENTED.  No attempt to bypass this glaring hole is made.
#  Thus, if the mode is for Fill Area, a warning message is printed and
#  the call is continued as if mode was for Line Drawing.
#
#  The second problem is color.  Color also does not seem to be implemented.
#  However, a compromise has been installed.  The color index is actually
#  an index to a line style/thickness pointer (as defined in contour.h).
#  Thus different line sytles/thicknesses can be assigned to each contour
#  level in the top level call to farb2d.
#
#  If and when fill area and color are implemented, only this routine and
#  how the top level routine fills the color array need be changed.
#
#  NOTE: There was an inconsistency found in the way usrplt is called.
#  See farb2d.x for more information and what was done about it.
#
# History
#  21Jan91 - Created based on the prototype that came with the farb2d
#            package.  However, no code in the prototype exists in
#            this routine.  Jonathan D. Eisenhamer, STScI.
#
# Bugs
#  - Besides IRAF's GIO shortcomings, nothing alse really.
#---------------------------------------------------------------------------

procedure usrplt( x,y,n,ncol,mode )

real x[n], y[n]  # The vector end points of the line or polygon region.
int  n           # The number of points in the x and y arrays.
int  ncol        # The line style/thickness to draw in.
int  mode        # The mode to plot in.  Possible values are:
                 #   0 -> Fill Area.
                 #   1 -> Line Drawing.

# Declarations
include "newcont_global.h"

begin
        
        # If the mode is for Fill Area, print a warning message and continue
        # on as if Line Drawing had been specified.
        if( mode == MODE_FILL_AREA ) {
            call gseti (gp, G_FACOLOR, ncol)
            call gfill (gp, x, y, n, GF_SOLID)
        }
        
        # Line drawing mode.
        
        # Set the graphics characteristics based on the color index.
        else {
            call gseti (gp, G_PLCOLOR, ncol)
            call gpline( gp, x, y, n )
        }
end
#---------------------------------------------------------------------------
# End of usrplt.
#---------------------------------------------------------------------------
