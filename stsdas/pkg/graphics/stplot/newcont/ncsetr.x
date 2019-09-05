include "newcont.h"

# nc_set_contour - Setup the contour/color arrays.
#
# Description
#   This routine takes in the various user-specified parameters and determines
#   the contour levels and colors for use in the final contour plot.
#
# History
#   24Jan91 - Created by Jonathan D. Eisenhamer, STScI.
#    4Feb91 - Added the plev percent level specifier. jde
#---------------------------------------------------------------------------

procedure nc_set_contour( image, nx, ny, approximate_levels, max_levels, 
                          function, list, negative_color, m_contour, 
                          m_color, plev, floor, ceiling, interval, ncontours,
                          contours, colors )

real image[nx, ny]         # I:  The image being contoured.
int  nx, ny                # I:  The x, y dimensions of image.
int  approximate_levels    # I:  The approximate number of contour levels
                           #     to calculate.
int  max_levels            # I:  Maximum number of possible contours.
int  function              # I:  Function to use to determine contour levels.
char list[ARB]             # I:  A list of possible contour levels for a file
                           #     containing such a list.
int  negative_color        # I:  The color for "negative" contours.
int  m_contour             # I:  Every major_contour contour should be
                           #     written in m_color style.
int  m_color               # I:  The color to write every m_contour.
real plev                  # I:  Percent level.
real floor, ceiling        # IO: The range to contour.
real interval              # IO: The interval between contour levels.
int  ncontours             # IO: The number of contours specified.
real contours[ARB]         # O:  The contour levels.
int  colors[ARB]           # O:  The color for the corresponding contours.

# Declarations.
real finc                  # Contouring determinination.  Possible values:
                           #   finc < 0, -finc is the number of contours
                           #   finc == 0, then figure everything automatically.
                           #   finc >, finc is the interval between contours.
real range                 # Range of image values.
real x                     # Generic.

int constant               # If non-zero, then a constant image was detected.
int i                      # Contour/color index.

# Function declarations.
int strlen()

begin
        
        # Call CLGEN always.  This computes a linear set of contour levels based
        # on the number of contours requested, etc.  For the other options, CLGEN
        # also rounds out the floor and ceiling, which is very useful.
        
        # User can specify either the number of contours or the contour
        # interval, or let us pick a nice number.  Get params and
        # encode the FINC param expected by clgen.
        if( IS_INDEFI( ncontours ) ) {
            if( IS_INDEFR( interval ) )
                finc = 0
            else
                finc = interval
        } else
            finc = - abs (ncontours)
        
        # Construct the contours.
        call nc_clgen( image, nx, nx, ny, floor, ceiling, finc,
                       approximate_levels,
                       max_levels, contours, ncontours, constant )
        interval = finc
        
        # If a constant field was found, then abort.
        if ( constant != 0 )
            call error( 1, "t_contour: Constant field, no contours possible" )
        
        # If a list of contours is specified, decode the contours.
        if ( strlen( list ) > 0 ) {
            call nc_contours_from_list( list, max_levels, contours,
                                        colors, ncontours )
            
            # If the percent level is specified, assume that the list is full
            # of level specifications.  Reassign the contours to values from the
            # image of the percentage.
            if ( !IS_INDEFR( plev ) ) {
                range = ceiling - floor
                do i = 1, ncontours
                    contours[i] = plev * contours[i] * range + floor
            }
            
            # Handle functions and percent levels.
        } else {
            
            # If the percentage level is specified, then use that level as the
            # interval, and make sure the function is set to linear.
            if( !IS_INDEFR( plev ) ) {
                interval = plev * ( ceiling - floor )
                ncontours = 0
                finc = floor - interval
                repeat {
                    ncontours = ncontours + 1
                    finc = finc + interval
                    contours[ncontours] = finc
                } until( finc >= ceiling )
                
                # If the function is log or rlog, then do it.
            } else if( function != FUNC_LINEAR ) {
                interval = 9. / ( ncontours - 1 )
                finc = 0.
                range = ceiling - floor
                if( function == FUNC_LOG )
                    do i = 1, ncontours {
                        x = log10( 1. + finc )
                        contours[i] = floor + ( range * x )
                        finc = finc + interval
                    }
                else
                    do i = ncontours, 1, -1 {
                        x = log10( 1. + finc )
                        contours[i] = floor + ( range * ( 1. - x ) )
                        finc = finc + interval
                    }
            }
            
            # Determine the colors for the contours
            for ( i = 1; i <= ncontours; i = i + 1 )
                if ( contours[i] < 0. )
                    colors[i] = negative_color
                else
                    colors[i] = SOLID_THIN
            colors[ncontours+1] = SOLID_THIN
            
        }
        
        # Emphasize some number of contours.
        if ( !IS_INDEFI( m_contour ) )
            do i = 1, ncontours+1, m_contour 
                switch( m_color ) {
                    
                    # Just bold the current type.
                case BOLD:
                    switch( colors[i] ) {
                    case SOLID_THIN: colors[i] = SOLID_THICK
                    case SOLID_THICK: colors[i] = SOLID_THIN
                    case DOTTED_THIN: colors[i] = DOTTED_THICK
                    case DOTTED_THICK: colors[i] = DOTTED_THIN
                    case DASHED_THIN: colors[i] = DASHED_THICK
                    case DASHED_THICK: colors[i] = DASHED_THIN
                    case DOTDASH_THIN: colors[i] = DOTDASH_THICK
                    case DOTDASH_THICK: colors[i] = DOTDASH_THIN
                    }
                    
                    # Just set the specified color
                default: colors[i] = m_color
                    
                }
        
end
#---------------------------------------------------------------------------
# End of nc_set_contour
#---------------------------------------------------------------------------
