include <gset.h>
include "newcont.h"

#---------------------------------------------------------------------------
.help nc_draw_scale Oct92 source
.ih
NAME
nc_draw_scale - Draw a scale representing the contour levels.
.ih
DESCRIPTION
This draw a small box on the right side of the plot.  The box represents
the scale from the lowest to highest contour.  Each level is drawn in its
respective color.  The low, high, and middle contours are labeled.
.endhelp
#---------------------------------------------------------------------------

procedure nc_draw_scale (gp, contours, colors, ncontours, mode)

pointer gp                      # The graphics pointer.
real    contours[ncontours]     # The contour levels.
int     colors[ncontours+1]     # The colors for the respective contour levels.
int     ncontours               # The number of contours avaliable.
int     mode                    # Mode to draw the contours in.

# Declarations
pointer label                   # The box label.
pointer sp                      # Stack pointer.

real    cx, cy                  # Text position.
real    max_value               # New maximum contour value.
real    old_line_width          # Original line width.
real    old_text_size           # Original text size.
real    x[5], y[5]              # End points for the contour levels.
real    xl, xr, yb, yt          # New viewport boundaries.
real    xs, xe, ys, ye          # The viewport boundaries.
real    xsize, ysize            # Size of the old viewport.

int     i                       # Contour index.
int     old_wcs                 # The old WCS for the graphics transformation.

# Function declarations.
real    gstatr()
int     gstati()

begin
        call smark (sp)
        call salloc (label, SZ_LINE, TY_CHAR)
        
        # Get the viewport.
        call ggview (gp, xs, xe, ys, ye)
        xsize = xe - xs
        ysize = ye - ys
        
        # Prepare to setup a new transformation.
        old_wcs = gstati (gp, G_WCS)
        call gseti (gp, G_WCS, UNUSED_WCS)
        
        # Determine where the box will go.  It will be to the right approximately
        # five character away.  The rest will scale according to the size of the
        # original viewport.
        xl = xe +  (BOX_RIGHT_DISPLACEMENT * xsize)
        xr = xl +  (BOX_SIZE_X * xsize)
        yb = ys +  (BOX_UP_DISPLACEMENT * ysize)
        yt = ye -  (BOX_DOWN_DISPLACEMENT * ysize)
        if (mode == MODE_LINE_DRAW)
            max_value = contours[ncontours]
        else
            max_value = ((contours[ncontours] - contours[ncontours-1])*.5) +
                        contours[ncontours]
        call gswind (gp, 0., 1., contours[1], max_value)
        call gsview (gp, xl, xr, yb, yt)
        
        # Now draw the line for each contour.
        old_line_width = gstatr (gp, G_PLWIDTH)
        call gsetr (gp, G_PLWIDTH, LINE_WIDTH * max (xsize, ysize))
        x[1] = 0.
        x[2] = 1.
        x[3] = 1.
        x[4] = 0.
        x[5] = 0.
        if (mode == MODE_LINE_DRAW) {
            do i = 1, ncontours {
                y[1] = contours[i]
                y[2] = contours[i]
                call usrplt (x, y, 2, colors[i], mode)
            }
        } else {
            do i = 1, ncontours - 1 {
                y[1] = contours[i]
                y[2] = contours[i]
                y[3] = contours[i+1]
                y[4] = contours[i+1]
                y[5] = contours[i]
                call usrplt (x, y, 5, colors[i], mode)
            }
            y[1] = contours[ncontours]
            y[2] = contours[ncontours]
            y[3] = max_value
            y[4] = max_value
            y[5] = contours[ncontours]
            call usrplt (x, y, 5, colors[ncontours+1], mode)
        }
        
        # Now put in the labels.  There will be three:  low, middle, high.
        cy =  TEXT_SIZE * max (xsize, ysize)
        old_text_size = gstatr (gp, G_TXSIZE)
        call gsetr (gp, G_TXSIZE, cy)
        cx = 1. + (TEXT_OFFSET * xsize)
        call sprintf(Memc[label], SZ_LINE, "%6.3g")
        call pargr (contours[1])  
        call gtext (gp, cx, contours[1], Memc[label], "h=l;v=c")
        
        call sprintf (Memc[label], SZ_LINE, "%6.3g")
        call pargr (contours[ncontours])
        call gtext (gp, cx, contours[ncontours], Memc[label], "h=l;v=c")
        
        i = 1 +  (ncontours / 2)
        call sprintf (Memc[label], SZ_LINE, "%6.3g")
        call pargr (contours[i])
        call gtext (gp, cx, contours[i], Memc[label], "h=l;v=c")
        
        # Draw the bounding regions.
        yb = yb - (BOX_EXTRA * ysize)
        yt = yt + (BOX_EXTRA * ysize)
        call gsview (gp, xl, xr, yb, yt)
        
        y[1] = contours[1]
        y[2] = contours[1]
        y[3] = max_value
        y[4] = max_value
        y[5] = contours[1]
        call gsetr (gp, G_PLWIDTH, LINE_WIDTH * max (xsize, ysize) / 1.5)
        call usrplt (x, y, 5, 1, MODE_LINE_DRAW)

        # Restore line width.
        call gsetr (gp, G_PLWIDTH, old_line_width)
        
        # Restore graphics parameters.
        call gseti (gp, G_WCS, old_wcs)
        call gsetr (gp, G_TXSIZE, old_text_size)
        
        call sfree (sp)
        
end
#---------------------------------------------------------------------------
# End of nc_draw_scale
#---------------------------------------------------------------------------
