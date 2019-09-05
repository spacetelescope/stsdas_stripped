include <config.h>
include <fset.h>
include <imhdr.h>
include <gset.h>
include <xwhen.h>
include "newcont.h"

#---------------------------------------------------------------------------
.help newcont 9Aug95 source
.ih
NAME
t_newcont - Draw contour with bicubic smoothing.
.ih
DESCRIPTION
This is another contour implementation based on a package published in
the _ACM_Transactions_on_Mathematical_Software_.  See the header for the
routine FARB2D for more information (in farb2d.x).

Why another routine (one may ask)? Well, this routine produces a better
finished looking result- that's about it.  There are a few enhancements,
contour lists, etc., also, but the plot.contour package looks "ragged"
and there are times contour lines cross.  This algorithm does not produce
crossed contours.  It also smooths the contours based on the data as
opposed to smoothing based on the calculated plot vectors.  This results
in contours that "look better" and better resemble the data.

Also, when gfill and color a reliably implemented, this routine will
be able to produce smooth, color-filled regions (or other filled region
types), color contours, etc, with minimal modification.  At the moment,
colors are implemented as different GIO line types and thicknesses.
.ih
BUGS
Cannot label contours.  Does not appear doable with this algorithm.
However, if there were an GIO implementation of NCAR's dashpat package,
it might work.  The problem is that farb2d does not follow a single
contour, but draws complete contours for rectangular regions of the
image.

It is of moderate speed at the low resolution but gets very slow
with higher resolution.  Also, the amount of GIO information produced
is much greater (2 to 10 times) that produced by plot.contour and there
may be problems with actual hardcopy plots (printer memory overflow, etc.)
.endhelp
#---------------------------------------------------------------------------

procedure t_newcont

# Declarations
include "newcont_global.h"

pointer c               # The contour values for each level.
pointer device          # The name of the device.
pointer function_str    # String representing what function to use.
pointer icol            # The color index for each contour level.
pointer im              # Image descriptor.
pointer imsect          # The image name.
pointer label           # Plot labels.
pointer list            # List of contour levels.
pointer mw              # MWCS descriptor.
pointer sp              # Stack pointer.
pointer subras          # Subsampled image data.
pointer system_id       # System identification string.
pointer title           # The title to place on the image.
pointer tlabel          # Temporary label.
pointer xd, yd          # Arrays containing x, y axis values.

real ceiling            # The maximum value to contour.
real dummy              # Place holder.
real dx                 # Scaling.
real floor              # The minimum value to contour.
real hdx                # Half of dx.
real interval           # The interval between contours.
real plev               # Percent level of contours.
real left, right, 
     bottom, top        # The viewport edges in NDC units.
real log_x1, log_x2     # The logical extent of the WCS in X.
real log_y1, log_y2     # The logical extent of the WCS in Y.
real wx1, wx2, wy1, wy2 # Graphics viewport system.
real xs, xe, ys, ye     # Limits in pixels of the image.
real ybot               # The bottom of text placement.
real zero               # Zero point shift for the contours.

int     cont_mode               # Contour mode.
int negative_color      # Color of negative contours.
int epa                 # Has something to do with error return.
int function            # What function will be used to determine the
                        # contours.
int j                   # Generic.
int m_color             # How to emphasize major contours.
int m_contour           # How many contours should be emphasized.
int ncols               # Number of columns in the image.
int ncontours           # Number of contours to draw.
int nlines              # Number of lines in the image.
int nx, ny              # Number of columns/rows in the subsampled image.
int old_onint           # Error handling.
int status              # Error status return.
int tcojmp[LEN_JUMPBUF] # Error handling.
int xres, yres          # The size the image should be reduced to.

bool append             # True if append to a previous plot.
bool do_wcslab          # True if wcslabeling is to be done.
bool nhi                # High/low marking.
bool perimeter          # True if the titles, etc. should be drawn.
bool pre                # True if the plot and subsample should preserve
                        # the original image's aspect ratio.
bool    rain            # TRUE to use a rainbow.
bool scale_box          # Display a contour level scale.
bool sub                # True if image reduction should be done by 
                        # subsampling.
                        # as opposed to block averaging.

# Commons.
common /tcocom/ tcojmp

# Function declarations
real clgetr()
int  clgeti(), nc_get_function(), strdic(), strlen()
bool clgetb(), streq()
pointer gopen(), plt_getdata(), immap(), mw_openim()

extern tco_onint()

begin
        call smark (sp)
        call salloc (device, SZ_LINE, TY_CHAR)
        call salloc (imsect, SZ_LINE, TY_CHAR)
        call salloc (label, SZ_LINE, TY_CHAR)
        call salloc (tlabel, SZ_LINE, TY_CHAR)
        call salloc (system_id, SZ_LINE, TY_CHAR)
        call salloc (title, SZ_LINE, TY_CHAR)
        call salloc (list, SZ_LINE, TY_CHAR)
        call salloc (function_str, SZ_LINE, TY_CHAR)
        
        # Get all the parameters.
        call clgstr ("image", Memc[imsect], SZ_FNAME)
        
        call clgstr ("cont_mode", Memc[device], SZ_FNAME)
        cont_mode = strdic (Memc[device], Memc[device], SZ_FNAME, MODE_DICT) - 1
        call clgstr ("device", Memc[device], SZ_FNAME)
        call clgstr ("function", Memc[function_str], SZ_LINE)
        call clgstr ("list", Memc[list], SZ_LINE)
        call clgstr ("title", Memc[title], SZ_LINE)
        append = clgetb ("append")
        interval = clgetr ("interval")
        ceiling = clgetr ("ceiling")
        floor = clgetr ("floor")
        m_color = clgeti ("major_color")
        m_contour = clgeti ("major_contours")
        ncontours = clgeti ("ncontours")
        negative_color = clgeti ("negative_color")
        nhi = clgetb ("nhi")
        perimeter = clgetb ("perimeter")
        plev = clgetr ("plev")
        pre = clgetb ("preserve")
        scale_box = clgetb ("scale_box")
        sub = clgetb ("subsample")
        left = clgetr ("left")
        right = clgetr ("right")
        bottom = clgetr ("bottom")
        top = clgetr ("top")
        do_wcslab = clgetb  ("wcslab")
        xres = clgeti ("xres")
        yres = clgeti ("yres")
        zero = clgetr ("zero")
        rain = clgetb ("rain")
        
        # The floor and ceiling are in absolute units, but the zero shift is
        # applied first, so correct the numbers for the zero shift.
        if  (!IS_INDEFR (zero)) {
            if  (!IS_INDEFR (floor))
                floor = floor - zero
            if  (!IS_INDEFR (ceiling))
                ceiling = ceiling - zero
        }
        
        # Map image.  Retrieve values from header that will be needed.
        im = immap (Memc[imsect], READ_ONLY, 0)
        ncols = IM_LEN(im,1)
        nlines = IM_LEN(im,2)
        if (streq (Memc[title], "imtitle")) {
            call strcpy (Memc[imsect], Memc[title], SZ_LINE)
            call strcat (": ", Memc[title], SZ_LINE)
            call strcat (IM_TITLE(im), Memc[title], SZ_LINE)
        }
        
        # Save the original image size.
        xs = 1.0
        xe = real (ncols)
        ys = 1.0
        ye = real (nlines)
        
        # Get data with proper resolution.  Procedure plt_getdata returns
        # a pointer to the data matrix to be contoured.  The resolution
        # is decreased by the specified method in this procedure.  The
        # dimensions of the data array are also returned.
        nx = 0
        ny = 0
        subras = plt_getdata (im, sub, pre, xres, yres, nx, ny)
        
        # Apply the zero point shift.
        if  (!IS_INDEFR (zero))
            call asubkr (Memr[subras], zero, Memr[subras], nx * ny)
        
        # Construct the x, y dimensions.  For sampled data, the coordinate
        # vectors are setup such that the coordinate of each reduced pixel is
        # the center coordinate of that pixel overlayed upon the un-reduced
        # pixel system.
        call malloc (xd, nx, TY_REAL)
        call malloc (yd, ny, TY_REAL)
        if  (xe > real (nx)) {
            dx = xe / real (nx)
            hdx =  (dx / 2.) + 0.5
        } else {
            dx = 1.
            hdx = 1.
        }
        for (j = 0; j < nx; j = j + 1)
            Memr[xd+j] =  (real (j) * dx) + hdx
        if (ye > real (ny)) {
            dx = ye / real (ny)
            hdx =  (dx / 2.) + 0.5
        } else {
            dx = 1.
            hdx = 1.
        }
        for (j = 0; j < ny; j = j + 1)
            Memr[yd+j] =  (real (j) * dx) + hdx
        
        # Get the contour levels.
        call malloc (c, MAXIMUM_CONTOURS, TY_REAL)
        call malloc (icol, MAXIMUM_CONTOURS + 1, TY_INT)
        function = nc_get_function (Memc[function_str])
        if (!IS_INDEFI (ncontours))
            ncontours = ncontours - 1
        call nc_set_contour (Memr[subras], nx, ny, APPROXIMATE_CONTOURS, 
                             MAXIMUM_CONTOURS, function, 
                             Memc[list], negative_color,
                             m_contour, m_color, plev,
                             floor, ceiling, interval, ncontours,
                             Memr[c], Memi[icol])

        # Open device and make contour plot.
        if (append) {
            gp = gopen (Memc[device], APPEND, STDGRAPH)
            call gswind (gp, 1., xe, 1., ye)
        } else {
            gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

            # The viewport can be set by the user.  If not, the viewport is
            # assumed to be centered on the device.  In either case, the
            # viewport to window mapping is established in map_viewport.
            call sp_map_viewport (gp, xe, ye, left, right,
                                  bottom, top, pre, (perimeter | do_wcslab))
        }
        
        if (rain)
            call nc_rain (gp, ncontours+1, Memi[icol])
        
        # Install interrupt exception handler.
        call zlocpr (tco_onint, epa)
        call xwhen (X_INT, epa, old_onint)
        
        # Make the contour plot.  If an interrupt occurs ZSVJMP is reentered
        # with an error status.
        call zsvjmp (tcojmp, status)
        if (status == OK)
            call farb2d (Memr[xd], nx, Memr[yd], ny, Memr[subras], nx,
                         Memr[c], Memi[icol], ncontours, cont_mode)
        
        else {
            call gcancel (gp)
            call fseti (STDOUT, F_CANCEL, OK)
        }
        call gseti (gp, G_PLCOLOR, 1)

        # Mark lows/highs if desired.
        if  (nhi) {
            call gswind (gp, 1., real (nx), 1., real (ny))
            call nc_local_minmax (Memr[subras], nx, nx, ny)
        }
        
        # Now come back to the real world.  Reset the window.
        call gswind (gp, xs, xe, ys, ye)
        
        # If the scale box is requested, draw it.
        if  (scale_box)
            call nc_draw_scale (gp, Memr[c], Memi[icol], ncontours, cont_mode)
        
        # The perimeter needs to know the edges of the viewport.
        call ggview (gp, wx1, wx2, wy1, wy2)
        
        # Do wcslabeling if requested.  NOTE: This modifies some of the
        # viewport information.
        if (do_wcslab) {
            if (clgetb ("usewcs")) {
                call sp_wcsparams (mw, log_x1, log_x2, log_y1, log_y2)
                call wcslab (mw, log_x1, log_x2, log_y1, log_y2, gp, "")
            } else {
                mw = mw_openim (im)
                call wcslab (mw, xs, xe, ys, ye, gp, "")
            }
            call mw_close (mw)
            call ggview (gp, dummy, dummy, wy1, wy2)
        }
        
        # Now find window and output text string title.  The window is
        # set to the full image coordinates for labelling.
        if (perimeter)  {
            
            call gseti (gp, G_PLTYPE, GL_SOLID)
            
            # Draw the appropriate bounding box.
            if (!do_wcslab)
                call draw_perimeter (gp)

            
            call gseti (gp, G_WCS, 0)
            ybot = min (wy2 + .06, 0.99)
            call gtext (gp, (wx1 + wx2) / 2.0, ybot, Memc[title],
                        "h=c;v=t;f=b;s=.7")
            
            # Add system id banner to plot.
            call gseti (gp, G_CLIP, NO)
            call sysid (Memc[system_id], SZ_LINE)
            ybot = max (wy1 - 0.08, 0.01)
            call gtext (gp, (wx1+wx2)/2.0, ybot, Memc[system_id], "h=c;v=b;s=.5")
            
            # Put what contours, intervals and scaling was used.
            call sprintf (Memc[label], SZ_LINE, 
                          "contoured from %g to %g")
            call pargr  (Memr[c])
            call pargr  (Memr[c+ncontours-1])
            if  (function == FUNC_LOG || function == FUNC_RLOG)
                call strcat (", using log function", Memc[label], SZ_LINE)
            else if  (strlen (Memc[list]) != 0)
                call strcat (", using user-supplied list", Memc[label], SZ_LINE)
            else {
                call sprintf (Memc[tlabel], SZ_LINE, ", interval = %g")
                call pargr (interval)
                call strcat (Memc[tlabel], Memc[label], SZ_LINE)
                if  (!IS_INDEFR (zero)) {
                    call sprintf (Memc[tlabel], SZ_LINE, ", scaled by %g")
                    call pargr (zero)
                    call strcat (Memc[tlabel], Memc[label], SZ_LINE)
                }
            }
            ybot = max (wy1 - 0.06, .03)
            call gtext (gp, (wx1 + wx2) / 2.0, ybot, Memc[label], "h=c;v=b;s=.6")
        }
        
        # Close down.
        call gclose (gp)
        call imunmap (im)
        call sfree (sp)
        
end

#---------------------------------------------------------------------------
# TCO_ONINT -- Interrupt handler for the task contour.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.
#---------------------------------------------------------------------------

procedure tco_onint (vex, next_handler)

int vex  # virtual exception
int next_handler # not used

int tcojmp[LEN_JUMPBUF]
common /tcocom/ tcojmp

begin
  call xer_reset()
  call zdojmp (tcojmp, vex)
end
#---------------------------------------------------------------------------
# End of tco_oinit
#---------------------------------------------------------------------------
