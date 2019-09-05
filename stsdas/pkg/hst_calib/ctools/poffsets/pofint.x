# Graphics parameters.
define  DISTANCE                .05
define  GUESS_SIZE              .05
define  INITIAL_SIZE            .1
define  CURRENT_SIZE            .15
define  SZ_JUST                 10

# Memory management.
define  Shifts                  Memr[shifts+$1-1]

#---------------------------------------------------------------------------
.help pof_int May93 source
.ih
NAME
pof_int -- Interactively set shifts.
.ih
RETURNS
The average shift of the correlation functions.  If a shift is not
determined, then and INDEFD is returned.
.ih
ARGUMENTS
.ls gp (I: pointer)
The graphics device descriptor to use for the interaction.
.le
.ls corr (I: double[len,n])
The correlations functions to determine the shift from.
.le
.ls len (I: int)
Length of the correlation functions.
.le
.ls n (I: int)
The number of correlation functions present.
.le
.ls initial (I: double)
The refined shift as determined by some other means, such as
pof_maxcorr.
.le
.ls guess (I: int)
The initial guess of the shift as determined by wavelength scales.
.le
.ls file (I: char[ARB])
The current file which a shift is being found for.
.le
.ls group (I: int)
The group of the file.
.le
.ih
DESCRIPTION
Each correlation function is drawn on the graphics device.  If values
for the initial and guess shifts are given (I.e. not INDEF), those
locations are drawn.  Then using keys, as described below, the user
can modify where the shift actually lies.  Once an integral shift is
determined, it is computed to subpixel precision using quadratic
refinement.  The shifts for each function are then averaged together
to determine the final shift.

The following key commands are available to change the shift
calculated:

.ls ?
Help:  List the available cursor key commands.
.le
.ls d
Delete Shift:  This key undefines the shift for the current
correlation.  If the correlation is one of many sections for the
current 
spectrum, it will not be included in the final shift average.  If this
is the only section for a spectrum, no shift will be output for the
spectrum.  An informational message will be displayed indicating that
no shift for this section is present.
.le
.ls m
Mark New Shift:  The shift is set to the X-position of the graphics
cursor, with quadratic refinement.  A line will appear, called
"current" at the new location of the current shift.
.le
.ls n
Go To Next Section:  If there is more than one section specified for
the current spectrum, 
this key will go to the next section.  If there is only one section
(the default), this key has no effect.
.le
.ls p
Go To Previous Section:  If there is more than one section specified
for the current spectrum, this key will go to the previous section.
If there is only one section (the default), this key has no effect.
.le
.ls q or EOF character
Go To The Next Correlation:  If there are more sections for the
current spectrum, the next section correlation is displayed.  If there
are no more sections, the first section of the next spectrum is
displayed.  If all the spectra have been processed, the task ends,
writing the shifts to the output table.
.le
.ls r
Restore Original Shift:  Resets the shift for the current correlation
to the same value as was originally calculated, i.e. the shift marked
by the line labeled "initial".
.le
.ls s
Show Cursor Coordinates:  This just echoes the current location of the
graphics cursor.  There is no effect on the calculated shifts.
.le
.endhelp
#---------------------------------------------------------------------------
double procedure pof_int (gp, corr, len, n, initial, guess, file, group)

pointer gp                      # I:  Graphics descriptor.
double  corr[len,n]             # I:  The correlations.
int     len                     # I:  Size of each correlation.
int     n                       # I:  How many correlations there are.
double  initial                 # I:  Initial shift.
int     guess                   # I:  Guess of where the shift is.
char    file[ARB]               # I:  The file being worked on.
int     group                   # I:  Group of the file working on.

# Shifts.
int     cs                      # Current correlation section working.
int     hlen                    # Half size of correlation arrays.
real    rinit                   # Real version of initial parameter.
double  shift                   # The final shift.
pointer shifts                  # User defined shifts.

# Graphics.
int     clgcur()                # Read gaphics cursor.
real    cx, cy                  # Cursor position.
bool    redraw                  # Redraw the plot.
int     wcs                     # Graphics wcs.

# Misc.
int     i, ix                   # Generic.
int     key                     # Key hit by cursor.
real    rx                      # Generic.
pointer sp                      # Stack pointer.
pointer sx                      # Generic string.

begin
        call smark (sp)
        call salloc (sx, max (SZ_LINE, SZ_PATHNAME), TY_CHAR)

        # Get half len of the arrays.
        hlen = len / 2
        
        # Convert to real.
        if (IS_INDEFD (initial))
            rinit = INDEFR
        else
            rinit = initial
        
        # Activate the workstation.
        call greactivate (gp, 0)

        # Initialize for new shifts.
        call salloc (shifts, n, TY_REAL)
        do i = 1, n
            Shifts(i) = rinit
        
        # Interact.
        cs = 0
        key = 'n'
        repeat {
            switch (key) {
            case '?':
                call gpagefile (gp, "ctools$doc/poffsets.key", NULL)
                
            case 'd':       # Undefine the shift for this section.
                Shifts(cs) = INDEFR
                redraw = true

            case 'm':       # Mark new shift
                ix = nint (hlen + 1 - cx)
                ix = max (1, ix)
                ix = min (len, ix)
                if (ix > 1 && ix < len) {
                    Shifts(cs) = ((corr[ix-1,cs] - corr[ix,cs]) /
                                  (corr[ix-1,cs] + corr[ix+1,cs] -
                                   2.d0 * corr[ix,cs])) - 0.5d0
                    Shifts(cs) = hlen - ix - Shifts(cs) + 1.d0
                } else
                    Shifts(cs) = cx
                
                redraw = true
                
            case 'n':       # Go to next section.
                cs = min (n, cs+1)
                redraw = true
                
            case 'p':       # Go to previous section.
                cs = max (1, cs - 1)
                redraw = true
                
            case 'q':       # Go to next section.  If last section, break out.
                if (cs == n)
                    break
                cs = min (n, cs+1)
                redraw = true
                
            case 'r':       # Restore to original.
                Shifts(cs) = rinit
                redraw = true
                
            case 's':       # Show point
                call printf (" cx = %g cy = %g\n")
                call pargr (cx)
                call pargr (cy)
                
            default:    # Beep 'em.
                call eprintf ("\007")
            }       

            # Redraw the plot.
            if (redraw)
                call pof_plot (gp, cs, corr[1,cs], len,
                               Shifts(cs), rinit, guess, file, group)
            redraw = false
            
        } until (clgcur ("cursor", cx, cy, wcs, key, Memc[sx], SZ_LINE) == EOF)

        # Recalculate the shift.
        ix = 0
        rx = 0.
        do i = 1, n
            if (abs (Shifts(i)) < len) {
                ix = ix + 1
                rx = rx + Shifts(i)
            }
        if (ix <= 0)
            shift = INDEFD
        else
            shift = rx / ix
        
        # That's all folks.
        call gdeactivate (gp, 0)
        call sfree (sp)
        return (shift)
end
#---------------------------------------------------------------------------
# End of pof_int
#---------------------------------------------------------------------------
procedure pof_plot (gp, section, corr, len, cshift, ishift, guess, file, group)

pointer gp                      # I:  Graphics descrtiptor.
int     section                 # I:  Correlation section.
double  corr[len]               # I:  Correlation.
int     len                     # I:  Length of correlation.
real    cshift                  # I:  Currently defined shift.
real    ishift                  # I:  Initial shift.
int     guess                   # I:  Guess.
char    file[ARB]               # I:  The file.
int     group                   # I:  Group of the file.

# Misc.
pointer c                       # Real version of correlation.
int     hcx                     # Half point of correlation array.
real    rx                      # Generic.
pointer sp                      # Stack pointer.
pointer sx                      # Generic string.

begin
        call smark (sp)
        call gclear (gp)

        # Convert to real data.
        call salloc (c, len, TY_REAL)
        call achtdr (corr, Memr[c], len)

        # Determine the limits to the shift.
        rx = len / 2
        hcx = int (rx) + 1

        # Create the title.
        call salloc (sx, SZ_LINE, TY_CHAR)
        call sprintf (Memc[sx], SZ_LINE, "%s[%d] correlation section %d")
        call pargstr (file)
        call pargi (group)
        call pargi (section)
        
        # Plot the data.
        call gploto (gp, Memr[c], len, rx, -rx, Memc[sx])

        # Draw the guess.
        if (!IS_INDEFI(guess))
            call pof_pshift (gp, real (guess), Memr[c+hcx-guess-1], "guess",
                             GUESS_SIZE)

        # Draw the initial.
        if (!IS_INDEFR(ishift))
            call pof_pshift (gp, ishift, Memr[c+hcx-int (ishift)-1], 
                             "initial", INITIAL_SIZE)

        # Draw the current.
        if (!IS_INDEFR(cshift))
            call pof_pshift (gp, cshift, Memr[c+hcx-int (cshift)-1], "current",
                             CURRENT_SIZE)
        else
            call printf ("WARNING: NO SHIFT CURRENTLY DEFINED FOR THIS CORRELATION")
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of pof_plot
#---------------------------------------------------------------------------
procedure pof_pshift (gp, x, y, label, size)

pointer gp                      # I:  Graphics descriptor.
real    x, y                    # I:  Position of point.
char    label[ARB]              # I:  Label of the point.
real    size                    # I:  Size of the line to draw.

char    just[SZ_JUST]           # Text justification.
real    ty                      # Y-pos of the label.
real    wx1, wx2, wy1, wy2      # Edges of the graph.
real    y_sz                    # Y-extent of the graph.
real    y1, y2                  # End y-points of the marking line.

begin
        # Calculate extent of graph.
        call ggwind (gp, wx1, wx2, wy1, wy2)
        y_sz = wy2 - wy1

        # Draw line according to which half of the graph we are on.
        if (y > (wy1+wy2)/2.) {
            y2 = y - (DISTANCE * y_sz)
            y1 = y2 - (size * y_sz)
            ty = y1 
            call strcpy ("v=t", just, SZ_JUST)
        } else {
            y1 = y + (DISTANCE * y_sz)
            y2 = y1 + (size * y_sz)
            ty = y2
            call strcpy ("v=b", just, SZ_JUST)
       }

        # Draw the line
        call gline (gp, x, y1, x, y2)
        
        # Determine text justification depending on what half of the graph.
        if (x > (wx1+wx2)/2.)
            call strcat (";h=l", just, SZ_JUST)
        else
            call strcat (";h=r", just, SZ_JUST)

        # Write the text.
        call gtext (gp, x, ty, label, just)
end
#---------------------------------------------------------------------------
# End of pof_pshift
#---------------------------------------------------------------------------
