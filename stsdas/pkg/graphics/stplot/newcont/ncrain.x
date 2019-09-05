include <psiescape.h>

# Define extent of colors.
define  MINCOLOR        240
define  MAXCOLOR        0.

#---------------------------------------------------------------------------
procedure nc_rain (gp, n, colors)

pointer gp                      # I:  The graphics device.
int     n                       # I:  Number of colors to define.
int     colors[n]               # O:  The colors LUT index.

# Declarations.
real    delta                   # Change in HSV hue.
real    hue                     # Current HSV hue.
real    red, green, blue        # Red, green, blue components.

int     i                       # Generic.

pointer r, g, b                 # Color arrays in RGB space.
pointer sp                      # Stack pointer.

begin
        # In HSV space, we will make a rainbow from RED (0) to BLUE (240)
        # These colors will be converted to RGB and the output device
        # will be informed of the changes.

        call smark(sp)
        call salloc(r, n+2, TY_SHORT)
        call salloc(g, n+2, TY_SHORT)
        call salloc(b, n+2, TY_SHORT)

        # Define black and white.
        Mems[r] = 0
        Mems[g] = 0
        Mems[b] = 0
        Mems[r+1] = 255
        Mems[g+1] = 255
        Mems[b+1] = 255
        
        # Go around the HSV space from red to blue in appropriate steps.
        delta = (MAXCOLOR - MINCOLOR) / (n-1)
        hue = MINCOLOR
        do i = 1, n {
            call hsv_to_rgb (hue, 1., 1., red, green, blue)
            Mems[r+i+1] = int (red * 255.)
            Mems[g+i+1] = int (green * 255.)
            Mems[b+i+1] = int (blue * 255.)
            colors[i] = i+2
            hue = hue + delta
        }

        # Set the graphics device graphics LUT to the colors.
        call gescape (gp, PS_GR_RED_LUT, Mems[r], n+2)
        call gescape (gp, PS_GR_GREEN_LUT, Mems[g], n+2)
        call gescape (gp, PS_GR_BLUE_LUT, Mems[b], n+2)

        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of nc_rain
#---------------------------------------------------------------------------
procedure hsv_to_rgb (ih, is, iv, r, g, b)

real    ih              # I:  The hue in HSV space (degrees)
real    is              # I:  The saturation in HSV space (0 to 1. inclusive).
real    iv              # I:  The value in HSV space (0 to 1. inclusive).
real    r               # O:  The red in RGB space.
real    g               # O:  The green in RGB space.
real    b               # O:  The blue in RGB space.

# Declarations
real    f, p, q, t      # Conversions.
real    h, s, v         # Temporary values for these.

int     i               # Generic.

begin
        h = ih
        s = is
        v = iv

        # hue is in degrees but can be any order.  Bring it into 0 to 360.
        while (h < 0.)
            h = h + 360.
        while (h > 360.)
            h = h - 360.
        
        if (s == 0.){
            r = v
            g = v
            g = v
        } else {
            if (h == 360.)
                h = 0.
            h = h / 60.
            i = int (h)
            f = h - i
            p = v*(1.-s)
            q = v*(1.-(s*f))
            t = v*(1.-(s*(1.-f)))
            switch (i) {
            case 0:
                r = v; g = t; b = p
            case 1:
                r = q; g = v; b = p
            case 2:
                r = p; g = v; b = t
            case 3:
                r = p; g = q; b = v
            case 4:
                r = t; g = p; b = v
            case 5:
                r = v; g = p; b = q
            }       
        }
end
#---------------------------------------------------------------------------
# End of hsv_to_rgb
#---------------------------------------------------------------------------
