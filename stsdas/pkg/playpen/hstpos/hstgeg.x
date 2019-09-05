include <gset.h>

# Define the rotations between geocentric and geomagnetic systems.
define  THETAX  11.4
define  THETAY  0.
define  THETAZ  19.3

# Define the longitude parameters.
define  LNG_INT 1.
define  LNG_MIN (-180.)
define  LNG_MAX 180.

#---------------------------------------------------------------------------
.help hst_geomag Jun92 stplot
.ih
NAME
hst_geomag -- Draw lines of constant latitude for geomagnetic system.
.endhelp
#---------------------------------------------------------------------------
procedure hst_geomag (gp, latint, n_latitudes)

pointer gp                      # I:  Graphics descriptor.
real    latint                  # I:  Latitude interval.
int     n_latitudes             # I:  Number of north/south latitudes.

# Declarations.
real    gx, gy                  # Geocentric coordinates.
real    lat                     # Current latitude.
real    lng                     # Current longitude.
real    max_lat, min_lat        # Maximum/minimum latitudes.
real    thetax, thetay, thetaz  # Rotation of coordinate systems.

int     i                       # Index
int     n_lng                   # Number of longitude intervals.
int     old_type                # Remember old line type.

pointer geo_trans               # Spherical transformation descriptor.
pointer sp                      # Stack pointer.
pointer x, y                    # Graphics coordinates.

# Function prototypes.
int     gstati()
pointer spt_initr()

begin
        n_lng = (LNG_MAX - LNG_MIN) / LNG_INT
        n_lng = n_lng - 1
        call smark (sp)
        call salloc (x, n_lng, TY_REAL)
        call salloc (y, n_lng, TY_REAL)
        
        # Define the coordinate transformation.
        thetax = THETAX
        thetay = THETAY
        thetaz = THETAZ
        geo_trans = spt_initr (thetax, thetay, thetaz)

        # Set line type to dashed.
        old_type = gstati (gp, G_PLTYPE)
        call gseti (gp, G_PLTYPE, GL_DASHED)
        
        # Determine range of latitude.
        max_lat = max ((n_latitudes - 1) * latint, 0.)
        min_lat = -1. * max_lat

        # Draw the lines of constant latitude.
        for (lat = min_lat; lat <= max_lat; lat = lat + latint) {
            do i = 0, n_lng-1 {
                lng = LNG_MIN + (LNG_INT * i)
                call spt_backwardr (geo_trans, lng, lat, gx, gy)
                call mwcart (gx, gy, Memr[x+i], Memr[y+i])
            }
            call hst_isort (Memr[x], Memr[y], n_lng)
            call gpline (gp, Memr[x], Memr[y], n_lng)
        }

        # That's all folks.
        call gseti (gp, G_PLTYPE, old_type)
        call spt_close (geo_trans)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of hst_geomag
#---------------------------------------------------------------------------
