include <math.h>
include "sphere.h"

#---------------------------------------------------------------------------
.help sphere  Jun93   mwcs
.ih
NAME
spt_init[r|d]           -- Initialize the spherical transformation structure.
spt_forward[r|d]        -- Transform from initial system to new system.
spt_backward[r|d]       -- Transform from new system to initial system.
spt_close               -- Deallocate spherical transformation structure.
spt_sphere2cart         -- Convert spherical to cartesian.
spt_cart2sphere         -- Convert cartesian to spherical.
.endhelp
#---------------------------------------------------------------------------

pointer procedure spt_initd (thetax, thetay, thetaz)

double  thetax, thetay, thetaz  # I:  Rotation to be applied (degrees)

# Declarations.
real    radx, rady, radz        # Rotation angles in radians.

pointer center                  # Center of rotation.
pointer sp                      # Stack pointer.
pointer spt_ptr                 # Spherical transformation descriptor.

string  logical         "logical"
string  physical        "physical"

# Function prototypes.
pointer mw_open(), mw_sctran()

begin
        # Allocate the descriptor.
        call malloc (spt_ptr, SZ_SPT_DESCRIPTOR, TY_STRUCT)
        call malloc (SPT_IN_PTR(spt_ptr), SZ_VECTOR, TY_DOUBLE)
        call malloc (SPT_OUT_PTR(spt_ptr), SZ_VECTOR, TY_DOUBLE)

        # Make other temporary storage.
        call smark (sp)
        call salloc (center, SZ_VECTOR, TY_REAL)

        # Convert degrees to radians.
        radx = DEGTORAD(thetax)
        rady = DEGTORAD(thetay)
        radz = DEGTORAD(thetaz)

        # Create the mwcs structure.
        SPT_MWCS(spt_ptr) = mw_open (NULL, SZ_VECTOR)

        # Setup the MWCS transformations.
        Memr[center] = 0.
        Memr[center+1] = 0.
        Memr[center+2] = 0.
        call mw_rotate (SPT_MWCS(spt_ptr), radx, Memr[center], 6b) 
        call mw_rotate (SPT_MWCS(spt_ptr), rady, Memr[center], 5b) 
        call mw_rotate (SPT_MWCS(spt_ptr), radz, Memr[center], 3b)
        
        SPT_FOR_CT(spt_ptr) = mw_sctran (SPT_MWCS(spt_ptr),
                                         physical, logical, 7b)
        SPT_BACK_CT(spt_ptr) = mw_sctran (SPT_MWCS(spt_ptr),
                                          logical, physical, 7b)

        # That's all folks
        call sfree (sp)
        
        return (spt_ptr)
end
#---------------------------------------------------------------------------
# End of spt_initd.
#---------------------------------------------------------------------------
procedure spt_close (spt_ptr)

pointer spt_ptr                 # IO: Spherical transformation descriptor.

begin
        call mw_close (SPT_MWCS(spt_ptr))
        call mfree (SPT_IN_PTR(spt_ptr), TY_DOUBLE)
        call mfree (SPT_OUT_PTR(spt_ptr), TY_DOUBLE)
        call mfree (spt_ptr, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of spt_close
#---------------------------------------------------------------------------
procedure spt_forwardd ( spt_ptr, x, y, new_x, new_y)

pointer spt_ptr                 # I:  Spherical transformation descriptor.
double  x, y                    # I:  The input coordinates (degrees)
double  new_x, new_y            # O:  The output coordinates (degrees)

# Declarations.
double  radx, rady              # Radian version of the coordinates.

begin

        # Convert to radians.
        radx = DEGTORAD(x)
        rady = DEGTORAD(y)

        # Convert to cartesian coordinates.
        call spt_sphere2cart (radx, rady, SPT_IN_VECTOR(spt_ptr))

        # Perform the forward transformation.
        call mw_ctrand (SPT_FOR_CT(spt_ptr), SPT_IN_VECTOR(spt_ptr),
                     SPT_OUT_VECTOR(spt_ptr), SZ_VECTOR)

        # Convert to spherical coordinates.
        call spt_cart2sphere (SPT_OUT_VECTOR(spt_ptr), radx, rady)

        # Convert back to degrees.
        new_x = RADTODEG(radx)
        new_y = RADTODEG(rady)

        # That's all folks.
end
#---------------------------------------------------------------------------
# End of spt_forwardd
#---------------------------------------------------------------------------
procedure spt_backwardd ( spt_ptr, x, y, new_x, new_y)

pointer spt_ptr                 # I:  Spherical transformation descriptor.
double  x, y                    # I:  The input coordinates (degrees)
double  new_x, new_y            # O:  The output coordinates (degrees)

# Declarations.
double  radx, rady              # Radian version of the coordinates.

begin

        # Convert to radians.
        radx = DEGTORAD(x)
        rady = DEGTORAD(y)

        # Convert to cartesian coordinates.
        call spt_sphere2cart (radx, rady, SPT_IN_VECTOR(spt_ptr))

        # Perform the backward transformation.
        call mw_ctrand (SPT_BACK_CT(spt_ptr), SPT_IN_VECTOR(spt_ptr),
                     SPT_OUT_VECTOR(spt_ptr), SZ_VECTOR)

        # Convert to spherical coordinates.
        call spt_cart2sphere (SPT_OUT_VECTOR(spt_ptr), radx, rady)

        # Convert back to degrees.
        new_x = RADTODEG(radx)
        new_y = RADTODEG(rady)

        # That's all folks.
end
#---------------------------------------------------------------------------
# End of spt_backwardd
#---------------------------------------------------------------------------
procedure spt_sphere2cart (x, y, v)

double  x, y            # I:  The spherical coordinates (radians)
double  v[3]            # O:  The cartesian coordinates vector.

begin
        v[1] = cos (y) * cos (x)
        v[2] = cos (y) * sin (x)
        v[3] = sin (y)
end
#---------------------------------------------------------------------------
# End of spt_sphere2cart
#---------------------------------------------------------------------------

procedure spt_cart2sphere (v, x, y)

double  v[3]                    # I:  The cartesian vector.
double  x, y                    # O:  The spherical coordinates (radians)

begin
        x = atan2 (v[2], v[1])
        y = asin (v[3])
end
#---------------------------------------------------------------------------
# End of spt_cart2sphere
#---------------------------------------------------------------------------
pointer procedure spt_initr (thetax, thetay, thetaz)

real    thetax, thetay, thetaz  # I:  Rotation between coordinate systems.

# Declarations.
double  dx, dy, dz              # Double version of the angles.

# Function prototypes.
pointer spt_initd()

begin
        dx = thetax
        dy = thetay
        dz = thetaz

        return (spt_initd (dx, dy, dz))
end
#---------------------------------------------------------------------------
# End of spt_intir
#---------------------------------------------------------------------------
procedure spt_forwardr (spt_ptr, x, y, new_x, new_y)

pointer spt_ptr                 # I:  Transformation descriptor.
real    x, y                    # I:  Input coordinates (degrees)
real    new_x, new_y            # O:  Output coordinates (degrees)

# Declarations
double  dx, dy, dnew_x, dnew_y  # Double versions.

begin
        dx = x
        dy = y
        call spt_forwardd (spt_ptr, dx, dy, dnew_x, dnew_y)
        new_x = dnew_x
        new_y = dnew_y
end
#---------------------------------------------------------------------------
# End of spt_forwardr
#---------------------------------------------------------------------------
procedure spt_backwardr (spt_ptr, x, y, new_x, new_y)

pointer spt_ptr                 # I:  Transformation descriptor.
real    x, y                    # I:  Input coordinates (degrees).
real    new_x, new_y            # O:  Output coordinates (degrees).

# Declarations
double  dx, dy, dnew_x, dnew_y  # Double versions.

begin
        dx = x
        dy = y
        call spt_backwardd (spt_ptr, dx, dy, dnew_x, dnew_y)
        new_x = dnew_x
        new_y = dnew_y
end
#---------------------------------------------------------------------------
# End of spt_backwardr
#---------------------------------------------------------------------------
