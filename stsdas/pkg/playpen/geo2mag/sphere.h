#---------------------------------------------------------------------------
.help   sphere.h        Jun92   mwcs
.ih
NAME
sphere.h        -- Descpriptor for the spherical transformation routines.
.endhelp
#---------------------------------------------------------------------------

define  SPT_MWCS        Memi($1)        # MWCS descriptor.
define  SPT_FOR_CT      Memi($1+1)      # MWCS forward transformation.
define  SPT_BACK_CT     Memi($1+2)      # MWCS backward transformation.
define  SPT_IN_PTR      Memi($1+3)      # Input cartesian vector.
define  SPT_IN_VECTOR   Memd(SPT_IN_PTR($1))
define  SPT_OUT_PTR     Memi($1+4)      # Output cartesian vector.
define  SPT_OUT_VECTOR  Memd(SPT_OUT_PTR($1))

define  SZ_SPT_DESCRIPTOR       5

# Define the size of the vectors.  This is a three dimensional transformation.
define  SZ_VECTOR       3
#---------------------------------------------------------------------------
# End of sphere.h
#---------------------------------------------------------------------------
