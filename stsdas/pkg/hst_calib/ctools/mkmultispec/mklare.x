include <imhdr.h>
include <imio.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mk_large Mar93 source
.ih
NAME
mk_large -- Determine the "largest" MWCS descriptor.
.ih
USAGE
call mk_large (fit)
.ih
ARGUMENTS
.ls fit (pointer :input)
The FIT descriptor.
.le
.ih
DESCRIPTION
For multigroup images, the MULTISPEC keywords become group parameters.
Since these parameters must be preallocated for all the groups, and
each group may have different fits, one needs to determine which fit
will take the most parameters.  This is then used to create the parameters.

For the TABLE version of MULTISPEC, the number of coefficients is just
the size of the fit arrays.
.endhelp
#---------------------------------------------------------------------------
procedure mk_large (fit)

pointer fit                     # I:  The FIT descriptor.

int     i                       # Generic.
pointer im                      # Temporary image descriptor.
int     imaccf()                # YES if the header parameter exists.
pointer image                   # Temporary image file name.
pointer immap()                 # Open an image.
pointer impl1i()                # Put a line of data.
int     n                       # How many WAT2_XXX header parameters.
pointer sp                      # Stack Pointer.
pointer xstr                    # Generic.

begin
        call smark (sp)
        call salloc (image, SZ_PATHNAME, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)
        
        # Create the image name.
        call mktemp ("tmp$mkmsimage", Memc[image], SZ_PATHNAME)
        im = immap (Memc[image], NEW_IMAGE, 0)
        IM_NDIM(im) = 1
        IM_LEN(im,1) = 1
        call amovi (1, impl1i (im), 1)

        # Look for the most MULTISPEC keywords.
        FIT_LARGE(fit) = 0
        do i = 1, FIT_N_MW(fit) {

            # Save the MWCS in the image.
            IM_HDRLEN(im) = IM_LENHDRMEM(im)
            call mw_saveim (FIT_MW(fit,i), im)

            # The primary concern is how many WAT2_XXX keywords there
            # there are.  The LARGE value will be the one with the
            # largest number.
            n = 0
            while (true) {
                call sprintf (Memc[xstr], SZ_LINE, "WAT2_%03.3d")
                call pargi (n+1)
                if (imaccf (im, Memc[xstr]) == NO)
                    break
                n = n + 1
            }

            # Check whether this is the largest and close the image.
            if (FIT_LARGE(fit) < n)
                FIT_LARGE(fit) = n
        }
        
        # That's all folks.
        call imunmap (im)
        iferr (call imdelete (Memc[image]))
            ;
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of mk_large
#---------------------------------------------------------------------------
