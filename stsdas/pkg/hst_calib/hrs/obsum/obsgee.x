include <imio.h>

#---------------------------------------------------------------------------
.help obs_get_time Jun92 source
.ih
NAME
obs_get_time -- Get the time for the next group of the given image.
.ih
USAGE
call obs_get_time (im, group, time)
.ih
ARGUMENTS
.fs im (pointer)
I:  The image descriptor.  If NULL, then return an undefined time.
.fe
.fs group (int)
IO:  The current group.
.fe
.fs time (double)
O:  The packet time of the group.  If no image is passed, or the last group
has been done, time will be INDEF.
.fe
.endhelp
#---------------------------------------------------------------------------
procedure obs_get_time (im, group, time)

pointer im                      # I:  The image descriptor.
int     group                   # IO: The current group.
double  time                    # O:  The packet time.

# Declarations.
real    minimum, maximum        # Minimum, maximum of current group.

# Function prototypes.
double  imgetd()

begin
        # If no image, return undefined time.
        if (im == NULL)
            time = INDEFD

        # If all groups have been processed, return undefined time.
        else if ( group >= IM_CLSIZE(im))
            time = INDEFD

        # Get the packet time of next group.
        else {
            group = group + 1
            call gf_opengr (im, group, minimum, maximum, 0)
            time = imgetd (im, "PKTTIME")
        }
        
end
#---------------------------------------------------------------------------
# End of obs_get_time
#---------------------------------------------------------------------------
