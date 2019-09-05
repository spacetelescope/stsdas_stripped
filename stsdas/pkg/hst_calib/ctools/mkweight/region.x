include <gset.h>
include <mach.h>
include <plset.h>

define  ROP     PIX_SRC

# The region descriptor.
define  RL_MAX_N        Memi[$1]                # Max regions.
define  RL_PTR          Memi[($1)+1]            # The region array.
define  RL_RS           Memi[RL_PTR($1)+(3*($2))] # Start of the region.
define  RL_RN           Memi[RL_PTR($1)+(3*($2))+1] # Number of points in region.
define  RL_RV           Memi[RL_PTR($1)+(3*($2))+2] # Value of the region.
define  RL_N            Memi[RL_PTR($1)]        # Number of regions defined + 1.
define  RL_LEN          Memi[RL_PTR($1)+1]      # Size of the whole region.
define  RL_VAL          Memi[RL_PTR($1)+2]      # Maximum value of a region.
define  RL_SZ           2                       # Size of the structure.

# Rate of growth of the region descriptor.
define  RL_GROW         20

# What value a region is marked with.
define  RL_VALUE        1

#---------------------------------------------------------------------------
.help region May93 source
.ih
NAME
.nf
ma_open_region   -- Open a region list.
ma_close_region  -- Close a region list.
ma_delete_region -- Delete all regions containing specified point.
ma_draw_all      -- Draw all regions.
ma_add_region    -- Add a region (range).
ma_draw_region   -- Draw a region.
ma_r2pl          -- Stuff the regions into a pixel list.
.endhelp
#---------------------------------------------------------------------------
pointer procedure ma_open_region (len)

int     len                     # I:  Length of the region.

pointer rl                      # Region descriptor.

errchk  malloc

begin
        call malloc (rl, RL_SZ, TY_STRUCT)
        RL_MAX_N(rl) = RL_GROW
        call malloc (RL_PTR(rl), 3*RL_MAX_N(rl), TY_INT)
        RL_N(rl) = 0
        RL_LEN(rl) = len
        RL_VAL(rl) = MAX_INT

        return (rl)
end
#---------------------------------------------------------------------------
# End of ma_open_region
#---------------------------------------------------------------------------
procedure ma_delete_region (rl, x)

pointer rl                      # I:  The region descriptor.
real    x                       # I:  The point.

int     i                       # Current region.
int     ix                      # Integer version of x.
int     n                       # New number of regions.

begin
        # Since all things are actually integers, change the point to one.
        ix = x

        # Go through all regions, removing any that contain the point.
        n = 0
        do i = 1, RL_N(rl) {
            if (ix < RL_RS(rl,i) || ix > RL_RS(rl,i) + RL_RN(rl,i) - 1) {
                n = n + 1
                if (n < i) {
                    RL_RS(rl,n) = RL_RS(rl,i)
                    RL_RN(rl,n) = RL_RN(rl,i)
                    RL_RV(rl,n) = RL_RV(rl,i)
                }
            }
        }
        RL_N(rl) = n
end
#---------------------------------------------------------------------------
# End of ma_delete_region
#---------------------------------------------------------------------------
procedure ma_draw_all (rl, gp, y, mtype, msize)

pointer rl                      # I:  Region descriptor.
pointer gp                      # I:  Graphics descriptor.
real    y                       # I:  Verticle position of markers.
int     mtype                   # I:  Marker type.
real    msize                   # I:  Marker size.

int     i                       # Generic.
begin
        do i = 1, RL_N(rl)
            call ma_draw_region (gp, real (RL_RS(rl,i)),
                                 real (RL_RS(rl,i) + RL_RN(rl,i) - 1),
                                 y, mtype, msize)
end
#---------------------------------------------------------------------------
# End of ma_draw_all
#---------------------------------------------------------------------------
procedure ma_add_region (rl, s, e)

pointer rl                      # I:  Region descriptor.
real    s                       # I:  Start of region.
real    e                       # I:  End of region.

int     i, j                    # Generic.
int     is                      # Integer version of s.
int     n                       # Number of points in the region.

begin
        # Convert to integers and determine range.
        is = min (s, e)
        n = max (s, e)
        n = n - is + 1

        # See if there is some room.  If not, make some.
        if (RL_N(rl) == RL_MAX_N(rl)) {
            RL_MAX_N(rl) = RL_MAX_N(rl) + RL_GROW
            call realloc (RL_PTR(rl), 3*RL_MAX_N(rl), TY_INT)
        }

        # The table has to be in increasing order on the start of the
        # range.  Find where this range should go.
        i = 1
        while ( i <= RL_N(rl)) {
            if (is < RL_RS(rl,i))
                break
            i = i + 1
        } 
        
        # Back all the later ranges.
        do j = RL_N(rl), i, -1 {
            RL_RS(rl,j+1) = RL_RS(rl,j)
            RL_RN(rl,j+1) = RL_RN(rl,j)
            RL_RV(rl,j+1) = RL_RV(rl,j)
        }

        # Put in the new one.
        RL_RS(rl,i) = is
        RL_RN(rl,i) = n
        RL_RV(rl,i) = RL_VALUE
        RL_N(rl) = RL_N(rl) + 1
end
#---------------------------------------------------------------------------
# End of ma_add_region
#---------------------------------------------------------------------------
procedure ma_draw_region (gp, s, e, y, mtype, msize)

pointer gp                      # I:  Graphics descriptor.
real    s                       # I:  Start of region.
real    e                       # I:  End of region.
real    y                       # I:  Y coordinate of the line.
int     mtype                   # I:  The marker type.
real    msize                   # I:  Size of marker.

bool    fp_equalr()             # Are floating point values equal?

begin

        # If the region represents a single point, draw a mark.
        if (fp_equalr (s, e))
            call gmark (gp, s, y, mtype, msize, msize)

        # Nope, this is an extended region.  Draw a line.
        else {
            call gmark (gp, s, y, GM_VLINE, msize, msize)
            call gmark (gp, e, y, GM_VLINE, msize, msize)
            call gline (gp, s, y, e, y)
        }
end
#---------------------------------------------------------------------------
# End of ma_draw_region
#---------------------------------------------------------------------------
procedure ma_r2pl (rl, pl)

pointer rl                      # I:  Region descriptor.
pointer pl                      # I:  Pixel list descriptor.

begin
        # Fix the region count for the pixel list.
        RL_N(rl) = RL_N(rl) + 1
        
        # Since the array storing the region is what the pixel list
        # expects, we just have to stuff it.
        call pl_plri (pl, 1, Memi[RL_PTR(rl)], 1, RL_LEN(rl), ROP)

        # Fix the region count back.
        RL_N(rl) = RL_N(rl) - 1
end
#---------------------------------------------------------------------------
# End of ma_r2pl
#---------------------------------------------------------------------------
procedure ma_close_region (rl)

pointer rl                      # IO: Region descriptor, NULL on return.

begin
        call mfree (RL_PTR(rl), TY_INT)
        call mfree (rl, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of ma_close_region
#---------------------------------------------------------------------------
procedure ma_pl2r (pl, rl, line)

pointer pl                      # I:  Pixel list descriptor.
pointer rl                      # I:  Region descriptor.
int     line                    # I:  The line of the pixel list to save.

int     len                     # Size of a pixel list line.
long    v[2]                    # Vector of pixel list to modify.

begin
        # Delete the old array.  Since we don't know how many regions
        # there may be, allocate enough for one region per pixel.
        len = RL_LEN(rl)
        call mfree (RL_PTR(rl), TY_INT)
        call malloc (RL_PTR(rl), 3*len, TY_INT)
        
        # Since the array storing the region is what the pixel list
        # expects, we just have to get it.
        v[1] = 1
        v[2] = line
        call pl_glri (pl, v, Memi[RL_PTR(rl)], 1, len, ROP)

        # Fix the region count back.
        RL_N(rl) = RL_N(rl) - 1
end
#---------------------------------------------------------------------------
# End of ma_pl2r
#---------------------------------------------------------------------------
procedure ma_clear_region (rl)

pointer rl                      # I: Region descriptor.

begin
        RL_N(rl) = 0
end
#---------------------------------------------------------------------------
# End of ma_clear_region
#---------------------------------------------------------------------------
