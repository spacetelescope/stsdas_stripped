include <gset.h>
include <imhdr.h>
include <plset.h>
include <tbset.h>

# GIO marker types.
define  GIO_MARKER_DICT "|point|fill|box|plus|cross|diamond|hline|vline|hebar|vebar|circle"
define  POINT                   1
define  FILL                    2
define  BOX                     3
define  PLUS                    4
define  CROSS                   5
define  DIAMOND                 6
define  HLINE                   7
define  VLINE                   8
define  HEBAR                   9
define  VEBAR                   10
define  CIRCLE                  11

#---------------------------------------------------------------------------
.help t_mkweight Jun93 source
.ih
NAME
t_mkweight -- Make simple binary weighting file.
.endhelp
#---------------------------------------------------------------------------
procedure t_mkweight

# Image IO.
pointer imgl1r()                # Retrieve a line from an image.
pointer immap()                 # Open an image.
pointer impl1s()                # Put image data short.
int     imtgetim()              # Get next file name from image.list.
int     imtlen()                # Lenght of image list.
pointer imtopen()               # Open an image name list.
pointer in_data                 # Input image contents.
pointer in_file                 # Name of current input file.
pointer in_grp_list             # Image group name list.
pointer in_list                 # Input file name list.
pointer input                   # Input image descriptor.
int     n_groups                # Number of groups for current input file.
pointer out_data                # Output data.
pointer out_grp_list            # Output image group name list.
pointer out_list                # Output file list.
pointer output                  # Output image descriptor.
bool    tp_fetch()              # Get next image group name.
pointer tp_open()               # Open an image group name list.

# Graphics.
int     clgcur()                # Read cursor.
pointer gopen()                 # Open the graphics device.
pointer gp                      # Graphics descriptor.
int     key                     # Key that was hit.
int     mdict2gio()             # Convert marker types.
real    msize                   # Marker size.
int     mtype                   # Marker type.
int     wcs                     # Current graphics wcs.
real    y                       # Y-position of markers.

# Ranges
real    b                       # Beginning of a range.
bool    interactive             # TRUE to get regions from screen.
int     len                     # Length of the pixel list.
pointer ma_open_region()        # Open a region list.
pointer pl                      # Pixel list descriptor.
pointer pl_create()             # Create a new pixel list.
pointer rl                      # Region descriptor.

# Misc.
bool    bx                      # Generic.
int     checkdim()              # Get dimension of input data.
real    clgetr()                # Get real value parameter.
int     clgwrd()                # Get dictionary word from parameters.
int     i                       # Generic.
pointer ones                    # An array of ones to xor with the output.
real    rx, ry, ry2             # Generic.
short   shx                     # Generic.
pointer sp                      # Stack pointer.
pointer sx, sy                  # Generic string.

begin
        call smark (sp)
        call salloc (in_file, SZ_PATHNAME, TY_CHAR)
        call salloc (sx, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)
        call salloc (sy, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)

        # Get the input image list.
        call clgstr ("input", Memc[sx], SZ_PATHNAME)
        in_list = imtopen (Memc[sx])
        if (imtlen (in_list) <= 0)
            call error (1, "mkweight: No input files specified")

        # Get the output image list.
        call clgstr ("output", Memc[sx], SZ_PATHNAME)
        out_list = imtopen (Memc[sx])
        if (imtlen (out_list) != imtlen (in_list))
            call error (1, "mkweight: Number of output files must match number of input files")
                
        # Open the graphics device.
        call clgstr ("device", Memc[sx], SZ_LINE)
        gp = gopen (Memc[sx], NEW_FILE, STDGRAPH)
        
        # Get the marker type to use.
        mtype = mdict2gio (clgwrd ("marker", Memc[sx], SZ_LINE,
                                   GIO_MARKER_DICT))

        # Get the marker size.
        msize = clgetr ("msize")

        # For each input, determine the mask.
        interactive = true
        while (imtgetim (in_list, Memc[sx], SZ_PATHNAME) > 0) {
            i = imtgetim (out_list, Memc[sy], SZ_PATHNAME)

            # If still interactive, forget the last length.
            if (interactive)
                len = INDEFI
            
            # Operate over all the groups.
            in_grp_list = tp_open (Memc[sx], 0, n_groups)
            out_grp_list = tp_open (Memc[sy], n_groups, i)
            while (tp_fetch (in_grp_list, Memc[in_file])) {
                
                # Open input/output.
                input = immap (Memc[in_file], READ_ONLY, NULL)
                if (checkdim (input) > 1) {
                    call sprintf (Memc[sx], SZ_LINE, "mkweight: %s is not one dimensional")
                    call pargstr (Memc[in_file])
                    call error (1, Memc[sx])
                }
                in_data = imgl1r (input)

                bx = tp_fetch (out_grp_list, Memc[sx])
                output = immap (Memc[sx], NEW_COPY, input)
                out_data = impl1s (output)

                # If the length is unknown, open up new region/pixel lists.
                if (IS_INDEFI(len)) {
                    len = IM_LEN(input,1)
                    pl = pl_create (1, len, 1)
                    rl = ma_open_region (len)
                    call realloc (ones, len, TY_SHORT)
                    shx = 1
                    call amovks (shx, Mems[ones], len)
                }

                # Else, check consistency with previous lengths.
                else
                    if (len != IM_LEN(input,1)) {
                        call sprintf (Memc[sx], SZ_LINE,
                                      "mkweight: Image %s size %d is inconsistent with previous data %d")
                        call pargstr (Memc[in_file])
                        call pargi (IM_LEN(input,1))
                        call pargi (len)
                        call error (1, Memc[sx])
                    }

                # Plot and retrieve regions if interactive.
                if (interactive) {
                    
                    # plot the data.
                    call gclear (gp)
                    call ma_plot_data (gp, Memc[in_file], Memr[in_data], len)
                    
                    # Determine the y position of the region markers.
                    call ggwind (gp, rx, rx, ry, ry2)
                    y = ry + (.05 * (ry2 - ry))
                    
                    # Get the regions.
                    b = INDEFR
                    while (clgcur ("cursor", rx, ry, wcs, key,
                                   Memc[sx], SZ_LINE) != EOF) {
                        switch (key) {
                        case '?':
                            call gpagefile (gp,
                                            "ctools$doc/mkweight.key",
                                            NULL)
                        case 'a':
                            interactive = false
                            call ma_r2pl (rl, pl)
                            break
                            
                        case 'd':
                            call ma_delete_region (rl, rx)
                            
                            # Since a region has been deleted, Redraw the plot
                            # and the regions.
                            call gclear (gp)
                            call ma_plot_data (gp, Memc[in_file], Memr[in_data],
                                               len)
                            call ma_draw_all (rl, gp, y, mtype, msize)
                            
                            b = INDEFR
                            
                        case 'q':
                            break
                            
                        case 'r':
                            if (IS_INDEFR (b)) {
                                b = rx
                                call printf (" again\n")
                            } else {
                                ry = min (b, rx)
                                rx = max (b, rx)
                                call ma_add_region (rl, ry, rx)
                                call printf ("\n")
                                call ma_draw_region (gp, ry, rx, y, mtype,
                                                     msize)
                                b = INDEFR
                            }
                            
                        case 's':
                            b = INDEFR
                            call ma_add_region (rl, rx, rx)
                            call ma_draw_region (gp, rx, rx, y, mtype, msize)
                            
                        default:
                            call printf ("\007")
                            b = INDEFR
                        }
                    }
                }

                # If interactive, save and clear.
                # In both cases, the output is XOR'ed to make it a weight.
                # Technically using PIX_NOT(PIX_SRC) should do the same thing
                # but it doesn't work.  Sent complaint to iraf.
                if (interactive) {
                    call ma_r2pl (rl, pl)
                    call pl_glps (pl, 1, Mems[out_data], 1, len,
                                  PIX_SRC)
                    call axors (Mems[out_data], Mems[ones], Mems[out_data], len)
                    call ma_clear_region (rl)
                    call pl_clear (pl)
                }

                # Else, just save.
                else {
                    call pl_glps (pl, 1, Mems[out_data], 1, len,
                                  PIX_SRC)
                    call axors (Mems[out_data], Mems[ones], Mems[out_data], len)
                }
                
                # Close up everything.
                call imunmap (output)
                call imunmap (input)
            }

            # Close region/pixel lists if still interactive.
            if (interactive) {
                call ma_close_region (rl)
                call pl_close (pl)
            }
            
            # Close the group lists.
            call tp_close (out_grp_list)
            call tp_close (in_grp_list)
        }
        
        # Close region/pixel lists if non-interactive.
        if (!interactive) {
            call ma_close_region (rl)
            call pl_close (pl)
        }

        # That's all folks
        call mfree (ones, TY_SHORT)
        call imtclose (out_list)
        call imtclose (in_list)
        call gclose (gp)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_mkweight
#---------------------------------------------------------------------------
int procedure mdict2gio (mtype)

int     mtype                   # I:  Type from dictionary

int     gtype                   # GIO type.

begin
        switch (mtype) {
        case POINT:     gtype = GM_POINT
        case FILL:      gtype = GM_FILL
        case BOX:       gtype = GM_BOX
        case PLUS:      gtype = GM_PLUS
        case CROSS:     gtype = GM_CROSS
        case DIAMOND:   gtype = GM_DIAMOND
        case HLINE:     gtype = GM_HLINE
        case VLINE:     gtype = GM_VLINE
        case HEBAR:     gtype = GM_HEBAR
        case VEBAR:     gtype = GM_VEBAR
        case CIRCLE:    gtype = GM_CIRCLE
        default:
            call eprintf ("Unknown graphics marker type\n")
            gtype = GM_BOX
        }

        return (gtype)
end
#---------------------------------------------------------------------------
# End of mdict2gio
#---------------------------------------------------------------------------
