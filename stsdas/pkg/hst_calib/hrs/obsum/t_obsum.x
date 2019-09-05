include <imio.h>
include "obsum.h"

# Memory management definitions.
define  CHAR            Memc[Memi[$1+$2-1]]
define  GROUP           Memi[group+$1-1]
define  IMAGE           Memi[image+$1-1]
define  TIME            Memd[time+$1-1]

# Current period of the orbit.
define  CURRENT_PERIOD  97

#---------------------------------------------------------------------------
.help obsum Jun92 hrs
.ih
NAME
obsum -- Create a summary report of hrs observations.
.endhelp
#---------------------------------------------------------------------------
procedure t_obsum

# Declarations.
double  djunk                   # Double junk.
int     period                  # Orbital period.
double  stime                   # Start of observation.

int     i, j                    # Generics.
int     n_ext                   # Total number of extensions.
int     n_images                # Number of images to order.
int     shp_group               # How many shp groups have been dealt with.
int     total                   # Total number of groups output per obs.
int     udl_group               # Number of UDL groups seen per zobnum.

bool	show_diode		# Show science diodes.
bool    show_dop                # Check on doppler/idling.
bool    show_groups             # Show general group report.

pointer diodeout		# Output for diode report.
pointer dopout                  # Output for doppler stuff.
pointer ext_list                # The array of extensions.
pointer file_list               # List of file names.
pointer group                   # Array of current groups.
pointer image                   # Array of image descriptors.
pointer input                   # The input observation template
pointer list                    # List of SHP files.
pointer out                     # The output descriptor.
pointer root                    # Root observation name.
pointer sp                      # Stack pointer
pointer time                    # Array of packet times.
pointer title                   # The title string.
pointer sx			# String holder.

string  title_fmt       "\n              TIME         W11/14 ZOBNUM PTYPE PFC SYS CARPOS INTE COADDS   SUM\n"
string  obs_start_fmt   "%9.9s                       %3d SHP       %3d UDL       %3d SDP\n"

# Function prototypes.
int     imtgetim(), obs_get_next(), strlen()
bool    clgetb(), obs_aindef()
pointer immap(), imtopen(), obs_open_out()

begin
        call smark (sp)
        call salloc (input, SZ_LINE, TY_CHAR)
        call salloc (root, SZ_LINE, TY_CHAR)
        call salloc (title, SZ_LINE, TY_CHAR)
        call salloc (sx, SZ_LINE, TY_CHAR)

        # Get the parameters.
        call clgstr ("input", Memc[input], SZ_LINE)
        call clgstr ("title", Memc[title], SZ_LINE)
        show_groups = clgetb ("show_groups")
        show_dop = clgetb ("doppler")
	show_diode = clgetb ("diode")
        period = CURRENT_PERIOD

        # Parse the extensions.
        call obs_ext_parse (ZEXTENSIONS, SZ_LINE, ext_list, n_ext)

        # Allocate arrays necessary to hold info from each extension for
        # the data set.
        call obs_char_alloc (SZ_LINE, n_ext, file_list)
        call calloc (image, n_ext, TY_INT)
        call calloc (time, n_ext, TY_DOUBLE)
        call calloc (group, n_ext, TY_INT)

        # Create the output string.
        out = obs_open_out()
        dopout = obs_open_out()
	diodeout = obs_open_out()
        
        # Open the list of files.  This is based on the SHP files.
        call strcpy (Memc[input], Memc[sx], SZ_LINE)
        call strcat (CHAR(ext_list,ZSHP), Memc[sx], SZ_LINE)
        list = imtopen (Memc[sx])

        # Write out the title.
        call printf ("%s")
        call pargstr (Memc[title])
        if (show_groups) 
            call printf (title_fmt)
        else
            call printf ("\n")

        # Go through the list of files containing the initial extension.
        # Print out the information for each observation set.
        while (imtgetim (list, CHAR(file_list,1), SZ_LINE) != EOF) {

            # Construct the names of the other files and retrieve the root.
            i = strlen (CHAR(file_list,1)) - strlen (CHAR(ext_list,1))
            call fnroot (CHAR(file_list,1), Memc[root], SZ_LINE)
            do j = 2, n_ext {
                call strcpy (CHAR(file_list,1), CHAR(file_list,j), i)
                call strcat (CHAR(ext_list,j), CHAR(file_list,j), SZ_LINE)
            }
            
            # Open all the images.
            do i = 1, n_ext
                iferr (IMAGE(i) = immap (CHAR(file_list,i), READ_ONLY, 0))
                    IMAGE(i) = NULL

            # Now, output the observation name and number of groups in
            # each file.
	    if (show_groups) {
		call printf (obs_start_fmt)
		call pargstr (Memc[root])
		if (IMAGE(ZSHP) != NULL)
		    call pargi (IM_CLSIZE(IMAGE(ZSHP)))
		else
		    call pargi (0)
		if (IMAGE(ZUDL) != NULL)
		    call pargi (IM_CLSIZE(IMAGE(ZUDL)))
		else
		    call pargi (0)
		if (IMAGE(ZSCI) != NULL)
		    call pargi (IM_CLSIZE(IMAGE(ZSCI)))
		else
		    call pargi (0)
	    }

            # The point of all this is to order all the groups of the
            # images in a time-increasing order.  Write out the information
            # for whatever group has the earliest time.

            # Not all extensions are included in the group to look for packet
            # times.  The non-packet extensions should be the last one in the
            # list.  Ignore them.
            n_images = n_ext - ZNO_PACKET
            
            # Get all the packet times
            do i = 1, n_images {
                GROUP(i) = 0
                call obs_get_time (IMAGE(i), GROUP(i), TIME(i))
            }

            # Keep outputting information until all the times have been
            # exhausted.
            total = 0
            shp_group = 0
            udl_group = 0
            stime = INDEFI
            while (!obs_aindef (TIME(1), n_images)) {

                # Increment number of output lines.
                total = total + 1
                i = obs_get_next (TIME(1), n_images)

                # If an shp, start a new group.
                if (i == ZSHP) {

                    # Flush stats for previous SHP group.
                    call obs_flush_out (out)
                    call obs_flush_out (dopout)
		    call obs_flush_out (diodeout)
                    shp_group = shp_group + 1
                    udl_group = 0
                }
                
                # Look for the UDL`s.  The pair for each observation number
                # bracket the acual observation.
                if (i == ZUDL)
                    if (udl_group == 0) {
                        stime = TIME(i)
                        udl_group = udl_group + 1
                    } else if (udl_group == 1 && show_dop) {
                        call obs_doppler (IMAGE(1), stime, TIME(i),
                                          period, shp_group, dopout)
                        udl_group = udl_group + 1
                    }

                # Print out information for the groups if requested.
                if (show_groups)
                    call obs_info (i, IMAGE(1), TIME(1), shp_group, total, out)

		# Print out information about the diodes if requested.
		if (show_diode && i == ZSCI)
		    call obs_diode (IMAGE(1), TIME(1), shp_group,
				    Memc[root], diodeout)

		# Get next time.
                call obs_get_time (IMAGE(i), GROUP(i), TIME(i))

                # If this was the science data, open the next group
                # of the extracted data also.
                if (i==ZSCI && !IS_INDEFD(TIME(i)) && IMAGE(ZX0H) != NULL)
                    call gf_opengr (IMAGE(ZX0H), GROUP(i), djunk, djunk, 0)
            }

            # Make sure the last infor was output.
            call obs_flush_out (out)
            call obs_flush_out (dopout)
	    call obs_flush_out (diodeout)
            call flush (STDOUT)

            # Close the images
            do i = 1, n_ext
                if (IMAGE(i) != NULL)
                    call imunmap (IMAGE(i))
            
        }

        # That's all folks.
        call imtclose (list)
        call obs_close_out (dopout)
        call obs_close_out (out)
        call mfree (group, TY_INT)
        call mfree (time, TY_DOUBLE)
        call mfree (image, TY_INT)
        call obs_char_free (file_list, n_ext)
        call obs_char_free (ext_list, n_ext)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_obsum
#---------------------------------------------------------------------------
