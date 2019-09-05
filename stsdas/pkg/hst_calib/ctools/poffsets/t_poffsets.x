include <gset.h>
include <od.h>
include "poffsets.h"

#---------------------------------------------------------------------------
.help t_poffsets Feb93 source
.ih
NAME
t_poffsets -- Main routine for task poffsets
.ih
DESCRIPTION
See the help for the task poffsets for full user information.
.ih
FEATURES
.ih
TO DO
- The input, zero_point, wavelengths, and zero_wave can all be either
images or tables.

- Add the wavelength searching.
.endhelp
#---------------------------------------------------------------------------
procedure t_poffsets

# POFFSETS descriptor.
pointer p                       # The descriptor.
pointer pof_open()              # Open the POFFSETS descriptor.

# Image/List I/O
int     imtgetim()              # Get next image name from list.
int     imtlen()                # Get number of items in image list.
pointer imtopen()               # Open an image list.
pointer od_map()                # Open ONED descriptor.
pointer pof_open_corr()         # Open the correlation outputs.
pointer pof_open_output()       # Open the output table.

# CL I/O
bool    clgetb()                # Get boolean-valued parameter.
int     clgeti()                # Get integer-valued parameter.
real    clgetr()                # Get real-valued parameter.
int     clgwrd()                # Get enumerated word value.

# Graphics I/O
pointer gopen()                 # Open the graphics device.

# Misc.
int     btoi()                  # Convert boolean to integer.
int     ix                      # Generic.
pointer method_par              # Method parameter value.       
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.
pointer sx                      # Generic string.
pointer zero_point              # Zero-point image name.

begin
        call smark (sp)
        call salloc (method_par, SZ_LINE, TY_CHAR)
        call salloc (sx, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)
        call salloc (zero_point, SZ_LINE, TY_CHAR)

        # Create the offsets memory structure.
        p = pof_open()
        
        # Get input list
        call clgstr ("input", Memc[sx], SZ_LINE)
        IN_LIST(p) = imtopen (Memc[sx])
        if (imtlen (IN_LIST(p)) <= 0)
            call error (1, "poffsets: No input files specified")

        # Get method and wavelength source and do some sanity checking.
        METHOD(p) = clgwrd ("method", Memc[method_par], SZ_LINE, METHOD_DICT)
        WSOURCE(p) = clgwrd ("wsource", Memc[sx], SZ_LINE, WSOURCE_DICT)
        if (METHOD(p) == METHOD_WAVE && WSOURCE(p) == WSOURCE_NONE)
            call error (1, "poffsets: Bad parameters: Need a wavelength source for method = 'wave'")

        # Various parameters.
        WINDOW(P) = clgeti ("window")
        DELTA(p) = clgeti ("delta")
        SECTION(p) = clgeti ("sections")
        FW(p) = clgeti ("fwidth")
        ZFW(p) = clgeti ("zero_fwidth")
        NITER(p) = clgeti ("niter")
        NSIG(p) = clgetr ("nsig")
        VERBOSE(p) = btoi (clgetb ("verbose"))
        call clgstr ("zero_point", Memc[zero_point], SZ_LINE)
        
        # Open output table.
        call clgstr ("output", Memc[sx], SZ_LINE)
        OUTTAB(p) = pof_open_output (Memc[sx], (WSOURCE(p) != WSOURCE_NONE),
				     Memc[zero_point])

        # Open the zero-point image.  If none is specified, use first file
        # from the input list.
        if (strlen (Memc[zero_point]) <= 0) {
            ix = imtgetim (IN_LIST(p), Memc[zero_point], SZ_LINE)
            call imtrew (IN_LIST(p))
        }
        iferr (ZERO(p) = od_map (Memc[zero_point], READ_ONLY, NULL)) {
            call sprintf (Memc[sx], SZ_LINE, "poffsets: cannot open zero-point file %s")
            call pargstr (Memc[zero_point])
            call error (1, Memc[sx])
        }

	# Set the zero point file in the table header.
	call pof_zero(OUTTAB(p), OD_NAME(ZERO(p)))

        # Get the wavelength list.  If no files are specified, close the
        # list to indicate that none are available.
        WAVE_LIST(p) = NULL
        if (WSOURCE(p) == WSOURCE_IMAGE) {
            call clgstr ("wavelength", Memc[sx], SZ_LINE)
            WAVE_LIST(p) = imtopen (Memc[sx])
            if (imtlen (WAVE_LIST(p)) <= 0)
                call imtclose (WAVE_LIST(p))
        }
        
        # Get the zero-point wavelength.  If none is specified, try to
        # use the HRS/FOS standard ".c0h" extension on the zero-point
        # image.  If that is not found, use the first wavelength file
        # from the "wavelength" list.  If the list is empty, give up.
        ZERO_WAVE(p) = NULL
        if (WSOURCE(p) == WSOURCE_IMAGE) {
            call clgstr ("zero_wave", Memc[sx], SZ_LINE)
            if (strlen (Memc[sx]) <= 0) {
                if (WAVE_LIST(p) != NULL) {
                    ix = imtgetim (WAVE_LIST(p), Memc[sx], SZ_LINE)
                    call imtrew (WAVE_LIST(p))
                } else {
                    call change_ext (Memc[zero_point], "c0h",
                                     Memc[sx], SZ_LINE)
                }
            }
            iferr (ZERO_WAVE(p) = od_map (Memc[sx], READ_ONLY, NULL))
                ;
        }
        
        # If looking at WCS information, make sure the zero point has
        # the information.
        if (WSOURCE(p) == WSOURCE_WCS) {
            if (OD_MW(ZERO(p)) == NULL)
                call error (1, "zero-point image has no WCS information, cannot use 'usewcs' option")
        }       

        # See if correlations should be calculated.  Can be no for only a few
        # methods.
        USECORR(p) = btoi (clgetb ("usecorr"))
        if (USECORR(p) == NO && METHOD(p) == METHOD_MAX) {
            call eprintf ("poffsets: Must use correlation with method %s\n")
            call pargstr (Memc[method_par])
            call error (1, "poffsets: Change method or set usecorr = yes")
        }
        
        # See if the user wants to interact.  If so, there must be correlations
        # to interact with.
        INTERACTIVE(p) = btoi (clgetb ("interactive"))
        if (INTERACTIVE(p) == YES && USECORR(p) == NO) {
            call eprintf ("poffsets: Cannot use interactive option when usecorr = no.\n")
            call eprintf ("poffsets: Disabling interactive option.\n")
            INTERACTIVE(p) = NO
        }
        GP(p) = NULL
        if (INTERACTIVE(p) == YES) {
            call clgstr ("device", Memc[sx], SZ_LINE)
            GP(p) = gopen (Memc[sx], NEW_FILE+AW_DEFER, STDGRAPH)
        }
        
        # Open correlation table.
        call clgstr ("correlations", Memc[sx], SZ_LINE)
        if (strlen (Memc[sx]) > 0 && USECORR(p) == YES)
            CORTAB(p) = pof_open_corr (Memc[sx])
        else
            CORTAB(p) = NULL

        # Find the pixel offsets with correlation.
        call pof_poffsets (p)
        
        # That's all folks.
        if (CORTAB(p) != NULL)
            call tbtclo (CORTAB(p))
        call pof_close_output (OUTTAB(p))
        if (WAVE_LIST(p) != NULL)
            call imtclose (WAVE_LIST(p))
        call od_unmap (ZERO_WAVE(p))
        call od_unmap (ZERO(p))
        call imtclose (IN_LIST(p))
        if (GP(p) != NULL)
            call gclose (GP(p))
        call pof_close (p)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_poffsets
#---------------------------------------------------------------------------
