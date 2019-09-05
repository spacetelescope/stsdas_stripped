include	<od.h>
include "poffsets.h"

# Define where to skip the loop.
define  skip_           10

#---------------------------------------------------------------------------
.help pof_poffsets Feb93 source
.ih
NAME
pof_poffsets -- Determine pixel offsets of a list of data by cross correlation.
.ih
USAGE
call pof_poffsets (p)
.ih
ARGUMENTS
.fs p (I: pointer)
The descriptor containing all the information, including the image lists,
wavelength list, correlation parameters, etc.
.fe
.endhelp
#---------------------------------------------------------------------------
procedure pof_poffsets (p)

pointer p                       # Main memory of information.

# Data I/O
int     group                   # Current group of input data.
int     imtgetim()              # Get next image from list.
pointer in                      # Current image descriptor.
pointer in_name                 # Current name of input file.
pointer od_map()                # Open specified OD file.
pointer wave                    # Current wavelength descriptor.

# Correlation/Shift parameters.
int     column                  # Current correlation column counter.
pointer corr                    # The correlation array.
int     corr_len                # Needed size of the correlation array.
int     corr_size               # Current size of the correlation array.
int     guess                   # Guess at the shift.
pointer in_data                 # Data array.
int     max_search_size         # Largest pixel offset calculable.
double  pof_int()               # Interactively determine shift.
double  pof_maxcorr             # Find shift from maximum of correlation.
int     pof_table_guess()       # Determine shift from wavelength tables.
int     pof_wcs_guess()         # Determine shift from WCS information.
int     search_size             # Pixel shift domain to search.
double  shift                   # Final shift.
int     wshift                  # Shift as determined by wavelength tables.
pointer zero_data               # Data for zero-point spectrum

# Misc.
int     i                       # Generic.
bool    itob()                  # Convert integer to boolean.
pointer sp                      # Stack Pointer.
pointer sx, sy                  # Generic string

begin
        call smark (sp)
        call salloc (in_name, SZ_LINE, TY_CHAR)
        call salloc (sx, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)
        call salloc (sy, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)

        # Get the zero point data.
        call salloc (zero_data, OD_LEN(ZERO(p)), TY_DOUBLE)
        call od_get (ZERO(p), Memd[zero_data])
        call pof_mnfilt (Memd[zero_data], OD_LEN(ZERO(p)), ZFW(p))
        
        # Loop over each group of each input image.
        max_search_size = OD_LEN(ZERO(p)) / 2 - 1
        in_data = NULL
        column = 0
        corr_size = 0
        while (imtgetim (IN_LIST(p), Memc[in_name], SZ_LINE) > 0) {

            # Open the file to correlate against.
            iferr (in = od_map (Memc[in_name], READ_ONLY, NULL)) {
                call sprintf (Memc[sx], SZ_LINE, "poffsets: cannot open input file %s")
                call pargstr (Memc[in_name])
                call error (1, Memc[sx])
            }

            # If the lengths of the zero-point and current data are
            # not the same, then punt.
            if (OD_LEN(ZERO(p)) != OD_LEN(in)) {
                call eprintf ("poffsets: data lengths are not the same for %s and %s\n")
                call pargstr (OD_NAME(ZERO(p)))
                call pargstr (OD_NAME(in))
                goto skip_
            }

            # If memory has not been allocated, do it now.
            if (in_data == NULL)
                call salloc (in_data, OD_LEN(in), TY_DOUBLE)

            # Get the next wavelength.
            # If there is no list, try to construct a file name based on
            # the current input file.  If the list exists, but is exhausted,
            # use the last image.
            if (WSOURCE(p) == WSOURCE_IMAGE) {
                if (WAVE_LIST(p) == NULL) {
                    call pof_change_ext (Memc[in_name], "c0h",
                                         Memc[sx], SZ_LINE)
                    iferr (wave = od_map (Memc[sx], READ_ONLY, NULL)) {
                        call sprintf (Memc[sy], SZ_LINE, "poffsets: cannot open wavelength file %s")
                        call pargstr (Memc[sx])
                        call error (1, Memc[sy])
                    }
                } else if (imtgetim (WAVE_LIST(p), Memc[sx], SZ_LINE) > 0) {
                    call od_unmap (wave)
                    iferr (wave = od_map (Memc[sx], READ_ONLY, NULL)) {
                        call sprintf (Memc[sy], SZ_LINE, "poffsets: cannot open wavelength file %s")
                        call pargstr (Memc[sx])
                        call error (1, Memc[sy])
                    }
                }
                if (OD_NGRP(wave) > 1)
                    call od_open_group (wave, 1)
            }
            
            # Loop through all the groups of the data.
            do group = 1, OD_NGRP(in) {

                # Open the group.
                if (group > 1)
                    call od_open_group (in, group)
                call od_get (in, Memd[in_data])

                if (VERBOSE(p) == YES) {
                    call printf ("Searching for shift of data %s[%d]:\n")
                    call pargstr (OD_NAME(in))
                    call pargi (OD_GRP(in))
                }
                
                # Apply filter.
                call pof_mnfilt (Memd[in_data], OD_LEN(in), FW(p))

                # If there is a wavelength file, open the next group.
                if (WSOURCE(p) == WSOURCE_IMAGE && group > 1) {
                    iferr (call od_open_group (wave, group)) {
                        call eprintf ("poffsets: Not enough groups in wavelengh %s\n")
                        call pargstr (OD_NAME(wave))
                        call eprintf ("  to match input %s, using last group of wavelenght\n")
                        call pargstr (OD_NAME(in))
                    }
                }

                # Find the wavelength shift if a source has been specified.
                switch (WSOURCE(p)) {
                case WSOURCE_IMAGE:
                    wshift = pof_table_guess (p, wave)
                case WSOURCE_WCS:
                    wshift = pof_wcs_guess (p, in)
                default:
                    wshift = INDEFI
                }

                # If using wavelengths to guess the shift, make sure there
                # is a valid one.  Set the search size.
                guess = INDEFI
                if (METHOD(p) == METHOD_WAVE) {
                    if (IS_INDEFI(wshift)) {
                        call eprintf ("poffsets: Using wavelength for guessing failed, using standard search\n")
                    } else {
                        guess = wshift
                        search_size = min (max_search_size, abs(guess) +
                                           DELTA(p))
                    }
                }

                # If no guess, set search size to whole window.
                if (IS_INDEFI(guess))
                    search_size = WINDOW(p)

                # Check to make sure the search size is reasonable.
                if (search_size > max_search_size ||
                    search_size <= 0) {
                    call eprintf ("poffsets: Search size %d is bad, using maximum %d\n")
                    call pargi (search_size)
                    call pargi (max_search_size)
                    search_size = max_search_size
                }
                
                # Get the shift.  Use correlation or just the guess.
                if (VERBOSE(p) == YES) {
                    if (!IS_INDEFI (guess)) {
                        call printf ("    predicted shift is %d\n")
                        call pargi (guess)
                    }
                }

                if (USECORR(p) == YES) {
                    corr_len = search_size * 2 + 1
                    i = corr_len * SECTION(p)
                    if (i > corr_size) {
                        corr_size = i
                        call realloc (corr, corr_size, TY_DOUBLE)
                    }
                    call pof_correlate (Memd[zero_data], Memd[in_data],
                                        OD_LEN(ZERO(p)), SECTION(p),
                                        search_size, Memd[corr])
                    
                    if (CORTAB(p) != NULL)
                        call pof_write_corr (CORTAB(p), Memd[corr], corr_len,
                                             SECTION(p), column)
                    
                    shift = pof_maxcorr (Memd[corr], corr_len, SECTION(p),
                                         NSIG(P), NITER(p), guess, DELTA(p),
                                         itob (VERBOSE(p)))

                    if (INTERACTIVE(p) == YES)
                        shift = pof_int (GP(p), Memd[corr], corr_len,
                                         SECTION(p), shift, guess,
                                         OD_NAME(in), group)
                }

                # Else, the guess is the shift
                else {
                    if (IS_INDEFI(guess) && USECORR(p) == NO) {
                        call eprintf ("poffsets: No guess found and usecorr=no, thus shift cannot be determined\n")
                        shift = INDEFD
                    } else
                        shift = guess
                }
                
                # Was a shift found?
                if (IS_INDEFD(shift)) {
                    call eprintf ("poffsets: cannot find shift for file %s[%d]\n")
                    call pargstr (OD_NAME(in))
                    call pargi (OD_GRP(in))
                } else {

                    # Write the results.  Write the wavelenghts depending
                    # on how they were determined.
                    switch (WSOURCE(p)) {
                    case WSOURCE_IMAGE:
                        call pof_write_output (OUTTAB(p), OD_NAME(in),
                                               OD_GRP(in), shift,
                                               OD_NAME(wave), OD_GRP(wave),
                                               wshift)
                    case WSOURCE_WCS:
                        call pof_write_output (OUTTAB(p), OD_NAME(in),
                                               OD_GRP(in), shift,
                                               "%%IMAGE_WCS%%", INDEFI,
                                               wshift)
                    default:
                        call pof_write_output (OUTTAB(p), OD_NAME(in),
                                               OD_GRP(in), shift,
                                               "", 0, 0)
                    }
                }
                
                # Flush output.
                call flush (STDOUT)
            }

skip_
            # Close the file.
            call od_unmap (in)
        }

        # That's all folks.
        call od_unmap (wave)
        call mfree (corr, TY_DOUBLE)
        call sfree (sp)

end
#---------------------------------------------------------------------------
# End of pof_poffsets
#---------------------------------------------------------------------------
