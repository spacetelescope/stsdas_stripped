include <error.h>
include <math/curfit.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mkms Mar93 source
.ih
NAME
mkms -- Make MULTISPEC WCS from wavelength table information.
.ih
DESCRIPTION
This task takes input spectra and wavelength tables, uses iraf curfit
routines to fit the wavelength table, creates a MULTISPEC world
coordinate system (WCS) description of the fit and creates a new copy
of the input spectrum with the new WCS.
.ih
PROGRAM/INTERFACE ISSUES
This task makes extensive use of routines from gflib which may
constitute IRAF interface violations.

This task makes use of the imio call imparse, which is an interface
violation according to IRAF.  However, since IRAF does not provide the
need interface valid call, this violation is necessary.

This tasks makes use of the CURFIT package in IRAF.  Though there are
no interface violations, the package has been known to upgrade without
warning.

This task uses a modified copy of the routine icg_fitd from the IC_FIT
package under GTOOLS.  This necessitated the copying of two internal
include files, names.h and icfit.h.  Though no violations occur, if
there are drastic changes to the IC_FIT stuff, this may break.

For group parameters, the names of the keywords needs to be known.
Technically, we should never know what the keywords are for MWCS, but
it is necessary to add them.  So, if MWCS ever changes its interface,
this task will have to change.
.endhelp
#---------------------------------------------------------------------------
procedure t_mkms

# Misc.
pointer gopen()
int     i                       # Generic.
pointer sp                      # Stack pointer.
int     strlen()                # Get string length.
pointer xstr, xstr1, xstr2      # Generic.

# Input file information
pointer input                   # Image descriptor of current input file.
pointer lists                   # Lists descriptor.
pointer mk_alloc_lists()        # Open the input file lists.
pointer mk_map()                # Open an image/table.
bool    mk_next_lists()         # Get next files from input lists.
int     n_groups                # Number of groups in the input image.
pointer wave                    # Wave image/table descriptor.
pointer wght                    # Weight image/table descriptor.

# Parameter information
bool    clgetb()                # Get boolean-valued parameter.
int     clgeti()                # Get integer-valued parameter.
real    clgetr()                # Get real-valued parameter.
pointer coeff_fmt               # Coefficient format in header.
bool    interactive             # TRUE if interactive.
bool    verbose                 # Print optional information to standard out.

# Fit information.
pointer fit                     # Fit descriptor.
pointer mk_open_fit()           # Open the fit descriptor.

# Duplicated outputs.
string  donefile_str    "%s --> %s\n"
string  group_err       "ERROR: Wrong number of groups in %s and %s\n"

begin
        # Memory.
        call smark (sp)
        call salloc (coeff_fmt, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)
        call salloc (xstr1, SZ_LINE, TY_CHAR)
        call salloc (xstr2, SZ_LINE, TY_CHAR)
        
        # Open the data file lists.
        lists = mk_alloc_lists (N_LISTS)
        call clgstr ("input", MK_TEMPLATE(lists,INPUT), TMPLT_SIZE)
        call clgstr ("output", MK_TEMPLATE(lists,OUTPUT), TMPLT_SIZE)
        call clgstr ("wave", MK_TEMPLATE(lists,WAVE), TMPLT_SIZE)
        call clgstr ("weight", MK_TEMPLATE(lists,WEIGHT), TMPLT_SIZE)
        call mk_open_lists (lists)

        # Setup the fitting descriptor.
        call clgstr ("function", Memc[xstr], SZ_LINE)
        call clgstr ("label",Memc[xstr1], SZ_LINE)
        call clgstr ("units",Memc[xstr2], SZ_LINE)
        fit = mk_open_fit (Memc[xstr],
                           clgeti (ORDER_PAR),
                           clgetr (LOW_PAR),
                           clgetr (HIGH_PAR),
                           clgeti (NITER),
                           clgetr (GROW),
                           clgetb (MREJ),
                           Memc[xstr1], Memc[xstr2])

        # Get various other parameters.
        interactive = clgetb ("interactive")
        verbose = clgetb ("verbose")
        call clgstr ("format", Memc[coeff_fmt], SZ_LINE)
        call strcat (" ", Memc[coeff_fmt], SZ_LINE)

        # Open the graphics output if interactive fitting will be performed.
        FIT_GP(fit) = NULL
        if (interactive) {
            call clgstr ("device", Memc[xstr], SZ_LINE)
            FIT_GP(fit) = gopen (Memc[xstr], NEW_FILE, STDGRAPH)

            # The TABLE option of the function is not valid with interactive
            # fitting.
            if (FIT_INIT_FUNC(fit) == MK_TABLE)
                call error (1, "mkms: TABLE not valid for interactive fitting, using SPLINE3\n")
        }
        
        # For each input file, calculate the fit and create the MULTISPEC
        # MWCS.
        wave = NULL
        wght = NULL
        while (mk_next_lists (lists)) {

            # Open the input spectrum.
            input = mk_map (MK_FILE(lists,INPUT), READ_ONLY)

            # Get the number of groups, unless not all the groups will
            # be examined.
            n_groups = IO_NGRP(input)

            # Retrieve the wavelength file.  Check that sizes, etc. are the same.
            if (MK_NEW(lists,WAVE) == YES) {
                call mk_unmap (wave)
                wave = mk_map (MK_FILE(lists,WAVE), READ_ONLY)
            }
            if (IO_LEN(wave) != IO_LEN(input)){
                call eprintf ("ERROR: Wavelength data %s not same size as input %s\n")
                call pargstr (MK_FILE(lists,WAVE))
                call pargstr (MK_FILE(lists,INPUT))
                next
            }
            if (n_groups > 1 && IO_NGRP(wave) > 1 &&
                n_groups != IO_NGRP(wave)) {
                call eprintf (group_err)
                call pargstr (MK_FILE(lists,WAVE))
                call pargstr (MK_FILE(lists,INPUT))
                next
            }
            
            if (MK_NEW(lists,WEIGHT) == YES) {
                call mk_unmap (wght)
                wght = mk_map (MK_FILE(lists,WEIGHT), READ_ONLY)
            }
            if (wght != NULL) {
                if (IO_LEN(wght) != IO_LEN(input)){
                    call eprintf ("ERROR: Weight data %s not same size as input %s\n")
                    call pargstr (MK_FILE(lists,WEIGHT))
                    call pargstr (MK_FILE(lists,INPUT))
                    next
                }
                if (n_groups > 1 && IO_NGRP(wght) > 1 &&
                    n_groups != IO_NGRP(wght)) {
                    call eprintf (group_err)
                    call pargstr (MK_FILE(lists,WEIGHT))
                    call pargstr (MK_FILE(lists,INPUT))
                    next
                }
            }                
            # Setup the working arrays for the fitting.
            call mk_alloc_fit (IO_LEN(input), fit)

            # Print out what file we are dealing with.
            if (verbose) {
                if (strlen (MK_FILE(lists,OUTPUT)) <= 0) {
                    call printf (donefile_str)
                    call pargstr (MK_FILE(lists,INPUT))
                    call pargstr (MK_FILE(lists,INPUT))
                } else {
                    call printf (donefile_str)
                    call pargstr (MK_FILE(lists,INPUT))
                    call pargstr (MK_FILE(lists,OUTPUT))
                }
                call flush (STDOUT)
            }

            # If either the wavelengths or weights have changed, then refit.
            if (MK_NEW(lists,WAVE) == YES || MK_NEW(lists,WEIGHT) == YES ) {

                # Release the previous fits.
                call mk_free_mw (fit)
                
                # Since the fitting only involves the wavelength and weight
                # tables, determine the number of groups involved for just
                # these data.  If the number of groups involved is less than
                # the input group count, fill the fit descriptor with the
                # last fit.  If the function is a table, only allow
                # the one fit to be generated.
                if (wght != NULL)
                    i = IO_NGRP(wght)
                else
                    i = 0
                i = min(n_groups, max(IO_NGRP(wave), i))
                if (FIT_INIT_FUNC(fit) == MK_TABLE)
                    i = 1
                call mk_alloc_mw (i, fit)
                
                # If interactive, allow the user to determine the fits.
                # NOTE: For multigroup data, all the groups must be fit
                # in order to determine the maximum size of the MULTISPEC
                # keywords to place in the group parameter block.
                iferr (call mk_dofit (wave, wght, fit, Memc[coeff_fmt],
                                      interactive))
                    call erract (EA_WARN)
                
                # Determine the "largest" function/order pair.
                call mk_large (fit)
            }
            
            # If necessary, copy the data and add group parameters.
            call mk_copy (MK_FILE(lists,OUTPUT), n_groups, fit, input)

            # Perform the fit with the specified function/order.
            iferr (call mk_ms (input, n_groups, fit))
                call erract (EA_WARN)

            # Close the data file.
            call mk_unmap (input)

        }

        # If interactive, write back the latest parameters for the fit.
        if (interactive)
            call mk_write_ic (fit)

        # That's all folks.
        call mk_free_mw (fit)
        call mk_unmap (wave)
        call mk_unmap (wght)
        if (FIT_GP(fit) != NULL)
            call gclose (FIT_GP(fit))
        call mk_close_fit (fit)
        call mk_close_lists (lists)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_mkms
#---------------------------------------------------------------------------
