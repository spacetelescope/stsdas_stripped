include <pkg/gtools.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help fit Mar92 source
.ih
NAME
.nf
mk_open_fit     -- Open the fitting descriptor.
mk_close_fit    -- Close the fitting descriptor.
mk_alloc_fit    -- Setup the working arrays in the fit descriptor.
mk_alloc_mw     -- Allocate MWCS storage array.
mk_free_mw      -- Free MWCS descriptors.
mk_write_ic     -- Write ICFIT descriptor parameters to task pset.
.fi
.ih
USAGE
.nf
fit = mk_open_fit (function, order, low, high, niter, grow, mrej,
                   label, units)
call mk_close_fit (fit)
call mk_alloc_fit (length, fit)
call mk_alloc_mw (n_fits, fit)
call mk_free_mw (fit)
call mk_write_ic (fit)
.fi
.ih
ARGUMENTS
.ls fit (pointer)
The fit descriptor.  This contains the working arrays and
function/order list used to store/calculate fits.
.le
.ls function (char[ARB])
The default function to use, if no interactive fitting is performed or
all remaining spectra are to be fit to the last interactive session.
.le
.ls order (int)
The default order of function to use, if no interactive fitting is
performed or all remaining spectra are to be fit to the last
interactive session.
.le
.ls low (real :input)
The low sigma rejection.
.le
.ls high (real :input)
The high sigma rejection.
.le
.ls niter (int :input)
Number of iterations to par down the solution.
.le
.ls grow (real :input)
The growth rate.
.le
.ls mrej (bool :input)
TRUE to mark rejections.
.le
.ls label (char[ARB] :input)
The label to use for the wavelengths.
.le
.ls units (char[ARB] :input)
The units to user for the wavelenghts.
.le
.ls length (int)
The length of the data arrays to will be used in the fitting.
.le
.ls n_fits (int)
The number of CURFIT descriptors that will be needed.
.le
.ih
DESCRIPTION
These are all the utility routines for handling the FIT structure, as
defined in mkms.h.  The structure holds all the necessary information
for using the IC_FIT, CURFIT, and MWCS routines to fit the wavelengths either
interactively or with parameter-specified information.  There is an
MWCS descriptor (MW) for each wavelength table fit.  There is only
one IC_FIT descriptor (IC).  This descriptor holds the fit parameters.
The structure also holds the pointers to the data arrays, and graphics for
interactive curve fitting.  
.endhelp
#---------------------------------------------------------------------------
pointer procedure mk_open_fit (function, order, low, high, niter, grow, mrej,
                               label, units)

char    function[ARB]           # I:  The default function to use for fitting.
int     order                   # I:  The default order of function.
real    low                     # I:  The low sigma rejection.
real    high                    # I:  The high sigma rejection.
int     niter                   # I:  The number of iterations.
real    grow                    # I:  The growth.
bool    mrej                    # I:  TRUE to mark rejected pixels.
char    label[ARB]              # I:  Label for wavelengths.
char    units[ARB]              # I:  Units for wavelengths.


int     btoi()                  # Boolean to integer conversion.
pointer gt_init()               # Open the GTOOLS descriptor.
int     i                       # Generic.
pointer fit                     # The fit descriptor.
pointer sp                      # Stack pointer.
int     strdic()                # Find a string in a string dictionary.
int     strlen()                # Get length of string.
pointer tx                      # Generic string.

errchk  gt_init, ic_open, malloc

begin
        # Memory.
        call smark (sp)
        call salloc (tx, SZ_LINE, TY_CHAR)
        
        # Create the basic structure.
        call malloc (fit, MK_SZ_FIT, TY_STRUCT)

        # Set the initial function parameters.
        FIT_INIT_FUNC(fit) = strdic (function, Memc[tx], SZ_LINE, MK_FUNCTIONS)

        # Clear out other initial defaults.
        FIT_MAX_MW(fit) = 0
        FIT_N_MW(fit) = 0
        FIT_MW_PTR(fit) = NULL
        FIT_LEN(fit) = 0
        FIT_X_PTR(fit) = NULL
        FIT_Y_PTR(fit) = NULL
        FIT_W_PTR(fit) = NULL
        FIT_GP(fit) = NULL
        FIT_LARGE(fit) = 0

        # Create the interactive curve fitting structure.
        call ic_open (FIT_IC(fit))
        call ic_pstr (FIT_IC(fit), FUNCTION, function)
        call ic_puti (FIT_IC(fit), ORDER_IC, order)
        call ic_putr (FIT_IC(fit), LOW_IC, low)
        call ic_putr (FIT_IC(fit), HIGH_IC, high)
        call ic_puti (FIT_IC(fit), NITER, niter)
        call ic_putr (FIT_IC(fit), GROW, grow)
        call ic_puti (FIT_IC(fit), MREJ, btoi(mrej))
        call ic_pstr (FIT_IC(fit), "help", "stsdas$lib/scr/icgfit.key")

        # Create the gtools descriptor.
        FIT_GT(fit) = gt_init()
        call gt_setr (FIT_GT(fit), GTXMIN, INDEFR)
        call gt_setr (FIT_GT(fit), GTXMAX, INDEFR)
        call gt_setr (FIT_GT(fit), GTYMIN, INDEFR)
        call gt_setr (FIT_GT(fit), GTYMAX, INDEFR)
        call gt_sets (FIT_GT(fit), GTTYPE, "line")

        # Copy the label and units into the fit structure.
        i = strlen (label)
        call malloc (FIT_LABEL_PTR(fit), i, TY_CHAR)
        call strcpy (label, FIT_LABEL(fit), i)
        i = strlen (units)
        call malloc (FIT_UNITS_PTR(fit), i, TY_CHAR)
        call strcpy (units, FIT_UNITS(fit), i)

        # That's all folks.
        call sfree (sp)
        return (fit)
end
#---------------------------------------------------------------------------
# End of mk_open_fit
#---------------------------------------------------------------------------
procedure mk_close_fit (fit)

pointer fit             # IO: The fit descriptor, NULL on exit.

errchk  ic_closed, gt_free, mfree

begin
        call mfree (FIT_LABEL_PTR(fit), TY_CHAR)
        call mfree (FIT_UNITS_PTR(fit), TY_CHAR)

        call gt_free (FIT_GT(fit))
        call ic_closed (FIT_IC(fit))
        
        call mfree (FIT_W_PTR(fit), TY_DOUBLE)
        call mfree (FIT_Y_PTR(fit), TY_DOUBLE)
        call mfree (FIT_X_PTR(fit), TY_DOUBLE)

        call mfree (FIT_MW_PTR(fit), TY_POINTER)
        
        call mfree (fit, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of mk_close_fit
#---------------------------------------------------------------------------
procedure mk_alloc_fit (length, fit)

pointer fit                     # I:  The fit descriptor.
int     length                  # I:  The length of the arrays.

int     i                       # Generic.

errchk  realloc

begin
        if (length > FIT_LEN(fit)) {
            call realloc (FIT_X_PTR(fit), length, TY_DOUBLE)
            call realloc (FIT_Y_PTR(fit), length, TY_DOUBLE)
            call realloc (FIT_W_PTR(fit), length, TY_DOUBLE)

            # Load the X array with just a linear set of numbers.
            # Load the weight array with equal weights.
            do i = 1, length {
                FIT_X(fit,i) = i
                FIT_W(fit,i) = 1.0D0
            }

            FIT_LEN(fit) = length
        }
end
#---------------------------------------------------------------------------
# End of mk_alloc_fit
#---------------------------------------------------------------------------
procedure mk_alloc_mw (n_fits, fit)

pointer fit                     # I:  The FIT descriptor.
int     n_fits                  # I:  Number of fits to allocate for.

int     i                       # Generic.

errchk  realloc

begin
        FIT_N_MW(fit) = n_fits
        if (FIT_N_MW(fit) > FIT_MAX_MW(fit)) {
            FIT_MAX_MW(fit) = FIT_N_MW(fit)
            call realloc (FIT_MW_PTR(fit), FIT_MAX_MW(fit), TY_POINTER)
        }
        do i = 1, FIT_N_MW(fit)
            FIT_MW(fit,i) = NULL
end
#---------------------------------------------------------------------------
# End of mk_alloc_mw
#---------------------------------------------------------------------------
procedure mk_free_mw (fit)

pointer fit                     # I:  The FIT descriptor.

int     i                       # Generic.

begin
        do i = 1, FIT_N_MW(fit)
            iferr (call mw_close(FIT_MW(fit,i)))
                ;
        FIT_N_MW(fit) = 0
end
#---------------------------------------------------------------------------
# End of mk_free_mw
#---------------------------------------------------------------------------
procedure mk_write_ic (fit)

pointer fit                     # I:  The FIT descriptor.

real    ic_getr()               # Get real-valued IC parameter.
int     ic_geti()               # Get integer-valued IC parameter.
bool    itob()                  # Integer to boolean conversion.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

errchk  clpstr, clputb, clputi, clputr, ic_gstr

begin
        call smark(sp)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        call ic_gstr (FIT_IC(fit), FUNCTION, Memc[xstr], SZ_LINE)
        call clpstr (FUNCTION, Memc[xstr])
        call clputi (ORDER_PAR, ic_geti (FIT_IC(fit), ORDER_IC))
        call clputr (LOW_PAR, ic_getr (FIT_IC(fit), LOW_IC))
        call clputr (HIGH_PAR, ic_getr (FIT_IC(fit), HIGH_IC))
        call clputi (NITER, ic_geti (FIT_IC(fit), NITER))
        call clputr (GROW, ic_getr (FIT_IC(fit), GROW))
        call clputb (MREJ, itob(ic_geti (FIT_IC(fit), MREJ)))
        
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of mk_write_ic
#---------------------------------------------------------------------------
